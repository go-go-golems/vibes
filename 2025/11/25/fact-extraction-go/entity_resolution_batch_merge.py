#!/usr/bin/env python3
"""
Entity Resolution with LLM-based Batch Merging

Implements Phase 3 of the Entity Resolution Enhancement Proposal:
- Batch merging instead of pairwise comparisons
- Globally consistent merge decisions
- Structured JSON output

Author: Manus AI
Date: November 19, 2025
"""

import sqlite3
import json
import os
from typing import List, Dict, Set, Tuple
from dataclasses import dataclass, asdict
from openai import OpenAI
import logging

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class EntityInfo:
    """Information about an entity for merging"""
    name: str
    mention_count: int
    contexts: List[str]
    relationships: List[str]
    doc_ids: List[str]

@dataclass
class MergeGroup:
    """A group of entities that should be merged"""
    canonical_name: str
    aliases: List[str]
    confidence: float
    reason: str

class LLMBatchMerger:
    """
    LLM-based batch entity merging for globally consistent resolution
    """
    
    def __init__(self, db_path: str, candidates_file: str, model: str = "gpt-4.1-mini"):
        self.db_path = db_path
        self.candidates_file = candidates_file
        self.model = model
        self.client = None
        self.entity_info = {}
        
    def _init_client(self):
        """Initialize OpenAI client"""
        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            raise ValueError("OPENAI_API_KEY environment variable not set")
        
        base_url = os.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1")
        
        self.client = OpenAI(api_key=api_key, base_url=base_url)
        logger.info(f"Initialized OpenAI client with model: {self.model}")
    
    def _load_entity_info(self):
        """Load entity information from database"""
        logger.info(f"Loading entity information from {self.db_path}")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Get all entities with their contexts
        cursor.execute("""
            SELECT actor, action, doc_id
            FROM rdf_triples
            WHERE actor IS NOT NULL
            UNION ALL
            SELECT target, action, doc_id
            FROM rdf_triples
            WHERE target IS NOT NULL
        """)
        
        entity_data = {}
        for row in cursor.fetchall():
            entity, action, doc_id = row
            if entity not in entity_data:
                entity_data[entity] = {
                    'contexts': [],
                    'relationships': [],
                    'doc_ids': set(),
                    'mention_count': 0
                }
            
            entity_data[entity]['contexts'].append(action)
            entity_data[entity]['relationships'].append(action)
            entity_data[entity]['doc_ids'].add(doc_id)
            entity_data[entity]['mention_count'] += 1
        
        # Convert to EntityInfo objects
        for name, data in entity_data.items():
            self.entity_info[name] = EntityInfo(
                name=name,
                mention_count=data['mention_count'],
                contexts=list(set(data['contexts']))[:5],  # Top 5 unique contexts
                relationships=list(set(data['relationships']))[:5],
                doc_ids=list(data['doc_ids'])[:10]
            )
        
        conn.close()
        logger.info(f"Loaded information for {len(self.entity_info)} entities")
    
    def _load_candidates(self) -> Dict[str, List[Tuple[str, float]]]:
        """Load candidate pairs from JSON file"""
        logger.info(f"Loading candidates from {self.candidates_file}")
        
        with open(self.candidates_file, 'r') as f:
            candidates = json.load(f)
        
        logger.info(f"Loaded candidates for {len(candidates)} entities")
        return candidates
    
    def _build_merge_prompt(self, entity_group: List[str]) -> str:
        """Build prompt for LLM to merge a group of entities"""
        
        prompt = """You are an expert entity resolution system. Your task is to identify which entities in the following list refer to the same real-world person, organization, or thing.

For each entity, I will provide:
- Name
- Mention count (how many times it appears)
- Sample contexts (actions/relationships)
- Sample document IDs

Your task:
1. Identify groups of entities that refer to the same real-world entity
2. For each group, select the most canonical/complete name
3. Provide a confidence score (0.0-1.0) for each merge decision
4. Explain your reasoning

Return your answer as a JSON array of merge groups:
```json
[
  {
    "canonical_name": "The best/most complete name for this entity",
    "aliases": ["name1", "name2", "name3"],
    "confidence": 0.95,
    "reason": "Brief explanation of why these are the same entity"
  }
]
```

Only include groups with 2+ entities. If an entity doesn't match any others, don't include it.

Entities to analyze:

"""
        
        for entity_name in entity_group:
            info = self.entity_info.get(entity_name)
            if not info:
                continue
            
            prompt += f"\n---\nName: {info.name}\n"
            prompt += f"Mentions: {info.mention_count}\n"
            prompt += f"Sample contexts: {', '.join(info.contexts[:3])}\n"
            prompt += f"Sample docs: {', '.join(info.doc_ids[:3])}\n"
        
        prompt += "\n---\n\nProvide your merge groups as JSON:"
        
        return prompt
    
    def _call_llm(self, prompt: str) -> List[MergeGroup]:
        """Call LLM to get merge decisions"""
        
        if self.client is None:
            self._init_client()
        
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "You are an expert entity resolution system. Always respond with valid JSON."},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.0,
                response_format={"type": "json_object"}
            )
            
            content = response.choices[0].message.content
            
            # Parse JSON response
            try:
                data = json.loads(content)
                
                # Handle both array and object responses
                if isinstance(data, dict):
                    if 'merge_groups' in data:
                        groups_data = data['merge_groups']
                    elif 'groups' in data:
                        groups_data = data['groups']
                    else:
                        # Assume the dict itself is a single group
                        groups_data = [data]
                else:
                    groups_data = data
                
                merge_groups = []
                for group in groups_data:
                    merge_groups.append(MergeGroup(
                        canonical_name=group['canonical_name'],
                        aliases=group['aliases'],
                        confidence=group.get('confidence', 0.9),
                        reason=group.get('reason', '')
                    ))
                
                return merge_groups
                
            except json.JSONDecodeError as e:
                logger.error(f"Failed to parse LLM response as JSON: {e}")
                logger.error(f"Response content: {content}")
                return []
                
        except Exception as e:
            logger.error(f"LLM call failed: {e}")
            return []
    
    def _build_candidate_groups(self, candidates: Dict[str, List[Tuple[str, float]]]) -> List[Set[str]]:
        """Build groups of entities that should be considered together"""
        
        # Use union-find to build connected components
        parent = {}
        
        def find(x):
            if x not in parent:
                parent[x] = x
            if parent[x] != x:
                parent[x] = find(parent[x])
            return parent[x]
        
        def union(x, y):
            px, py = find(x), find(y)
            if px != py:
                parent[px] = py
        
        # Build connections
        for entity, cands in candidates.items():
            for cand_name, score in cands:
                union(entity, cand_name)
        
        # Group by root
        groups = {}
        for entity in parent.keys():
            root = find(entity)
            if root not in groups:
                groups[root] = set()
            groups[root].add(entity)
        
        # Filter to groups with 2+ entities
        result = [group for group in groups.values() if len(group) >= 2]
        
        logger.info(f"Built {len(result)} candidate groups")
        return result
    
    def run(self, max_group_size: int = 10) -> List[MergeGroup]:
        """
        Run the complete batch merging pipeline
        
        Args:
            max_group_size: Maximum entities to consider in a single LLM call
            
        Returns:
            List of merge groups
        """
        # Load data
        self._load_entity_info()
        candidates = self._load_candidates()
        
        # Build candidate groups
        candidate_groups = self._build_candidate_groups(candidates)
        
        # Process each group
        all_merge_groups = []
        total_cost = 0.0
        
        for i, group in enumerate(candidate_groups):
            logger.info(f"Processing group {i+1}/{len(candidate_groups)} ({len(group)} entities)")
            
            # Split large groups
            group_list = list(group)
            for j in range(0, len(group_list), max_group_size):
                subgroup = group_list[j:j+max_group_size]
                
                # Build prompt
                prompt = self._build_merge_prompt(subgroup)
                
                # Call LLM
                merge_groups = self._call_llm(prompt)
                all_merge_groups.extend(merge_groups)
                
                # Estimate cost (rough approximation)
                tokens_in = len(prompt) // 4
                tokens_out = 200
                cost = (tokens_in * 0.15 / 1_000_000) + (tokens_out * 0.60 / 1_000_000)
                total_cost += cost
        
        # Save results
        output_file = "entity_merge_groups.json"
        with open(output_file, 'w') as f:
            json.dump([asdict(g) for g in all_merge_groups], f, indent=2)
        
        logger.info(f"Found {len(all_merge_groups)} merge groups")
        logger.info(f"Estimated cost: ${total_cost:.4f}")
        logger.info(f"Results saved to {output_file}")
        
        # Print statistics
        self._print_statistics(all_merge_groups)
        
        return all_merge_groups
    
    def _print_statistics(self, merge_groups: List[MergeGroup]):
        """Print statistics about merge results"""
        
        total_entities = sum(len(g.aliases) for g in merge_groups)
        avg_confidence = sum(g.confidence for g in merge_groups) / len(merge_groups) if merge_groups else 0
        
        print("\n" + "="*60)
        print("LLM-BASED BATCH MERGING STATISTICS")
        print("="*60)
        print(f"Merge groups found: {len(merge_groups)}")
        print(f"Total entities merged: {total_entities}")
        print(f"Average confidence: {avg_confidence:.3f}")
        
        # Show examples
        print("\nExample merge groups (top 5):")
        for i, group in enumerate(merge_groups[:5]):
            print(f"\n{i+1}. {group.canonical_name}")
            print(f"   Aliases: {', '.join(group.aliases)}")
            print(f"   Confidence: {group.confidence:.3f}")
            print(f"   Reason: {group.reason}")
        print("="*60)
    
    def apply_merges_to_db(self, merge_groups: List[MergeGroup]):
        """Apply merge decisions to the database"""
        logger.info("Applying merges to database...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        total_updates = 0
        for group in merge_groups:
            canonical = group.canonical_name
            
            for alias in group.aliases:
                if alias == canonical:
                    continue
                
                # Update actors
                cursor.execute("""
                    UPDATE rdf_triples
                    SET actor = ?
                    WHERE actor = ?
                """, (canonical, alias))
                total_updates += cursor.rowcount
                
                # Update targets
                cursor.execute("""
                    UPDATE rdf_triples
                    SET target = ?
                    WHERE target = ?
                """, (canonical, alias))
                total_updates += cursor.rowcount
        
        conn.commit()
        conn.close()
        
        logger.info(f"Applied {total_updates} updates to database")
        print(f"\n✅ Successfully updated {total_updates} entity references in database")


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="LLM-based batch entity merging")
    parser.add_argument("--db", default="fact_extraction.db", help="Path to SQLite database")
    parser.add_argument("--candidates", default="entity_candidates_embeddings.json", help="Candidates JSON file")
    parser.add_argument("--model", default="gpt-4.1-mini", help="LLM model to use")
    parser.add_argument("--apply", action="store_true", help="Apply merges to database")
    parser.add_argument("--max-group-size", type=int, default=10, help="Max entities per LLM call")
    
    args = parser.parse_args()
    
    # Run batch merging
    merger = LLMBatchMerger(args.db, args.candidates, args.model)
    merge_groups = merger.run(max_group_size=args.max_group_size)
    
    # Apply to database if requested
    if args.apply:
        merger.apply_merges_to_db(merge_groups)


if __name__ == "__main__":
    main()
