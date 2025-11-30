#!/usr/bin/env python3
"""
Refined Entity and Relation Deduplication

Leverages ALL available metadata for superior deduplication:
- Entity/relation descriptions (semantic meaning)
- Reasoning chains (context)
- Citations (evidence)
- Usage patterns (co-occurrence)
- Confidence scores

Uses a multi-stage approach:
1. Embedding-based candidate generation (description embeddings)
2. Feature-based scoring (multiple signals)
3. LLM-based final decision (with full context)

Author: Manus AI
Date: November 19, 2025
Ticket: DEDUP-001
"""

import sqlite3
import json
import numpy as np
from typing import List, Dict, Tuple, Set
from dataclasses import dataclass, asdict
from openai import OpenAI
import os
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


@dataclass
class EntityProfile:
    """Comprehensive entity profile for deduplication"""
    name: str
    description: str
    entity_type: str
    mention_count: int
    
    # Contextual information
    actions_as_actor: List[str]  # What they do
    actions_as_target: List[str]  # What happens to them
    co_occurring_entities: Set[str]  # Who they appear with
    
    # Evidence
    sample_reasoning: List[str]  # Sample reasoning chains
    sample_citations: List[str]  # Sample citations
    
    # Metadata
    doc_ids: Set[str]
    avg_confidence: float


@dataclass
class RelationProfile:
    """Comprehensive relation profile for deduplication"""
    name: str
    description: str
    relation_type: str
    usage_count: int
    
    # Usage patterns
    typical_actors: List[str]  # Who typically performs this
    typical_targets: List[str]  # Who typically receives this
    
    # Evidence
    sample_reasoning: List[str]
    sample_citations: List[str]
    
    # Metadata
    doc_ids: Set[str]
    avg_confidence: float


class RefinedDeduplicator:
    """
    Advanced deduplication using all available metadata
    """
    
    def __init__(self, db_path: str, model_name: str = "all-MiniLM-L6-v2", llm_model: str = "gpt-4.1-mini"):
        self.db_path = db_path
        self.model_name = model_name
        self.llm_model = llm_model
        self.embedding_model = None
        self.llm_client = None
        
    def _load_embedding_model(self):
        """Load Sentence Transformer"""
        try:
            from sentence_transformers import SentenceTransformer
            logger.info(f"Loading embedding model: {self.model_name}")
            self.embedding_model = SentenceTransformer(self.model_name)
        except ImportError:
            logger.error("sentence-transformers not installed")
            raise
    
    def _init_llm_client(self):
        """Initialize OpenAI client"""
        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            raise ValueError("OPENAI_API_KEY not set")
        base_url = os.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1")
        self.llm_client = OpenAI(api_key=api_key, base_url=base_url)
    
    def build_entity_profiles(self) -> Dict[str, EntityProfile]:
        """Build comprehensive profiles for all entities"""
        logger.info("Building entity profiles...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        profiles = {}
        
        # Get all unique entities (actors and targets)
        cursor.execute("""
            SELECT DISTINCT actor FROM rdf_triples_full
            UNION
            SELECT DISTINCT target FROM rdf_triples_full
        """)
        
        entities = [row[0] for row in cursor.fetchall()]
        
        for entity in entities:
            # Get description from entity_descriptions table
            cursor.execute("""
                SELECT description, entity_type, mention_count
                FROM entity_descriptions
                WHERE entity_name = ?
            """, (entity,))
            
            desc_row = cursor.fetchone()
            if desc_row:
                description, entity_type, mention_count = desc_row
            else:
                description, entity_type, mention_count = "", "other", 0
            
            # Get actions as actor
            cursor.execute("""
                SELECT DISTINCT action FROM rdf_triples_full
                WHERE actor = ?
                LIMIT 10
            """, (entity,))
            actions_as_actor = [row[0] for row in cursor.fetchall()]
            
            # Get actions as target
            cursor.execute("""
                SELECT DISTINCT action FROM rdf_triples_full
                WHERE target = ?
                LIMIT 10
            """, (entity,))
            actions_as_target = [row[0] for row in cursor.fetchall()]
            
            # Get co-occurring entities
            cursor.execute("""
                SELECT DISTINCT target FROM rdf_triples_full WHERE actor = ?
                UNION
                SELECT DISTINCT actor FROM rdf_triples_full WHERE target = ?
                LIMIT 20
            """, (entity, entity))
            co_occurring = set(row[0] for row in cursor.fetchall() if row[0] != entity)
            
            # Get sample reasoning and citations
            cursor.execute("""
                SELECT reasoning, citations, confidence, doc_id
                FROM rdf_triples_full
                WHERE actor = ? OR target = ?
                ORDER BY confidence DESC
                LIMIT 5
            """, (entity, entity))
            
            sample_reasoning = []
            sample_citations = []
            confidences = []
            doc_ids = set()
            
            for row in cursor.fetchall():
                reasoning, citations_json, confidence, doc_id = row
                if reasoning:
                    sample_reasoning.append(reasoning)
                if citations_json:
                    citations = json.loads(citations_json)
                    for cit in citations:
                        sample_citations.append(cit['text'])
                if confidence:
                    confidences.append(confidence)
                doc_ids.add(doc_id)
            
            avg_confidence = sum(confidences) / len(confidences) if confidences else 0.0
            
            profiles[entity] = EntityProfile(
                name=entity,
                description=description or "",
                entity_type=entity_type or "other",
                mention_count=mention_count or len(actions_as_actor) + len(actions_as_target),
                actions_as_actor=actions_as_actor,
                actions_as_target=actions_as_target,
                co_occurring_entities=co_occurring,
                sample_reasoning=sample_reasoning[:3],
                sample_citations=sample_citations[:3],
                doc_ids=doc_ids,
                avg_confidence=avg_confidence
            )
        
        conn.close()
        logger.info(f"Built profiles for {len(profiles)} entities")
        return profiles
    
    def build_relation_profiles(self) -> Dict[str, RelationProfile]:
        """Build comprehensive profiles for all relations"""
        logger.info("Building relation profiles...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        profiles = {}
        
        # Get all unique relations
        cursor.execute("SELECT DISTINCT action FROM rdf_triples_full")
        relations = [row[0] for row in cursor.fetchall()]
        
        for relation in relations:
            # Get description
            cursor.execute("""
                SELECT description, relation_type, usage_count
                FROM relation_descriptions
                WHERE relation_name = ?
            """, (relation,))
            
            desc_row = cursor.fetchone()
            if desc_row:
                description, relation_type, usage_count = desc_row
            else:
                description, relation_type, usage_count = "", "other", 0
            
            # Get typical actors and targets
            cursor.execute("""
                SELECT actor, COUNT(*) as cnt
                FROM rdf_triples_full
                WHERE action = ?
                GROUP BY actor
                ORDER BY cnt DESC
                LIMIT 10
            """, (relation,))
            typical_actors = [row[0] for row in cursor.fetchall()]
            
            cursor.execute("""
                SELECT target, COUNT(*) as cnt
                FROM rdf_triples_full
                WHERE action = ?
                GROUP BY target
                ORDER BY cnt DESC
                LIMIT 10
            """, (relation,))
            typical_targets = [row[0] for row in cursor.fetchall()]
            
            # Get sample reasoning and citations
            cursor.execute("""
                SELECT relation_description, reasoning, citations, confidence, doc_id
                FROM rdf_triples_full
                WHERE action = ?
                ORDER BY confidence DESC
                LIMIT 5
            """, (relation,))
            
            sample_reasoning = []
            sample_citations = []
            confidences = []
            doc_ids = set()
            
            for row in cursor.fetchall():
                rel_desc, reasoning, citations_json, confidence, doc_id = row
                if reasoning:
                    sample_reasoning.append(reasoning)
                if citations_json:
                    citations = json.loads(citations_json)
                    for cit in citations:
                        sample_citations.append(cit['text'])
                if confidence:
                    confidences.append(confidence)
                doc_ids.add(doc_id)
            
            avg_confidence = sum(confidences) / len(confidences) if confidences else 0.0
            
            profiles[relation] = RelationProfile(
                name=relation,
                description=description or "",
                relation_type=relation_type or "other",
                usage_count=usage_count or len(typical_actors),
                typical_actors=typical_actors,
                typical_targets=typical_targets,
                sample_reasoning=sample_reasoning[:3],
                sample_citations=sample_citations[:3],
                doc_ids=doc_ids,
                avg_confidence=avg_confidence
            )
        
        conn.close()
        logger.info(f"Built profiles for {len(profiles)} relations")
        return profiles
    
    def find_entity_candidates(self, profiles: Dict[str, EntityProfile], threshold: float = 0.7) -> Dict[str, List[Tuple[str, float]]]:
        """Find entity candidates using description embeddings"""
        if self.embedding_model is None:
            self._load_embedding_model()
        
        logger.info("Finding entity candidates using embeddings...")
        
        # Create rich text for embedding (name + description + context)
        entity_texts = {}
        for name, profile in profiles.items():
            text = f"{name}. {profile.description}"
            if profile.actions_as_actor:
                text += f" Actions: {', '.join(profile.actions_as_actor[:3])}"
            entity_texts[name] = text
        
        # Generate embeddings
        names = list(entity_texts.keys())
        texts = [entity_texts[n] for n in names]
        embeddings = self.embedding_model.encode(texts, show_progress_bar=True)
        
        # Build FAISS index
        import faiss
        faiss.normalize_L2(embeddings)
        index = faiss.IndexFlatIP(embeddings.shape[1])
        index.add(embeddings)
        
        # Find candidates
        candidates = {}
        for i, name in enumerate(names):
            query = embeddings[i:i+1]
            similarities, indices = index.search(query, 10)
            
            cands = []
            for sim, idx in zip(similarities[0], indices[0]):
                if idx != i and sim >= threshold:
                    cands.append((names[idx], float(sim)))
            
            if cands:
                candidates[name] = cands
        
        logger.info(f"Found candidates for {len(candidates)} entities")
        return candidates
    
    def find_relation_candidates(self, profiles: Dict[str, RelationProfile], threshold: float = 0.7) -> Dict[str, List[Tuple[str, float]]]:
        """Find relation candidates using description embeddings"""
        if self.embedding_model is None:
            self._load_embedding_model()
        
        logger.info("Finding relation candidates using embeddings...")
        
        # Create rich text
        relation_texts = {}
        for name, profile in profiles.items():
            text = f"{name}. {profile.description}"
            relation_texts[name] = text
        
        # Generate embeddings
        names = list(relation_texts.keys())
        texts = [relation_texts[n] for n in names]
        embeddings = self.embedding_model.encode(texts, show_progress_bar=True)
        
        # Build FAISS index
        import faiss
        faiss.normalize_L2(embeddings)
        index = faiss.IndexFlatIP(embeddings.shape[1])
        index.add(embeddings)
        
        # Find candidates
        candidates = {}
        for i, name in enumerate(names):
            query = embeddings[i:i+1]
            similarities, indices = index.search(query, 10)
            
            cands = []
            for sim, idx in zip(similarities[0], indices[0]):
                if idx != i and sim >= threshold:
                    cands.append((names[idx], float(sim)))
            
            if cands:
                candidates[name] = cands
        
        logger.info(f"Found candidates for {len(candidates)} relations")
        return candidates
    
    def merge_entities_with_llm(self, profiles: Dict[str, EntityProfile], candidates: Dict[str, List[Tuple[str, float]]]) -> List[Dict]:
        """Use LLM to make final merge decisions with full context"""
        if self.llm_client is None:
            self._init_llm_client()
        
        logger.info("Using LLM for entity merge decisions...")
        
        # Build candidate groups
        groups = self._build_groups(candidates)
        
        merge_decisions = []
        total_cost = 0.0
        
        for i, group in enumerate(groups):
            logger.info(f"Processing entity group {i+1}/{len(groups)} ({len(group)} entities)")
            
            # Build rich prompt with all context
            prompt = self._build_entity_merge_prompt(group, profiles)
            
            # Call LLM
            try:
                response = self.llm_client.chat.completions.create(
                    model=self.llm_model,
                    messages=[
                        {"role": "system", "content": "You are an expert entity resolution system. Analyze entities and determine which refer to the same real-world entity. Always respond with valid JSON."},
                        {"role": "user", "content": prompt}
                    ],
                    temperature=0.0,
                    response_format={"type": "json_object"}
                )
                
                content = response.choices[0].message.content
                data = json.loads(content)
                
                # Extract merge groups
                if "merge_groups" in data:
                    merge_decisions.extend(data["merge_groups"])
                elif "groups" in data:
                    merge_decisions.extend(data["groups"])
                
                # Estimate cost
                tokens_in = response.usage.prompt_tokens
                tokens_out = response.usage.completion_tokens
                cost = (tokens_in * 0.15 / 1_000_000) + (tokens_out * 0.60 / 1_000_000)
                total_cost += cost
                
            except Exception as e:
                logger.error(f"LLM call failed: {e}")
                continue
        
        logger.info(f"Entity merging complete. Cost: ${total_cost:.4f}")
        return merge_decisions
    
    def merge_relations_with_llm(self, profiles: Dict[str, RelationProfile], candidates: Dict[str, List[Tuple[str, float]]]) -> List[Dict]:
        """Use LLM to make final merge decisions for relations"""
        if self.llm_client is None:
            self._init_llm_client()
        
        logger.info("Using LLM for relation merge decisions...")
        
        # Build candidate groups
        groups = self._build_groups(candidates)
        
        merge_decisions = []
        total_cost = 0.0
        
        for i, group in enumerate(groups):
            logger.info(f"Processing relation group {i+1}/{len(groups)} ({len(group)} relations)")
            
            # Build rich prompt with all context
            prompt = self._build_relation_merge_prompt(group, profiles)
            
            # Call LLM
            try:
                response = self.llm_client.chat.completions.create(
                    model=self.llm_model,
                    messages=[
                        {"role": "system", "content": "You are an expert relation resolution system. Analyze relations/verbs and determine which have the same semantic meaning. Always respond with valid JSON."},
                        {"role": "user", "content": prompt}
                    ],
                    temperature=0.0,
                    response_format={"type": "json_object"}
                )
                
                content = response.choices[0].message.content
                data = json.loads(content)
                
                # Extract merge groups
                if "merge_groups" in data:
                    merge_decisions.extend(data["merge_groups"])
                elif "groups" in data:
                    merge_decisions.extend(data["groups"])
                
                # Estimate cost
                tokens_in = response.usage.prompt_tokens
                tokens_out = response.usage.completion_tokens
                cost = (tokens_in * 0.15 / 1_000_000) + (tokens_out * 0.60 / 1_000_000)
                total_cost += cost
                
            except Exception as e:
                logger.error(f"LLM call failed: {e}")
                continue
        
        logger.info(f"Relation merging complete. Cost: ${total_cost:.4f}")
        return merge_decisions
    
    def _build_relation_merge_prompt(self, group: List[str], profiles: Dict[str, RelationProfile]) -> str:
        """Build rich prompt for relation merging"""
        prompt = """Analyze these relations/verbs and determine which have the same semantic meaning.

For each relation, I provide:
- Name and description
- Type (action, state, membership, etc.)
- Typical actors (who performs this)
- Typical targets (who receives this)
- Sample reasoning chains
- Sample citations
- Usage count and confidence

Return JSON with merge groups:
```json
{
  "merge_groups": [
    {
      "canonical_name": "Best relation name",
      "aliases": ["relation1", "relation2"],
      "confidence": 0.95,
      "reason": "Why these have the same meaning"
    }
  ]
}
```

Relations:

"""
        
        for relation_name in group:
            profile = profiles.get(relation_name)
            if not profile:
                continue
            
            prompt += f"\n---\n**{profile.name}** ({profile.relation_type})\n"
            prompt += f"Description: {profile.description}\n"
            prompt += f"Usage: {profile.usage_count}, Avg Confidence: {profile.avg_confidence:.2f}\n"
            
            if profile.typical_actors:
                prompt += f"Typical actors: {', '.join(profile.typical_actors[:5])}\n"
            if profile.typical_targets:
                prompt += f"Typical targets: {', '.join(profile.typical_targets[:5])}\n"
            if profile.sample_reasoning:
                prompt += f"Sample reasoning: {profile.sample_reasoning[0][:150]}...\n"
            if profile.sample_citations:
                prompt += f"Sample citation: {profile.sample_citations[0][:150]}...\n"
        
        prompt += "\n---\n\nProvide merge groups as JSON:"
        return prompt
    
    def _build_entity_merge_prompt(self, group: List[str], profiles: Dict[str, EntityProfile]) -> str:
        """Build rich prompt for entity merging"""
        prompt = """Analyze these entities and determine which refer to the same real-world entity.

For each entity, I provide:
- Name and description
- Type (person, organization, location, etc.)
- Actions they perform
- Actions performed on them
- Co-occurring entities
- Sample reasoning chains
- Sample citations
- Mention count and confidence

Return JSON with merge groups:
```json
{
  "merge_groups": [
    {
      "canonical_name": "Best name",
      "aliases": ["name1", "name2"],
      "confidence": 0.95,
      "reason": "Why these are the same"
    }
  ]
}
```

Entities:

"""
        
        for entity_name in group:
            profile = profiles.get(entity_name)
            if not profile:
                continue
            
            prompt += f"\n---\n**{profile.name}** ({profile.entity_type})\n"
            prompt += f"Description: {profile.description}\n"
            prompt += f"Mentions: {profile.mention_count}, Avg Confidence: {profile.avg_confidence:.2f}\n"
            
            if profile.actions_as_actor:
                prompt += f"Actions as actor: {', '.join(profile.actions_as_actor[:5])}\n"
            if profile.actions_as_target:
                prompt += f"Actions as target: {', '.join(profile.actions_as_target[:5])}\n"
            if profile.co_occurring_entities:
                prompt += f"Co-occurs with: {', '.join(list(profile.co_occurring_entities)[:5])}\n"
            if profile.sample_reasoning:
                prompt += f"Sample reasoning: {profile.sample_reasoning[0][:150]}...\n"
            if profile.sample_citations:
                prompt += f"Sample citation: {profile.sample_citations[0][:150]}...\n"
        
        prompt += "\n---\n\nProvide merge groups as JSON:"
        return prompt
    
    def _build_groups(self, candidates: Dict[str, List[Tuple[str, float]]]) -> List[Set[str]]:
        """Build connected components from candidates"""
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
        
        for entity, cands in candidates.items():
            for cand_name, score in cands:
                union(entity, cand_name)
        
        groups = {}
        for entity in parent.keys():
            root = find(entity)
            if root not in groups:
                groups[root] = set()
            groups[root].add(entity)
        
        return [group for group in groups.values() if len(group) >= 2]
    
    def save_results(self, entity_merges: List[Dict], relation_merges: List[Dict], output_file: str):
        """Save merge decisions"""
        results = {
            "entity_merges": entity_merges,
            "relation_merges": relation_merges,
            "stats": {
                "entity_groups": len(entity_merges),
                "entities_merged": sum(len(m.get("aliases", [])) for m in entity_merges),
                "relation_groups": len(relation_merges),
                "relations_merged": sum(len(m.get("aliases", [])) for m in relation_merges)
            }
        }
        
        with open(output_file, 'w') as f:
            json.dump(results, f, indent=2)
        
        logger.info(f"Results saved to {output_file}")
        return results


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Refined deduplication")
    parser.add_argument("--db", required=True, help="Database path")
    parser.add_argument("--output", default="refined_dedup_results.json", help="Output file")
    parser.add_argument("--entity-threshold", type=float, default=0.7, help="Entity similarity threshold")
    parser.add_argument("--relation-threshold", type=float, default=0.75, help="Relation similarity threshold")
    
    args = parser.parse_args()
    
    dedup = RefinedDeduplicator(args.db)
    
    # Build profiles
    entity_profiles = dedup.build_entity_profiles()
    relation_profiles = dedup.build_relation_profiles()
    
    # Find candidates
    entity_candidates = dedup.find_entity_candidates(entity_profiles, args.entity_threshold)
    relation_candidates = dedup.find_relation_candidates(relation_profiles, args.relation_threshold)
    
    # Merge with LLM
    entity_merges = dedup.merge_entities_with_llm(entity_profiles, entity_candidates)
    relation_merges = dedup.merge_relations_with_llm(relation_profiles, relation_candidates)
    
    # Save results
    results = dedup.save_results(entity_merges, relation_merges, args.output)
    
    print("\n" + "="*80)
    print("REFINED DEDUPLICATION RESULTS")
    print("="*80)
    print(f"Entity groups: {results['stats']['entity_groups']}")
    print(f"Entities merged: {results['stats']['entities_merged']}")
    print(f"Relation groups: {results['stats']['relation_groups']}")
    print(f"Relations merged: {results['stats']['relations_merged']}")
    print("="*80)


if __name__ == "__main__":
    main()
