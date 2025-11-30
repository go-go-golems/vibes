#!/usr/bin/env python3
"""
Entity Deduplication using LLM

This module identifies and merges duplicate entities with different names:
- "Alan Dershowitz" vs "Alan M. Dershowitz" vs "Professor Alan Dershowitz"
- "Virginia Roberts" vs "Virginia Roberts Giuffre" vs "Virginia Giuffre"

Process:
1. Extract all unique actors and targets from database
2. Use LLM to identify similar entities
3. Create canonical name mapping
4. Update all triples to use canonical names
"""

import json
import sqlite3
from collections import Counter, defaultdict
from typing import List, Dict, Tuple
from openai import OpenAI

client = OpenAI()

class EntityDeduplicator:
    """Handles entity deduplication operations"""
    
    def __init__(self, db_path: str = "fact_extraction.db"):
        self.db_path = db_path
        self.entity_mapping = {}  # variant_name -> canonical_name
        self.entity_groups = []  # List of entity groups
        
    def extract_entities(self) -> Tuple[List[str], List[str]]:
        """Extract all unique actors and targets from database"""
        print("📊 Extracting entities from database...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Get all actors
        cursor.execute("SELECT DISTINCT actor FROM rdf_triples")
        actors = [row[0] for row in cursor.fetchall()]
        
        # Get all targets
        cursor.execute("SELECT DISTINCT target FROM rdf_triples")
        targets = [row[0] for row in cursor.fetchall()]
        
        # Combine and deduplicate
        all_entities = list(set(actors + targets))
        
        conn.close()
        
        print(f"  ✓ Found {len(actors)} unique actors")
        print(f"  ✓ Found {len(targets)} unique targets")
        print(f"  ✓ Total unique entities: {len(all_entities)}")
        
        return actors, targets, all_entities
    
    def find_person_entities(self, entities: List[str]) -> List[str]:
        """Filter to likely person names (heuristic)"""
        print("\n🔍 Filtering to person entities...")
        
        # Simple heuristic: person names are typically:
        # - 2-4 words
        # - Start with capital letters
        # - Don't contain certain keywords
        
        exclude_keywords = [
            'LLC', 'Inc', 'Corp', 'Club', 'Trust', 'Foundation',
            'Office', 'Department', 'Court', 'University', 'School',
            'Hotel', 'magazine', 'newspaper', 'book', 'file', 'document',
            'victim', 'unnamed', 'underage', 'public', 'media'
        ]
        
        person_entities = []
        for entity in entities:
            # Skip if too short or too long
            words = entity.split()
            if len(words) < 2 or len(words) > 5:
                continue
            
            # Skip if contains exclude keywords
            if any(keyword.lower() in entity.lower() for keyword in exclude_keywords):
                continue
            
            # Check if starts with capital (likely a name)
            if entity[0].isupper():
                person_entities.append(entity)
        
        print(f"  ✓ Identified {len(person_entities)} likely person entities")
        print(f"  Examples: {', '.join(person_entities[:10])}")
        
        return person_entities
    
    def group_similar_entities_llm(self, entities: List[str]) -> List[Dict]:
        """Use LLM to identify groups of similar entities"""
        print(f"\n🤖 Grouping {len(entities)} entities using LLM...")
        
        # Process in batches to avoid token limits
        batch_size = 50
        all_groups = []
        
        for i in range(0, len(entities), batch_size):
            batch = entities[i:i+batch_size]
            print(f"  Processing batch {i//batch_size + 1}/{(len(entities)-1)//batch_size + 1}...")
            
            entities_str = "\n".join([f"{j+1}. {entity}" for j, entity in enumerate(batch)])
            
            prompt = f"""You are analyzing entity names from a legal document corpus. 
Identify groups of names that refer to the same person (different spellings, with/without middle names, titles, etc.).

Entities:
{entities_str}

For each group of similar entities, provide:
1. A canonical name (the most complete/formal version)
2. All variant names in that group

Return your response as a JSON array:
[
  {{
    "canonical": "Alan M. Dershowitz",
    "variants": ["Alan Dershowitz", "Professor Alan Dershowitz", "Alan M. Dershowitz"]
  }},
  ...
]

Only include groups with 2+ entities. If an entity has no variants, don't include it.
Return ONLY the JSON array, no other text."""

            try:
                response = client.chat.completions.create(
                    model="gpt-4.1-mini",
                    messages=[{"role": "user", "content": prompt}],
                    temperature=0.1
                )
                
                content = response.choices[0].message.content.strip()
                
                # Extract JSON
                if "```json" in content:
                    content = content.split("```json")[1].split("```")[0].strip()
                elif "```" in content:
                    content = content.split("```")[1].split("```")[0].strip()
                
                groups = json.loads(content)
                all_groups.extend(groups)
                
                print(f"    Found {len(groups)} entity groups in this batch")
                
            except Exception as e:
                print(f"    ⚠️  Error processing batch: {e}")
                continue
        
        print(f"  ✓ Found {len(all_groups)} total entity groups")
        
        self.entity_groups = all_groups
        return all_groups
    
    def create_mapping(self, groups: List[Dict]) -> Dict[str, str]:
        """Create mapping from variant names to canonical names"""
        print(f"\n🗺️  Creating entity mapping...")
        
        mapping = {}
        for group in groups:
            canonical = group['canonical']
            for variant in group['variants']:
                mapping[variant] = canonical
        
        print(f"  ✓ Created mapping for {len(mapping)} entity variants")
        print(f"\n  Sample mappings:")
        for i, (variant, canonical) in enumerate(list(mapping.items())[:5]):
            if variant != canonical:
                print(f"    {variant} → {canonical}")
        
        self.entity_mapping = mapping
        return mapping
    
    def update_database(self, mapping: Dict[str, str]) -> Tuple[int, int]:
        """Update database to use canonical entity names"""
        print(f"\n💾 Updating database with canonical names...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Update actors
        actor_updates = 0
        for variant, canonical in mapping.items():
            cursor.execute("""
                UPDATE rdf_triples 
                SET actor = ?
                WHERE actor = ?
            """, (canonical, variant))
            actor_updates += cursor.rowcount
        
        # Update targets
        target_updates = 0
        for variant, canonical in mapping.items():
            cursor.execute("""
                UPDATE rdf_triples 
                SET target = ?
                WHERE target = ?
            """, (canonical, variant))
            target_updates += cursor.rowcount
        
        conn.commit()
        conn.close()
        
        print(f"  ✓ Updated {actor_updates} actor references")
        print(f"  ✓ Updated {target_updates} target references")
        
        return actor_updates, target_updates
    
    def save_mapping(self, output_path: str = "entity_mapping.json"):
        """Save entity mapping to JSON file"""
        print(f"\n💾 Saving entity mapping to {output_path}...")
        
        mapping_data = {
            "entity_groups": self.entity_groups,
            "mapping": self.entity_mapping,
            "stats": {
                "total_groups": len(self.entity_groups),
                "total_mappings": len(self.entity_mapping)
            }
        }
        
        with open(output_path, 'w') as f:
            json.dump(mapping_data, f, indent=2)
        
        print(f"  ✓ Saved mapping data")
    
    def run_pipeline(self):
        """Execute the complete deduplication pipeline"""
        print("=" * 80)
        print("ENTITY DEDUPLICATION PIPELINE")
        print("=" * 80)
        
        # Step 1: Extract entities
        actors, targets, all_entities = self.extract_entities()
        
        # Step 2: Filter to person entities
        person_entities = self.find_person_entities(all_entities)
        
        # Step 3: Group similar entities
        groups = self.group_similar_entities_llm(person_entities)
        
        # Step 4: Create mapping
        mapping = self.create_mapping(groups)
        
        # Step 5: Update database
        actor_updates, target_updates = self.update_database(mapping)
        
        # Step 6: Save mapping
        self.save_mapping()
        
        print("\n" + "=" * 80)
        print("✓ DEDUPLICATION COMPLETE")
        print("=" * 80)
        
        print(f"\nSummary:")
        print(f"  - Total entities: {len(all_entities)}")
        print(f"  - Person entities: {len(person_entities)}")
        print(f"  - Entity groups found: {len(groups)}")
        print(f"  - Variants mapped: {len(mapping)}")
        print(f"  - Database updates: {actor_updates + target_updates}")
        
        print(f"\nTop entity groups:")
        sorted_groups = sorted(groups, key=lambda g: len(g['variants']), reverse=True)
        for i, group in enumerate(sorted_groups[:10], 1):
            print(f"  {i}. {group['canonical']} ({len(group['variants'])} variants)")
            for variant in group['variants'][:3]:
                if variant != group['canonical']:
                    print(f"     - {variant}")

def main():
    deduplicator = EntityDeduplicator(db_path="fact_extraction.db")
    deduplicator.run_pipeline()

if __name__ == "__main__":
    main()
