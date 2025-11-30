#!/usr/bin/env python3
"""
Tag Clustering using LLM-based Semantic Grouping

Simpler approach that doesn't require embeddings API:
1. Extract all unique tags
2. Use LLM to group tags into semantic clusters
3. Assign triples to clusters based on their tags
"""

import json
import sqlite3
from collections import Counter
from typing import List, Dict
from openai import OpenAI

client = OpenAI()

class SimpleTagger:
    """Handles tag clustering using LLM semantic grouping"""
    
    def __init__(self, db_path: str = "fact_extraction.db", n_clusters: int = 30):
        self.db_path = db_path
        self.n_clusters = n_clusters
        self.clusters = {}  # cluster_id -> {theme, tags}
        
    def extract_tags(self) -> List[str]:
        """Extract all unique tags from database"""
        print("📊 Extracting tags from database...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("SELECT triple_tags FROM rdf_triples WHERE triple_tags IS NOT NULL")
        
        all_tags = []
        for (tags_json,) in cursor.fetchall():
            if tags_json:
                tags = json.loads(tags_json)
                all_tags.extend(tags)
        
        conn.close()
        
        tag_counts = Counter(all_tags)
        unique_tags = list(tag_counts.keys())
        
        print(f"  ✓ Found {len(unique_tags)} unique tags")
        print(f"  Top 10: {', '.join([f'{tag}({count})' for tag, count in tag_counts.most_common(10)])}")
        
        return unique_tags, tag_counts
    
    def cluster_tags_with_llm(self, tags: List[str], tag_counts: Counter) -> Dict:
        """Use LLM to group tags into semantic clusters"""
        print(f"\n🤖 Clustering {len(tags)} tags using LLM...")
        
        # Sort tags by frequency for better context
        sorted_tags = [tag for tag, _ in tag_counts.most_common()]
        tags_str = ", ".join(sorted_tags)
        
        prompt = f"""You are analyzing tags from a legal document corpus about the Epstein case. 
Group these {len(tags)} tags into approximately {self.n_clusters} semantic clusters.

Tags: {tags_str}

For each cluster, provide:
1. A concise theme name (2-4 words)
2. The tags that belong to that cluster

Return your response as a JSON array of objects with this structure:
[
  {{
    "cluster_id": 0,
    "theme": "Legal Proceedings",
    "tags": ["deposition", "court filing", "testimony", ...]
  }},
  ...
]

Focus on creating meaningful, coherent clusters. Tags can appear in multiple clusters if relevant.
Return ONLY the JSON array, no other text."""

        response = client.chat.completions.create(
            model="gpt-4.1-mini",
            messages=[{"role": "user", "content": prompt}],
            temperature=0.3
        )
        
        content = response.choices[0].message.content.strip()
        
        # Extract JSON from response
        if "```json" in content:
            content = content.split("```json")[1].split("```")[0].strip()
        elif "```" in content:
            content = content.split("```")[1].split("```")[0].strip()
        
        clusters = json.loads(content)
        
        print(f"  ✓ Created {len(clusters)} clusters")
        for cluster in clusters[:5]:
            print(f"    - {cluster['theme']}: {len(cluster['tags'])} tags")
        
        # Convert to dict for easier lookup
        self.clusters = {c['cluster_id']: c for c in clusters}
        
        return self.clusters
    
    def assign_triples_to_clusters(self) -> int:
        """Assign each triple to relevant clusters based on its tags"""
        print(f"\n📌 Assigning triples to clusters...")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Add cluster columns if they don't exist
        try:
            cursor.execute("ALTER TABLE rdf_triples ADD COLUMN cluster_ids TEXT")
            cursor.execute("ALTER TABLE rdf_triples ADD COLUMN cluster_themes TEXT")
        except sqlite3.OperationalError:
            pass
        
        # Build tag-to-cluster mapping
        tag_to_clusters = {}
        for cluster_id, cluster_data in self.clusters.items():
            for tag in cluster_data['tags']:
                if tag not in tag_to_clusters:
                    tag_to_clusters[tag] = []
                tag_to_clusters[tag].append((cluster_id, cluster_data['theme']))
        
        # Get all triples with tags
        cursor.execute("SELECT id, triple_tags FROM rdf_triples WHERE triple_tags IS NOT NULL")
        triples = cursor.fetchall()
        
        updated_count = 0
        for triple_id, tags_json in triples:
            if not tags_json:
                continue
            
            tags = json.loads(tags_json)
            
            # Find all clusters that match these tags
            matching_clusters = set()
            for tag in tags:
                if tag in tag_to_clusters:
                    for cluster_id, theme in tag_to_clusters[tag]:
                        matching_clusters.add((cluster_id, theme))
            
            if matching_clusters:
                # Take top 3 clusters
                top_clusters = list(matching_clusters)[:3]
                cluster_ids = [str(c[0]) for c in top_clusters]
                cluster_themes = [c[1] for c in top_clusters]
                
                cursor.execute("""
                    UPDATE rdf_triples 
                    SET cluster_ids = ?, cluster_themes = ?
                    WHERE id = ?
                """, (json.dumps(cluster_ids), json.dumps(cluster_themes), triple_id))
                
                updated_count += 1
        
        conn.commit()
        conn.close()
        
        print(f"  ✓ Updated {updated_count} triples with cluster assignments")
        return updated_count
    
    def save_clusters(self, output_path: str = "tag_clusters.json"):
        """Save cluster information to JSON file"""
        print(f"\n💾 Saving clusters to {output_path}...")
        
        clusters_list = []
        for cluster_id, cluster_data in sorted(self.clusters.items()):
            clusters_list.append({
                "cluster_id": cluster_id,
                "theme": cluster_data['theme'],
                "tag_count": len(cluster_data['tags']),
                "tags": cluster_data['tags']
            })
        
        with open(output_path, 'w') as f:
            json.dump(clusters_list, f, indent=2)
        
        print(f"  ✓ Saved {len(clusters_list)} clusters")
    
    def run_pipeline(self):
        """Execute the complete clustering pipeline"""
        print("=" * 80)
        print("TAG CLUSTERING PIPELINE (LLM-based)")
        print("=" * 80)
        
        # Step 1: Extract tags
        tags, tag_counts = self.extract_tags()
        
        # Step 2: Cluster with LLM
        clusters = self.cluster_tags_with_llm(tags, tag_counts)
        
        # Step 3: Assign triples
        self.assign_triples_to_clusters()
        
        # Step 4: Save results
        self.save_clusters()
        
        print("\n" + "=" * 80)
        print("✓ CLUSTERING COMPLETE")
        print("=" * 80)
        
        print(f"\nSummary:")
        print(f"  - Unique tags: {len(tags)}")
        print(f"  - Clusters created: {len(clusters)}")
        print(f"\nAll cluster themes:")
        for cluster_id, cluster_data in sorted(clusters.items()):
            print(f"  {cluster_id}. {cluster_data['theme']} ({len(cluster_data['tags'])} tags)")

def main():
    clusterer = SimpleTagger(db_path="fact_extraction.db", n_clusters=25)
    clusterer.run_pipeline()

if __name__ == "__main__":
    main()
