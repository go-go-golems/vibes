#!/usr/bin/env python3
"""
Entity Resolution with Embedding-based Candidate Generation

Implements Phase 1 of the Entity Resolution Enhancement Proposal:
- Generate entity embeddings using Sentence Transformers
- Use FAISS for efficient similarity search
- Replace string-based blocking with semantic similarity

Author: Manus AI
Date: November 19, 2025
"""

import sqlite3
import json
import numpy as np
from typing import List, Dict, Tuple, Set
from dataclasses import dataclass
from collections import defaultdict
import logging

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class EntityMention:
    """Represents a single mention of an entity"""
    entity_name: str
    doc_id: str
    context: str  # Surrounding text or relationship info
    role: str  # 'actor' or 'target'
    
@dataclass
class EntityProfile:
    """Aggregated profile of an entity across documents"""
    canonical_name: str
    aliases: Set[str]
    mention_count: int
    contexts: List[str]
    relationships: List[str]
    doc_ids: Set[str]


class EmbeddingBasedEntityResolver:
    """
    Entity resolution using semantic embeddings and FAISS
    """
    
    def __init__(self, db_path: str, model_name: str = "all-MiniLM-L6-v2"):
        self.db_path = db_path
        self.model_name = model_name
        self.model = None
        self.index = None
        self.entity_profiles = {}
        
    def _load_model(self):
        """Load the Sentence Transformer model"""
        try:
            from sentence_transformers import SentenceTransformer
            logger.info(f"Loading embedding model: {self.model_name}")
            self.model = SentenceTransformer(self.model_name)
            logger.info("Model loaded successfully")
        except ImportError:
            logger.error("sentence-transformers not installed. Install with: pip install sentence-transformers")
            raise
            
    def _load_entities_from_db(self) -> List[EntityMention]:
        """Load all entity mentions from the database"""
        logger.info(f"Loading entities from {self.db_path}")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Get all actors and targets from RDF triples
        cursor.execute("""
            SELECT doc_id, actor, action, target, explicit_topic, implicit_topic, triple_tags
            FROM rdf_triples
        """)
        
        mentions = []
        for row in cursor.fetchall():
            doc_id, actor, action, target, explicit_topic, implicit_topic, tags_json = row
            
            # Create context from the triple
            context = f"{action}"
            if explicit_topic:
                context += f" (topic: {explicit_topic})"
            if implicit_topic:
                context += f" (implicit: {implicit_topic})"
                
            # Add actor mention
            if actor:
                mentions.append(EntityMention(
                    entity_name=actor,
                    doc_id=doc_id,
                    context=context,
                    role='actor'
                ))
            
            # Add target mention
            if target:
                mentions.append(EntityMention(
                    entity_name=target,
                    doc_id=doc_id,
                    context=context,
                    role='target'
                ))
        
        conn.close()
        logger.info(f"Loaded {len(mentions)} entity mentions")
        return mentions
    
    def _build_entity_profiles(self, mentions: List[EntityMention]) -> Dict[str, EntityProfile]:
        """Build entity profiles by aggregating mentions"""
        logger.info("Building entity profiles...")
        
        profiles = defaultdict(lambda: {
            'aliases': set(),
            'contexts': [],
            'relationships': [],
            'doc_ids': set(),
            'mention_count': 0
        })
        
        for mention in mentions:
            name = mention.entity_name
            profiles[name]['aliases'].add(name)
            profiles[name]['contexts'].append(mention.context)
            profiles[name]['relationships'].append(f"{mention.role}: {mention.context}")
            profiles[name]['doc_ids'].add(mention.doc_id)
            profiles[name]['mention_count'] += 1
        
        # Convert to EntityProfile objects
        entity_profiles = {}
        for name, data in profiles.items():
            entity_profiles[name] = EntityProfile(
                canonical_name=name,
                aliases=data['aliases'],
                mention_count=data['mention_count'],
                contexts=data['contexts'][:10],  # Keep top 10 contexts
                relationships=data['relationships'][:10],  # Keep top 10 relationships
                doc_ids=data['doc_ids']
            )
        
        logger.info(f"Built {len(entity_profiles)} entity profiles")
        return entity_profiles
    
    def _generate_embeddings(self, entity_profiles: Dict[str, EntityProfile]) -> Tuple[np.ndarray, List[str]]:
        """Generate embeddings for all entities"""
        logger.info("Generating embeddings...")
        
        if self.model is None:
            self._load_model()
        
        # Create text representations for embedding
        entity_names = []
        entity_texts = []
        
        for name, profile in entity_profiles.items():
            entity_names.append(name)
            
            # Combine name with context for richer embedding
            text = f"{name}"
            if profile.contexts:
                # Add top 3 contexts
                text += " | " + " | ".join(profile.contexts[:3])
            
            entity_texts.append(text)
        
        # Generate embeddings
        embeddings = self.model.encode(entity_texts, show_progress_bar=True)
        
        logger.info(f"Generated embeddings for {len(entity_names)} entities")
        logger.info(f"Embedding dimension: {embeddings.shape[1]}")
        
        return embeddings, entity_names
    
    def _build_faiss_index(self, embeddings: np.ndarray):
        """Build FAISS index for efficient similarity search"""
        try:
            import faiss
            logger.info("Building FAISS index...")
            
            # Normalize embeddings for cosine similarity
            faiss.normalize_L2(embeddings)
            
            # Create index
            dimension = embeddings.shape[0]
            self.index = faiss.IndexFlatIP(embeddings.shape[1])  # Inner product (cosine after normalization)
            self.index.add(embeddings)
            
            logger.info(f"FAISS index built with {self.index.ntotal} vectors")
            
        except ImportError:
            logger.error("faiss-cpu not installed. Install with: pip install faiss-cpu")
            raise
    
    def find_candidates(self, entity_name: str, k: int = 10, threshold: float = 0.7) -> List[Tuple[str, float]]:
        """
        Find top-k most similar entities using FAISS
        
        Args:
            entity_name: Entity to find candidates for
            k: Number of candidates to return
            threshold: Minimum similarity threshold
            
        Returns:
            List of (entity_name, similarity_score) tuples
        """
        if entity_name not in self.entity_profiles:
            return []
        
        # Get embedding for query entity
        profile = self.entity_profiles[entity_name]
        text = f"{entity_name} | " + " | ".join(profile.contexts[:3])
        query_embedding = self.model.encode([text])
        
        # Normalize
        import faiss
        faiss.normalize_L2(query_embedding)
        
        # Search
        similarities, indices = self.index.search(query_embedding, k + 1)  # +1 to exclude self
        
        # Filter and format results
        candidates = []
        for sim, idx in zip(similarities[0], indices[0]):
            candidate_name = self.entity_names[idx]
            if candidate_name != entity_name and sim >= threshold:
                candidates.append((candidate_name, float(sim)))
        
        return candidates
    
    def run(self, top_k: int = 10, similarity_threshold: float = 0.7):
        """
        Run the complete embedding-based entity resolution pipeline
        
        Args:
            top_k: Number of candidates to consider for each entity
            similarity_threshold: Minimum similarity score
        """
        # Load entities
        mentions = self._load_entities_from_db()
        
        # Build profiles
        self.entity_profiles = self._build_entity_profiles(mentions)
        
        # Generate embeddings
        embeddings, self.entity_names = self._generate_embeddings(self.entity_profiles)
        
        # Build FAISS index
        self._build_faiss_index(embeddings)
        
        # Find candidates for all entities
        logger.info("Finding candidates for all entities...")
        all_candidates = {}
        
        for entity_name in self.entity_profiles.keys():
            candidates = self.find_candidates(entity_name, k=top_k, threshold=similarity_threshold)
            if candidates:
                all_candidates[entity_name] = candidates
        
        # Save results
        output_file = "entity_candidates_embeddings.json"
        with open(output_file, 'w') as f:
            json.dump(all_candidates, f, indent=2)
        
        logger.info(f"Found candidates for {len(all_candidates)} entities")
        logger.info(f"Results saved to {output_file}")
        
        # Print statistics
        self._print_statistics(all_candidates)
        
        return all_candidates
    
    def _print_statistics(self, candidates: Dict[str, List[Tuple[str, float]]]):
        """Print statistics about candidate generation"""
        total_entities = len(self.entity_profiles)
        entities_with_candidates = len(candidates)
        total_candidate_pairs = sum(len(cands) for cands in candidates.values())
        
        print("\n" + "="*60)
        print("EMBEDDING-BASED CANDIDATE GENERATION STATISTICS")
        print("="*60)
        print(f"Total entities: {total_entities}")
        print(f"Entities with candidates: {entities_with_candidates}")
        print(f"Total candidate pairs: {total_candidate_pairs}")
        if entities_with_candidates > 0:
            print(f"Avg candidates per entity: {total_candidate_pairs / entities_with_candidates:.1f}")
        
        # Show some examples
        print("\nExample candidates (top 5 entities):")
        for i, (entity, cands) in enumerate(list(candidates.items())[:5]):
            print(f"\n{entity}:")
            for cand_name, score in cands[:3]:
                print(f"  - {cand_name} (similarity: {score:.3f})")
        print("="*60)


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Embedding-based entity resolution")
    parser.add_argument("--db", default="fact_extraction.db", help="Path to SQLite database")
    parser.add_argument("--model", default="all-MiniLM-L6-v2", help="Sentence Transformer model name")
    parser.add_argument("--top-k", type=int, default=10, help="Number of candidates per entity")
    parser.add_argument("--threshold", type=float, default=0.7, help="Similarity threshold")
    
    args = parser.parse_args()
    
    # Run entity resolution
    resolver = EmbeddingBasedEntityResolver(args.db, args.model)
    resolver.run(top_k=args.top_k, similarity_threshold=args.threshold)


if __name__ == "__main__":
    main()
