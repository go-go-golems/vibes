#!/usr/bin/env python3
"""
Rich Embeddings for Fact and Entity Search

Creates semantic embeddings that combine:
- The fact itself (actor-action-target)
- Reasoning chain explaining the extraction
- Citations from the source document

This enables powerful semantic search over facts with full context.

Author: Manus AI
Date: November 19, 2025
Ticket: FACT-005
"""

import sqlite3
import json
import numpy as np
from typing import List, Dict, Tuple
from dataclasses import dataclass
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


@dataclass
class FactEmbedding:
    """A fact with its rich embedding"""
    fact_id: int
    actor: str
    action: str
    target: str
    reasoning: str
    citations_text: str
    confidence: float
    embedding: np.ndarray
    composite_text: str  # The text that was embedded


class RichFactSearchIndex:
    """
    Search index for facts using rich embeddings
    """
    
    def __init__(self, db_path: str, model_name: str = "all-MiniLM-L6-v2"):
        self.db_path = db_path
        self.model_name = model_name
        self.model = None
        self.index = None
        self.fact_embeddings = []
        
    def _load_model(self):
        """Load Sentence Transformer model"""
        try:
            from sentence_transformers import SentenceTransformer
            logger.info(f"Loading embedding model: {self.model_name}")
            self.model = SentenceTransformer(self.model_name)
            logger.info("Model loaded successfully")
        except ImportError:
            logger.error("sentence-transformers not installed")
            raise
    
    def _load_facts_from_db(self) -> List[Dict]:
        """Load all facts with reasoning and citations"""
        logger.info(f"Loading facts from {self.db_path}")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            SELECT id, actor, action, target, reasoning, citations, confidence, doc_id
            FROM rdf_triples_enhanced
        """)
        
        facts = []
        for row in cursor.fetchall():
            fact_id, actor, action, target, reasoning, citations_json, confidence, doc_id = row
            
            # Parse citations
            citations = json.loads(citations_json) if citations_json else []
            citations_text = " | ".join([c["text"] for c in citations])
            
            facts.append({
                "id": fact_id,
                "actor": actor,
                "action": action,
                "target": target,
                "reasoning": reasoning or "",
                "citations_text": citations_text,
                "confidence": confidence or 0.0,
                "doc_id": doc_id
            })
        
        conn.close()
        logger.info(f"Loaded {len(facts)} facts")
        return facts
    
    def _create_composite_text(self, fact: Dict) -> str:
        """
        Create rich composite text for embedding
        
        Combines multiple signals:
        1. The fact itself (structured)
        2. Reasoning (semantic context)
        3. Citations (evidence)
        """
        # Start with the core fact
        composite = f"{fact['actor']} {fact['action']} {fact['target']}"
        
        # Add reasoning (provides semantic context)
        if fact['reasoning']:
            composite += f" | Reasoning: {fact['reasoning']}"
        
        # Add citations (provides evidence and additional context)
        if fact['citations_text']:
            # Truncate citations if too long
            citations = fact['citations_text'][:500]
            composite += f" | Evidence: {citations}"
        
        return composite
    
    def build_index(self):
        """Build FAISS index with rich fact embeddings"""
        if self.model is None:
            self._load_model()
        
        # Load facts
        facts = self._load_facts_from_db()
        
        if not facts:
            logger.warning("No facts found in database")
            return
        
        # Create composite texts
        logger.info("Creating composite texts for embedding...")
        composite_texts = []
        for fact in facts:
            composite_texts.append(self._create_composite_text(fact))
        
        # Generate embeddings
        logger.info("Generating embeddings...")
        embeddings = self.model.encode(composite_texts, show_progress_bar=True)
        
        # Store fact embeddings
        for fact, embedding, composite in zip(facts, embeddings, composite_texts):
            self.fact_embeddings.append(FactEmbedding(
                fact_id=fact["id"],
                actor=fact["actor"],
                action=fact["action"],
                target=fact["target"],
                reasoning=fact["reasoning"],
                citations_text=fact["citations_text"],
                confidence=fact["confidence"],
                embedding=embedding,
                composite_text=composite
            ))
        
        # Build FAISS index
        try:
            import faiss
            logger.info("Building FAISS index...")
            
            # Normalize for cosine similarity
            embeddings_array = np.array([fe.embedding for fe in self.fact_embeddings])
            faiss.normalize_L2(embeddings_array)
            
            # Create index
            dimension = embeddings_array.shape[1]
            self.index = faiss.IndexFlatIP(dimension)  # Inner product = cosine after normalization
            self.index.add(embeddings_array)
            
            logger.info(f"Index built with {self.index.ntotal} fact embeddings")
            
        except ImportError:
            logger.error("faiss-cpu not installed")
            raise
    
    def search(self, query: str, k: int = 10, min_confidence: float = 0.0) -> List[Tuple[FactEmbedding, float]]:
        """
        Search for facts using semantic similarity
        
        Args:
            query: Natural language query
            k: Number of results to return
            min_confidence: Minimum confidence threshold
            
        Returns:
            List of (FactEmbedding, similarity_score) tuples
        """
        if self.index is None:
            raise ValueError("Index not built. Call build_index() first.")
        
        # Encode query
        query_embedding = self.model.encode([query])
        
        # Normalize
        import faiss
        faiss.normalize_L2(query_embedding)
        
        # Search
        similarities, indices = self.index.search(query_embedding, k * 2)  # Get extra for filtering
        
        # Filter by confidence and format results
        results = []
        for sim, idx in zip(similarities[0], indices[0]):
            fact_emb = self.fact_embeddings[idx]
            if fact_emb.confidence >= min_confidence:
                results.append((fact_emb, float(sim)))
                if len(results) >= k:
                    break
        
        return results
    
    def search_by_entity(self, entity_name: str, k: int = 10) -> List[Tuple[FactEmbedding, float]]:
        """
        Search for facts involving a specific entity
        
        Args:
            entity_name: Name of the entity
            k: Number of results
            
        Returns:
            List of facts involving this entity, ranked by relevance
        """
        query = f"Facts about {entity_name} and their relationships"
        return self.search(query, k=k)
    
    def search_by_relationship(self, relationship: str, k: int = 10) -> List[Tuple[FactEmbedding, float]]:
        """
        Search for facts with a specific type of relationship
        
        Args:
            relationship: Type of relationship (e.g., "met with", "employed", "accused")
            k: Number of results
            
        Returns:
            List of facts with this relationship type
        """
        query = f"Relationships involving {relationship}"
        return self.search(query, k=k)
    
    def save_index(self, output_path: str):
        """Save the index to disk"""
        import faiss
        import pickle
        
        # Save FAISS index
        faiss.write_index(self.index, f"{output_path}.faiss")
        
        # Save fact embeddings (without numpy arrays to reduce size)
        facts_data = []
        for fe in self.fact_embeddings:
            facts_data.append({
                "fact_id": fe.fact_id,
                "actor": fe.actor,
                "action": fe.action,
                "target": fe.target,
                "reasoning": fe.reasoning,
                "citations_text": fe.citations_text,
                "confidence": fe.confidence,
                "composite_text": fe.composite_text
            })
        
        with open(f"{output_path}.json", 'w') as f:
            json.dump(facts_data, f, indent=2)
        
        logger.info(f"Index saved to {output_path}.faiss and {output_path}.json")
    
    def print_search_results(self, results: List[Tuple[FactEmbedding, float]], query: str):
        """Pretty print search results"""
        print("\n" + "="*80)
        print(f"SEARCH RESULTS FOR: \"{query}\"")
        print("="*80)
        
        if not results:
            print("No results found.")
            return
        
        for i, (fact, score) in enumerate(results, 1):
            print(f"\n{i}. [{score:.3f}] {fact.actor} --[{fact.action}]--> {fact.target}")
            print(f"   Confidence: {fact.confidence:.2f}")
            if fact.reasoning:
                print(f"   Reasoning: {fact.reasoning[:200]}...")
            if fact.citations_text:
                print(f"   Evidence: {fact.citations_text[:150]}...")
        
        print("="*80)


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Rich fact search with embeddings")
    parser.add_argument("--db", default="fact_extraction_enhanced.db", help="Database path")
    parser.add_argument("--model", default="all-MiniLM-L6-v2", help="Embedding model")
    parser.add_argument("--build", action="store_true", help="Build the index")
    parser.add_argument("--save", help="Save index to file")
    parser.add_argument("--search", help="Search query")
    parser.add_argument("--entity", help="Search by entity name")
    parser.add_argument("--relationship", help="Search by relationship type")
    parser.add_argument("-k", type=int, default=5, help="Number of results")
    
    args = parser.parse_args()
    
    # Create index
    index = RichFactSearchIndex(args.db, args.model)
    
    # Build index
    if args.build:
        index.build_index()
        
        if args.save:
            index.save_index(args.save)
    
    # Search
    if args.search:
        if index.index is None:
            index.build_index()
        results = index.search(args.search, k=args.k)
        index.print_search_results(results, args.search)
    
    if args.entity:
        if index.index is None:
            index.build_index()
        results = index.search_by_entity(args.entity, k=args.k)
        index.print_search_results(results, f"Entity: {args.entity}")
    
    if args.relationship:
        if index.index is None:
            index.build_index()
        results = index.search_by_relationship(args.relationship, k=args.k)
        index.print_search_results(results, f"Relationship: {args.relationship}")


if __name__ == "__main__":
    main()
