#!/usr/bin/env python3
"""
Fully Enhanced Fact Extraction with Entity and Relation Descriptions

Extracts RDF triples with:
- Reasoning chains
- Citations from source
- Confidence scores
- Entity descriptions (for actor and target)
- Relation descriptions

This enables:
- Better entity deduplication via description embeddings
- Better relation deduplication via description embeddings
- Richer semantic search

Author: Manus AI
Date: November 19, 2025
Ticket: FACT-006
"""

import os
import json
import sqlite3
from typing import List, Dict, Optional
from dataclasses import dataclass, asdict
from openai import OpenAI
import logging
from pathlib import Path

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


@dataclass
class Citation:
    """A citation from the source document"""
    text: str
    relevance: str


@dataclass
class EntityDescription:
    """Description of an entity"""
    name: str
    description: str  # What/who this entity is
    entity_type: str  # person, organization, location, event, concept, other


@dataclass
class RelationDescription:
    """Description of a relation/action"""
    relation: str
    description: str  # What this relation means
    relation_type: str  # action, state, attribute, membership, other


@dataclass
class EnhancedRDFTriple:
    """Fully enhanced RDF triple"""
    # Core triple
    actor: str
    action: str
    target: str
    
    # Optional metadata
    timestamp: Optional[str] = None
    location: Optional[str] = None
    triple_tags: Optional[List[str]] = None
    explicit_topic: Optional[str] = None
    implicit_topic: Optional[str] = None
    
    # Provenance (from FACT-004)
    reasoning: str = ""
    citations: List[Citation] = None
    confidence: float = 0.0
    
    # NEW: Entity descriptions (for deduplication)
    actor_description: str = ""
    actor_type: str = ""
    target_description: str = ""
    target_type: str = ""
    
    # NEW: Relation description (for deduplication)
    relation_description: str = ""
    relation_type: str = ""
    
    def __post_init__(self):
        if self.citations is None:
            self.citations = []


@dataclass
class ExtractionResult:
    """Result of extraction"""
    triples: List[EnhancedRDFTriple]
    
    # Aggregated entity descriptions (for building entity knowledge base)
    entity_descriptions: Dict[str, EntityDescription]
    
    # Aggregated relation descriptions (for building relation taxonomy)
    relation_descriptions: Dict[str, RelationDescription]
    
    metadata: Dict


class FullyEnhancedExtractor:
    """Extractor with full enhancements"""
    
    SYSTEM_PROMPT = """You are an expert fact extraction system. Extract structured facts (RDF triples) with complete metadata.

For EACH FACT, provide:
1. **Core Triple**: actor, action, target
2. **Reasoning**: Why you extracted this fact
3. **Citations**: Exact quotes supporting it
4. **Confidence**: Score 0.0-1.0
5. **Entity Descriptions**: Describe what/who the actor and target are
6. **Relation Description**: Describe what the action/relation means

Output JSON format:
```json
{
  "reasoning": "Overall analysis of the document...",
  "triples": [
    {
      "actor": "Entity name",
      "action": "Relationship/action",
      "target": "Entity name",
      "timestamp": "When (if mentioned)",
      "location": "Where (if mentioned)",
      "triple_tags": ["tag1", "tag2"],
      "explicit_topic": "Main topic",
      "implicit_topic": "Inferred topic",
      
      "reasoning": "Why this specific triple was extracted",
      "citations": [
        {
          "text": "Exact quote",
          "relevance": "Why this supports the fact"
        }
      ],
      "confidence": 0.95,
      
      "actor_description": "Description of who/what the actor is",
      "actor_type": "person|organization|location|event|concept|other",
      "target_description": "Description of who/what the target is",
      "target_type": "person|organization|location|event|concept|other",
      
      "relation_description": "Description of what this relation means",
      "relation_type": "action|state|attribute|membership|other"
    }
  ],
  
  "entity_descriptions": {
    "Entity Name": {
      "name": "Entity Name",
      "description": "Comprehensive description based on all mentions",
      "entity_type": "person|organization|location|event|concept|other"
    }
  },
  
  "relation_descriptions": {
    "relation name": {
      "relation": "relation name",
      "description": "What this relation means in context",
      "relation_type": "action|state|attribute|membership|other"
    }
  }
}
```

Guidelines:
- Provide detailed entity descriptions (help distinguish between entities with similar names)
- Provide clear relation descriptions (help identify synonymous relations)
- Be precise and factual
- Use exact quotes for citations
- Explain reasoning clearly
"""

    def __init__(self, model: str = "gpt-4.1-mini"):
        self.model = model
        self.client = None
        
    def _init_client(self):
        """Initialize OpenAI client"""
        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            raise ValueError("OPENAI_API_KEY not set")
        
        base_url = os.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1")
        self.client = OpenAI(api_key=api_key, base_url=base_url)
        logger.info(f"Initialized client with model: {self.model}")
    
    def extract(self, doc_id: str, doc_content: str) -> ExtractionResult:
        """Extract facts with full enhancements"""
        if self.client is None:
            self._init_client()
        
        logger.info(f"Extracting from: {doc_id}")
        
        user_prompt = f"""Extract facts with complete metadata from this document.

Document ID: {doc_id}

Content:
{doc_content}

Provide analysis and facts as JSON:"""
        
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": self.SYSTEM_PROMPT},
                    {"role": "user", "content": user_prompt}
                ],
                temperature=0.0,
                response_format={"type": "json_object"}
            )
            
            content = response.choices[0].message.content
            data = json.loads(content)
            
            # Parse triples
            triples = []
            for t in data.get("triples", []):
                citations = [Citation(**c) for c in t.get("citations", [])]
                
                triple = EnhancedRDFTriple(
                    actor=t.get("actor", ""),
                    action=t.get("action", ""),
                    target=t.get("target", ""),
                    timestamp=t.get("timestamp"),
                    location=t.get("location"),
                    triple_tags=t.get("triple_tags"),
                    explicit_topic=t.get("explicit_topic"),
                    implicit_topic=t.get("implicit_topic"),
                    reasoning=t.get("reasoning", ""),
                    citations=citations,
                    confidence=t.get("confidence", 0.0),
                    actor_description=t.get("actor_description", ""),
                    actor_type=t.get("actor_type", ""),
                    target_description=t.get("target_description", ""),
                    target_type=t.get("target_type", ""),
                    relation_description=t.get("relation_description", ""),
                    relation_type=t.get("relation_type", "")
                )
                triples.append(triple)
            
            # Parse entity descriptions
            entity_descriptions = {}
            for name, desc_data in data.get("entity_descriptions", {}).items():
                entity_descriptions[name] = EntityDescription(
                    name=desc_data.get("name", name),
                    description=desc_data.get("description", ""),
                    entity_type=desc_data.get("entity_type", "other")
                )
            
            # Parse relation descriptions
            relation_descriptions = {}
            for rel, desc_data in data.get("relation_descriptions", {}).items():
                relation_descriptions[rel] = RelationDescription(
                    relation=desc_data.get("relation", rel),
                    description=desc_data.get("description", ""),
                    relation_type=desc_data.get("relation_type", "other")
                )
            
            # Calculate cost
            tokens_in = response.usage.prompt_tokens
            tokens_out = response.usage.completion_tokens
            cost = (tokens_in * 0.15 / 1_000_000) + (tokens_out * 0.60 / 1_000_000)
            
            result = ExtractionResult(
                triples=triples,
                entity_descriptions=entity_descriptions,
                relation_descriptions=relation_descriptions,
                metadata={
                    "doc_id": doc_id,
                    "model": self.model,
                    "tokens_in": tokens_in,
                    "tokens_out": tokens_out,
                    "cost_usd": cost,
                    "overall_reasoning": data.get("reasoning", "")
                }
            )
            
            logger.info(f"Extracted {len(triples)} triples, {len(entity_descriptions)} entities, {len(relation_descriptions)} relations (cost: ${cost:.4f})")
            return result
            
        except Exception as e:
            logger.error(f"Extraction failed for {doc_id}: {e}")
            raise
    
    def save_to_db(self, db_path: str, doc_id: str, result: ExtractionResult):
        """Save to database"""
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        
        # Create tables
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS rdf_triples_full (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT NOT NULL,
                timestamp TEXT,
                actor TEXT NOT NULL,
                action TEXT NOT NULL,
                target TEXT NOT NULL,
                location TEXT,
                triple_tags TEXT,
                explicit_topic TEXT,
                implicit_topic TEXT,
                reasoning TEXT,
                citations TEXT,
                confidence REAL,
                actor_description TEXT,
                actor_type TEXT,
                target_description TEXT,
                target_type TEXT,
                relation_description TEXT,
                relation_type TEXT,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS entity_descriptions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                entity_name TEXT NOT NULL UNIQUE,
                description TEXT,
                entity_type TEXT,
                first_seen_doc TEXT,
                mention_count INTEGER DEFAULT 1,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS relation_descriptions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                relation_name TEXT NOT NULL UNIQUE,
                description TEXT,
                relation_type TEXT,
                first_seen_doc TEXT,
                usage_count INTEGER DEFAULT 1,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Insert triples
        for triple in result.triples:
            cursor.execute("""
                INSERT INTO rdf_triples_full (
                    doc_id, timestamp, actor, action, target, location,
                    triple_tags, explicit_topic, implicit_topic,
                    reasoning, citations, confidence,
                    actor_description, actor_type,
                    target_description, target_type,
                    relation_description, relation_type
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                doc_id, triple.timestamp, triple.actor, triple.action, triple.target,
                triple.location,
                json.dumps(triple.triple_tags) if triple.triple_tags else None,
                triple.explicit_topic, triple.implicit_topic,
                triple.reasoning,
                json.dumps([asdict(c) for c in triple.citations]),
                triple.confidence,
                triple.actor_description, triple.actor_type,
                triple.target_description, triple.target_type,
                triple.relation_description, triple.relation_type
            ))
        
        # Insert/update entity descriptions
        for name, desc in result.entity_descriptions.items():
            cursor.execute("""
                INSERT INTO entity_descriptions (entity_name, description, entity_type, first_seen_doc)
                VALUES (?, ?, ?, ?)
                ON CONFLICT(entity_name) DO UPDATE SET
                    description = CASE 
                        WHEN length(excluded.description) > length(description) 
                        THEN excluded.description 
                        ELSE description 
                    END,
                    mention_count = mention_count + 1,
                    updated_at = CURRENT_TIMESTAMP
            """, (name, desc.description, desc.entity_type, doc_id))
        
        # Insert/update relation descriptions
        for rel, desc in result.relation_descriptions.items():
            cursor.execute("""
                INSERT INTO relation_descriptions (relation_name, description, relation_type, first_seen_doc)
                VALUES (?, ?, ?, ?)
                ON CONFLICT(relation_name) DO UPDATE SET
                    description = CASE 
                        WHEN length(excluded.description) > length(description) 
                        THEN excluded.description 
                        ELSE description 
                    END,
                    usage_count = usage_count + 1,
                    updated_at = CURRENT_TIMESTAMP
            """, (rel, desc.description, desc.relation_type, doc_id))
        
        conn.commit()
        conn.close()
        
        logger.info(f"Saved to database")


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Fully enhanced fact extraction")
    parser.add_argument("--input", required=True, help="Input file or directory")
    parser.add_argument("--output", default="fact_extraction_full.db", help="Output database")
    parser.add_argument("--model", default="gpt-4.1-mini", help="LLM model")
    parser.add_argument("--limit", type=int, help="Limit documents")
    
    args = parser.parse_args()
    
    extractor = FullyEnhancedExtractor(model=args.model)
    
    # Get documents
    input_path = Path(args.input)
    if input_path.is_file():
        documents = [(input_path.stem, input_path.read_text())]
    else:
        documents = []
        for file_path in sorted(input_path.glob("*.txt"))[:args.limit]:
            documents.append((file_path.stem, file_path.read_text()))
    
    logger.info(f"Processing {len(documents)} documents")
    
    # Process
    total_cost = 0.0
    total_triples = 0
    total_entities = 0
    total_relations = 0
    
    for doc_id, content in documents:
        try:
            result = extractor.extract(doc_id, content)
            extractor.save_to_db(args.output, doc_id, result)
            
            total_cost += result.metadata["cost_usd"]
            total_triples += len(result.triples)
            total_entities += len(result.entity_descriptions)
            total_relations += len(result.relation_descriptions)
            
        except Exception as e:
            logger.error(f"Failed: {doc_id}: {e}")
            continue
    
    print(f"\n✅ Extraction complete!")
    print(f"Documents: {len(documents)}")
    print(f"Triples: {total_triples}")
    print(f"Entities: {total_entities}")
    print(f"Relations: {total_relations}")
    print(f"Cost: ${total_cost:.4f}")
    print(f"Database: {args.output}")


if __name__ == "__main__":
    main()
