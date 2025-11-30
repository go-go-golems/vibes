#!/usr/bin/env python3
"""
Enhanced Fact Extraction with Reasoning Chains and Citations

Extracts RDF triples along with:
- Reasoning chain explaining why the fact was extracted
- Citations from the source document supporting the fact
- Confidence scores

This enables richer embeddings and better fact search/verification.

Author: Manus AI  
Date: November 19, 2025
Ticket: FACT-004
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
    relevance: str  # Why this citation supports the fact


@dataclass
class RDFTripleWithReasoning:
    """RDF triple with reasoning and citations"""
    actor: str
    action: str
    target: str
    timestamp: Optional[str] = None
    location: Optional[str] = None
    actor_likely_type: Optional[str] = None
    triple_tags: Optional[List[str]] = None
    explicit_topic: Optional[str] = None
    implicit_topic: Optional[str] = None
    
    # New fields
    reasoning: str = ""  # Chain of thought explaining the extraction
    citations: List[Citation] = None  # Supporting quotes from document
    confidence: float = 0.0  # Confidence score (0.0-1.0)
    
    def __post_init__(self):
        if self.citations is None:
            self.citations = []


@dataclass
class ExtractionResult:
    """Result of extracting facts from a document"""
    triples: List[RDFTripleWithReasoning]
    metadata: Dict


class EnhancedFactExtractor:
    """Fact extractor with reasoning chains and citations"""
    
    SYSTEM_PROMPT = """You are an expert fact extraction system. Your task is to extract structured facts (RDF triples) from documents, along with reasoning and citations.

For each fact you extract, you must provide:
1. **Reasoning**: A brief chain of thought explaining WHY you extracted this fact and HOW you determined the entities and relationships
2. **Citations**: Specific quotes from the document that support this fact
3. **Confidence**: A score from 0.0 to 1.0 indicating your confidence in this fact

Output format (JSON):
```json
{
  "reasoning": "First, I'll analyze the document for key entities and relationships...",
  "triples": [
    {
      "actor": "Entity performing the action",
      "action": "The relationship or action",
      "target": "Entity receiving the action",
      "timestamp": "When it occurred (if mentioned)",
      "location": "Where it occurred (if mentioned)",
      "actor_likely_type": "person|organization|location|event|other",
      "triple_tags": ["tag1", "tag2"],
      "explicit_topic": "Main topic if explicitly stated",
      "implicit_topic": "Inferred topic",
      "reasoning": "Why I extracted this specific triple and how I identified the entities",
      "citations": [
        {
          "text": "Exact quote from document",
          "relevance": "Why this quote supports the fact"
        }
      ],
      "confidence": 0.95
    }
  ]
}
```

Guidelines:
- Be precise and factual
- Only extract facts explicitly stated or strongly implied
- Provide specific citations (exact quotes)
- Explain your reasoning clearly
- Assign realistic confidence scores
- Focus on important relationships, not trivial facts
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
        logger.info(f"Initialized OpenAI client with model: {self.model}")
    
    def extract(self, doc_id: str, doc_content: str) -> ExtractionResult:
        """
        Extract facts with reasoning and citations from a document
        
        Args:
            doc_id: Document identifier
            doc_content: Full text of the document
            
        Returns:
            ExtractionResult with triples, reasoning, and citations
        """
        if self.client is None:
            self._init_client()
        
        logger.info(f"Extracting facts from document: {doc_id}")
        
        # Build prompt
        user_prompt = f"""Extract facts from the following document. Provide reasoning and citations for each fact.

Document ID: {doc_id}

Document Content:
{doc_content}

Provide your analysis and extracted facts as JSON:"""
        
        # Call LLM
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
            for triple_data in data.get("triples", []):
                # Parse citations
                citations = []
                for cit in triple_data.get("citations", []):
                    citations.append(Citation(
                        text=cit.get("text", ""),
                        relevance=cit.get("relevance", "")
                    ))
                
                triple = RDFTripleWithReasoning(
                    actor=triple_data.get("actor", ""),
                    action=triple_data.get("action", ""),
                    target=triple_data.get("target", ""),
                    timestamp=triple_data.get("timestamp"),
                    location=triple_data.get("location"),
                    actor_likely_type=triple_data.get("actor_likely_type"),
                    triple_tags=triple_data.get("triple_tags"),
                    explicit_topic=triple_data.get("explicit_topic"),
                    implicit_topic=triple_data.get("implicit_topic"),
                    reasoning=triple_data.get("reasoning", ""),
                    citations=citations,
                    confidence=triple_data.get("confidence", 0.0)
                )
                triples.append(triple)
            
            # Calculate cost
            tokens_in = response.usage.prompt_tokens
            tokens_out = response.usage.completion_tokens
            cost = (tokens_in * 0.15 / 1_000_000) + (tokens_out * 0.60 / 1_000_000)
            
            result = ExtractionResult(
                triples=triples,
                metadata={
                    "doc_id": doc_id,
                    "model": self.model,
                    "tokens_in": tokens_in,
                    "tokens_out": tokens_out,
                    "cost_usd": cost,
                    "overall_reasoning": data.get("reasoning", "")
                }
            )
            
            logger.info(f"Extracted {len(triples)} triples from {doc_id} (cost: ${cost:.4f})")
            return result
            
        except Exception as e:
            logger.error(f"Extraction failed for {doc_id}: {e}")
            raise
    
    def save_to_db(self, db_path: str, doc_id: str, result: ExtractionResult):
        """Save extraction results to database"""
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        
        # Create enhanced schema if needed
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS rdf_triples_enhanced (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT NOT NULL,
                timestamp TEXT,
                actor TEXT NOT NULL,
                action TEXT NOT NULL,
                target TEXT NOT NULL,
                location TEXT,
                actor_likely_type TEXT,
                triple_tags TEXT,
                explicit_topic TEXT,
                implicit_topic TEXT,
                reasoning TEXT,
                citations TEXT,
                confidence REAL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Insert triples
        for triple in result.triples:
            cursor.execute("""
                INSERT INTO rdf_triples_enhanced (
                    doc_id, timestamp, actor, action, target, location,
                    actor_likely_type, triple_tags, explicit_topic, implicit_topic,
                    reasoning, citations, confidence
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                doc_id,
                triple.timestamp,
                triple.actor,
                triple.action,
                triple.target,
                triple.location,
                triple.actor_likely_type,
                json.dumps(triple.triple_tags) if triple.triple_tags else None,
                triple.explicit_topic,
                triple.implicit_topic,
                triple.reasoning,
                json.dumps([asdict(c) for c in triple.citations]),
                triple.confidence
            ))
        
        # Save metadata
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS extraction_metadata (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT NOT NULL,
                model TEXT,
                tokens_in INTEGER,
                tokens_out INTEGER,
                cost_usd REAL,
                overall_reasoning TEXT,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cursor.execute("""
            INSERT INTO extraction_metadata (
                doc_id, model, tokens_in, tokens_out, cost_usd, overall_reasoning
            ) VALUES (?, ?, ?, ?, ?, ?)
        """, (
            doc_id,
            result.metadata["model"],
            result.metadata["tokens_in"],
            result.metadata["tokens_out"],
            result.metadata["cost_usd"],
            result.metadata.get("overall_reasoning", "")
        ))
        
        conn.commit()
        conn.close()
        
        logger.info(f"Saved {len(result.triples)} triples to database")


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Enhanced fact extraction with reasoning and citations")
    parser.add_argument("--input", required=True, help="Input document file or directory")
    parser.add_argument("--output", default="fact_extraction_enhanced.db", help="Output database")
    parser.add_argument("--model", default="gpt-4.1-mini", help="LLM model")
    parser.add_argument("--limit", type=int, help="Limit number of documents to process")
    
    args = parser.parse_args()
    
    # Initialize extractor
    extractor = EnhancedFactExtractor(model=args.model)
    
    # Get documents
    input_path = Path(args.input)
    if input_path.is_file():
        documents = [(input_path.stem, input_path.read_text())]
    else:
        documents = []
        for file_path in sorted(input_path.glob("*.txt"))[:args.limit]:
            documents.append((file_path.stem, file_path.read_text()))
    
    logger.info(f"Processing {len(documents)} documents")
    
    # Process documents
    total_cost = 0.0
    total_triples = 0
    
    for doc_id, content in documents:
        try:
            result = extractor.extract(doc_id, content)
            extractor.save_to_db(args.output, doc_id, result)
            
            total_cost += result.metadata["cost_usd"]
            total_triples += len(result.triples)
            
            logger.info(f"Progress: {total_triples} triples, ${total_cost:.4f} total cost")
            
        except Exception as e:
            logger.error(f"Failed to process {doc_id}: {e}")
            continue
    
    print(f"\n✅ Extraction complete!")
    print(f"Documents processed: {len(documents)}")
    print(f"Total triples: {total_triples}")
    print(f"Total cost: ${total_cost:.4f}")
    print(f"Database: {args.output}")


if __name__ == "__main__":
    main()
