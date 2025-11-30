#!/usr/bin/env python3
"""
Fact Extraction Pipeline - Go-style Implementation in Python
Replicates the Epstein-doc-explorer methodology for extracting RDF triples from documents.
"""

import json
import os
import re
import sqlite3
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import List, Optional

from openai import OpenAI

# Analysis prompt template (replicated from the TypeScript version)
ANALYSIS_PROMPT = """You are analyzing a document from a legal/investigative document collection. The document ID is "{doc_id}".

IMPORTANT: You have ALL the information you need in the document text below. Do NOT attempt to read files, explore directories, or gather additional context. Analyze ONLY the text provided.

**CRITICAL IDENTIFICATION RULES:**
This document may contain communications involving Jeffrey Epstein. He may appear under these identifiers:
- Email: jeeitunes@gmail.com
- Email: e:jeeitunes@gmail.com
- Name: jee
- Name: Jeffrey Epstein
- Name: Jeffrey
- Name: Epstein

When you see ANY of these identifiers as a sender, participant, or actor, you MUST use "Jeffrey Epstein" as the actor name in your RDF triples. DO NOT use "jee", "unknown person", or any other placeholder.

Here is the document text:
```
{content}
```

Your task is to analyze this document and extract structured information. Focus on:

1. **Main actors/participants** - People, organizations, entities mentioned or involved
2. **Key events and actions** - What happened, when, between whom
3. **Temporal information** - Dates, times, sequences of events
4. **Document type and content** - What kind of document is this?
5. **Key themes and topics** - What is this document about?

Return ONLY a valid JSON object with the following structure:

```json
{{
  "one_sentence_summary": "A brief one-sentence summary including main actors",
  "paragraph_summary": "A detailed paragraph (3-5 sentences) explaining the document's content, context, significance, and key points.",
  "date_range_earliest": "YYYY-MM-DD or YYYY-MM-DDTHH:MM format if dates are visible, otherwise null",
  "date_range_latest": "YYYY-MM-DD or YYYY-MM-DDTHH:MM format if dates are visible, otherwise null",
  "category": "One of: court_filing, email, letter, memorandum, report, transcript, financial_document, media_article, book_excerpt, photo_caption, mixed_document, public_record, other",
  "content_tags": ["array", "of", "relevant", "document-level", "tags"],
  "rdf_triples": [
    {{
      "timestamp": "YYYY-MM-DD or YYYY-MM-DDTHH:MM if available, otherwise omit",
      "actor": "PERSON NAME ONLY - Use 'Jeffrey Epstein' when you see jeeitunes@gmail.com or 'jee'",
      "action": "the action verb (e.g., 'sent email to', 'met with', 'testified before')",
      "target": "PERSON NAME ONLY - not organizations, movies, places",
      "location": "physical location if mentioned, otherwise omit",
      "actor_likely_type": "OPTIONAL - only if actor is unknown/unnamed/redacted",
      "tags": ["tags", "for", "this", "triple"],
      "explicit_topic": "short phrase describing the main theme directly evidenced",
      "implicit_topic": "short phrase describing what the interaction likely relates to"
    }}
  ]
}}
```

Guidelines for RDF triples:
- Create a sequential array capturing the key relationships and events in the document
- Include timestamps when dates/times are mentioned
- **CRITICAL - Actor field**: Actor must ALWAYS be a PERSON NAME ONLY
- Use consistent naming (e.g., always "Jeffrey Epstein" not "Epstein" or "jee")
- Actions should be descriptive verb phrases
- Focus on person-to-person AND person-to-entity relationships
- Order triples chronologically when timestamps are available

Return ONLY the JSON object, no additional text or explanation."""


@dataclass
class RDFTriple:
    """Represents a single relationship extracted from a document"""
    actor: str
    action: str
    target: str
    explicit_topic: str
    implicit_topic: str
    tags: List[str] = field(default_factory=list)
    timestamp: Optional[str] = None
    location: Optional[str] = None
    actor_likely_type: Optional[str] = None


@dataclass
class DocumentAnalysis:
    """Complete analysis of a document"""
    doc_id: str
    one_sentence_summary: str
    paragraph_summary: str
    category: str
    content_tags: List[str]
    rdf_triples: List[RDFTriple]
    date_range_earliest: Optional[str] = None
    date_range_latest: Optional[str] = None


@dataclass
class AnalysisResult:
    """Full result of analyzing a document"""
    doc_id: str
    file_path: str
    full_text: str
    analysis: DocumentAnalysis
    input_tokens: int = 0
    output_tokens: int = 0
    cost_usd: float = 0.0
    error: Optional[str] = None


class FactExtractor:
    """Main fact extraction pipeline"""
    
    def __init__(self, db_path: str = "fact_extraction.db"):
        self.db_path = db_path
        self.client = OpenAI()  # Uses OPENAI_API_KEY from environment
        self.init_database()
    
    def init_database(self):
        """Initialize SQLite database with schema"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Create documents table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS documents (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT UNIQUE NOT NULL,
                file_path TEXT NOT NULL,
                one_sentence_summary TEXT NOT NULL,
                paragraph_summary TEXT NOT NULL,
                date_range_earliest TEXT,
                date_range_latest TEXT,
                category TEXT NOT NULL,
                content_tags TEXT NOT NULL,
                full_text TEXT,
                analysis_timestamp TEXT NOT NULL,
                input_tokens INTEGER,
                output_tokens INTEGER,
                cost_usd REAL,
                error TEXT,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Create RDF triples table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS rdf_triples (
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
                sequence_order INTEGER NOT NULL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
            )
        """)
        
        # Create indexes
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_documents_doc_id ON documents(doc_id)")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_documents_category ON documents(category)")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_rdf_triples_doc_id ON rdf_triples(doc_id)")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_rdf_triples_actor ON rdf_triples(actor)")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_rdf_triples_timestamp ON rdf_triples(timestamp)")
        
        conn.commit()
        conn.close()
        print(f"✓ Database initialized at: {self.db_path}\n")
    
    def extract_json(self, text: str) -> str:
        """Extract JSON from markdown code blocks"""
        pattern = r'```(?:json)?\s*([\s\S]*?)\s*```'
        match = re.search(pattern, text)
        if match:
            return match.group(1)
        return text
    
    def analyze_document(self, doc_id: str, file_path: str, content: str) -> AnalysisResult:
        """Analyze a single document using OpenAI"""
        prompt = ANALYSIS_PROMPT.format(doc_id=doc_id, content=content)
        
        print(f"Analyzing {doc_id}...")
        
        try:
            # Use gpt-4.1-mini as specified
            completion = self.client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[{"role": "user", "content": prompt}],
                max_tokens=16000
            )
            
            response_text = completion.choices[0].message.content
            json_text = self.extract_json(response_text)
            
            # Parse the JSON response
            data = json.loads(json_text)
            
            # Convert to dataclass objects
            triples = []
            for triple_data in data.get("rdf_triples", []):
                # Skip malformed triples
                if not isinstance(triple_data, dict):
                    continue
                    
                # Ensure required fields exist
                if not triple_data.get("target"):
                    print(f"  ⚠️  Skipping triple with missing target: {triple_data}")
                    continue
                    
                triple = RDFTriple(
                    actor=triple_data.get("actor", "unknown"),
                    action=triple_data.get("action", "unknown action"),
                    target=triple_data.get("target", "unknown"),
                    explicit_topic=triple_data.get("explicit_topic", "unknown"),
                    implicit_topic=triple_data.get("implicit_topic", "unknown"),
                    tags=triple_data.get("tags", []),
                    timestamp=triple_data.get("timestamp"),
                    location=triple_data.get("location"),
                    actor_likely_type=triple_data.get("actor_likely_type")
                )
                triples.append(triple)
            
            analysis = DocumentAnalysis(
                doc_id=doc_id,
                one_sentence_summary=data["one_sentence_summary"],
                paragraph_summary=data["paragraph_summary"],
                category=data["category"],
                content_tags=data.get("content_tags", []),
                rdf_triples=triples,
                date_range_earliest=data.get("date_range_earliest"),
                date_range_latest=data.get("date_range_latest")
            )
            
            # Calculate cost (rough estimate for gpt-4.1-mini)
            input_tokens = completion.usage.prompt_tokens
            output_tokens = completion.usage.completion_tokens
            cost_usd = (input_tokens * 0.00015 / 1000) + (output_tokens * 0.0006 / 1000)
            
            print(f"  ✓ Analyzed {doc_id}: {len(triples)} triples extracted")
            
            return AnalysisResult(
                doc_id=doc_id,
                file_path=file_path,
                full_text=content,
                analysis=analysis,
                input_tokens=input_tokens,
                output_tokens=output_tokens,
                cost_usd=cost_usd
            )
            
        except Exception as e:
            print(f"  ❌ Error analyzing {doc_id}: {e}")
            return AnalysisResult(
                doc_id=doc_id,
                file_path=file_path,
                full_text=content,
                analysis=DocumentAnalysis(
                    doc_id=doc_id,
                    one_sentence_summary="Error during analysis",
                    paragraph_summary="An error occurred during document analysis.",
                    category="other",
                    content_tags=[],
                    rdf_triples=[]
                ),
                error=str(e)
            )
    
    def save_result(self, result: AnalysisResult):
        """Save analysis result to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Insert document
        cursor.execute("""
            INSERT OR REPLACE INTO documents 
            (doc_id, file_path, one_sentence_summary, paragraph_summary, date_range_earliest,
             date_range_latest, category, content_tags, full_text, analysis_timestamp,
             input_tokens, output_tokens, cost_usd, error)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            result.doc_id,
            result.file_path,
            result.analysis.one_sentence_summary,
            result.analysis.paragraph_summary,
            result.analysis.date_range_earliest,
            result.analysis.date_range_latest,
            result.analysis.category,
            json.dumps(result.analysis.content_tags),
            result.full_text,
            datetime.now().isoformat(),
            result.input_tokens,
            result.output_tokens,
            result.cost_usd,
            result.error
        ))
        
        # Insert RDF triples
        for i, triple in enumerate(result.analysis.rdf_triples):
            cursor.execute("""
                INSERT INTO rdf_triples
                (doc_id, timestamp, actor, action, target, location, actor_likely_type,
                 triple_tags, explicit_topic, implicit_topic, sequence_order)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                result.doc_id,
                triple.timestamp,
                triple.actor,
                triple.action,
                triple.target,
                triple.location,
                triple.actor_likely_type,
                json.dumps(triple.tags),
                triple.explicit_topic,
                triple.implicit_topic,
                i
            ))
        
        conn.commit()
        conn.close()
    
    def process_directory(self, directory: str):
        """Process all text files in a directory"""
        files = list(Path(directory).glob("*.txt"))
        print(f"Found {len(files)} documents to analyze\n")
        
        total_cost = 0.0
        total_triples = 0
        
        for i, file_path in enumerate(files):
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            doc_id = file_path.stem
            result = self.analyze_document(doc_id, str(file_path), content)
            self.save_result(result)
            
            total_cost += result.cost_usd
            total_triples += len(result.analysis.rdf_triples)
            
            print(f"Progress: {i+1}/{len(files)} documents analyzed\n")
            
            # Small delay to avoid rate limiting
            time.sleep(0.5)
        
        print("\n=== Analysis Complete ===")
        print(f"Total documents: {len(files)}")
        print(f"Total triples extracted: {total_triples}")
        print(f"Total cost: ${total_cost:.4f}")


def main():
    """Main entry point"""
    extractor = FactExtractor()
    extractor.process_directory("sample_data")


if __name__ == "__main__":
    main()
