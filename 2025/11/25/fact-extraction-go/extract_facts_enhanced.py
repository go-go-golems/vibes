#!/usr/bin/env python3
"""
Enhanced Fact Extraction Pipeline

Processes documents with:
1. Base RDF triple extraction
2. Automatic tag clustering
3. Entity deduplication
4. Progress checkpointing
"""

import json
import sqlite3
import time
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import List, Optional
from openai import OpenAI

# Import our advanced features
import sys
sys.path.append('/home/ubuntu/fact-extraction-go')
from tag_clustering_simple import SimpleTagger
from entity_deduplication import EntityDeduplicator

client = OpenAI()

@dataclass
class RDFTriple:
    actor: str
    action: str
    target: str
    explicit_topic: str
    implicit_topic: str
    tags: List[str]
    timestamp: Optional[str] = None
    location: Optional[str] = None
    actor_likely_type: Optional[str] = None

@dataclass
class DocumentAnalysis:
    doc_id: str
    one_sentence_summary: str
    paragraph_summary: str
    category: str
    content_tags: List[str]
    date_range_earliest: Optional[str]
    date_range_latest: Optional[str]
    rdf_triples: List[RDFTriple]
    full_text: str
    input_tokens: int
    output_tokens: int
    cost_usd: float

class EnhancedFactExtractor:
    """Enhanced fact extractor with clustering and deduplication"""
    
    def __init__(self, db_path: str = "fact_extraction_200.db"):
        self.db_path = db_path
        self.init_database()
        
    def init_database(self):
        """Initialize database with enhanced schema"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Documents table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS documents (
                doc_id TEXT PRIMARY KEY,
                one_sentence_summary TEXT,
                paragraph_summary TEXT,
                category TEXT,
                content_tags TEXT,
                date_range_earliest TEXT,
                date_range_latest TEXT,
                full_text TEXT,
                input_tokens INTEGER,
                output_tokens INTEGER,
                cost_usd REAL,
                processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # RDF triples table with cluster columns
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS rdf_triples (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT,
                timestamp TEXT,
                actor TEXT,
                action TEXT,
                target TEXT,
                location TEXT,
                actor_likely_type TEXT,
                triple_tags TEXT,
                explicit_topic TEXT,
                implicit_topic TEXT,
                sequence_order INTEGER,
                cluster_ids TEXT,
                cluster_themes TEXT,
                FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
            )
        """)
        
        # Processing log for checkpointing
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS processing_log (
                doc_id TEXT PRIMARY KEY,
                status TEXT,
                error TEXT,
                processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        conn.commit()
        conn.close()
        
        print(f"✓ Database initialized at: {self.db_path}")
    
    def is_processed(self, doc_id: str) -> bool:
        """Check if document already processed"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute("SELECT status FROM processing_log WHERE doc_id = ?", (doc_id,))
        result = cursor.fetchone()
        conn.close()
        return result is not None and result[0] == 'success'
    
    def analyze_document(self, doc_id: str, file_path: Path, content: str) -> DocumentAnalysis:
        """Analyze a single document (same as original)"""
        
        prompt = f"""Analyze this legal document and extract structured information.

Document ID: {doc_id}

Content:
{content[:8000]}

Extract the following:

1. **one_sentence_summary**: A single sentence summarizing the document
2. **paragraph_summary**: A 2-3 sentence summary
3. **category**: Document type (email, court_filing, deposition, transcript, letter, memo, book_excerpt, media_article, financial_document, flight_log, calendar, address_book, public_record, report, mixed_document)
4. **content_tags**: 3-7 tags describing the content
5. **date_range**: Earliest and latest dates mentioned (if any)
6. **rdf_triples**: Extract person-to-person and person-to-entity relationships

For RDF triples, identify:
- **actor**: The person performing the action
- **action**: What they did (verb phrase)
- **target**: Who/what they interacted with
- **timestamp**: When it happened (if mentioned)
- **location**: Where it happened (if mentioned)
- **actor_likely_type**: person, organization, or unknown
- **tags**: Contextual tags for this relationship
- **explicit_topic**: What this interaction directly says
- **implicit_topic**: What it likely implies

IMPORTANT: Identify Jeffrey Epstein consistently, even if referred to as "jeeitunes@gmail.com", "jee", or other aliases.

Return JSON:
{{
  "one_sentence_summary": "...",
  "paragraph_summary": "...",
  "category": "...",
  "content_tags": ["tag1", "tag2", ...],
  "date_range_earliest": "YYYY-MM-DD or null",
  "date_range_latest": "YYYY-MM-DD or null",
  "rdf_triples": [
    {{
      "actor": "Person Name",
      "action": "action description",
      "target": "Target Name",
      "timestamp": "YYYY-MM-DD or null",
      "location": "Location or null",
      "actor_likely_type": "person",
      "tags": ["tag1", "tag2"],
      "explicit_topic": "what it says",
      "implicit_topic": "what it implies"
    }}
  ]
}}"""

        response = client.chat.completions.create(
            model="gpt-4.1-mini",
            messages=[{"role": "user", "content": prompt}],
            temperature=0.3
        )
        
        content_response = response.choices[0].message.content.strip()
        
        # Extract JSON
        if "```json" in content_response:
            json_text = content_response.split("```json")[1].split("```")[0].strip()
        elif "```" in content_response:
            json_text = content_response.split("```")[1].split("```")[0].strip()
        else:
            json_text = content_response
        
        data = json.loads(json_text)
        
        # Convert to dataclass objects
        triples = []
        for triple_data in data.get("rdf_triples", []):
            if not isinstance(triple_data, dict):
                continue
            if not triple_data.get("target"):
                continue
            
            triple = RDFTriple(
                actor=triple_data.get("actor", "unknown"),
                action=triple_data.get("action", "unknown action"),
                target=triple_data.get("target", "unknown"),
                explicit_topic=triple_data.get("explicit_topic", "unknown"),
                implicit_topic=triple_data.get("implicit_topic", "unknown"),
                tags=triple_data.get("tags", []) if isinstance(triple_data.get("tags"), list) else [],
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
            date_range_earliest=data.get("date_range_earliest"),
            date_range_latest=data.get("date_range_latest"),
            rdf_triples=triples,
            full_text=content,
            input_tokens=response.usage.prompt_tokens,
            output_tokens=response.usage.completion_tokens,
            cost_usd=response.usage.prompt_tokens * 0.00000015 + response.usage.completion_tokens * 0.0000006
        )
        
        return analysis
    
    def save_result(self, result: DocumentAnalysis):
        """Save analysis result to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Save document
        cursor.execute("""
            INSERT OR REPLACE INTO documents 
            (doc_id, one_sentence_summary, paragraph_summary, category, content_tags,
             date_range_earliest, date_range_latest, full_text, input_tokens, output_tokens, cost_usd)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            result.doc_id,
            result.one_sentence_summary,
            result.paragraph_summary,
            result.category,
            json.dumps(result.content_tags),
            result.date_range_earliest,
            result.date_range_latest,
            result.full_text,
            result.input_tokens,
            result.output_tokens,
            result.cost_usd
        ))
        
        # Save triples
        for i, triple in enumerate(result.rdf_triples):
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
        
        # Log success
        cursor.execute("""
            INSERT OR REPLACE INTO processing_log (doc_id, status)
            VALUES (?, 'success')
        """, (result.doc_id,))
        
        conn.commit()
        conn.close()
    
    def process_directory(self, directory: str, limit: Optional[int] = None):
        """Process all documents in directory"""
        data_dir = Path(directory)
        files = sorted(data_dir.glob("*.txt"))
        
        if limit:
            files = files[:limit]
        
        print(f"Found {len(files)} documents to analyze")
        
        total_cost = 0
        processed = 0
        skipped = 0
        
        for i, file_path in enumerate(files, 1):
            doc_id = file_path.stem
            
            # Skip if already processed
            if self.is_processed(doc_id):
                skipped += 1
                continue
            
            print(f"\nAnalyzing {doc_id}... ({i}/{len(files)})")
            
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                
                result = self.analyze_document(doc_id, file_path, content)
                self.save_result(result)
                
                total_cost += result.cost_usd
                processed += 1
                
                print(f"  ✓ Analyzed: {len(result.rdf_triples)} triples extracted")
                print(f"  Cost: ${result.cost_usd:.4f} | Total: ${total_cost:.4f}")
                
                # Progress checkpoint every 10 docs
                if processed % 10 == 0:
                    print(f"\n📊 Progress: {processed}/{len(files)} documents | ${total_cost:.4f}")
                
                # Rate limiting
                time.sleep(0.5)
                
            except Exception as e:
                print(f"  ✗ Error: {e}")
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                cursor.execute("""
                    INSERT OR REPLACE INTO processing_log (doc_id, status, error)
                    VALUES (?, 'error', ?)
                """, (doc_id, str(e)))
                conn.commit()
                conn.close()
        
        print(f"\n{'='*80}")
        print(f"Extraction Complete:")
        print(f"  Processed: {processed}")
        print(f"  Skipped: {skipped}")
        print(f"  Total cost: ${total_cost:.4f}")
        print(f"{'='*80}")

def main():
    print("="*80)
    print("ENHANCED FACT EXTRACTION PIPELINE")
    print("="*80)
    
    # Step 1: Extract facts from 200 documents
    extractor = EnhancedFactExtractor(db_path="fact_extraction_200.db")
    extractor.process_directory("data_200", limit=200)
    
    # Step 2: Run tag clustering
    print("\n" + "="*80)
    print("RUNNING TAG CLUSTERING")
    print("="*80)
    clusterer = SimpleTagger(db_path="fact_extraction_200.db", n_clusters=30)
    clusterer.run_pipeline()
    
    # Step 3: Run entity deduplication
    print("\n" + "="*80)
    print("RUNNING ENTITY DEDUPLICATION")
    print("="*80)
    deduplicator = EntityDeduplicator(db_path="fact_extraction_200.db")
    deduplicator.run_pipeline()
    
    print("\n" + "="*80)
    print("✓ COMPLETE PIPELINE FINISHED")
    print("="*80)

if __name__ == "__main__":
    main()
