# Fact Extraction Pipeline - Epstein Document Explorer Replication

This project replicates the fact extraction methodology from the [Epstein-doc-explorer](https://github.com/maxandrews/Epstein-doc-explorer) repository, implementing it in Python with Go-style patterns (dataclasses, explicit typing, structured error handling).

## Overview

The pipeline extracts structured **RDF triples** (subject-action-object relationships) from legal documents using LLM-powered analysis. Each triple captures:

- **Actor** (subject): The person performing the action
- **Action** (verb): What they did
- **Target** (object): Who/what they interacted with
- **Metadata**: Timestamps, locations, tags, explicit/implicit topics

## Architecture

```
┌─────────────────┐
│  Raw Documents  │
│   (30 .txt)     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  LLM Analysis   │
│ (gpt-4.1-mini)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  JSON Response  │
│  (structured)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ SQLite Database │
│  - documents    │
│  - rdf_triples  │
└─────────────────┘
```

## Key Features

### 1. **RDF Triple Extraction**
Extracts relationships in the form:
```
Jeffrey Epstein → attended event with → Donald Trump at Mar-a-Lago
```

### 2. **Semantic Tagging**
Each triple includes:
- **Tags**: Contextual labels (e.g., `sexual_abuse`, `legal_strategy`, `media`)
- **Explicit Topic**: What the interaction directly says
- **Implicit Topic**: What it likely implies

### 3. **Entity Recognition**
Handles multiple identifiers for the same person:
- `jeeitunes@gmail.com` → Jeffrey Epstein
- `jee` → Jeffrey Epstein
- Consistent naming across all triples

### 4. **Temporal Information**
Extracts and stores timestamps when mentioned in documents

### 5. **Document Categorization**
Classifies documents into types: `email`, `court_filing`, `transcript`, `book_excerpt`, etc.

## Implementation

### Core Components

#### 1. `extract_facts.py` - Main Pipeline
```python
class FactExtractor:
    def analyze_document(doc_id, file_path, content) -> AnalysisResult
    def save_result(result: AnalysisResult)
    def process_directory(directory: str)
```

#### 2. Database Schema
```sql
-- Documents table
CREATE TABLE documents (
    doc_id TEXT UNIQUE,
    one_sentence_summary TEXT,
    paragraph_summary TEXT,
    category TEXT,
    content_tags TEXT,  -- JSON array
    date_range_earliest TEXT,
    date_range_latest TEXT,
    full_text TEXT,
    input_tokens INTEGER,
    output_tokens INTEGER,
    cost_usd REAL
);

-- RDF triples table
CREATE TABLE rdf_triples (
    doc_id TEXT,
    actor TEXT NOT NULL,
    action TEXT NOT NULL,
    target TEXT NOT NULL,
    timestamp TEXT,
    location TEXT,
    actor_likely_type TEXT,
    triple_tags TEXT,  -- JSON array
    explicit_topic TEXT,
    implicit_topic TEXT,
    sequence_order INTEGER
);
```

#### 3. Analysis Prompt
The prompt instructs the LLM to:
- Extract person-to-person and person-to-entity relationships
- Identify Jeffrey Epstein under various aliases
- Tag relationships with contextual metadata
- Provide both explicit and implicit interpretations
- Return structured JSON

## Results

### Processing Statistics
- **Documents analyzed**: 30
- **Total triples extracted**: 256
- **Average triples per document**: 8.5
- **Total cost**: $0.0994 (~$0.10)
- **Input tokens**: 529,636
- **Output tokens**: 33,197

### Top Actors (by relationship count)
1. Jeffrey Epstein: 39 relationships
2. Alan M. Dershowitz: 25 relationships
3. Donald J. Trump: 18 relationships
4. Paul Cassell: 14 relationships
5. Gordon Getty: 13 relationships

### Top Tags
1. sexual abuse: 11 occurrences
2. media: 8 occurrences
3. investigation: 8 occurrences
4. allegations: 8 occurrences
5. real estate: 7 occurrences

### Document Categories
- book_excerpt: 6
- transcript: 5
- court_filing: 4
- mixed_document: 4
- financial_document: 3
- letter: 3
- media_article: 2
- email: 1

## Usage

### Run Extraction
```bash
python3 extract_facts.py
```

### Analyze Results
```bash
python3 analyze_results.py
```

### Query Database
```bash
sqlite3 fact_extraction.db
```

Example queries:
```sql
-- Find all Jeffrey Epstein relationships
SELECT actor, action, target, location 
FROM rdf_triples 
WHERE actor LIKE '%Epstein%';

-- Find relationships by tag
SELECT actor, action, target 
FROM rdf_triples 
WHERE triple_tags LIKE '%sexual_abuse%';

-- Get document summaries
SELECT doc_id, one_sentence_summary, category 
FROM documents;
```

## Comparison to Original

### Original (TypeScript)
- Uses Claude AI via Anthropic SDK
- Node.js/TypeScript runtime
- Processes 2000+ documents
- Includes tag clustering (K-means)
- Entity deduplication with LLM
- React visualization frontend

### This Implementation (Python)
- Uses OpenAI API (gpt-4.1-mini)
- Python runtime with Go-style patterns
- Processes 30 sample documents
- Core extraction pipeline only
- Demonstrates methodology at low cost
- Focuses on replicating the extraction logic

## Files

```
fact-extraction-go/
├── extract_facts.py          # Main extraction pipeline
├── analyze_results.py         # Analysis and reporting
├── main.go                    # Go implementation (alternative)
├── go.mod                     # Go dependencies
├── sample_data/               # 30 sample documents
├── fact_extraction.db         # SQLite database with results
├── extraction.log             # Processing log
├── analysis_report.txt        # Analysis output
└── README.md                  # This file
```

## Key Insights from Extraction

The pipeline successfully identified:

1. **Social Networks**: Connections between Jeffrey Epstein, Donald Trump, Prince Andrew, Alan Dershowitz, and others
2. **Event Attendance**: Parties, conferences, and social gatherings
3. **Legal Proceedings**: Depositions, court filings, witness testimony
4. **Temporal Patterns**: When relationships and events occurred
5. **Implicit Meanings**: What interactions suggest beyond their surface description

## Future Enhancements

To fully replicate the original:
- Add tag clustering (K-means on embeddings)
- Implement entity deduplication
- Add hop distance calculation from Jeffrey Epstein
- Create network visualization
- Process full document corpus
- Add community editing features

## Cost Efficiency

Processing 30 documents cost ~$0.10, demonstrating the approach is cost-effective for large-scale document analysis. The original project processed ~2000 documents for approximately $50.

## License

MIT (matching the original repository)
