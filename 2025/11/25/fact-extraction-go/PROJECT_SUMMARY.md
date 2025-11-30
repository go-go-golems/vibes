# Fact Extraction & Graph Query System - Project Summary

**Date**: November 19, 2025  
**Goal**: Replicate Epstein-doc-explorer fact extraction methodology with Go-based graph querying

---

## What Was Built

### 1. Python Fact Extraction Pipeline (`extract_facts.py`)
- **Purpose**: Extract RDF triples from legal documents using LLM analysis
- **Model**: OpenAI gpt-4.1-mini
- **Input**: 30 sample documents from Epstein corpus
- **Output**: SQLite database with 256 RDF triples
- **Cost**: $0.0994 (~$0.10)

### 2. Go Graph Query CLI (`graph-query/`)
- **Purpose**: Graph database interface for relationship queries
- **Technology**: Cayley graph database with BoltDB backend
- **Features**: 
  - Load SQLite data into graph
  - Query actor relationships
  - Find neighbors (direct connections)
  - Path finding between entities
  - Graph statistics

### 3. Analysis & Documentation
- **Extraction diary**: DIARY_EXTRACTION.md
- **Cayley diary**: DIARY_CAYLEY.md  
- **Analysis report**: analysis_report.txt
- **Query results**: queries_output.txt

---

## Key Results

### Extraction Statistics
| Metric | Value |
|--------|-------|
| Documents processed | 30 |
| Triples extracted | 256 |
| Avg triples/doc | 8.5 |
| Total cost | $0.0994 |
| Input tokens | 529,636 |
| Output tokens | 33,197 |

### Graph Statistics
| Metric | Value |
|--------|-------|
| Total quads | 1,776 |
| Unique subjects | 344 |
| Unique predicates | 220 |
| Tags stored | 522 |

### Top Actors
1. Jeffrey Epstein: 39 relationships
2. Alan M. Dershowitz: 25 relationships
3. Donald J. Trump: 18 relationships
4. Paul Cassell: 14 relationships
5. Gordon Getty: 13 relationships

---

## Files & Structure

```
fact-extraction-go/
├── extract_facts.py              # Python extraction pipeline
├── analyze_results.py            # Analysis script
├── fact_extraction.db            # SQLite database (256 triples)
├── extraction.log                # Processing log
├── analysis_report.txt           # Detailed analysis
├── DIARY_EXTRACTION.md           # Extraction process diary
├── DIARY_CAYLEY.md               # Graph DB integration diary
├── README.md                     # Project documentation
├── sample_data/                  # 30 sample documents
├── graph-query/                  # Go CLI tool
│   ├── main.go                   # Graph query implementation
│   ├── go.mod                    # Go dependencies
│   ├── graph-query               # Compiled binary
│   ├── facts.db                  # Cayley BoltDB database
│   ├── run_queries.sh            # Query script
│   └── queries_output.txt        # Query results
└── PROJECT_SUMMARY.md            # This file
```

---

## How to Use

### Run Extraction (Python)
```bash
cd fact-extraction-go
python3 extract_facts.py
```

### Analyze Results
```bash
python3 analyze_results.py
```

### Query Graph (Go)
```bash
cd graph-query

# Load data into Cayley
./graph-query load

# Query specific actor
./graph-query query "Jeffrey Epstein"

# Find neighbors
./graph-query neighbors "Prince Andrew"

# Show statistics
./graph-query stats

# Run all queries
./run_queries.sh
```

---

## Sample Queries & Results

### Jeffrey Epstein's Relationships
- 39 total relationships extracted
- Connections to: Ghislaine Maxwell, Donald Trump, Prince Andrew, victims
- Activities: Events, meetings, abuse allegations, legal proceedings

### Prince Andrew's Network
**Outgoing**: 1 connection (Jeffrey Epstein)
**Incoming**: 5 connections (Jeffrey Epstein x2, Jane Doe #3, Paul Cassell, Virginia Roberts Giuffre)

### Ghislaine Maxwell's Network  
**Outgoing**: 2 connections (Virginia Roberts Giuffre x2)
**Incoming**: 4 connections (Jeffrey Epstein x3, Virginia Roberts Giuffre)

---

## Next Steps (Future Work)

### Tag Clustering
**What**: Group 522 tags into semantic clusters using K-means
**How**: 
1. Generate embeddings for each tag
2. Apply K-means clustering (e.g., 30 clusters)
3. Assign each triple to top-3 relevant clusters
**Benefit**: Better filtering and categorization

### Entity Deduplication
**What**: Merge duplicate entities with different names
**Examples**:
- "Alan Dershowitz" vs "Alan M. Dershowitz" vs "Professor Alan Dershowitz"
- "Virginia Roberts" vs "Virginia Roberts Giuffre" vs "Virginia Giuffre"
**How**:
1. Use LLM to identify similar entities
2. Create canonical name mapping
3. Update all triples to use canonical names
**Benefit**: More accurate relationship counts

### Scale to 200 Documents
**Current**: 30 docs = $0.10
**Projected**: 200 docs = ~$0.67
**Enhancements needed**:
- Batch processing
- Progress checkpointing
- Better error recovery
- Entity deduplication (critical at scale)
- Tag clustering (for better analysis)

---

## Technical Achievements

✅ Faithful replication of original methodology  
✅ Cost-effective extraction ($0.10 for 30 docs)  
✅ Go-based graph database with Cayley  
✅ CLI tool for graph queries  
✅ Comprehensive documentation  
✅ Structured data model (RDF triples + metadata)  
✅ Type-safe implementation  

---

## Lessons Learned

### Extraction
1. LLM output validation is critical (~4% of triples had missing fields)
2. Prompt engineering significantly impacts quality
3. Metadata capture (explicit/implicit topics) adds valuable context
4. Entity consistency requires active management

### Graph Database
1. Cayley's quad model naturally fits RDF triples with metadata
2. BoltDB backend provides serverless, embedded operation
3. Graph traversal API has learning curve but powerful
4. Type safety in Go caught many bugs at compile time

### Tooling
1. Python faster for prototyping, Go better for production tools
2. Dataclasses provide Go-like structure in Python
3. CLI frameworks (Cobra) make professional tools easy
4. Documentation-as-you-go captures valuable context

---

## Cost Analysis

### Current (30 documents)
- Extraction: $0.0994
- Storage: ~5MB SQLite + ~2MB BoltDB
- Time: ~15 minutes

### Projected (200 documents)
- Extraction: ~$0.67
- Storage: ~35MB SQLite + ~15MB BoltDB  
- Time: ~1.5 hours

### Projected (2000 documents - full corpus)
- Extraction: ~$6.70
- Storage: ~350MB SQLite + ~150MB BoltDB
- Time: ~15 hours

**Note**: Original project cost ~$50 for 2000 docs using Claude with caching. Our approach is more cost-effective but lacks prompt caching.

---

## Conclusion

Successfully replicated the core fact extraction methodology from Epstein-doc-explorer in Python, then built a Go-based graph query tool using Cayley. The system demonstrates:

- **Accurate extraction**: 256 high-quality RDF triples with rich metadata
- **Graph querying**: Fast relationship exploration via Cayley
- **Scalability**: Can process 200+ documents cost-effectively
- **Extensibility**: Ready for tag clustering and entity deduplication

The project provides a solid foundation for larger-scale document analysis and relationship discovery.
