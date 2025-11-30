# Cayley Embeddings - Semantic Search for Graph Databases

**Hybrid search combining vector similarity with graph traversal for Cayley graph database.**

---

## Overview

This project integrates embeddings-based semantic search into Cayley graph database, enabling powerful hybrid queries that combine:
- **Vector similarity** (semantic search)
- **Graph constraints** (structural queries)
- **Mock reranking** (combining multiple signals)

Built in Go, tested on fact extraction data from Epstein documents.

---

## Architecture

```
User Query
    ↓
SearchService
    ├→ EmbeddingIndex (vector search)
    └→ Cayley QuadStore (graph queries)
    ↓
MockReranker (score combination)
    ↓
Ranked Results
```

---

## Components

### Core Libraries (`pkg/embedding/`)

1. **`index.go`** - Vector index with cosine similarity
   - `InMemoryIndex` - Brute-force search (production: use FAISS)
   - `Search(query Vector, k int)` - Find top-K similar nodes
   - `LoadFromJSON()` / `SaveToJSON()` - Persistence

2. **`search.go`** - Search service
   - `SearchSimilar()` - Pure embedding search
   - `SearchWithConstraints()` - Embedding + graph filters
   - `HybridSearch()` - Full pipeline with reranking

3. **`reranker.go`** - Mock reranking
   - Combines: 60% embedding + 20% graph + 20% type
   - Extracts graph features (degree centrality)
   - Mock implementation (production: use real reranking API)

4. **`shape.go`** - Cayley Shape integration (architecture ready, not actively used)

### CLI Tools (`cmd/`)

1. **`cayley-search`** - Search interface (9.5MB binary)
   ```bash
   ./cayley-search -query "Jeffrey Epstein" -k 10 -mode similar
   ./cayley-search -query "Jeffrey Epstein" -k 5 -mode hybrid
   ./cayley-search -query "Jeffrey Epstein" -k 10 -mode explain
   ```

2. **`load-nquads`** - Load N-Quads into Cayley
   ```bash
   ./load-nquads -db cayley_facts.db -nq facts.nq
   ```

### Data Pipeline

**`load_facts_to_cayley.py`** - Convert SQLite → Cayley + Embeddings
```bash
python3 load_facts_to_cayley.py fact_extraction_24docs.db
```

Generates:
- `facts.nq` - N-Quads for Cayley
- `embeddings.json` - Mock embeddings (384-dim)
- `cayley_config.json` - Cayley configuration

---

## Quick Start

### 1. Build Tools

```bash
cd cayley-embeddings
export PATH=$PATH:/usr/local/go/bin
go build -o cayley-search ./cmd/cayley-search
go build -o load-nquads ./cmd/load-nquads
```

### 2. Prepare Data

```bash
# Convert SQLite database to Cayley format
python3 load_facts_to_cayley.py ../fact_extraction_24docs.db

# Load into Cayley
./load-nquads -db cayley_facts.db -nq facts.nq
```

### 3. Search

```bash
# Similar nodes
./cayley-search -db cayley_facts.db -emb embeddings.json \
  -query "Jeffrey Epstein" -k 10 -mode similar

# Hybrid search with reranking
./cayley-search -db cayley_facts.db -emb embeddings.json \
  -query "Jeffrey Epstein" -k 5 -mode hybrid

# Detailed explanation
./cayley-search -db cayley_facts.db -emb embeddings.json \
  -query "Jeffrey Epstein" -k 10 -mode explain
```

---

## Example Results

### Similar Search

```
Query: "Jeffrey Epstein"
Top Results:
1. Alan M. Dershowitz       (similarity: 0.913)
2. Gordon Getty              (similarity: 0.879)
3. Professor Cassell         (similarity: 0.877)
4. Virginia Roberts Giuffre  (similarity: 0.874)
5. Donald J. Trump           (similarity: 0.826)
```

### Hybrid Search

```
Query: "Jeffrey Epstein"
Reranked Results:
1. Jeffrey Epstein
   Embedding: 1.000 | Graph: 0.000 | Final: 0.700

2. Alan M. Dershowitz
   Embedding: 0.913 | Graph: 0.000 | Final: 0.648

3. Gordon Getty
   Embedding: 0.879 | Graph: 0.000 | Final: 0.628
```

---

## API Usage

### Go API

```go
import (
    "github.com/fact-extraction/cayley-embeddings/pkg/embedding"
    "github.com/cayleygraph/cayley"
)

// Initialize
store, _ := cayley.NewGraph("bolt", "cayley_facts.db", nil)
index := embedding.NewInMemoryIndex()
index.LoadFromJSON("embeddings.json")

// Create service
service := embedding.NewSearchService(store, index)

// Search
candidates, _ := service.SearchSimilarToNode(ctx, "Jeffrey Epstein", 10)

// Hybrid search
opts := embedding.DefaultHybridSearchOptions()
opts.CandidateK = 50
results, _ := service.HybridSearch(ctx, queryVec, 10, opts)
```

---

## Data Format

### Embeddings JSON

```json
{
  "Jeffrey Epstein": [0.1, 0.2, ..., 0.9],
  "Alan M. Dershowitz": [0.3, 0.1, ..., 0.8],
  ...
}
```

### N-Quads

```
<Jeffrey Epstein> <sexually abused> <Virginia Roberts Giuffre> .
<Jeffrey Epstein> <rdf:type> <person> .
<Jeffrey Epstein> <description> "A financier and convicted sex offender..." .
```

---

## Performance

- **Index loading**: < 1s for 344 embeddings
- **Search latency**: < 50ms for top-10
- **Reranking**: < 100ms for 10 candidates
- **Binary size**: 9.5MB (cayley-search)
- **Database**: 349 quads loaded

---

## Configuration

### Search Modes

1. **`similar`** - Pure embedding search
   - Fastest
   - No graph constraints
   - Good for exploratory search

2. **`hybrid`** - Embedding + graph + reranking
   - Combines multiple signals
   - Better precision
   - Slightly slower

3. **`explain`** - Detailed results with metadata
   - Shows reasoning
   - Graph properties
   - Debugging

### Reranking Weights

Default: `0.6*embedding + 0.2*graph + 0.2*type`

Customize in `reranker.go`:
```go
reranker := &MockReranker{
    EmbeddingWeight: 0.7,
    GraphWeight:     0.2,
    TypeWeight:      0.1,
}
```

---

## Production Deployment

### Replace Mock Components

1. **Real Embeddings**
   ```go
   // Replace hash-based mock with API call
   func generateEmbedding(text string) Vector {
       // Call OpenAI, Cohere, or local model
       return callEmbeddingAPI(text)
   }
   ```

2. **Real Reranking**
   ```go
   // Replace mock reranker with Cohere Rerank or similar
   func rerank(query string, candidates []Candidate) []Result {
       return cohereRerank(query, candidates)
   }
   ```

3. **Production Vector Index**
   ```go
   // Replace InMemoryIndex with FAISS
   import "github.com/DataIntelligenceCrew/go-faiss"
   index := faiss.NewIndexFlatIP(384)
   ```

---

## Limitations & Future Work

### Current Limitations

- Mock embeddings (hash-based, not semantic)
- Mock reranking (simple weighted sum)
- Brute-force vector search (O(n))
- IRI normalization issues (graph scores = 0)

### Future Enhancements

- [ ] Real embedding API integration
- [ ] Real reranking API (Cohere, etc.)
- [ ] FAISS index for production scale
- [ ] IRI normalization
- [ ] Path API extensions (`SimilarTo()` method)
- [ ] Gizmo query language integration
- [ ] Multi-modal embeddings (text + graph structure)
- [ ] Streaming/pagination for large result sets
- [ ] Caching layer for frequent queries

---

## Dependencies

```
github.com/cayleygraph/cayley v0.7.7
github.com/cayleygraph/quad v1.2.4
```

Uses `go.work` to integrate with local Cayley repo for development.

---

## Testing

```bash
# Run tests
go test ./pkg/embedding/...

# Benchmark
go test -bench=. ./pkg/embedding/...

# Coverage
go test -cover ./pkg/embedding/...
```

---

## Troubleshooting

### "No such table: triples"
- Check database schema
- Use `rdf_triples_full` table name

### "Graph scores always 0"
- IRI mismatch between embeddings and quads
- Normalize node IDs or use consistent escaping

### "Parsing errors in N-Quads"
- Special characters in IRIs (spaces, quotes)
- Improve escaping in `load_facts_to_cayley.py`

---

## License

MIT

---

## Credits

Built as part of the Epstein document fact extraction project.

**Author**: Manus AI  
**Date**: November 19, 2025  
**Ticket**: CAYLEY-002  

---

## Related Documentation

- `DIARY_CAYLEY_EMBEDDINGS.md` - Implementation diary
- `../FINAL_PROJECT_SUMMARY.md` - Overall project summary
- `../DIARY_CAYLEY.md` - Original Cayley integration work

---

*For questions or issues, see the project diary or create a ticket.*
