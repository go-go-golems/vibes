# Vector Database Research

## chromem-go

[GitHub Repository](https://github.com/philippgille/chromem-go)

### Overview
Chromem-go is an embeddable vector database for Go with a Chroma-like interface and zero third-party dependencies. It's designed to be used in-memory with optional persistence, making it ideal for embedding in Go applications without requiring a separate database service.

### Key Features
1. **Zero Dependencies**: No third-party libraries required
2. **Embeddable**: Like SQLite, no client-server model, no separate DB to maintain
3. **Multithreaded Processing**: Makes use of Go's native concurrency features
4. **WebAssembly Support**: Experimental WASM binding

5. **Embedding Creators**:
   - **Hosted Services**:
     - OpenAI (default)
     - Azure OpenAI
     - GCP Vertex AI
     - Cohere
     - Mistral
     - Jina
     - mixedbread.ai
   - **Local Services**:
     - Ollama
     - LocalAI
   - **Custom**: Implement `chromem.EmbeddingFunc` interface

6. **Similarity Search**:
   - Exhaustive nearest neighbor search using cosine similarity

7. **Filters**:
   - Document filters: `$contains`, `$not_contains`
   - Metadata filters: Exact matches

8. **Storage Options**:
   - In-memory
   - Optional immediate persistence
   - Backup capabilities (export/import)

### Interface
Chromem-go provides a Chroma-like interface:

```go
// Set up chromem-go in-memory
db := chromem.NewDB()

// Create collection
collection, _ := db.CreateCollection("all-my-documents", nil, nil)

// Add docs to the collection
_ = collection.Add(ctx,
    []string{"doc1", "doc2"}, // unique IDs
    nil, // automatic embedding
    []map[string]string{{"source": "notion"}, {"source": "google-docs"}}, // metadata
    []string{"This is document1", "This is document2"}, // content
)

// Query/search similar results
results, _ := collection.Query(ctx,
    "This is a query document", // query text
    2, // number of results
    map[string]string{"metadata_field": "is_equal_to_this"}, // optional filter
    map[string]string{"$contains": "search_string"}, // optional filter
)
```

### Performance
On a mid-range 2020 Intel laptop CPU:
- Query 1,000 documents in 0.3 ms
- Query 100,000 documents in 40 ms
- Very few and small memory allocations

### Use Cases
- Retrieval augmented generation (RAG)
- Question answering (Q&A)
- Text and code search
- Recommendation systems
- Classification
- Clustering

### Advantages for Our Framework
1. **Simplicity**: Easy to integrate with no external dependencies
2. **Performance**: Fast for common use cases
3. **Go-native**: Designed specifically for Go applications
4. **Embedding Support**: Built-in support for various embedding providers
5. **Concurrency**: Native support for Go's concurrency model

## Other Go Vector Database Options

### 1. pgvector with Go
PostgreSQL with the pgvector extension can be used from Go applications for vector search capabilities.

**Key Features**:
- Production-ready database
- Scales to larger datasets
- Supports multiple index types (IVFFlat, HNSW)
- Transactional guarantees

**Drawbacks**:
- Requires running a separate PostgreSQL instance
- More complex setup and maintenance

### 2. Weaviate Go Client
Weaviate is a vector database with a Go client library.

**Key Features**:
- GraphQL-based API
- Multi-modal (text, images, etc.)
- Classification capabilities
- Cross-references between objects

**Drawbacks**:
- Requires running a separate Weaviate instance
- More complex architecture

### 3. Milvus Go Client
Milvus is a vector database with a Go client.

**Key Features**:
- Highly scalable
- Multiple index types
- Hybrid search (vector + scalar)
- Cloud-native architecture

**Drawbacks**:
- Requires running a separate Milvus cluster
- More complex to set up and maintain

## Recommendation for Our Framework

For our LLM agent framework, chromem-go appears to be the best fit because:

1. **Embeddability**: No need to run a separate database service
2. **Simplicity**: Easy to integrate and use
3. **Go-native**: Designed specifically for Go
4. **Zero Dependencies**: Keeps our framework lightweight
5. **Performance**: Sufficient for most use cases
6. **Embedding Support**: Built-in support for various embedding providers

For production use cases with very large datasets, we could provide adapters for other vector databases like pgvector, Weaviate, or Milvus, but chromem-go should be the default for simplicity and ease of use.
