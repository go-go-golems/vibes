# Go Fact Extractor Design - Using Geppetto Framework

**Date**: November 19, 2025  
**Goal**: Port Python fact extraction pipeline to Go using geppetto for better performance and native Cayley integration

---

## Architecture Overview

### Python Version (Current)
```
Document → OpenAI API → JSON Response → Parse → SQLite
```

**Limitations**:
- Sequential processing (no concurrency)
- Python overhead
- Separate process from Cayley graph
- Manual error handling and retries

### Go Version (Target)
```
Document → Geppetto Turn → OpenAI Engine → Event Stream → Parser → SQLite + Cayley
```

**Advantages**:
- Native concurrency with goroutines
- Type safety
- Direct Cayley integration
- Geppetto's event system for streaming
- Built-in retry and error handling

---

## Geppetto Framework Concepts

### 1. Turn-Based API
**Turn** = A conversation unit containing blocks (system, user, assistant, tool)

```go
type Turn struct {
    Blocks []Block
    Data   map[string]any
}
```

**Our Use Case**:
- System block: Extraction instructions
- User block: Document text
- Assistant block: Extracted facts (JSON)

### 2. Glazed Commands
**Command** = Declarative CLI with structured parameters

```go
type FactExtractCommand struct {
    *cmds.CommandDescription
}
```

**Parameters**:
- Input directory
- Output database
- Model selection
- Batch size
- Concurrency level

### 3. Step Settings
**StepSettings** = Configuration for LLM API calls

```go
stepSettings := &settings.StepSettings{
    API:  &settings.APISettings{...},
    Chat: &settings.ChatSettings{
        Engine: "gpt-4.1-mini",
        Stream: true,
    },
}
```

### 4. Event Router Pattern
**Events** = Streaming updates during inference

```go
router := events.NewEventRouter()
router.RegisterHandler(func(event events.Event) {
    // Handle streaming tokens, metadata, etc.
})
```

---

## Component Design

### 1. Document Loader
```go
type DocumentLoader struct {
    basePath string
}

func (dl *DocumentLoader) LoadDocuments(limit int) ([]Document, error)
```

**Responsibilities**:
- Read text files from directory
- Extract document ID from filename
- Limit to N documents
- Return structured Document type

### 2. Extraction Prompt Builder
```go
type PromptBuilder struct {
    systemPrompt string
}

func (pb *PromptBuilder) BuildTurn(doc Document) *turns.Turn
```

**Responsibilities**:
- Create system prompt (extraction instructions)
- Create user prompt (document text)
- Build Turn with proper blocks
- Add metadata to Turn.Data

### 3. Geppetto Extractor
```go
type GeppettoExtractor struct {
    engine       *openai.OpenAIEngine
    stepSettings *settings.StepSettings
    router       *events.EventRouter
}

func (ge *GeppettoExtractor) Extract(ctx context.Context, turn *turns.Turn) (*ExtractionResult, error)
```

**Responsibilities**:
- Run inference using OpenAI engine
- Handle streaming events
- Parse JSON response
- Return structured ExtractionResult

### 4. Result Parser
```go
type ResultParser struct{}

func (rp *ResultParser) Parse(assistantText string) (*ExtractionResult, error)
```

**Responsibilities**:
- Extract JSON from assistant response
- Parse into RDFTriple structs
- Validate required fields
- Handle parsing errors

### 5. Database Writer
```go
type DatabaseWriter struct {
    db *sql.DB
}

func (dw *DatabaseWriter) SaveResult(result *ExtractionResult) error
```

**Responsibilities**:
- Write to SQLite database
- Insert documents, triples, tags
- Handle transactions
- Track processing status

### 6. Cayley Integrator
```go
type CayleyIntegrator struct {
    store *cayley.Handle
}

func (ci *CayleyIntegrator) LoadTriples(triples []RDFTriple) error
```

**Responsibilities**:
- Convert RDF triples to quads
- Load into Cayley graph
- Build metadata quads
- Maintain graph consistency

---

## Data Structures

### Document
```go
type Document struct {
    ID       string
    FilePath string
    Content  string
}
```

### RDFTriple
```go
type RDFTriple struct {
    Actor           string
    Action          string
    Target          string
    ExplicitTopic   string
    ImplicitTopic   string
    Tags            []string
    Timestamp       *string
    Location        *string
    ActorLikelyType *string
}
```

### ExtractionResult
```go
type ExtractionResult struct {
    DocumentID string
    Triples    []RDFTriple
    CostUSD    float64
    TokensIn   int
    TokensOut  int
}
```

---

## Concurrency Model

### Worker Pool Pattern
```go
func (ge *GeppettoExtractor) ProcessBatch(
    ctx context.Context,
    docs []Document,
    workers int,
) ([]ExtractionResult, error) {
    jobs := make(chan Document, len(docs))
    results := make(chan ExtractionResult, len(docs))
    
    // Start workers
    var wg sync.WaitGroup
    for i := 0; i < workers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for doc := range jobs {
                result, err := ge.extractDocument(ctx, doc)
                if err != nil {
                    log.Error().Err(err).Msg("extraction failed")
                    continue
                }
                results <- result
            }
        }()
    }
    
    // Send jobs
    for _, doc := range docs {
        jobs <- doc
    }
    close(jobs)
    
    // Wait and collect
    wg.Wait()
    close(results)
    
    return collectResults(results), nil
}
```

**Benefits**:
- Parallel API calls
- Controlled concurrency
- Error isolation
- Progress tracking

---

## CLI Interface

### Commands

#### 1. Extract Command
```bash
go-extractor extract \
  --input data_200/ \
  --output fact_extraction.db \
  --model gpt-4.1-mini \
  --workers 5 \
  --limit 200
```

**Flags**:
- `--input`: Input directory with documents
- `--output`: SQLite database path
- `--model`: LLM model to use
- `--workers`: Number of concurrent workers
- `--limit`: Max documents to process
- `--stream`: Enable streaming output
- `--load-cayley`: Load results into Cayley after extraction

#### 2. Load Command
```bash
go-extractor load \
  --db fact_extraction.db \
  --cayley cayley.db
```

**Purpose**: Load existing SQLite results into Cayley graph

#### 3. Stats Command
```bash
go-extractor stats --db fact_extraction.db
```

**Output**:
- Total documents processed
- Total triples extracted
- Total cost
- Average triples per document
- Processing time

---

## Error Handling

### Retry Strategy
```go
func (ge *GeppettoExtractor) extractWithRetry(
    ctx context.Context,
    doc Document,
    maxRetries int,
) (*ExtractionResult, error) {
    var lastErr error
    for i := 0; i < maxRetries; i++ {
        result, err := ge.extractDocument(ctx, doc)
        if err == nil {
            return result, nil
        }
        lastErr = err
        time.Sleep(time.Duration(i+1) * time.Second)
    }
    return nil, fmt.Errorf("failed after %d retries: %w", maxRetries, lastErr)
}
```

### Error Types
- **API Errors**: Rate limits, timeouts, auth failures
- **Parsing Errors**: Invalid JSON, missing fields
- **Database Errors**: Connection issues, constraint violations
- **Validation Errors**: Empty actors, invalid data

---

## Progress Tracking

### Event-Based Progress
```go
type ProgressEvent struct {
    Processed int
    Total     int
    Current   string
    Cost      float64
}

func (ge *GeppettoExtractor) reportProgress(event ProgressEvent) {
    fmt.Printf("\r[%d/%d] Processing %s... Cost: $%.4f",
        event.Processed, event.Total, event.Current, event.Cost)
}
```

### Checkpointing
```go
func (dw *DatabaseWriter) SaveCheckpoint(docID string) error {
    _, err := dw.db.Exec(
        "INSERT INTO processing_log (doc_id, status, timestamp) VALUES (?, ?, ?)",
        docID, "success", time.Now(),
    )
    return err
}
```

**Benefits**:
- Resume from failures
- Track progress across runs
- Avoid reprocessing

---

## Testing Strategy

### Unit Tests
```go
func TestPromptBuilder(t *testing.T) {
    pb := NewPromptBuilder()
    doc := Document{ID: "test", Content: "Alice met Bob"}
    turn := pb.BuildTurn(doc)
    
    assert.Equal(t, 2, len(turn.Blocks))
    assert.Equal(t, "system", turn.Blocks[0].Role)
}
```

### Integration Tests
```go
func TestEndToEndExtraction(t *testing.T) {
    // Setup test database
    db := setupTestDB(t)
    defer db.Close()
    
    // Run extraction on sample document
    extractor := NewGeppettoExtractor(...)
    result, err := extractor.Extract(ctx, sampleDoc)
    
    require.NoError(t, err)
    assert.Greater(t, len(result.Triples), 0)
}
```

### Benchmark Tests
```go
func BenchmarkExtraction(b *testing.B) {
    extractor := NewGeppettoExtractor(...)
    for i := 0; i < b.N; i++ {
        extractor.Extract(ctx, sampleDoc)
    }
}
```

---

## Migration Path

### Phase 1: Core Extraction
1. Implement document loader
2. Build prompt builder
3. Create geppetto extractor
4. Add result parser
5. Implement database writer

### Phase 2: Concurrency
1. Add worker pool
2. Implement progress tracking
3. Add checkpointing
4. Test with 30 documents

### Phase 3: Cayley Integration
1. Implement quad conversion
2. Add Cayley loader
3. Test graph queries
4. Verify data consistency

### Phase 4: Advanced Features
1. Add tag clustering
2. Implement entity deduplication
3. Add batch processing
4. Optimize performance

---

## Performance Targets

### Python Baseline (30 documents)
- Time: ~5 minutes
- Cost: $0.10
- Throughput: 6 docs/min

### Go Target (30 documents)
- Time: ~1 minute (5x faster with 5 workers)
- Cost: $0.10 (same)
- Throughput: 30 docs/min

### Go Target (200 documents)
- Time: ~7 minutes
- Cost: $0.23
- Throughput: 28 docs/min

---

## File Structure

```
fact-extraction-go/
├── cmd/
│   └── go-extractor/
│       └── main.go              # CLI entry point
├── pkg/
│   ├── extractor/
│   │   ├── document.go          # Document loader
│   │   ├── prompt.go            # Prompt builder
│   │   ├── geppetto.go          # Geppetto extractor
│   │   ├── parser.go            # Result parser
│   │   └── worker.go            # Worker pool
│   ├── storage/
│   │   ├── sqlite.go            # SQLite writer
│   │   └── cayley.go            # Cayley integrator
│   └── types/
│       └── types.go             # Shared types
├── go.mod
├── go.sum
└── README_GO.md                 # Go implementation docs
```

---

## Dependencies

```go
require (
    github.com/go-go-golems/geppetto v0.x.x
    github.com/go-go-golems/glazed v0.x.x
    github.com/cayleygraph/cayley v0.7.7
    github.com/mattn/go-sqlite3 v1.14.x
    github.com/spf13/cobra v1.8.x
    github.com/rs/zerolog v1.31.x
    github.com/pkg/errors v0.9.1
)
```

---

## Next Steps

1. ✅ Study geppetto framework
2. ✅ Design architecture
3. ⏳ Implement core extractor
4. ⏳ Add concurrency
5. ⏳ Integrate with Cayley
6. ⏳ Test and benchmark
7. ⏳ Document and deliver

---

*Design Document v1.0 - November 19, 2025*
