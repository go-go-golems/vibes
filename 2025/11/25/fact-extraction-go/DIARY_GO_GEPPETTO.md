# Diary: Go Fact Extractor with Geppetto Framework

**Date**: November 19, 2025  
**Goal**: Port Python fact extraction to Go using geppetto framework

---

## Session 1: Framework Study and Architecture Design

### Repositories Cloned
1. **geppetto** - Core LLM framework
2. **pinocchio** - CLI tool built on geppetto

### Key Concepts Learned

#### 1. Turn-Based API
Geppetto uses a **Turn** abstraction for LLM conversations:
```go
type Turn struct {
    ID       string
    Blocks   []Block
    Metadata map[string]interface{}
    Data     map[string]interface{}
}
```

**Block** represents atomic units (system, user, assistant, tool):
```go
type Block struct {
    ID       string
    Kind     BlockKind  // User, LLMText, ToolCall, System, etc.
    Role     string     // "user", "assistant", "system"
    Payload  map[string]any  // Contains actual content
    Metadata map[string]interface{}
}
```

**Key insight**: Unlike traditional message arrays, Turns use Blocks with Payloads. Text content is stored in `Payload["text"]`.

#### 2. Glazed Commands
Geppetto integrates with **glazed** for CLI building:
- Declarative parameter definitions
- Layer-based configuration
- Automatic help generation
- Type-safe parameter parsing

#### 3. Step Settings
Configuration for LLM API calls:
```go
stepSettings := &settings.StepSettings{
    API: &settings.APISettings{
        APIKeys:  map[string]string{"openai-api-key": key},
        BaseUrls: map[string]string{"openai-base-url": url},
    },
    Chat: &settings.ChatSettings{
        Engine:  &model,
        ApiType: &apiType,  // Must be types.ApiType, not string
        Stream:  false,
    },
    OpenAI: &openaisettings.Settings{},  // Required!
}
```

**Critical discovery**: All three settings (API, Chat, OpenAI) are required for the engine to work.

#### 4. Event Router Pattern
Streaming updates during inference:
```go
router := events.NewEventRouter()
router.RegisterHandler(func(event events.Event) {
    // Handle tokens, metadata, etc.
})
```

### Architecture Designed

Created comprehensive design document (`GO_EXTRACTOR_DESIGN.md`) covering:
- Component breakdown (loader, prompt builder, extractor, parser, storage)
- Concurrency model (worker pool pattern)
- Data structures (Document, RDFTriple, ExtractionResult)
- CLI interface (extract, load, stats commands)
- Error handling and retry strategies
- Testing approach

**Design philosophy**: Keep it simple, follow geppetto patterns, prioritize type safety.

---

## Session 2: Implementation

### Components Built

#### 1. Types Package (`pkg/types/types.go`)
Defined core data structures:
- `Document`: Input document representation
- `RDFTriple`: Extracted fact in RDF format
- `ExtractionResult`: Complete extraction output with metadata
- `ExtractionResponse`: JSON structure from LLM

**Challenge**: Matching Python's optional fields with Go's type system.  
**Solution**: Used pointers for optional string fields (`*string`).

#### 2. Document Loader (`pkg/extractor/document.go`)
Simple file system loader:
- Reads `.txt` files from directory
- Extracts document ID from filename
- Limits to N documents
- Returns slice of Documents

**Works perfectly** - No issues.

#### 3. Prompt Builder (`pkg/extractor/prompt.go`)
Builds Turn from Document:
```go
turn := &turns.Turn{
    Data: map[string]any{"document_id": doc.ID},
}
turns.AppendBlock(turn, turns.NewSystemTextBlock(systemPrompt))
turns.AppendBlock(turn, turns.NewUserTextBlock(userPrompt))
```

**Challenge**: Understanding Turn vs Message abstraction.  
**Solution**: Used helper functions `NewSystemTextBlock`, `NewUserTextBlock`.

#### 4. Result Parser (`pkg/extractor/parser.go`)
Extracts JSON from LLM response:
- Handles markdown code blocks (```json ... ```)
- Handles raw JSON
- Validates required fields (actor, action)
- Filters out invalid triples

**Regex patterns**:
- Code block: `` ```(?:json)?\s*\n?([\s\S]*?)``` ``
- Raw JSON: `\{[\s\S]*\}`

#### 5. Geppetto Extractor (`pkg/extractor/geppetto.go`)
Main extraction logic using OpenAI engine.

**Challenges encountered**:

1. **Block.Content doesn't exist**
   - Error: `block.Content undefined`
   - Reality: Content is in `block.Payload["text"]`
   - Fix: Changed from `block.Content.(*turns.TextContent)` to `block.Payload["text"].(string)`

2. **ApiType must be types.ApiType**
   - Error: `cannot use &apiType (value of type *string) as *types.ApiType`
   - Reality: ApiType is a custom type, not string
   - Fix: Used `aitypes.ApiTypeOpenAI` constant

3. **Missing OpenAI settings**
   - Error: `missing client settings`
   - Reality: StepSettings requires OpenAI field
   - Fix: Added `OpenAI: &openaisettings.Settings{}`

4. **Still not working**
   - Current error: `missing client settings`
   - Hypothesis: Need to study how pinocchio initializes the engine

#### 6. SQLite Storage (`pkg/storage/sqlite.go`)
Database writer with schema:
- `documents` table: Metadata and costs
- `rdf_triples` table: Extracted facts
- `processing_log` table: Success/failure tracking
- Indexes on actor, action, target

**Challenge**: Handling optional fields in SQL.  
**Solution**: Helper function `ptrToString` converts `*string` to `string`.

#### 7. CLI Application (`cmd/go-extractor/main.go`)
Cobra-based CLI with commands:
- `extract`: Process documents
- `stats`: Show statistics
- Flags: input, output, model, limit, verbose

**Compiles successfully** - 14MB binary.

### Build Process

```bash
go mod init github.com/fact-extraction/go-extractor
go mod tidy  # Downloaded 30+ dependencies
go build -o go-extractor ./cmd/go-extractor
```

**Dependencies**:
- geppetto (local replace)
- go-sqlite3
- zerolog (logging)
- cobra (CLI)
- Plus transitive deps from geppetto

---

## Session 3: Debugging Geppetto Integration

### Test Attempts

#### Attempt 1: Missing data directory
```
Error: failed to read directory: open ../data_30: no such file or directory
```
**Fix**: Used correct path `/home/ubuntu/fact-extraction-go/sample_data`

#### Attempt 2: No chat engine specified
```
Error: inference failed: no chat engine specified
```
**Fix**: Added `ApiType: &apiType` to ChatSettings

#### Attempt 3: Type mismatch on ApiType
```
Error: cannot use &apiType (value of type *string) as *types.ApiType
```
**Fix**: Changed to `apiType := aitypes.ApiTypeOpenAI`

#### Attempt 4: No base URL
```
Error: inference failed: no base URL for openai
```
**Fix**: Added BaseUrls map to APISettings

#### Attempt 5: Missing client settings
```
Error: inference failed: missing client settings
```
**Fix**: Added `OpenAI: &openaisettings.Settings{}`

#### Attempt 6: Still missing client settings
```
Error: inference failed: missing client settings
```
**Status**: STUCK - Need to study pinocchio examples more carefully

### Current Hypothesis

The error "missing client settings" suggests the OpenAI client isn't being created properly. Looking at `cmd/llm-runner/main.go`, they create settings like:

```go
st := &settings.StepSettings{
    API:    &settings.APISettings{...},
    Chat:   &settings.ChatSettings{Engine: &s.Model, Stream: s.Stream},
    OpenAI: &openaisettings.Settings{},
}
```

But they don't directly create the engine - they use it through a different path. Need to trace how the engine actually gets the client.

### Next Steps to Debug

1. Create a minimal geppetto inference example
2. Study how pinocchio's simple-chat example works
3. Check if we need to use a different API (maybe not OpenAIEngine directly?)
4. Look at geppetto test files for examples

---

## Lessons Learned

### What Worked Well

1. **Architecture design first**: Having a clear design document made implementation straightforward
2. **Type safety**: Go's type system caught many errors at compile time
3. **Helper functions**: Geppetto provides good helpers like `NewUserTextBlock`
4. **Modular structure**: Separating concerns (loader, parser, storage) made debugging easier

### What Was Challenging

1. **Framework learning curve**: Geppetto's abstractions (Turn, Block, Payload) differ from typical message APIs
2. **Type system complexity**: Multiple layers of settings with specific types
3. **Documentation gaps**: Had to read source code to understand usage
4. **Configuration complexity**: Many required fields not obvious from examples

### Key Insights

1. **Blocks use Payloads**: Don't assume `block.Content` exists - use `block.Payload["text"]`
2. **Types matter**: ApiType is not a string, it's `types.ApiType`
3. **All settings required**: API, Chat, and OpenAI settings all needed
4. **Examples are essential**: Reading real code (pinocchio) more helpful than docs

### What I'd Do Differently

1. **Start with minimal example**: Should have created a "hello world" geppetto inference first
2. **Study examples first**: Should have read pinocchio's simple-chat before implementing
3. **Incremental testing**: Should have tested each component in isolation
4. **Ask for help sooner**: Spent too long debugging configuration issues

---

## Status Summary

### ✅ Completed
- Architecture design
- All core components implemented
- CLI application structure
- Compiles successfully (14MB binary)
- Database schema and storage layer

### ⏸️ In Progress
- Geppetto OpenAI client configuration
- Getting basic inference working
- Testing on sample documents

### 🔜 Next Steps
1. Study pinocchio simple-chat example in detail
2. Create minimal geppetto inference test
3. Debug "missing client settings" error
4. Test on 2-3 documents
5. Compare performance with Python

---

## Code Snippets for Future Reference

### Creating a Turn
```go
turn := &turns.Turn{
    Data: map[string]any{"document_id": doc.ID},
}
turns.AppendBlock(turn, turns.NewSystemTextBlock(systemPrompt))
turns.AppendBlock(turn, turns.NewUserTextBlock(userPrompt))
```

### Extracting Text from Turn
```go
func extractAssistantText(turn *turns.Turn) string {
    var text string
    for _, block := range turn.Blocks {
        if block.Role == "assistant" || block.Kind == turns.BlockKindLLMText {
            if textContent, ok := block.Payload["text"].(string); ok {
                text += textContent
            }
        }
    }
    return text
}
```

### Creating Step Settings
```go
apiType := aitypes.ApiTypeOpenAI
stepSettings := &settings.StepSettings{
    API: &settings.APISettings{
        APIKeys: map[string]string{
            "openai-api-key": apiKey,
        },
        BaseUrls: map[string]string{
            "openai-base-url": "https://api.openai.com/v1",
        },
    },
    Chat: &settings.ChatSettings{
        Engine:  &model,
        ApiType: &apiType,
        Stream:  false,
    },
    OpenAI: &openaisettings.Settings{},
}
```

---

## Resources

### Documentation
- Geppetto README: `/home/ubuntu/geppetto/README.md`
- Geppetto AGENT.md: `/home/ubuntu/geppetto/AGENT.md`
- Pinocchio examples: `/home/ubuntu/pinocchio/cmd/examples/`

### Key Files
- OpenAI engine: `/home/ubuntu/geppetto/pkg/steps/ai/openai/engine_openai.go`
- Turn types: `/home/ubuntu/geppetto/pkg/turns/types.go`
- Block helpers: `/home/ubuntu/geppetto/pkg/turns/helpers_blocks.go`
- Simple chat example: `/home/ubuntu/pinocchio/cmd/examples/simple-chat/main.go`

### Useful Commands
```bash
# Build
go build -o go-extractor ./cmd/go-extractor

# Test
./go-extractor extract --input /path/to/docs --limit 2 --verbose

# Check dependencies
go mod graph | grep geppetto
```

---

*Diary entry: November 19, 2025 - Session 3 (8:46 AM)*


---

## Session 4: Breakthrough - Direct OpenAI Client

### Problem Identified

**Root cause**: Geppetto's `OpenAIEngine` always uses streaming mode (hardcoded in the implementation):
```go
// Always use streaming mode
log.Debug().Msg("OpenAI using streaming mode")
stream, err := client.CreateChatCompletionStream(ctx, *req)
```

**Manus LLM proxy limitation**: The Manus proxy (`https://api.manus.im/api/llm-proxy/v1`) does not support streaming:
```
Error: 400 Bad Request, body: {"error":"Streaming is not supported"}
```

### Solution

Use the OpenAI Go client (`github.com/sashabaranov/go-openai`) directly instead of geppetto's engine:

```go
config := openai.DefaultConfig(apiKey)
config.BaseURL = baseURL  // Use Manus proxy URL

client := openai.NewClientWithConfig(config)

req := openai.ChatCompletionRequest{
    Model: "gpt-4.1-mini",
    Messages: []openai.ChatCompletionMessage{...},
    Stream: false,  // Explicitly disable streaming
}

resp, err := client.CreateChatCompletion(ctx, req)
```

### Test Results

✅ **Success!** Direct client works perfectly:
```
Running inference...

=== Response ===
Hello! How can I assist you today?

=== Usage ===
Prompt tokens: 23
Completion tokens: 10
Total tokens: 33

=== Success! ===
```

### Architecture Decision

**Hybrid approach**:
1. Use geppetto for Turn/Block abstractions (clean data structures)
2. Use OpenAI client directly for API calls (avoid streaming issues)
3. Convert between geppetto Turns and OpenAI messages

**Benefits**:
- Simpler configuration (no complex StepSettings)
- Works with Manus proxy (no streaming)
- Still leverages geppetto's Turn abstraction
- More control over API parameters

### Implementation Plan

Update `pkg/extractor/geppetto.go`:
1. Replace `OpenAIEngine` with direct `openai.Client`
2. Add Turn → Messages converter
3. Add Response → Turn converter
4. Keep the same public API (`Extract(doc) -> result`)

---

## Key Learnings - Updated

### What I Learned Today

1. **Geppetto always streams**: The OpenAIEngine is hardcoded to use streaming mode
2. **Manus proxy limitation**: Doesn't support streaming (returns 400 error)
3. **Hybrid approach works**: Can use geppetto abstractions without its engine
4. **Environment variables matter**: Must use `OPENAI_BASE_URL` not hardcoded URL
5. **Test incrementally**: Minimal test revealed the streaming issue immediately

### Framework Trade-offs

**Geppetto Pros**:
- Clean Turn/Block abstractions
- Event-driven architecture
- Good for complex workflows

**Geppetto Cons**:
- Always uses streaming
- Complex configuration (StepSettings, layers, etc.)
- Not compatible with all OpenAI proxies

**Direct Client Pros**:
- Simple configuration
- Full control over streaming
- Works with any OpenAI-compatible API

**Direct Client Cons**:
- Need to manage message arrays manually
- No built-in event system
- Less abstraction

### Best Practice

For OpenAI-compatible APIs that don't support streaming:
- Use geppetto's Turn/Block types for data modeling
- Use OpenAI client directly for API calls
- Convert between the two as needed

---

*Diary entry: November 19, 2025 - Session 4 (8:50 AM) - BREAKTHROUGH!*


---

## Session 5: Success - Go Extractor Working!

### Implementation

Created `pkg/extractor/openai.go` with direct OpenAI client:
- Reads API key and base URL from environment
- Creates `openai.Client` with custom config
- Explicitly disables streaming (`Stream: false`)
- Calculates cost based on token usage
- Returns structured `ExtractionResult`

### Test Results

✅ **Go extractor working!**

```bash
./go-extractor extract --input /path/to/docs --output go_test.db --limit 3
```

**Results**:
- Documents processed: 1/3 (2 failed due to JSON parsing)
- Triples extracted: 11
- Cost: $0.0011
- Time: 1m 2.5s (includes LLM API calls)
- Avg triples/doc: 11.0

**Performance**:
- First document: ~60 seconds (2815 tokens in, 1057 tokens out)
- Successful extraction and storage
- Stats command works perfectly

### Issue Found

Some documents fail with:
```
Error: json: cannot unmarshal array into Go struct field RDFTriple.triples.target of type string
```

**Cause**: LLM sometimes returns `target` as an array instead of string.

**Solution needed**: Update RDFTriple type to handle flexible JSON (use `json.RawMessage` or custom unmarshaler).

### Comparison with Python

| Metric | Python | Go | Notes |
|--------|--------|-----|-------|
| Setup | Simple | Moderate | Go needs compilation |
| Performance | ~6 docs/min | ~1 doc/min (sequential) | Go will be faster with concurrency |
| Binary size | N/A | 14MB | Standalone binary |
| Dependencies | pip install | go build | Go has no runtime deps |
| Error handling | Try/except | Explicit errors | Go more verbose |
| Type safety | Runtime | Compile-time | Go catches errors earlier |

### What Works

✅ Document loading  
✅ Prompt building  
✅ OpenAI API calls (non-streaming)  
✅ Response parsing (mostly)  
✅ SQLite storage  
✅ Cost calculation  
✅ Statistics reporting  
✅ CLI interface  

### What Needs Work

⏸️ JSON parsing for flexible types (target can be string or array)  
⏸️ Concurrency (worker pool for parallel processing)  
⏸️ Progress tracking (checkpoints every N documents)  
⏸️ Retry logic (handle API errors gracefully)  

---

## Final Status

### ✅ Completed
- Direct OpenAI client integration
- Full extraction pipeline
- Database storage
- CLI application
- **WORKING END-TO-END!**

### 📊 Results
- Successfully extracted facts from real documents
- Cost tracking working
- Statistics reporting working
- Performance acceptable for sequential processing

### 🎯 Achievement Unlocked
**Go fact extractor is functional!** 🎉

The hybrid approach (geppetto abstractions + direct OpenAI client) works perfectly. The extractor successfully processes documents, extracts RDF triples, stores them in SQLite, and provides statistics.

---

*Diary entry: November 19, 2025 - Session 5 (8:53 AM) - SUCCESS!*
