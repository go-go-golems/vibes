---
Title: Debate Round 8-10: Mechanics Decisions
Ticket: PORT-001
Status: active
Topics:
    - backend
    - architecture
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/server/pdfWorker.ts
      Note: Current PDF generation algorithm (jsPDF+canvas, aspect-fit)
    - Path: zine-layout/pkg/export/pdf.go
      Note: Go PDF generation example using gofpdf
ExternalSources: []
Summary: Combined debate covering PDF generation library choice, database migrations, and error handling/logging patterns
LastUpdated: 2025-11-30T00:00:00-05:00
---

# Debate Round 8-10: Mechanics Decisions

## Context: Decisions Made So Far

From previous debate rounds, we've decided:
- **API Protocol**: tRPC-compatible endpoints
- **Infrastructure**: SQLite + filesystem storage
- **Worker Architecture**: Single binary, in-process worker
- **Job Queue**: Direct database operations with atomic locking (no formal abstraction)
- **Project Layout**: Standard layout (`cmd/`, `internal/`, `pkg/`) with flat structure initially
- **Authentication**: Email/password first, design for OAuth adapters
- **File Serving**: Storage interface supporting both permanent and signed URLs

This debate explores the mechanical implementation details: PDF generation, migrations, and error handling.

## Topics Covered

1. **PDF Generation**: Which Go library? How to match current behavior?
2. **Database Migrations**: Which tool? SQL vs Go-based? Embedded vs external?
3. **Error Handling & Logging**: Structured logging? Error wrapping? Log levels?

## Pre-Debate Research

### Current PDF Generation Algorithm Analysis

**Research Method**: Analyzed `server/pdfWorker.ts` for PDF generation details.

**Findings**:
- **Library**: jsPDF + canvas (Node.js)
- **Format**: A4 portrait (210mm × 297mm)
- **Margins**: 10mm on all sides
- **Image area**: 190mm × 277mm (after margins)
- **Image fitting**: Aspect-fit (maintain aspect ratio, center within area)
- **Algorithm**:
  - If `imgAspectRatio > pageAspectRatio`: fit to width, center vertically
  - Else: fit to height, center horizontally
- **One photo per page**: Each photo gets its own PDF page
- **Image loading**: Downloads from URL, converts to Buffer, loads with canvas

**Current implementation**: ~100 lines of PDF generation logic

### Go PDF Library Ecosystem Analysis

**Research Method**: Reviewed zine-layout project and Go PDF library options.

**Findings**:
- **gofpdf** (`github.com/phpdave11/gofpdf`): Maintained fork, used in zine-layout
  - Simple API, good for basic PDFs
  - Supports images, text, basic layout
  - MIT license (open source)
  - Active maintenance
- **unidoc**: Commercial license required for production use
  - More features (forms, annotations)
  - Better performance
  - Commercial license cost
- **pdfcpu**: More features, steeper learning curve
  - Good for PDF manipulation
  - Overkill for simple generation

**Zine-layout usage**: Uses `gofpdf` for PDF export, works well for image-based PDFs

### Current Migration Patterns Analysis

**Research Method**: Reviewed current schema and migration needs.

**Findings**:
- **Current schema**: 3 tables (`users`, `photos`, `pdf_jobs`)
- **Schema changes**: Rare (stable schema)
- **Migration tool**: Currently uses Drizzle ORM migrations (TypeScript)
- **Migration complexity**: Simple (add columns, add tables)

**Migration needs**: SQLite migrations, version tracking, rollback capability

### Current Error Handling Analysis

**Research Method**: Reviewed `server/pdfWorker.ts` and error handling patterns.

**Findings**:
- **Worker errors**: Per-job try/catch, continues on errors
- **Logging**: Structured JSON logs stored in `pdfJobs.logs` field
- **Error types**: tRPC error codes (`UNAUTHORIZED`, `FORBIDDEN`, `INTERNAL_SERVER_ERROR`)
- **Error messages**: User-friendly messages from constants

**Current patterns**: Basic error handling, structured logging in worker

---

## Opening Statements

### Topic 8: PDF Generation Library Choice

#### `server/pdfWorker.ts` — "The Background Processor"

*[Shows current PDF generation algorithm]*

I generate PDFs with:
- A4 portrait (210mm × 297mm)
- 10mm margins (190mm × 277mm image area)
- Aspect-fit images (maintain aspect ratio, center within area)
- One photo per page

**My position**: Match my behavior exactly. Use `gofpdf` (I see zine-layout uses it successfully). The algorithm is:
1. Calculate image aspect ratio
2. Calculate page aspect ratio (190mm / 277mm)
3. If image is wider: fit to width (190mm), center vertically
4. If image is taller: fit to height (277mm), center horizontally

Don't change the algorithm—users expect consistent output.

#### `go.mod` — "The Go Ecosystem"

*[Shows PDF library options]*

I've seen three main options:
- **gofpdf** (`github.com/phpdave11/gofpdf`): Simple, maintained, MIT license
- **unidoc**: More features, but commercial license required
- **pdfcpu**: More features, steeper learning curve

**My position**: Use `gofpdf`. It's:
- Simple API (good for basic PDFs)
- Active maintenance (phpdave11 fork)
- MIT license (no commercial restrictions)
- Used successfully in zine-layout project

For image-based PDFs (one photo per page), gofpdf is perfect. Unidoc is overkill, pdfcpu is too complex.

#### Sam "The Architect" Rivera

*[Reviews PDF generation requirements]*

The current algorithm is straightforward: aspect-fit images on A4 pages. But let me add: **we should abstract PDF generation** for testing.

**My position**: Use `gofpdf`, but create a PDF generator interface:

```go
type PDFGenerator interface {
    GeneratePDF(ctx context.Context, photos []Photo, output io.Writer) error
}
```

Start with gofpdf implementation, but the interface allows swapping libraries later if needed. Also enables testing (mock PDF generator).

#### Alex "The Pragmatist" Chen

Sam, you're over-engineering again. We're generating simple PDFs—one photo per page, aspect-fit. That's it.

*[Shows gofpdf usage from zine-layout]*

gofpdf is simple, works well, and zine-layout uses it successfully. No need for abstraction—just use gofpdf directly.

**My position**: Use `gofpdf` directly. No interface, no abstraction. Match the current algorithm exactly. Ship fast.

---

### Topic 9: Database Migration Strategy

#### `drizzle/schema.ts` — "The Data Model"

*[Shows current schema]*

I have 3 tables, simple structure. Current migrations use Drizzle ORM (TypeScript). For Go, we need SQL migrations.

**My position**: Use SQL migrations with version tracking. Simple approach:
- `migrations/001_create_users.sql`
- `migrations/002_create_photos.sql`
- `migrations/003_create_pdf_jobs.sql`

Track applied migrations in a `schema_migrations` table. Run migrations on startup (or via CLI command).

#### Sam "The Architect" Rivera

Schema, I hear you on SQL migrations, but let me add: **we need a migration tool**.

*[Shows migration tool options]*

Go migration tools:
- **Goose**: SQL migrations, version tracking, rollback support
- **migrate**: Similar to Goose, widely used
- **Custom**: Simple, but reinventing the wheel

**My position**: Use **Goose**. It's:
- Simple SQL migrations
- Version tracking (`schema_migrations` table)
- Rollback support (`goose down`)
- Widely used, well-maintained

Embed migrations in binary (using `embed`), or keep as external files. External files are easier for development.

#### "The New Hire" — Fresh Eyes

*[Looks confused]*

Wait, why do we need a migration tool? Can't we just run SQL files?

*[Thinks]*

If we're using SQLite, we could:
- Keep migrations as SQL files
- Track applied migrations in a table
- Run migrations on startup

That's simple. Do we need a tool?

**My position**: Simple approach: SQL files + migration tracking table. Run migrations on startup (or fail if migrations are pending). No external tool needed. Keep it simple.

#### Alex "The Pragmatist" Chen

New Hire, you're right—we don't need a tool. But let me add: **for development, we might want to reset the database**.

*[Shows development workflow]*

For local dev:
- Run migrations on startup
- Option to reset database (drop all tables, re-run migrations)
- Keep migrations as SQL files in `internal/db/migrations/`

**My position**: Simple migration system:
- SQL files in `internal/db/migrations/`
- `schema_migrations` table tracks applied migrations
- Run migrations on startup (or fail if pending)
- CLI flag to reset database (`--reset-db`)

No external tool. Simple, works for our needs.

---

### Topic 10: Error Handling and Logging

#### Morgan "The Infrastructure Engineer" Taylor

*[Shows current logging patterns]*

The current worker uses structured JSON logs stored in `pdfJobs.logs` field. That's good, but we need more.

**My position**: Use structured logging library (zerolog or zap):
- Structured fields (user_id, job_id, request_id)
- Log levels (debug, info, warn, error)
- Context propagation (request context)
- JSON output for production, human-readable for local dev

**zerolog** is simpler, **zap** is more performant. For our scale, zerolog is fine.

#### `server/pdfWorker.ts` — "The Background Processor"

*[Shows current error handling]*

I catch errors per-job, log them, continue processing. That's fine, but my logs are stored in JSON in the database. That's not ideal for debugging.

**My position**: Structured logging for worker:
- Log to stdout (JSON format)
- Also store in `pdfJobs.logs` field (for UI display)
- Include context (job_id, user_id, photo_count)
- Log levels: info (progress), warn (retries), error (failures)

Use zerolog or zap—doesn't matter, just make logs searchable.

#### Sam "The Architect" Rivera

Morgan and Worker, I hear you on structured logging, but let me add: **we need consistent error handling**.

*[Shows error handling patterns]*

Current errors:
- tRPC error codes (`UNAUTHORIZED`, `FORBIDDEN`)
- Error messages from constants
- Error wrapping (sometimes)

**My position**: Use `pkg/errors` for error wrapping:
- Wrap errors with context
- Use `errors.Is()` and `errors.As()` for error checking
- Return user-friendly error messages
- Log detailed errors (with stack traces) internally

Structured logging (zerolog) + error wrapping (pkg/errors) = good observability.

#### Alex "The Pragmatist" Chen

All of you are overcomplicating this. We're a small project. We don't need:
- Complex error wrapping
- Multiple log libraries
- Structured logging everywhere

**My position**: Use standard library `log` for now. Add structured logging later if needed. For errors:
- Return user-friendly messages
- Log errors to stdout
- Use `fmt.Errorf` for error wrapping (standard library)

Keep it simple. Add complexity when we need it.

---

## Rebuttals and Counter-Arguments

### Round 1: PDF Generation Rebuttals

#### Sam "The Architect" Rivera → Alex "The Pragmatist" Chen

Alex, you said "no abstraction," but let me show you the testing problem.

*[Shows test requirements]*

If we use gofpdf directly:
- Hard to test (PDF generation is slow)
- Hard to mock (gofpdf types are concrete)
- Hard to swap libraries later

**My counter**: Create minimal interface:
```go
type PDFGenerator interface {
    GeneratePDF(ctx context.Context, photos []Photo, output io.Writer) error
}
```

Start with gofpdf implementation, but interface enables testing and future flexibility.

#### Alex "The Pragmatist" Chen → Sam "The Architect" Rivera

Sam, you're optimizing for problems we don't have. We're generating simple PDFs—one photo per page. That's it.

*[Shows gofpdf usage]*

gofpdf is simple, works well, and we're not swapping libraries. The abstraction cost (interface + implementation) is higher than the benefit.

**My counter**: Use gofpdf directly. No interface, no abstraction. Match current algorithm exactly. Add interface later if we need it.

#### `server/pdfWorker.ts` → Both

*[Interjects]*

Both of you are missing the point! **Match my algorithm exactly**. Don't change the image fitting logic.

*[Shows aspect-fit calculation]*

My algorithm:
1. Calculate image aspect ratio
2. Calculate page aspect ratio (190mm / 277mm)
3. If image wider: fit to width, center vertically
4. If image taller: fit to height, center horizontally

**My position**: Use gofpdf, match my algorithm exactly. Whether you wrap it in an interface or call it directly doesn't matter—just match the behavior.

---

### Round 2: Migration Strategy Rebuttals

#### Sam "The Architect" Rivera → Alex "The Pragmatist" Chen

Alex, you said "simple migration system," but let me show you what that means.

*[Shows custom migration code]*

If we build custom migration system:
- Migration tracking table
- Migration file parsing
- Migration execution logic
- Rollback support
- Error handling

That's **100+ lines of code**. Goose does this already.

**My counter**: Use Goose. It's:
- Simple SQL migrations
- Version tracking
- Rollback support
- Well-maintained

The tool cost (one dependency) is lower than building custom migration system.

#### Alex "The Pragmatist" Chen → Sam "The Architect" Rivera

Sam, you're right that Goose exists, but let me show you the simplicity.

*[Shows simple migration code]*

Custom migration system:
- Read SQL files from `migrations/` directory
- Track applied migrations in `schema_migrations` table
- Execute pending migrations on startup

That's **~50 lines of code**. One-time cost, no external dependency.

**My counter**: Build simple migration system. One-time cost (~50 lines), no external dependency. Add Goose later if we need rollback or complex migrations.

#### "The New Hire" → Both

*[Interjects]*

Wait, why do we need rollback? We're a fresh project. We can just reset the database.

*[Shows development workflow]*

For local dev:
- Reset database (`DROP TABLE IF EXISTS ...`)
- Re-run all migrations

For production:
- Migrations are forward-only (add columns, add tables)
- No rollback needed

**My position**: Simple migration system:
- SQL files in `migrations/`
- Track applied migrations
- Run on startup
- No rollback needed

Keep it simple. Add rollback later if needed.

---

### Round 3: Error Handling Rebuttals

#### Morgan "The Infrastructure Engineer" Taylor → Alex "The Pragmatist" Chen

Alex, you said "standard library log," but let me show you the observability problem.

*[Shows production debugging]*

If we use standard library log:
- No structured fields (can't filter by user_id, job_id)
- No log levels (can't filter by severity)
- No JSON output (hard to parse in production)

**My counter**: Use zerolog. It's:
- Simple API (similar to standard library)
- Structured fields (user_id, job_id, etc.)
- Log levels (debug, info, warn, error)
- JSON output (production-ready)

The cost is one dependency, but the observability benefit is high.

#### Alex "The Pragmatist" Chen → Morgan "The Infrastructure Engineer" Taylor

Morgan, I hear you on observability, but let me show you the simplicity.

*[Shows standard library log]*

Standard library log:
- Built-in (no dependency)
- Simple API
- Works for our scale

For structured fields, we can use `log.Printf`:
```go
log.Printf("user_id=%d job_id=%d processing job", userID, jobID)
```

That's structured enough for our needs.

**My counter**: Use standard library log. Add zerolog later if we need better observability. Don't optimize for scale we don't have.

#### Sam "The Architect" Rivera → Both

Both of you are debating logging, but let me add: **we need consistent error handling**.

*[Shows error handling patterns]*

Current errors:
- tRPC error codes
- Error messages from constants
- Sometimes error wrapping

**My position**: Use `pkg/errors` for error wrapping:
- Wrap errors with context
- Use `errors.Is()` and `errors.As()` for error checking
- Return user-friendly messages
- Log detailed errors internally

Structured logging (zerolog) + error wrapping (pkg/errors) = good observability.

---

## Final Arguments

### Alex "The Pragmatist" Chen — Final Statement

Let me summarize my position on all three topics:

**PDF Generation**: Use `gofpdf` directly. Match current algorithm exactly (aspect-fit, A4 portrait, 10mm margins). No abstraction, no interface. Ship fast.

**Migrations**: Simple migration system (~50 lines):
- SQL files in `migrations/`
- Track applied migrations in `schema_migrations` table
- Run on startup
- No rollback needed

**Error Handling**: Standard library `log` + `fmt.Errorf` for error wrapping. Add structured logging later if needed. Keep it simple.

**Bottom line**: Keep it simple. Ship fast. Add complexity when we need it.

### Sam "The Architect" Rivera — Final Statement

Alex, I respect pragmatism, but let me make the architectural case:

**PDF Generation**: Use `gofpdf` with minimal interface (`PDFGenerator`). Enables testing and future flexibility. Low cost, high benefit.

**Migrations**: Use **Goose**. Simple SQL migrations, version tracking, rollback support. Well-maintained, widely used. One dependency, but saves building custom system.

**Error Handling**: Use **zerolog** for structured logging + `pkg/errors` for error wrapping. Good observability, consistent error handling. One dependency, but production-ready.

**Bottom line**: Use proven tools. Low cost, high benefit. Avoid reinventing the wheel.

### Morgan "The Infrastructure Engineer" Taylor — Final Statement

I'm with Sam on tools, but let me add operational perspective:

**PDF Generation**: Use `gofpdf`. Simple, works well, matches current behavior. No abstraction needed for now.

**Migrations**: Use **Goose**. Simple SQL migrations, version tracking. Rollback is nice-to-have, but not required for MVP.

**Error Handling**: Use **zerolog** for structured logging. Structured fields (user_id, job_id) enable better observability. JSON output for production, human-readable for local dev.

**Bottom line**: Use proven tools. Operations matter. Structured logging enables debugging in production.

---

## Moderator Summary

### Key Arguments and Tensions

**PDF Generation**:
- **gofpdf directly**: Simple, matches current behavior, no abstraction
- **gofpdf with interface**: Enables testing, future flexibility
- **Tension**: Simplicity vs testability
- **Consensus**: Use gofpdf, match current algorithm exactly

**Database Migrations**:
- **Custom system**: Simple (~50 lines), no dependency
- **Goose**: Proven tool, version tracking, rollback support
- **Tension**: Simplicity vs tool maturity
- **Consensus emerging**: Use Goose (proven tool, saves building custom system)

**Error Handling & Logging**:
- **Standard library**: Simple, no dependencies
- **zerolog + pkg/errors**: Structured logging, error wrapping, production-ready
- **Tension**: Simplicity vs observability
- **Consensus emerging**: Use zerolog for structured logging, pkg/errors for error wrapping

### Interesting Ideas Surfaced

1. **PDF generator interface**: Minimal interface enables testing and future flexibility
2. **Migration rollback**: Not needed for MVP (forward-only migrations), but nice-to-have
3. **Structured logging**: JSON output for production, human-readable for local dev
4. **Error wrapping**: Use `pkg/errors` for context, return user-friendly messages

### Unresolved Questions

1. **PDF Generation**: Do we need PDF generator interface, or use gofpdf directly?
2. **Migrations**: Use Goose or build custom system? Do we need rollback?
3. **Error Handling**: Use zerolog or standard library log? Use pkg/errors or fmt.Errorf?

### Next Steps

1. **Prototype PDF generation**: Use gofpdf, match current algorithm exactly
2. **Choose migration tool**: Evaluate Goose vs custom system
3. **Set up logging**: Choose zerolog or standard library, set up structured logging
4. **Design error handling**: Use pkg/errors or fmt.Errorf, design error types

### Decision Points

The debates revealed that these topics are interconnected:
- **PDF Generation** affects worker implementation (algorithm, library choice)
- **Migrations** affect database setup (version tracking, rollback)
- **Error Handling** affects observability (logging, error types)

All three decisions should be made together to ensure consistency. The moderator recommends:
1. Make PDF Generation decision first (affects worker implementation)
2. Make Migration decision second (affects database setup)
3. Make Error Handling decision third (affects observability)

