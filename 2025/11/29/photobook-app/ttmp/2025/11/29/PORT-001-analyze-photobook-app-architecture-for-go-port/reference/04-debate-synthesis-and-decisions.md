---
Title: Debate Synthesis and Final Decisions
Ticket: PORT-001
Status: active
Topics:
    - backend
    - architecture
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: Synthesis of all debate rounds with final architectural decisions and recommendations
LastUpdated: 2025-11-30T00:00:00-05:00
---

# Debate Synthesis and Final Decisions

## Overview

This document synthesizes the findings from all debate rounds (1-12) and presents the final architectural decisions for the Go backend migration. The debates explored foundational architecture, mechanics, and alignment decisions through data-driven argumentation.

## Decisions Made

### Foundation Decisions (Rounds 1-3)

#### ✅ API Protocol: tRPC-Compatible Endpoints

**Decision**: Maintain tRPC-compatible JSON-over-HTTP endpoints.

**Rationale**:
- Frontend already built with tRPC hooks (10 procedures)
- Zero frontend changes required
- RTK Query was considered but requires moderate refactoring
- tRPC compatibility is simpler path to MVP

**Implementation**:
- Accept `/api/trpc/<procedure>` POST requests
- Support procedure names: `photo.*`, `pdf.*`, `auth.*`, `system.*`
- Return tRPC-compatible JSON responses
- Match current payload shapes exactly

#### ✅ Infrastructure: SQLite + Filesystem Storage

**Decision**: Start with SQLite + filesystem storage, design for pluggable interfaces.

**Rationale**:
- Zero external dependencies for local dev
- Simple deployment (`go run main.go`)
- Pluggable interfaces allow swapping to MySQL/S3 later
- Matches "start simple, add complexity when needed" philosophy

**Implementation**:
- SQLite database (`./data/app.db`)
- Filesystem storage (`./data/storage/`)
- Storage interface (`Put`, `Open`, `Delete`, `GetSignedURL`)
- Database interface (repositories)

#### ✅ Worker Architecture: Single Binary, In-Process

**Decision**: Run PDF worker as in-process goroutine within API server.

**Rationale**:
- Simple deployment (one binary, one process)
- Good enough for MVP (single instance)
- Go goroutines handle concurrency well
- Can split to separate process later if needed

**Implementation**:
- Worker runs as background goroutine
- Polls for pending jobs every 10 seconds
- Proper locking via atomic status updates
- Fixes current race condition

---

### Architecture Decisions (Rounds 4-7)

#### ✅ Job Queue: Direct Database Operations

**Decision**: Use direct database operations with atomic locking, no formal queue abstraction.

**Rationale**:
- Single instance doesn't need queue abstraction
- Atomic status updates provide locking
- Simple implementation (~50 lines)
- Add abstraction later if we need multiple instances

**Implementation**:
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 5
RETURNING *;
```

#### ✅ Project Layout: Standard Layout, Flat Initially

**Decision**: Use standard Go layout (`cmd/`, `internal/`, `pkg/`) with flat structure initially.

**Rationale**:
- Standard layout is familiar
- Flat structure is simple for small project
- Add directories when files get big (>500 lines)
- Progressive structure (start simple, add organization when needed)

**Structure**:
```
cmd/
  api/
    main.go
internal/
  photos.go         # Photo service + repository
  pdfjobs.go        # PDF job service + worker
  auth.go           # Auth service
  storage.go        # Storage interface + disk implementation
  db.go             # Database connection + migrations
pkg/
  types.go          # Shared types
```

#### ✅ Authentication: Email/Password First, Design for OAuth Adapters

**Decision**: Implement email/password authentication first, design for OAuth adapters.

**Rationale**:
- Simple, no external dependencies
- Removes Manus OAuth dependency
- Adapter pattern allows adding OAuth providers later
- Best of both worlds: ship fast, extend later

**Implementation**:
- Core auth service (session management)
- Email/password adapter (registration, login)
- OAuth adapter interface (stub for future providers)
- JWT session cookies (same format as current)

#### ✅ File Serving: Storage Interface Supporting Both URL Types

**Decision**: Storage interface supporting both permanent and signed URLs.

**Rationale**:
- Matches current storage proxy behavior
- Permanent URLs for uploads, signed URLs for downloads
- Disk implementation: direct serving + HMAC-signed URLs
- S3 implementation: public URLs + pre-signed URLs

**Implementation**:
- `Put()` returns permanent URL
- `GetSignedURL()` returns time-limited URL
- Disk: direct serving with auth + HMAC signing
- S3: public URLs + pre-signed URLs

---

### Mechanics Decisions (Rounds 8-10)

#### ✅ PDF Generation: gofpdf Directly

**Decision**: Use `gofpdf` directly, match current algorithm exactly.

**Rationale**:
- Simple, works well for image-based PDFs
- Used successfully in zine-layout project
- MIT license (no commercial restrictions)
- No abstraction needed for MVP

**Implementation**:
- Use `github.com/phpdave11/gofpdf`
- Match current algorithm: A4 portrait, 10mm margins, aspect-fit
- One photo per page
- No PDF generator interface (add later if needed)

#### ✅ Migrations: Goose

**Decision**: Use Goose for SQL migrations.

**Rationale**:
- Proven tool, widely used
- Simple SQL migrations
- Version tracking
- Rollback support (nice-to-have)

**Implementation**:
- SQL files in `internal/db/migrations/`
- Goose tracks applied migrations
- Run migrations on startup
- External files (easier for development)

#### ✅ Error Handling: zerolog + pkg/errors

**Decision**: Use zerolog for structured logging, pkg/errors for error wrapping.

**Rationale**:
- Structured logging enables better observability
- Error wrapping provides context
- Production-ready from day one
- One dependency each, but high value

**Implementation**:
- zerolog for structured logging (JSON output)
- pkg/errors for error wrapping
- Log levels: debug, info, warn, error
- Context propagation (user_id, job_id, request_id)

---

### Alignment Decisions (Rounds 11-12)

#### ✅ Configuration: Simple Env Vars, Design for Glazed

**Decision**: Start with simple environment variable parsing, design for Glazed later.

**Rationale**:
- Simple for MVP (~50 lines)
- No external dependencies
- Design config struct to be easily migrated to Glazed
- Progressive approach (start simple, add complexity when needed)

**Implementation**:
- Typed config struct
- Validate required fields on startup
- Fail fast on missing vars
- Design for Glazed parameter layers migration

#### ✅ MVP Checklist

**Decision**: MVP includes all 10 tRPC procedures + basic infrastructure + proper error handling.

**MVP Requirements**:
1. **Core API** - All 10 tRPC procedures working
2. **Database** - SQLite with Goose migrations
3. **Storage** - Filesystem storage (disk implementation)
4. **Authentication** - Email/password with JWT sessions
5. **PDF Worker** - In-process worker with proper locking
6. **Error Handling** - tRPC error codes, structured logging (zerolog), error wrapping (pkg/errors)
7. **Validation** - Input validation, clear error messages

**Deferred**:
- OAuth providers (design for adapters, implement email/password first)
- S3 storage (design for interface, implement filesystem first)
- Advanced features (job history UI, admin features, etc.)

---

## Architecture Summary

### Technology Stack

- **Language**: Go 1.19+
- **API Protocol**: tRPC-compatible JSON-over-HTTP
- **Database**: SQLite (local), MySQL/Postgres (production-ready via interface)
- **Storage**: Filesystem (local), S3-compatible (production-ready via interface)
- **PDF Generation**: gofpdf (`github.com/phpdave11/gofpdf`)
- **Migrations**: Goose
- **Logging**: zerolog
- **Error Handling**: pkg/errors

### Project Structure

```
cmd/
  api/
    main.go          # Entry point, wires everything together
internal/
  config/           # Configuration (env vars)
  http/             # HTTP handlers, middleware, tRPC router
  auth/             # Authentication (email/password + adapter interface)
  photos/           # Photo domain (service, repository)
  pdfjobs/          # PDF job domain (service, worker, repository)
  storage/          # Storage interface + disk implementation
  db/               # Database connection + migrations
pkg/
  types/            # Shared types (User, Photo, PdfJob)
```

### Key Interfaces

**Storage Interface**:
```go
type Storage interface {
    Put(ctx context.Context, relKey string, r io.Reader, contentType string) (url string, err error)
    Open(ctx context.Context, relKey string) (io.ReadCloser, error)
    Delete(ctx context.Context, relKey string) error
    GetSignedURL(ctx context.Context, relKey string, expiration time.Duration) (string, error)
}
```

**Auth Adapter Interface**:
```go
type AuthAdapter interface {
    Authenticate(ctx context.Context, credentials interface{}) (*types.User, error)
    Register(ctx context.Context, info interface{}) (*types.User, error)
}
```

### Worker Implementation

**Job Claiming** (atomic locking):
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 5
RETURNING *;
```

**Worker Loop**:
```go
func (w *Worker) Run(ctx context.Context) {
    ticker := time.NewTicker(10 * time.Second)
    for {
        select {
        case <-ticker.C:
            jobs, _ := w.repo.ClaimPendingJobs(ctx, 5)
            for _, job := range jobs {
                w.processJob(ctx, job)
            }
        case <-ctx.Done():
            return
        }
    }
}
```

---

## Implementation Phases

### Phase 1: Core Infrastructure
- Set up Go project structure
- Implement configuration (env vars)
- Set up database (SQLite + Goose migrations)
- Implement storage interface (disk implementation)
- Set up logging (zerolog)

### Phase 2: Authentication
- Implement email/password authentication
- JWT session management
- Auth adapter interface (stub for OAuth)
- tRPC auth procedures (`auth.me`, `auth.logout`)

### Phase 3: Photo Management
- Photo repository (SQLite)
- Photo service (business logic)
- Photo storage (disk implementation)
- tRPC photo procedures (`photo.list`, `photo.upload`, `photo.updatePositions`, `photo.delete`, `photo.deleteAll`)

### Phase 4: PDF Generation
- PDF job repository
- PDF worker (in-process goroutine)
- PDF generation (gofpdf, match current algorithm)
- tRPC PDF procedures (`pdf.createJob`, `pdf.listJobs`, `pdf.getJob`)

### Phase 5: Integration & Testing
- Wire everything together in `main.go`
- Test all 10 tRPC procedures
- Verify frontend integration
- End-to-end testing

---

## Open Questions

1. **OAuth Providers**: Which providers to prioritize? (Google, GitHub, etc.)
2. **Signed URL Expiration**: What's the expiration time? (1 hour? Configurable?)
3. **Job Run History**: Do we need `job_runs` table for retries, or is simple status tracking enough?
4. **Config Migration**: When to migrate from simple env vars to Glazed parameter layers?

---

## References

- [Debate Round 1-3: Foundational Architecture](./debate-round-1-3-foundational-architecture.md)
- [Debate Round 4-7: Architecture and Mechanics](./debate-round-4-7-architecture-mechanics.md)
- [Debate Round 8-10: Mechanics](./debate-round-8-10-mechanics.md)
- [Debate Round 11-12: Configuration and MVP](./debate-round-11-12-alignment-mvp.md)
- [Current Architecture Reference](./01-current-architecture-and-data-flow.md)
- [Go Backend Migration Options Design Doc](../design-doc/01-go-backend-migration-options.md)

