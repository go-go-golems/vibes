---
Title: Go Backend Implementation Guide
Ticket: PORT-001
Status: active
Topics:
    - backend
    - frontend
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/client/src/pages/Home.tsx
      Note: Frontend consumer of the Go backend API
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: Current Node.js implementation to maintain parity with
    - Path: 2025/11/29/photobook-app/server/pdfWorker.ts
      Note: Current PDF worker implementation to port
ExternalSources: []
Summary: Comprehensive implementation guide for interns with project context, design patterns, decisions, and getting started instructions
LastUpdated: 2025-11-30T13:47:12.700651416-05:00
---


# Go Backend Implementation Guide

## Purpose

This document provides everything an intern (or any new developer) needs to understand and implement the Go backend for the photobook application. It includes project context, architectural decisions, design patterns, and step-by-step getting started instructions.

**Who this is for**: Developers new to the project who need to understand the architecture and start implementing features.

**What this covers**: Complete context, design decisions, patterns, and practical implementation guidance.

---

## Project Context

### What is the Photobook App?

The photobook application allows users to:
1. **Upload photos** - Users upload images via drag-and-drop or file picker
2. **Reorder photos** - Users drag photos to reorder them
3. **Generate PDFs** - Users request a PDF photobook with their photos in order

**Current State**: The app has a fully functional Node.js/TypeScript backend with a React frontend. We're porting the backend to Go to simplify deployment, improve performance, and remove external dependencies.

### Why Go?

**Problems with Current Implementation**:
- Production build issue: API server not included in production bundle
- External dependencies: Requires MySQL, Forge storage proxy, Manus OAuth
- Worker race conditions: No locking mechanism, duplicate job processing
- Complex infrastructure: Hard to run locally

**Benefits of Go**:
- Single binary deployment (no Node.js runtime needed)
- Better performance (compiled language)
- Simpler local dev (SQLite + filesystem, zero external deps)
- Better concurrency (goroutines for worker)

### Current Architecture (Node.js)

**Frontend**: React + tRPC hooks
- Uses 10 tRPC procedures for all API calls
- No direct fetch calls, all via tRPC hooks
- Fully integrated with backend

**Backend**: Node.js + Express + tRPC
- tRPC API server at `/api/trpc`
- MySQL database (via Drizzle ORM)
- Forge storage proxy (S3-like HTTP API)
- Manus OAuth (external authentication service)
- PDF worker (polls every 10s, no locking)

**Key Files**:
- `server/_core/index.ts` - API server entry point
- `server/pdfWorker.ts` - PDF generation worker
- `server/photoRouter.ts` - Photo management API
- `server/pdfRouter.ts` - PDF job API
- `drizzle/schema.ts` - Database schema

---

## Architectural Decisions

### Decision: tRPC-Compatible API

**What**: Maintain tRPC-compatible JSON-over-HTTP endpoints.

**Why**: 
- Frontend already built with tRPC hooks (10 procedures)
- Zero frontend changes required
- Faster path to MVP

**How**:
- Accept `/api/trpc/<procedure>` POST requests
- Support procedure names: `photo.*`, `pdf.*`, `auth.*`, `system.*`
- Return tRPC-compatible JSON responses
- Match current payload shapes exactly

**Example**:
```go
// Frontend calls: trpc.photo.list.useQuery()
// Backend receives: POST /api/trpc/photo.list
// Backend returns: { result: { data: [...] } }
```

### Decision: SQLite + Filesystem Storage

**What**: Start with SQLite database and filesystem storage.

**Why**:
- Zero external dependencies for local dev
- Simple deployment (`go run main.go`)
- Pluggable interfaces allow swapping to MySQL/S3 later

**How**:
- SQLite database: `./data/app.db`
- Filesystem storage: `./data/storage/`
- Storage interface allows swapping implementations
- Database interface allows swapping implementations

**Key Insight**: We design interfaces from the start, but implement the simplest backend first. This gives us flexibility without over-engineering.

### Decision: Single Binary, In-Process Worker

**What**: Run PDF worker as in-process goroutine within API server.

**Why**:
- Simple deployment (one binary, one process)
- Good enough for MVP (single instance)
- Go goroutines handle concurrency well

**How**:
- Worker runs as background goroutine
- Polls for pending jobs every 10 seconds
- Proper locking via atomic status updates
- Fixes current race condition

### Decision: Direct Database Operations (No Queue Abstraction)

**What**: Use direct database operations with atomic locking, no formal queue abstraction.

**Why**:
- Single instance doesn't need queue abstraction
- Atomic status updates provide locking
- Simple implementation (~50 lines)
- Add abstraction later if we need multiple instances

**How**:
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 5
RETURNING *;
```

This SQL statement atomically claims jobs, preventing race conditions.

### Decision: Email/Password Auth First, Design for OAuth Adapters

**What**: Implement email/password authentication first, design for OAuth adapters.

**Why**:
- Simple, no external dependencies
- Removes Manus OAuth dependency
- Adapter pattern allows adding OAuth providers later

**How**:
- Core auth service (session management)
- Email/password adapter (registration, login)
- OAuth adapter interface (stub for future providers)
- JWT session cookies (same format as current)

---

## Design Patterns

### Pattern: Pluggable Interfaces

**What**: Define interfaces for storage and database, implement simplest backend first.

**Why**: Allows swapping implementations without changing business logic.

**Example - Storage Interface**:
```go
type Storage interface {
    Put(ctx context.Context, relKey string, r io.Reader, contentType string) (url string, err error)
    Open(ctx context.Context, relKey string) (io.ReadCloser, error)
    Delete(ctx context.Context, relKey string) error
    GetSignedURL(ctx context.Context, relKey string, expiration time.Duration) (string, error)
}

// Disk implementation (for local dev)
type DiskStorage struct {
    basePath string
}

// S3 implementation (for production)
type S3Storage struct {
    bucket string
    client *s3.Client
}
```

**Key Insight**: Business logic (photo service, PDF worker) doesn't know about disk vs S3. It just calls `storage.Put()`.

### Pattern: Adapter Pattern for Authentication

**What**: Core auth service + adapters for different authentication methods.

**Why**: Allows adding OAuth providers without changing core auth logic.

**Example**:
```go
type AuthAdapter interface {
    Authenticate(ctx context.Context, credentials interface{}) (*types.User, error)
    Register(ctx context.Context, info interface{}) (*types.User, error)
}

// Email/password adapter
type EmailPasswordAdapter struct {
    userRepo UserRepository
}

// OAuth adapter (future)
type OAuthAdapter struct {
    provider string
    // ...
}
```

**Key Insight**: Core auth service doesn't know about email/password vs OAuth. It just calls `adapter.Authenticate()`.

### Pattern: Repository Pattern

**What**: Separate data access logic from business logic.

**Why**: Makes code testable and allows swapping database implementations.

**Example**:
```go
type PhotoRepository interface {
    Create(ctx context.Context, photo *types.Photo) (int64, error)
    ListByUser(ctx context.Context, userID int64) ([]*types.Photo, error)
    UpdatePositions(ctx context.Context, userID int64, updates []PositionUpdate) error
    Delete(ctx context.Context, userID int64, photoID int64) error
}

// SQLite implementation
type SQLitePhotoRepository struct {
    db *sql.DB
}
```

**Key Insight**: Photo service doesn't know about SQL. It just calls `repo.Create()`.

### Pattern: Service Layer

**What**: Business logic lives in service layer, not in handlers or repositories.

**Why**: Keeps handlers thin (just HTTP concerns) and repositories focused (just data access).

**Example**:
```go
type PhotoService struct {
    repo    PhotoRepository
    storage Storage
}

func (s *PhotoService) UploadPhoto(ctx context.Context, userID int64, file io.Reader, filename string) (*types.Photo, error) {
    // Business logic: validate, store, create record
    // Calls repo and storage, but contains business rules
}
```

**Key Insight**: Handlers parse HTTP, services contain business logic, repositories handle data access.

### Pattern: Atomic Job Claiming

**What**: Use database atomic updates to claim jobs, preventing race conditions.

**Why**: Prevents multiple workers from processing the same job.

**How**:
```sql
-- Atomic update: only one worker can claim a job
UPDATE pdf_jobs 
SET status = 'processing', updated_at = CURRENT_TIMESTAMP
WHERE status = 'pending' 
LIMIT 5
RETURNING *;
```

**Key Insight**: The `WHERE status = 'pending'` condition ensures only pending jobs are claimed. The `LIMIT 5` prevents claiming too many at once.

---

## Technology Stack

### Core Technologies

- **Go 1.19+**: Programming language
- **SQLite**: Local database (via `database/sql` or `modernc.org/sqlite`)
- **gofpdf**: PDF generation (`github.com/phpdave11/gofpdf`)
- **Goose**: Database migrations (`github.com/pressly/goose`)
- **zerolog**: Structured logging (`github.com/rs/zerolog`)
- **pkg/errors**: Error wrapping (`github.com/pkg/errors`)

### HTTP and API

- **net/http**: Standard library HTTP server
- **tRPC-compatible**: Custom tRPC protocol implementation
- **JWT**: Session management (via `github.com/golang-jwt/jwt`)

### Future Technologies (Pluggable)

- **MySQL/Postgres**: Production database (via `database/sql`)
- **S3/MinIO**: Production storage (via AWS SDK)
- **OAuth providers**: Google, GitHub, etc. (via adapters)

---

## Project Structure

### Standard Go Layout

```
cmd/
  api/
    main.go          # Entry point, wires everything together
internal/
  config/           # Configuration (env vars)
    config.go       # Config struct and loading
  http/             # HTTP layer
    trpc.go        # tRPC router and handler
    middleware.go  # Auth, logging, error recovery
  auth/             # Authentication
    auth.go        # Core auth service
    email_password.go  # Email/password adapter
    oauth.go       # OAuth adapter interface (stub)
  photos/           # Photo domain
    service.go     # Photo business logic
    repository.go  # Photo data access
  pdfjobs/          # PDF job domain
    service.go     # PDF job business logic
    repository.go  # PDF job data access
    worker.go      # PDF generation worker
  storage/          # Storage abstraction
    storage.go     # Storage interface
    disk.go        # Disk implementation
  db/               # Database
    db.go          # Database connection
    migrations/    # SQL migration files
pkg/
  types/            # Shared types
    types.go       # User, Photo, PdfJob types
```

### File Organization Principles

1. **Domain-Driven**: Group by domain (photos, pdfjobs, auth) not by layer
2. **Progressive Structure**: Start flat, add directories when files get big (>500 lines)
3. **Clear Boundaries**: Services don't import repositories directly, use interfaces

---

## Key Components

### 1. Configuration (`internal/config/config.go`)

**Purpose**: Load and validate environment variables.

**Pattern**: Typed config struct with validation.

**Example**:
```go
type Config struct {
    DatabaseURL string
    JWTSecret   string
    StoragePath string
    Port        int
}

func LoadConfig() (*Config, error) {
    cfg := &Config{
        DatabaseURL: os.Getenv("DATABASE_URL"),
        JWTSecret:   os.Getenv("JWT_SECRET"),
        StoragePath: getEnvOrDefault("STORAGE_PATH", "./data/storage"),
        Port:        getEnvIntOrDefault("PORT", 8080),
    }
    
    if cfg.DatabaseURL == "" {
        return nil, fmt.Errorf("DATABASE_URL is required")
    }
    if cfg.JWTSecret == "" {
        return nil, fmt.Errorf("JWT_SECRET is required")
    }
    
    return cfg, nil
}
```

### 2. Database (`internal/db/db.go`)

**Purpose**: Database connection and migration management.

**Pattern**: Connection pool + migration runner.

**Example**:
```go
func OpenDB(dsn string) (*sql.DB, error) {
    db, err := sql.Open("sqlite3", dsn)
    if err != nil {
        return nil, fmt.Errorf("failed to open database: %w", err)
    }
    
    // Run migrations
    if err := goose.Up(db, "internal/db/migrations"); err != nil {
        return nil, fmt.Errorf("failed to run migrations: %w", err)
    }
    
    return db, nil
}
```

### 3. Storage Interface (`internal/storage/storage.go`)

**Purpose**: Abstract blob storage operations.

**Pattern**: Interface with disk implementation.

**Example**:
```go
type Storage interface {
    Put(ctx context.Context, relKey string, r io.Reader, contentType string) (url string, err error)
    Open(ctx context.Context, relKey string) (io.ReadCloser, error)
    Delete(ctx context.Context, relKey string) error
    GetSignedURL(ctx context.Context, relKey string, expiration time.Duration) (string, error)
}

type DiskStorage struct {
    basePath string
    baseURL  string
}

func (s *DiskStorage) Put(ctx context.Context, relKey string, r io.Reader, contentType string) (string, error) {
    // Write file to disk, return URL
    filePath := filepath.Join(s.basePath, relKey)
    // ... create directory, write file
    return fmt.Sprintf("%s/media/%s", s.baseURL, relKey), nil
}
```

### 4. tRPC Router (`internal/http/trpc.go`)

**Purpose**: Handle tRPC-compatible requests.

**Pattern**: Parse JSON, route to handlers, return JSON.

**Example**:
```go
func (r *TRPCRouter) HandleRequest(w http.ResponseWriter, req *http.Request) {
    // Parse tRPC request
    var trpcReq TRPCRequest
    if err := json.NewDecoder(req.Body).Decode(&trpcReq); err != nil {
        http.Error(w, "invalid request", http.StatusBadRequest)
        return
    }
    
    // Route to handler
    handler := r.handlers[trpcReq.Procedure]
    result, err := handler(req.Context(), trpcReq.Input)
    
    // Return tRPC response
    resp := TRPCResponse{Result: result}
    json.NewEncoder(w).Encode(resp)
}
```

### 5. PDF Worker (`internal/pdfjobs/worker.go`)

**Purpose**: Process PDF generation jobs.

**Pattern**: Poll for jobs, claim atomically, process, update status.

**Example**:
```go
func (w *Worker) Run(ctx context.Context) {
    ticker := time.NewTicker(10 * time.Second)
    defer ticker.Stop()
    
    for {
        select {
        case <-ticker.C:
            jobs, err := w.repo.ClaimPendingJobs(ctx, 5)
            if err != nil {
                log.Error().Err(err).Msg("failed to claim jobs")
                continue
            }
            
            for _, job := range jobs {
                if err := w.processJob(ctx, job); err != nil {
                    log.Error().Err(err).Int64("job_id", job.ID).Msg("failed to process job")
                    w.repo.MarkFailed(ctx, job.ID, err.Error())
                }
            }
        case <-ctx.Done():
            return
        }
    }
}
```

---

## Getting Started

### Prerequisites

- **Go 1.19+**: Install from [golang.org](https://golang.org/dl/)
- **Git**: For version control
- **Text Editor**: VS Code with Go extension recommended

### Step 1: Set Up Project

```bash
# Create project directory
mkdir photobook-backend-go
cd photobook-backend-go

# Initialize Go module
go mod init photobook-backend-go

# Create directory structure
mkdir -p cmd/api
mkdir -p internal/{config,http,auth,photos,pdfjobs,storage,db/migrations}
mkdir -p pkg/types
```

### Step 2: Install Dependencies

```bash
# Core dependencies
go get github.com/phpdave11/gofpdf
go get github.com/pressly/goose/v3
go get github.com/rs/zerolog
go get github.com/pkg/errors
go get github.com/golang-jwt/jwt/v5

# SQLite driver
go get modernc.org/sqlite
```

### Step 3: Create Basic Structure

**Start with `cmd/api/main.go`**:
```go
package main

import (
    "context"
    "fmt"
    "log"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    
    "photobook-backend-go/internal/config"
    "photobook-backend-go/internal/db"
    "photobook-backend-go/internal/http"
)

func main() {
    // Load configuration
    cfg, err := config.LoadConfig()
    if err != nil {
        log.Fatalf("failed to load config: %v", err)
    }
    
    // Open database
    database, err := db.OpenDB(cfg.DatabaseURL)
    if err != nil {
        log.Fatalf("failed to open database: %v", err)
    }
    defer database.Close()
    
    // Create HTTP server
    server := http.NewServer(cfg, database)
    
    // Start server
    go func() {
        if err := server.Start(cfg.Port); err != nil {
            log.Fatalf("server failed: %v", err)
        }
    }()
    
    // Graceful shutdown
    sigChan := make(chan os.Signal, 1)
    signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
    <-sigChan
    
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()
    
    if err := server.Shutdown(ctx); err != nil {
        log.Printf("server shutdown error: %v", err)
    }
}
```

### Step 4: Set Up Environment Variables

Create `.env` file:
```bash
DATABASE_URL=sqlite://./data/app.db
JWT_SECRET=your-secret-key-here
STORAGE_PATH=./data/storage
PORT=8080
```

### Step 5: Create First Migration

Create `internal/db/migrations/001_create_users.sql`:
```sql
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    open_id TEXT UNIQUE NOT NULL,
    name TEXT,
    email TEXT,
    login_method TEXT,
    role TEXT CHECK(role IN ('user', 'admin')) DEFAULT 'user',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_signed_in DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Step 6: Implement First Feature

Start with a simple health check endpoint:

1. Create `internal/http/handlers.go`
2. Implement `system.health` procedure
3. Test with: `curl -X POST http://localhost:8080/api/trpc/system.health`

### Step 7: Build and Test

```bash
# Build
go build -o photobook-backend cmd/api/main.go

# Run
./photobook-backend

# Test health endpoint
curl -X POST http://localhost:8080/api/trpc/system.health \
  -H "Content-Type: application/json" \
  -d '{"procedure": "system.health", "input": {}}'
```

---

## Development Workflow

### 1. Understanding the Codebase

**Start Here**:
1. Read this document (you're doing it!)
2. Read [Current Architecture Reference](../reference/01-current-architecture-and-data-flow.md)
3. Read [Debate Synthesis](../reference/04-debate-synthesis-and-decisions.md)
4. Explore the Node.js implementation to understand behavior

**Key Files to Understand**:
- `server/_core/index.ts` - Current API server
- `server/pdfWorker.ts` - Current PDF worker
- `server/photoRouter.ts` - Current photo API
- `drizzle/schema.ts` - Current database schema

### 2. Implementing Features

**Pattern**:
1. **Define Types** (`pkg/types/types.go`) - Data structures
2. **Create Repository** (`internal/photos/repository.go`) - Data access
3. **Create Service** (`internal/photos/service.go`) - Business logic
4. **Create Handler** (`internal/http/handlers.go`) - HTTP layer
5. **Wire Together** (`cmd/api/main.go`) - Dependency injection

**Example - Photo Upload**:
```go
// 1. Define type
type Photo struct {
    ID       int64
    UserID   int64
    FileKey  string
    URL      string
    Filename string
    Position int
}

// 2. Repository method
func (r *PhotoRepository) Create(ctx context.Context, photo *Photo) (int64, error) {
    // SQL insert
}

// 3. Service method
func (s *PhotoService) UploadPhoto(ctx context.Context, userID int64, file io.Reader, filename string) (*Photo, error) {
    // Store file via storage interface
    // Create database record via repository
}

// 4. Handler
func (h *Handler) PhotoUpload(ctx context.Context, user *User, input PhotoUploadInput) (*PhotoUploadOutput, error) {
    // Call service
    photo, err := h.photoService.UploadPhoto(ctx, user.ID, input.Data, input.Filename)
    // Return response
}
```

### 3. Testing

**Unit Tests**:
- Test services with mock repositories
- Test repositories with test database
- Test handlers with mock services

**Integration Tests**:
- Test full flow (handler → service → repository → database)
- Use test database (separate SQLite file)

**Example Test**:
```go
func TestPhotoService_UploadPhoto(t *testing.T) {
    // Setup
    mockRepo := &MockPhotoRepository{}
    mockStorage := &MockStorage{}
    service := NewPhotoService(mockRepo, mockStorage)
    
    // Test
    photo, err := service.UploadPhoto(ctx, userID, fileReader, "test.jpg")
    
    // Assert
    assert.NoError(t, err)
    assert.Equal(t, "test.jpg", photo.Filename)
}
```

### 4. Debugging

**Logging**:
- Use zerolog for structured logging
- Include context (user_id, job_id, request_id)
- Use appropriate log levels (debug, info, warn, error)

**Example**:
```go
log.Info().
    Int64("user_id", userID).
    Str("filename", filename).
    Msg("uploading photo")
```

**Common Issues**:
- **Database locked**: SQLite doesn't handle concurrent writes well. Use connection pool with `_journal_mode=WAL`.
- **File permissions**: Ensure storage directory is writable.
- **tRPC parsing**: Check JSON payload format matches tRPC protocol.

---

## API Contract Reference

### tRPC Protocol

**Request Format**:
```json
{
  "procedure": "photo.list",
  "input": {}
}
```

**Response Format**:
```json
{
  "result": {
    "data": [...]
  }
}
```

**Error Format**:
```json
{
  "error": {
    "code": "UNAUTHORIZED",
    "message": "Authentication required"
  }
}
```

### Procedure Reference

**Photo Procedures**:
- `photo.list` - List user's photos (ordered by position)
- `photo.upload` - Upload a photo (base64 encoded)
- `photo.updatePositions` - Batch update photo positions
- `photo.delete` - Delete a photo
- `photo.deleteAll` - Delete all user's photos

**PDF Procedures**:
- `pdf.createJob` - Create PDF generation job
- `pdf.listJobs` - List user's PDF jobs
- `pdf.getJob` - Get PDF job details

**Auth Procedures**:
- `auth.me` - Get current user
- `auth.logout` - Logout (clear session)

**System Procedures**:
- `system.health` - Health check

See [Current Architecture Reference](../reference/01-current-architecture-and-data-flow.md) for detailed contracts.

---

## Common Patterns and Best Practices

### Error Handling

**Pattern**: Wrap errors with context, return user-friendly messages.

```go
func (s *PhotoService) UploadPhoto(ctx context.Context, userID int64, file io.Reader, filename string) (*Photo, error) {
    // Store file
    url, err := s.storage.Put(ctx, relKey, file, contentType)
    if err != nil {
        return nil, errors.Wrap(err, "failed to store photo")
    }
    
    // Create record
    photoID, err := s.repo.Create(ctx, photo)
    if err != nil {
        // Cleanup: delete stored file
        s.storage.Delete(ctx, relKey)
        return nil, errors.Wrap(err, "failed to create photo record")
    }
    
    return photo, nil
}
```

### Context Propagation

**Pattern**: Always pass context through function calls.

```go
func (s *PhotoService) UploadPhoto(ctx context.Context, ...) {
    // Context propagates to storage, repository, etc.
    url, err := s.storage.Put(ctx, ...)
}
```

**Why**: Enables cancellation, timeouts, and request tracing.

### Database Transactions

**Pattern**: Use transactions for multi-step operations.

```go
tx, err := db.BeginTx(ctx, nil)
if err != nil {
    return err
}
defer tx.Rollback()

// Multiple operations
if err := repo.CreatePhoto(tx, ...); err != nil {
    return err
}
if err := repo.UpdatePositions(tx, ...); err != nil {
    return err
}

return tx.Commit()
```

### Worker Job Processing

**Pattern**: Claim jobs atomically, handle errors gracefully.

```go
func (w *Worker) processJob(ctx context.Context, job *PdfJob) error {
    // Update status to processing (already claimed)
    // Process job
    // Update status to completed or failed
    
    if err := w.generatePDF(ctx, job); err != nil {
        w.repo.MarkFailed(ctx, job.ID, err.Error())
        return err
    }
    
    w.repo.MarkCompleted(ctx, job.ID, resultURL)
    return nil
}
```

---

## Next Steps

1. **Read the debates**: Understand why decisions were made
2. **Explore current implementation**: Understand behavior to maintain
3. **Start with infrastructure**: Config, database, storage
4. **Implement features incrementally**: Health check → Auth → Photos → PDFs
5. **Test thoroughly**: Unit tests, integration tests, manual testing

---

## Resources

- [Go Documentation](https://go.dev/doc/)
- [SQLite Documentation](https://www.sqlite.org/docs.html)
- [gofpdf Documentation](https://pkg.go.dev/github.com/phpdave11/gofpdf)
- [Goose Documentation](https://github.com/pressly/goose)
- [zerolog Documentation](https://github.com/rs/zerolog)
- [Current Architecture Reference](../reference/01-current-architecture-and-data-flow.md)
- [Debate Synthesis](../reference/04-debate-synthesis-and-decisions.md)

---

## Questions?

If you're stuck or have questions:
1. Check the debate rounds for design rationale
2. Review the current Node.js implementation for behavior reference
3. Ask for help! This is a learning project.

Good luck! 🚀

