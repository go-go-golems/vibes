---
Title: Go backend migration options
Ticket: PORT-001
Status: active
Topics:
    - backend
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/client/src/pages/Home.tsx
      Note: Frontend consumer; currently tRPC client
    - Path: 2025/11/29/photobook-app/drizzle/schema.ts
      Note: DB schema parity
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: Current Node API entry (tRPC/OAuth) to mirror in Go
    - Path: 2025/11/29/photobook-app/server/pdfRouter.ts
      Note: PDF job API contract to replicate
    - Path: 2025/11/29/photobook-app/server/pdfWorker.ts
      Note: Worker behavior to port
    - Path: 2025/11/29/photobook-app/server/photoRouter.ts
      Note: Photo API contract to replicate
    - Path: 2025/11/29/photobook-app/server/routers.ts
      Note: Router structure and PDF worker bootstrap to preserve
    - Path: 2025/11/29/photobook-app/shared/const.ts
      Note: Cookie name consistency
ExternalSources: []
Summary: Options for replacing Node/tRPC backend with Go (SQLite+disk first, pluggable to SQL/S3), plus auth strategy without Manus dependency
LastUpdated: 2025-11-29T21:35:00-05:00
---


# Go backend migration options

## Executive Summary

We need a Go backend that mirrors current photobook behaviors (photo upload/list/reorder/delete; PDF jobs with async processing) while simplifying local dev (SQLite + filesystem) and keeping a path to cloud (MySQL/Postgres + S3). Two options:

1) **Minimal Go monolith** — REST/JSON API, SQLite, disk storage, in-process polling worker with DB-based locking. Fastest to ship; good for local/offline; easy to swap DB/storage later.
2) **Go monolith with job runner abstraction** — Same API, but formal job queue (claim/complete), optional separate worker process, ready for multi-instance and cloud storage/DB.

Auth: default to email/password sessions (JWT cookie); keep optional OAuth provider hooks (Google/GitHub/etc.) instead of hard Manus dependency.

## Problem Statement

- Current production bundle serves only static assets; API is missing.
- Node/tRPC stack unused by the client; we want a Go service we can run locally with no external infra.
- Background PDF worker risks double-processing; no locking/queue.
- OAuth via Manus is not required; we need a simpler auth path that can later plug in common providers.

## Proposed Solution

Replace the backend with a Go HTTP API (REST/JSON) that matches current behavior. Use interfaces for storage and DB to allow SQLite+disk by default and S3/MySQL later. Implement a PDF worker with explicit job claiming to avoid duplicates. Provide a lightweight auth module (email/password) with optional OAuth adapters.

## Design Decisions

- Use REST/JSON instead of tRPC to simplify the Go implementation and make it easy for the React client to call.
- Keep schema parity with existing tables (`users`, `photos`, `pdfJobs` → `pdf_jobs`) to ease data migration.
- Provide pluggable storage (`disk`, later `s3`) and database (`sqlite`, later `mysql/postgres`).
- PDF worker uses DB row state transitions for locking; option to separate worker from API.
- Default auth is email/password + JWT cookie; OAuth providers added via adapters.

## Alternatives Considered

- Keep Node/tRPC and add Go only for PDF: rejected (still leaves API gap and duplicate stacks).
- gRPC instead of REST: heavier for the current frontend; not necessary for scope.
- Full queue/broker (e.g., Redis): overkill for local/offline; can add later if needed.

## Implementation Plan

1) Freeze REST contracts and add a small TS client for React.
2) Implement Go service (handlers, SQLite migrations, disk storage, JWT auth).
3) Build PDF worker with job claiming and status updates.
4) Wire React to REST endpoints (upload/reorder/delete/photos; create/poll PDF jobs; auth).
5) Add pluggable backends (MySQL/S3) and optional OAuth providers.

## Open Questions

- Do we need signed URLs for disk storage? (recommended for multi-user setups)
- Retention/cleanup policy for photos/PDFs?
- Which OAuth providers to prioritize, if any?

## References

- Current architecture reference: `reference/01-current-architecture-and-data-flow.md`
- Ticket: `index.md`

---

## Detailed Design

### Common API surface (applies to both options)

- Auth
  - `POST /api/auth/login` (email/password) → set JWT cookie `app_session_id`
  - `POST /api/auth/logout`
  - `GET /api/auth/me`
- Photos
  - `POST /api/photos` (multipart or JSON+base64) → create photo, store blob via storage interface
  - `GET /api/photos` → ordered list
  - `PATCH /api/photos/positions` → reorder
  - `DELETE /api/photos/:id`
  - `DELETE /api/photos` (delete all)
- PDF jobs
  - `POST /api/pdf-jobs` `{photoIds: number[]}` → `{jobId}`
  - `GET /api/pdf-jobs` → list jobs
  - `GET /api/pdf-jobs/:id` → job detail (status/resultUrl/logs)
- Health
  - `GET /api/health`

### Data model (parity with current schema)

- `users(id, open_id, name, email, login_method, role[user|admin], created_at, updated_at, last_signed_in)`
- `photos(id, user_id, file_key, url, filename, mime_type, size, position, created_at, updated_at)`
- `pdf_jobs(id, user_id, status[pending|processing|completed|failed], photo_ids(json), result_url, error_message, logs, created_at, updated_at, completed_at)`
- (Optional) `user_identities` for OAuth providers (provider, provider_user_id, user_id, metadata)

### Storage interface

- `Put(ctx, relKey string, data []byte, contentType string) (url string, err error)`
- `Open(ctx, relKey string) (io.ReadCloser, error)`
- `Delete(ctx, relKey string) error` (recommended to enable cleanup)

Implementations:
- **Disk (default)**: writes under `./data/storage`; serve via `/media/<relKey>` with auth or signed URLs.
- **S3/MinIO (later)**: use AWS SDK; keep key format `user-<id>/photos/...`, `user-<id>/pdfs/...`.

### Auth model

- Default: email/password (bcrypt hash). Sessions are JWT cookies (`app_session_id`, HS256, secret env).
- Optional OAuth: adapters for Google/GitHub/etc.; store mapping in `user_identities`.
- Admin: environment variable `OWNER_OPEN_ID` maps to admin role; also allow an `admins` table if needed later.

### Worker model

- Claim jobs with status `pending` → set to `processing` with `updated_at` for lease semantics.
- Retry/backoff: optional; at minimum, avoid duplicate claims via UPDATE WHERE status='pending'.
- Logging: store JSON log lines in `logs`; cap size to avoid oversized rows.
- PDF generation: gofpdf/unidoc; aspect-fit to A4 with margins; one photo per page.
- Output: upload PDF via storage interface; set `result_url`, `completed_at`, `status=completed`; on error set `status=failed` + `error_message`.

### Option 1 — Minimal Go monolith (SQLite + disk, polling worker)

- Single binary (API + worker goroutine).
- DB: SQLite file (e.g., `./data/app.db`); migrations via Goose or similar.
- Storage: disk under `./data/storage`; static handler for `/media/...`.
- Worker: ticker (e.g., 10s) with `UPDATE ... WHERE status='pending' LIMIT N` to claim; no external queue.
- Pros: simplest; no extra services; good offline story.
- Cons: single-process; polling; scaling requires changing worker + storage backends.

### Option 2 — Go monolith with job runner abstraction

- API binary plus (optional) separate worker binary sharing DB/storage.
- DB: start SQLite; also support MySQL/Postgres by swapping DSN.
- Runner: interfaces `Enqueue`, `Claim`, `Complete/Fail`, optional `job_runs` for retries.
- Storage: pluggable (`disk`, `s3`).
- Pros: ready for multiple instances; clearer separation of concerns; smoother path to cloud.
- Cons: a bit more code/complexity than Option 1.

---

## Detailed Design — Minimal Go monolith (SQLite + disk, tRPC-compatible surface)

The React frontend now calls tRPC under `/api/trpc`. We’ll preserve the same shapes and endpoints but implement them in Go using plain JSON over POST (tRPC-style batched envelopes). Internally we keep clean service interfaces so we can also expose REST later if desired.

### Go project layout (proposed)

```
cmd/api/main.go            // wire HTTP server, routes, middleware
internal/config/config.go  // env loading
internal/http/trpc.go      // tRPC-style router/handler multiplexer
internal/http/middleware.go// auth/session, logging, recovery
internal/auth/auth.go      // email/password, session JWT
internal/photos/service.go // use cases for photos
internal/photos/store.go   // DB + storage integration
internal/pdfjobs/service.go// job creation/query
internal/pdfjobs/worker.go // ticker worker
internal/storage/storage.go// interface + disk impl
internal/db/db.go          // SQLite connection + migrations
internal/db/migrations/... // SQL migrations
pkg/types/types.go         // shared models (User, Photo, PdfJob, error types)
```

### Schema (SQLite migrations)

- `users`
  - `id` INTEGER PRIMARY KEY AUTOINCREMENT
  - `open_id` TEXT UNIQUE NOT NULL (for email/password, use generated id; for OAuth, provider id)
  - `name` TEXT
  - `email` TEXT
  - `login_method` TEXT
  - `role` TEXT CHECK role IN ('user','admin') DEFAULT 'user'
  - `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP
  - `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP
  - `last_signed_in` DATETIME DEFAULT CURRENT_TIMESTAMP
- `photos`
  - `id` INTEGER PRIMARY KEY AUTOINCREMENT
  - `user_id` INTEGER NOT NULL
  - `file_key` TEXT NOT NULL
  - `url` TEXT NOT NULL
  - `filename` TEXT NOT NULL
  - `mime_type` TEXT
  - `size` INTEGER
  - `position` INTEGER NOT NULL DEFAULT 0
  - `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP
  - `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP
- `pdf_jobs`
  - `id` INTEGER PRIMARY KEY AUTOINCREMENT
  - `user_id` INTEGER NOT NULL
  - `status` TEXT CHECK status IN ('pending','processing','completed','failed') DEFAULT 'pending'
  - `photo_ids` TEXT NOT NULL -- JSON array
  - `result_url` TEXT
  - `error_message` TEXT
  - `logs` TEXT
  - `created_at` DATETIME DEFAULT CURRENT_TIMESTAMP
  - `updated_at` DATETIME DEFAULT CURRENT_TIMESTAMP
  - `completed_at` DATETIME

### Storage interface (disk default)

```go
type Storage interface {
    Put(ctx context.Context, relKey string, r io.Reader, contentType string) (url string, err error)
    Open(ctx context.Context, relKey string) (io.ReadCloser, error)
    Delete(ctx context.Context, relKey string) error
}
// disk implementation writes under ./data/storage/<relKey>
```

### Auth/session model

```go
// email/password auth
type AuthService interface {
    Register(ctx context.Context, email, password, name string) (*types.User, error)
    Login(ctx context.Context, email, password string) (*types.User, string /*jwt*/, error)
    Me(ctx context.Context, sessionToken string) (*types.User, error)
}

// JWT claims
type SessionClaims struct {
    UserID int64  `json:"uid"`
    OpenID string `json:"oid"`
    Role   string `json:"role"`
    Name   string `json:"name"`
    Exp    int64  `json:"exp"`
}
```

Cookies: `app_session_id` (same name as today), httpOnly, sameSite=Lax, secure when HTTPS.

Optional OAuth providers can be added later; store provider id in `open_id` plus a `user_identities` table if needed.

### tRPC-compatible handler surface

We’ll accept the same POST payloads to `/api/trpc/<procedure>` (single) and `/api/trpc` (batch). Each procedure returns JSON matching current shapes.

Example handler signatures:

```go
// photo.upload
func (h *Handler) PhotoUpload(ctx context.Context, user *types.User, in PhotoUploadInput) (*PhotoUploadOutput, error)

type PhotoUploadInput struct {
    Filename string `json:"filename"`
    MimeType string `json:"mimeType"`
    Data     string `json:"data"`    // base64
    Position int    `json:"position"`
}
type PhotoUploadOutput struct {
    URL     string `json:"url"`
    FileKey string `json:"fileKey"`
}

// photo.list
func (h *Handler) PhotoList(ctx context.Context, user *types.User) ([]types.Photo, error)
```

Similar methods for updatePositions, delete, deleteAll, pdf.createJob, pdf.listJobs, pdf.getJob, auth.me, auth.logout, system.health.

### Control flow (API)

1) Middleware parses cookie `app_session_id`, verifies JWT, loads user from DB (optional for public routes like health).
2) tRPC dispatcher maps `photo.upload`, `photo.list`, etc., to handlers.
3) Handlers call services:
   - `photos.Service.Upload(ctx, user, input)` → storage.Put → repo.InsertPhoto → return URL/fileKey.
   - `pdf.Service.CreateJob(ctx, user, photoIDs)` → repo.InsertJob(status=pending).
4) Responses are encoded as JSON matching existing return shapes.

### Control flow (worker)

```go
func (w *Worker) Run(ctx context.Context) {
    ticker := time.NewTicker(10 * time.Second)
    for {
        select {
        case <-ticker.C:
            w.processPending(ctx)
        case <-ctx.Done():
            return
        }
    }
}

func (w *Worker) processPending(ctx context.Context) error {
    jobs, err := w.repo.ClaimPending(ctx, 5) // UPDATE ... WHERE status='pending' RETURNING ...
    for _, job := range jobs {
        w.handleJob(ctx, job)
    }
    return nil
}
```

`handleJob` loads photos for the user, downloads from storage (disk open), renders PDF (gofpdf/unidoc), uploads via storage.Put, updates job to `completed` with `result_url`; on error set `failed` with `error_message`.

### DB repositories (sketch)

```go
type PhotoRepo interface {
    Create(ctx context.Context, p types.Photo) (int64, error)
    ListByUser(ctx context.Context, userID int64) ([]types.Photo, error)
    UpdatePositions(ctx context.Context, updates []PhotoPositionUpdate) error
    DeleteByID(ctx context.Context, userID, photoID int64) error
    DeleteAll(ctx context.Context, userID int64) error
}

type PdfJobRepo interface {
    Create(ctx context.Context, userID int64, photoIDs []int64) (int64, error)
    ListByUser(ctx context.Context, userID int64) ([]types.PdfJob, error)
    Get(ctx context.Context, userID, jobID int64) (*types.PdfJob, error)
    ClaimPending(ctx context.Context, limit int) ([]types.PdfJob, error) // marks processing
    MarkCompleted(ctx context.Context, jobID int64, url string, logs string) error
    MarkFailed(ctx context.Context, jobID int64, errMsg string, logs string) error
}
```

### Types (shared)

```go
type User struct {
    ID           int64     `json:"id"`
    OpenID       string    `json:"openId"`
    Email        *string   `json:"email,omitempty"`
    Name         *string   `json:"name,omitempty"`
    LoginMethod  *string   `json:"loginMethod,omitempty"`
    Role         string    `json:"role"`
    LastSignedIn time.Time `json:"lastSignedIn"`
}

type Photo struct {
    ID       int64   `json:"id"`
    UserID   int64   `json:"userId"`
    FileKey  string  `json:"fileKey"`
    URL      string  `json:"url"`
    Filename string  `json:"filename"`
    MimeType *string `json:"mimeType,omitempty"`
    Size     *int64  `json:"size,omitempty"`
    Position int     `json:"position"`
}

type PdfJob struct {
    ID          int64      `json:"id"`
    UserID      int64      `json:"userId"`
    Status      string     `json:"status"`
    PhotoIDs    []int64    `json:"photoIds"`
    ResultURL   *string    `json:"resultUrl,omitempty"`
    ErrorMessage *string   `json:"errorMessage,omitempty"`
    Logs        *string    `json:"logs,omitempty"`
    CreatedAt   time.Time  `json:"createdAt"`
    UpdatedAt   time.Time  `json:"updatedAt"`
    CompletedAt *time.Time `json:"completedAt,omitempty"`
}
```

### File serving for disk storage

- Serve `/media/*` from `./data/storage` with auth middleware to ensure only owner can fetch their photos/PDFs; or generate short-lived signed URLs (`/media/<relKey>?sig=...&exp=...` using HMAC).

### Migration of the React client

- Keep the existing tRPC call sites; swap the client to point to the Go `/api/trpc` endpoint (same procedure names and payload shapes).
- Alternatively, create a thin adapter that transforms tRPC calls to REST endpoints; but tRPC-compatible JSON over POST keeps changes minimal.

### Configurable switches

- `STORAGE_BACKEND` (`disk`|`s3`)
- `DB_DSN` (e.g., `sqlite://data/app.db` or MySQL DSN)
- `WORKER_ENABLED` (bool)
- `JWT_SECRET`, `SESSION_NAME` (`app_session_id`)

### Minimal deliverable checklist

- SQLite migrations for users/photos/pdf_jobs.
- Disk storage implementation + `/media` serving with auth/signed URLs.
- tRPC-compatible handlers for all current procedures.
- Worker with claim/update logic, PDF generation (gofpdf/unidoc).
- Auth: email/password + JWT cookie; optional OAuth stubs/hooks.
- Frontend pointed at Go `/api/trpc`; verify upload→list→reorder→delete→pdf flow.

### Migration/implementation steps (suggested)

1) Define REST contracts and add a TS client module for React.
2) Implement storage + DB abstractions (disk + SQLite first).
3) Implement auth (email/password) + JWT cookies; add optional OAuth hooks.
4) Build API handlers (photos, pdf-jobs, auth, health).
5) Build worker with job claiming and PDF generation.
6) Wire React to REST endpoints; add login UI.
7) (Optional) Enable S3/MySQL and deploy worker separately.

### Risks and mitigations

- **Duplicate job processing**: Use DB claiming; consider SKIP LOCKED when moving to Postgres/MySQL.
- **Orphaned blobs**: Add `Delete` to storage interface; delete on photo delete and failed job cleanup.
- **Large files**: Enforce max upload size; consider streaming uploads.
- **HTTP over non-HTTPS**: Ensure `secure` cookie handling behind a proxy that sets `x-forwarded-proto`.
