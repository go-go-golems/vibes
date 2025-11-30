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

This document outlines two architectural approaches for migrating the photobook application backend from Node.js/TypeScript to Go. The goal is to create a Go service that maintains feature parity with the current implementation (photo management, PDF generation, authentication) while simplifying local development and providing a clear path to cloud deployment.

**Core Requirements**: The Go backend must support photo upload/list/reorder/delete operations, asynchronous PDF job processing, user authentication, and storage management. It should work seamlessly with the existing React frontend that currently uses tRPC for API communication.

**Two Migration Options**:

1) **Minimal Go monolith** — This approach prioritizes simplicity and speed of implementation. It uses a REST/JSON API (compatible with tRPC protocol), SQLite for local development, filesystem-based storage, and an in-process polling worker with database-based job locking. This is the fastest path to a working Go backend, ideal for local/offline use cases, and designed with pluggable interfaces so database and storage backends can be swapped later without major refactoring.

2) **Go monolith with job runner abstraction** — This approach adds more structure for scalability. It maintains the same API surface but introduces formal job queue abstractions (claim/complete semantics), supports an optional separate worker process, and is designed from the start to handle multiple instances and cloud storage/database backends. This option requires more upfront design but provides a smoother transition path when scaling becomes necessary.

**Authentication Strategy**: The default authentication mechanism will be email/password with JWT session cookies, removing the hard dependency on Manus OAuth. However, the design includes optional OAuth provider hooks (Google, GitHub, etc.) that can be added later without major architectural changes. This provides flexibility while keeping the initial implementation simple.

## Problem Statement

The current Node.js/TypeScript backend has several critical issues that motivate the migration to Go:

**Production Build Gap**: The production build process (`pnpm build`) bundles `server/index.ts`, which only serves static files and completely omits the API server. The actual API server exists in `server/_core/index.ts` but is not included in the production bundle, meaning deployed applications have no backend functionality. This is a critical blocker that must be resolved.

**Infrastructure Complexity**: The current Node/tRPC stack requires external infrastructure dependencies (MySQL database, Forge storage proxy, Manus OAuth service) that complicate local development and deployment. We need a Go service that can run entirely locally with no external dependencies, using SQLite and filesystem storage by default, while maintaining the ability to swap in cloud services when needed.

**Worker Concurrency Issues**: The current PDF worker implementation polls for pending jobs every 10 seconds but has no locking mechanism. Multiple worker instances can process the same job simultaneously, leading to duplicate work, wasted resources, and potential data corruption. The Go implementation must include proper job claiming/locking to prevent this.

**Authentication Dependency**: The current implementation has a hard dependency on Manus OAuth, which adds complexity and vendor lock-in. We need a simpler authentication path that works out of the box (email/password) but can accommodate common OAuth providers (Google, GitHub, etc.) through pluggable adapters when needed.

## Proposed Solution

Replace the entire Node.js backend with a Go HTTP API that maintains behavioral parity with the current implementation. The solution uses a layered architecture with pluggable interfaces for storage and database access, allowing SQLite and filesystem storage by default while maintaining a clear upgrade path to MySQL/Postgres and S3-compatible storage.

**API Compatibility**: The Go service will implement a tRPC-compatible API surface, accepting the same JSON payloads and procedure names that the React frontend currently expects. This minimizes frontend changes—the existing tRPC client can point to the Go endpoint with minimal or no modifications.

**Storage Abstraction**: A `Storage` interface abstracts blob storage operations (`Put`, `Open`, `Delete`), with a disk-based implementation for local development and an S3-compatible implementation for production. Both implementations maintain the same key format (`user-<id>/photos/...`, `user-<id>/pdfs/...`) to ensure consistency.

**Database Abstraction**: Similarly, database access is abstracted through repository interfaces, allowing SQLite for local development and MySQL/Postgres for production. The schema maintains parity with the current MySQL schema to ease data migration.

**Worker Improvements**: The PDF worker implements explicit job claiming using database row-level state transitions. Jobs transition from `pending` → `processing` → `completed`/`failed`, with the `processing` state acting as a lock to prevent duplicate processing. This eliminates the race condition present in the current implementation.

**Authentication Simplification**: A lightweight authentication module provides email/password authentication with JWT session cookies by default. The design includes extension points for OAuth providers, allowing Google, GitHub, or other providers to be added without architectural changes.

## Design Decisions

**API Protocol**: We'll implement a tRPC-compatible JSON-over-HTTP protocol rather than pure REST. This decision preserves compatibility with the existing React frontend, which uses tRPC hooks extensively. The frontend can continue using the same procedure names (`photo.upload`, `pdf.createJob`, etc.) and payload shapes, minimizing migration effort. Internally, the Go service uses clean service interfaces that could expose REST endpoints later if desired, but the primary interface remains tRPC-compatible for frontend compatibility.

**Schema Parity**: The database schema maintains close parity with the existing MySQL tables (`users`, `photos`, `pdfJobs` → `pdf_jobs` in SQLite). Column names are normalized (camelCase → snake_case for SQLite conventions), but the data model remains identical. This ensures that data migration scripts can be straightforward and that the application logic remains consistent across both implementations.

**Pluggable Backends**: Both storage and database are abstracted behind interfaces, allowing implementations to be swapped via configuration. The default implementation uses SQLite and filesystem storage for local development (zero external dependencies), while production can use MySQL/Postgres and S3-compatible storage. The interfaces are designed to be minimal but complete, ensuring that swapping implementations doesn't require changes to business logic.

**Worker Locking Strategy**: The PDF worker uses database row state transitions (`pending` → `processing` → `completed`/`failed`) as a locking mechanism. When claiming jobs, the worker atomically updates the status from `pending` to `processing`, preventing other workers from claiming the same job. This approach works with any SQL database and doesn't require external coordination services like Redis. The worker can run as a goroutine within the API process (Option 1) or as a separate binary (Option 2), with the same locking mechanism working in both cases.

**Authentication Default**: Email/password authentication with JWT session cookies is the default, removing the Manus dependency. The JWT format matches the current implementation (HS256, same cookie name `app_session_id`) to maintain session compatibility. OAuth providers are added via adapter pattern—the core auth service doesn't know about OAuth, but adapters can be registered to handle OAuth flows and map provider identities to users.

## Alternatives Considered

**Hybrid Approach (Node API + Go Worker)**: We considered keeping the Node.js/tRPC API server and only migrating the PDF worker to Go. This was rejected because it doesn't solve the production build issue (the API server still wouldn't be bundled), creates maintenance burden with two language stacks, and doesn't address the infrastructure complexity problem. A full migration to Go provides a cleaner architecture and better long-term maintainability.

**gRPC Protocol**: Using gRPC instead of REST/JSON was considered for better type safety and performance. However, this would require significant frontend changes (gRPC-web setup, code generation, different client libraries) and adds complexity that isn't justified for the current scope. The tRPC-compatible JSON-over-HTTP approach provides a good balance of type safety (through shared TypeScript types) and simplicity.

**External Queue/Broker (Redis/RabbitMQ)**: A full message queue system like Redis or RabbitMQ would provide robust job distribution and retry mechanisms. However, this adds external infrastructure dependencies that complicate local development and deployment. The database-based job claiming approach works well for the current scale and can be upgraded to a proper queue system later if needed. For multi-instance deployments, PostgreSQL's `SKIP LOCKED` feature provides efficient job claiming without external dependencies.

## Implementation Plan

**Phase 1: API Contract Definition** — Freeze the tRPC procedure contracts (input/output types, procedure names) and create a small TypeScript client module for the React frontend. This ensures that both frontend and backend teams have a clear contract to work against. The client module can be a thin wrapper around fetch calls that matches the existing tRPC hook interface, allowing gradual migration.

**Phase 2: Core Go Service** — Implement the foundational Go service including HTTP handlers, SQLite database migrations, disk-based storage implementation, and JWT-based authentication. This phase focuses on getting a working local development environment with all core features (photo CRUD, user auth, basic PDF job creation). The service should be runnable with a single binary and no external dependencies.

**Phase 3: PDF Worker** — Build the PDF generation worker with proper job claiming logic, status updates, and error handling. This includes implementing the PDF generation algorithm (matching the current jsPDF+canvas approach: A4 portrait, 10mm margins, aspect-fit images), storage integration for uploading completed PDFs, and structured logging. The worker should handle failures gracefully and provide clear error messages.

**Phase 4: Frontend Integration** — Wire the React frontend to the Go API endpoints, ensuring all existing functionality works (photo upload/list/reorder/delete, PDF job creation/polling, authentication). This may require updating the tRPC client configuration to point to the Go endpoint, but the procedure calls should remain largely unchanged. Add any missing UI elements (login form, job status display) as needed.

**Phase 5: Production Readiness** — Add pluggable backends for MySQL/Postgres and S3-compatible storage, allowing the service to run in production environments. Implement optional OAuth providers (starting with Google/GitHub) as adapters to the core auth system. Add monitoring, logging, and deployment configurations.

## Open Questions

**Signed URLs for Disk Storage**: When serving files from disk storage via `/media/*` endpoints, do we need signed URLs with expiration times? For single-user local development, direct file serving with authentication middleware is sufficient. However, for multi-user setups or production deployments, signed URLs provide better security (time-limited access, no need to expose authentication cookies) and can prevent URL sharing. Recommendation: implement signed URLs from the start to avoid refactoring later, but make them optional via configuration.

**Data Retention and Cleanup**: What is the retention policy for photos and generated PDFs? Should photos be automatically deleted after a certain period? Should failed PDF jobs be cleaned up? Should completed PDFs be archived or deleted? These decisions affect storage costs and user experience. Recommendation: start with no automatic cleanup (manual deletion only) but add a cleanup job/endpoint that can be run periodically. Document the policy clearly for users.

**OAuth Provider Priority**: If we implement OAuth providers, which ones should be prioritized? Common choices include Google (widest adoption), GitHub (developer-friendly), and Microsoft (enterprise). The current implementation uses Manus OAuth, but we're removing that dependency. Recommendation: start with email/password only, then add Google OAuth as the first provider (most universal). GitHub can follow if there's developer demand. The adapter pattern allows adding providers incrementally without architectural changes.

## References

**Current Architecture Documentation**: The comprehensive reference document `reference/01-current-architecture-and-data-flow.md` contains detailed information about the current Node.js implementation, including API contracts, data flows, authentication mechanisms, PDF generation algorithms, and identified gaps. This document serves as the source of truth for ensuring behavioral parity during migration.

**Ticket Overview**: The main ticket index (`index.md`) provides an overview of the migration effort, links to all related documentation, and tracks the overall progress. It's the entry point for understanding the scope and status of the Go port project.

---

## Detailed Design

This section provides the technical specifications for implementing the Go backend. The design applies to both migration options (minimal monolith and job runner abstraction), with differences noted where applicable. The API surface, data model, storage interface, and authentication model are common to both approaches.

### Common API surface (applies to both options)

The API surface maintains compatibility with the current tRPC implementation, accepting the same procedure names and payload shapes. The React frontend can continue using tRPC hooks with minimal changes—only the endpoint URL needs to be updated.

**Authentication Endpoints**: These endpoints handle user authentication and session management. The login endpoint accepts email/password credentials, validates them, and sets a JWT session cookie (`app_session_id`) matching the current implementation. The logout endpoint clears the session cookie, and the `me` endpoint returns the current authenticated user's information.

**Photo Management Endpoints**: These endpoints handle the core photobook functionality. Photo upload accepts either multipart form data or JSON with base64-encoded image data (matching current implementation), stores the blob via the storage interface, and creates a database record. The list endpoint returns photos ordered by the `position` field. The positions endpoint allows batch reordering of photos. Delete endpoints support both individual photo deletion and bulk deletion of all user photos.

**PDF Job Endpoints**: These endpoints manage asynchronous PDF generation. Creating a job accepts an array of photo IDs and returns a job ID. The job is inserted with `status=pending` and will be picked up by the worker. The list endpoint returns all jobs for the authenticated user, and the detail endpoint provides full job information including status, result URL (when completed), error messages (when failed), and structured logs.

**Health Check**: A simple health endpoint for monitoring and load balancer checks. Returns a simple JSON response indicating service availability.

### Data model (parity with current schema)

The database schema maintains close parity with the current MySQL schema to ensure data migration is straightforward and application logic remains consistent. Column names are normalized to snake_case for SQLite conventions, but the data model is identical.

**Users Table**: Stores user account information. The `open_id` field serves dual purpose: for email/password authentication, it stores a generated unique identifier; for OAuth providers, it stores the provider's user ID. The `role` field uses a CHECK constraint to ensure only valid values (`user` or `admin`). Timestamps track account creation, last update, and last sign-in for session management.

**Photos Table**: Stores photo metadata and references to storage blobs. The `file_key` field contains the storage path (e.g., `user-42/photos/abc123-image.jpg`), while `url` contains the full URL for accessing the photo (either a direct file path for disk storage or a signed URL for S3). The `position` field enables user-defined ordering of photos. Note that deleting a photo row doesn't automatically delete the storage blob—this must be handled explicitly via the storage interface.

**PDF Jobs Table**: Tracks PDF generation jobs with their current status and results. The `photo_ids` field stores a JSON array of photo IDs (as text in SQLite, matching the current MySQL implementation). The `status` field transitions through `pending` → `processing` → `completed`/`failed`, with the `processing` state acting as a lock. The `logs` field stores structured JSON log entries for debugging and progress tracking. The `completed_at` timestamp is set when a job finishes (either successfully or with failure).

**User Identities Table (Optional)**: This table is only needed if implementing OAuth providers. It stores the mapping between OAuth provider identities and local user accounts, allowing users to link multiple OAuth accounts to a single local account. The `metadata` field can store provider-specific information (profile picture URL, etc.).

### Storage interface

The storage interface abstracts blob storage operations, allowing implementations to be swapped without changing business logic. The interface is designed to be minimal but complete, covering the essential operations needed for photo and PDF storage.

**Core Operations**: The `Put` method stores a blob at a given relative key (e.g., `user-42/photos/abc123-image.jpg`) and returns a URL for accessing it. The `Open` method retrieves a blob for reading (used by the PDF worker to load photos). The `Delete` method removes a blob, which is essential for cleanup operations (deleting photos should also delete their storage blobs, failed jobs may need cleanup, etc.).

**Disk Implementation (Default)**: The disk-based implementation writes files under `./data/storage/<relKey>`, maintaining the directory structure implied by the key. Files are served via HTTP endpoints at `/media/<relKey>` with authentication middleware ensuring only the owner can access their files. Alternatively, signed URLs can be generated using HMAC signatures with expiration times for better security in multi-user scenarios.

**S3-Compatible Implementation (Production)**: The S3 implementation uses the AWS SDK (or MinIO SDK for S3-compatible storage) to store blobs in cloud storage. It maintains the same key format (`user-<id>/photos/...`, `user-<id>/pdfs/...`) to ensure consistency. The implementation generates pre-signed URLs for accessing files, with configurable expiration times. This implementation can be swapped in via configuration without code changes to the rest of the application.

### Auth model

The authentication model provides a simple default (email/password) while maintaining extensibility for OAuth providers. The session mechanism matches the current implementation to ensure compatibility.

**Email/Password Authentication**: The default authentication method uses email addresses as usernames and bcrypt-hashed passwords for secure storage. When a user logs in, the system validates credentials, generates a JWT session token, and sets it as an HTTP-only cookie named `app_session_id` (matching the current implementation). The JWT uses HS256 algorithm with a secret from environment variables, and includes user ID, open ID, role, name, and expiration time in the claims.

**OAuth Provider Adapters**: OAuth providers (Google, GitHub, etc.) are added via adapter pattern. The core authentication service doesn't know about OAuth—instead, adapters handle OAuth flows (authorization redirect, token exchange, user info fetching) and map provider identities to local user accounts. Provider mappings are stored in the optional `user_identities` table, allowing users to link multiple OAuth accounts to a single local account.

**Admin Role Assignment**: Admin users are identified via the `OWNER_OPEN_ID` environment variable, matching the current implementation. When a user's `open_id` matches this value, their role is automatically set to `admin`. For more complex admin management in the future, an `admins` table can be added, but the environment variable approach is sufficient for initial implementation.

### Worker model

The PDF worker implements asynchronous job processing with proper concurrency control to prevent duplicate processing. The design matches the current worker's behavior while fixing the locking issues.

**Job Claiming**: Jobs are claimed atomically by updating their status from `pending` to `processing` in a single database operation (`UPDATE ... WHERE status='pending' LIMIT N`). This prevents multiple workers from claiming the same job. The `updated_at` timestamp can be used for lease semantics—if a job remains in `processing` state for too long (e.g., worker crashed), it can be reset to `pending` for retry. For PostgreSQL/MySQL, `SKIP LOCKED` can be used for more efficient claiming in multi-instance deployments.

**Retry and Backoff**: Retry logic is optional for the initial implementation. The minimum requirement is avoiding duplicate claims through the atomic status update. If a job fails, it transitions to `failed` state with an error message. Future enhancements could add automatic retry with exponential backoff, but manual retry (user creates a new job) is acceptable for initial implementation.

**Structured Logging**: The worker stores structured log entries (JSON array) in the `logs` field of the job record. Each log entry includes timestamp, level (info/warn/error), and message. Log size should be capped (e.g., 64KB) to prevent oversized database rows. Logs are useful for debugging failed jobs and tracking progress during PDF generation.

**PDF Generation Algorithm**: The PDF generation matches the current implementation: A4 portrait format, 10mm margins on all sides, one photo per page. Images are aspect-fitted within the available space (190mm × 277mm), maintaining aspect ratio and centering. The Go implementation uses gofpdf or unidoc libraries for PDF generation, loading images via the storage interface and rendering them to PDF pages.

**Job Completion**: When PDF generation succeeds, the worker uploads the PDF via the storage interface, sets `result_url` to the storage URL, sets `completed_at` timestamp, and transitions status to `completed`. On any error, the worker sets `status=failed`, stores the error message in `error_message`, and includes error details in the logs. The job record provides complete information for debugging and user feedback.

### Option 1 — Minimal Go monolith (SQLite + disk, polling worker)

This option prioritizes simplicity and speed of implementation, creating a single binary that handles both API requests and background job processing.

**Architecture**: The service runs as a single Go binary that starts an HTTP server for API requests and a background goroutine for PDF job processing. Both share the same database connection and storage interface, simplifying deployment and development. The worker runs on a ticker (e.g., every 10 seconds) and processes jobs in the same process as the API server.

**Database**: SQLite is used as the default database, stored as a single file (e.g., `./data/app.db`). Migrations are managed via a tool like Goose or similar, ensuring schema changes are versioned and repeatable. SQLite provides excellent performance for single-user or small-scale deployments and requires no external database server.

**Storage**: Files are stored on the local filesystem under `./data/storage`, maintaining the directory structure from storage keys. A static file handler serves files via `/media/*` endpoints with authentication middleware. Alternatively, signed URLs can be generated for better security in multi-user scenarios.

**Worker Implementation**: The worker uses a simple polling approach: every 10 seconds, it queries for pending jobs, claims them atomically via status update, and processes them sequentially. There's no external queue or message broker—everything is coordinated through the database. This approach is simple and works well for single-instance deployments.

**Advantages**: This is the simplest approach with the fastest time to implementation. It requires no external services (no database server, no message queue, no cloud storage), making it ideal for local development and offline use cases. The pluggable interfaces allow upgrading to cloud backends later without major refactoring.

**Limitations**: This approach is single-process and doesn't scale horizontally without changes. The polling approach has some overhead (checking for jobs every 10 seconds even when idle), though this is negligible for small deployments. Scaling to multiple instances requires moving to Option 2 or adding external coordination.

### Option 2 — Go monolith with job runner abstraction

This option adds more structure for scalability, introducing formal job queue abstractions and supporting separate worker processes for better resource isolation and horizontal scaling.

**Architecture**: The service can run as either a single binary (API + worker) or as separate binaries (API server and worker process) sharing the same database and storage backends. The job runner abstraction provides clean interfaces (`Enqueue`, `Claim`, `Complete`, `Fail`) that can be implemented with database-backed queues or external message brokers later.

**Database Flexibility**: While SQLite is still supported for local development, this option is designed to work seamlessly with MySQL or Postgres by simply swapping the database DSN. The repository interfaces abstract database-specific features (like `SKIP LOCKED` in Postgres for efficient job claiming), allowing the same code to work across database backends.

**Job Runner Abstraction**: The job runner provides formal interfaces for job management: `Enqueue` creates a new job, `Claim` atomically claims pending jobs (with optional limit), `Complete` marks a job as successfully finished, and `Fail` marks a job as failed with error details. An optional `job_runs` table can track retry attempts, enabling sophisticated retry logic with exponential backoff.

**Storage Pluggability**: Storage remains pluggable (`disk` for local, `s3` for production), but this option is designed with cloud storage in mind from the start. The storage interface is the same as Option 1, but the architecture assumes that storage may be remote and accessed over the network.

**Advantages**: This approach is ready for multiple instances from day one—multiple API servers can run behind a load balancer, and multiple worker processes can claim jobs without conflicts. The separation of concerns (API vs. worker) makes it easier to scale each component independently. The path to cloud deployment is smoother since the architecture already assumes distributed components.

**Trade-offs**: This option requires more upfront design and code complexity than Option 1. The job runner abstraction adds an extra layer, and supporting separate worker processes requires more configuration. However, this complexity pays off when scaling becomes necessary, avoiding a major architectural refactor later.

---

## Detailed Design — Minimal Go monolith (SQLite + disk, tRPC-compatible surface)

This section provides the detailed technical design for Option 1 (Minimal Go monolith). This is the recommended starting point for the migration, as it provides the fastest path to a working Go backend while maintaining compatibility with the existing React frontend.

**tRPC Compatibility**: The React frontend currently uses tRPC hooks to call backend procedures under `/api/trpc`. Rather than requiring frontend changes to use REST, we'll implement a tRPC-compatible API surface in Go. This means accepting the same JSON payloads and procedure names (`photo.upload`, `pdf.createJob`, etc.) that the frontend expects. The Go implementation uses plain JSON over POST requests, matching tRPC's batched envelope format. Internally, the Go service uses clean service interfaces that could expose REST endpoints later if desired, but the primary interface remains tRPC-compatible to minimize frontend migration effort.

### Go project layout (proposed)

The project follows standard Go project layout conventions with clear separation of concerns. The `cmd/` directory contains the main application entry point, `internal/` contains private implementation code (not importable by other projects), and `pkg/` contains public packages that could be reused.

**Application Entry**: `cmd/api/main.go` wires together the HTTP server, routes, and middleware, following dependency injection patterns. This is the single point where all components are assembled and the server is started.

**Configuration**: `internal/config/config.go` handles environment variable loading and validation, providing a typed configuration struct to the rest of the application. This centralizes all configuration logic and makes it easy to see what environment variables are required.

**HTTP Layer**: `internal/http/trpc.go` implements the tRPC-compatible router and handler multiplexer, parsing incoming JSON requests and routing them to appropriate handlers. `internal/http/middleware.go` contains HTTP middleware for authentication, logging, error recovery, and request context management.

**Business Logic**: `internal/auth/auth.go` implements authentication logic (email/password validation, JWT generation/verification, session management). `internal/photos/service.go` contains photo management business logic (upload, list, reorder, delete), while `internal/photos/store.go` handles database and storage integration. Similarly, `internal/pdfjobs/service.go` handles PDF job creation and querying, while `internal/pdfjobs/worker.go` implements the background worker that processes jobs.

**Infrastructure**: `internal/storage/storage.go` defines the storage interface and provides the disk-based implementation. `internal/db/db.go` manages database connections and provides migration utilities. `internal/db/migrations/` contains SQL migration files (versioned, sequential) that define the schema.

**Shared Types**: `pkg/types/types.go` contains shared data models (User, Photo, PdfJob, error types) that are used across the application. These types match the current TypeScript types to ensure API compatibility.

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

The API request flow follows a standard middleware → router → handler → service → repository pattern, ensuring clean separation of concerns and testability.

**Request Processing**: When a request arrives, authentication middleware first parses the `app_session_id` cookie, verifies the JWT signature and expiration, and loads the user record from the database. For public routes (like health checks), authentication is optional. The middleware attaches the authenticated user (or nil) to the request context for downstream handlers to access.

**Routing**: The tRPC dispatcher parses the incoming JSON payload to determine which procedure is being called (`photo.upload`, `photo.list`, `pdf.createJob`, etc.) and routes the request to the appropriate handler function. The dispatcher handles both single procedure calls and batched calls (multiple procedures in one request).

**Handler Execution**: Handlers receive the request context (with authenticated user), parse input parameters, and call service layer methods. For example, `photo.upload` handler calls `photos.Service.Upload(ctx, user, input)`, which orchestrates the business logic: validates input, calls `storage.Put()` to store the blob, calls `repo.InsertPhoto()` to create the database record, and returns the URL and file key. Similarly, `pdf.createJob` handler calls `pdf.Service.CreateJob(ctx, user, photoIDs)`, which validates the photo IDs belong to the user and calls `repo.InsertJob()` with `status=pending`.

**Response Encoding**: Handlers return Go structs that match the expected JSON response shapes. The tRPC dispatcher encodes these structs as JSON and returns them to the client. The response format matches exactly what the current Node.js implementation returns, ensuring frontend compatibility.

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

This section identifies potential risks in the Go implementation and provides mitigation strategies based on lessons learned from the current Node.js implementation.

**Duplicate Job Processing**: The current Node.js worker has no locking mechanism, allowing multiple instances to process the same job. **Mitigation**: The Go implementation uses database-based job claiming with atomic status updates (`UPDATE ... WHERE status='pending'`). When moving to Postgres/MySQL, use `SKIP LOCKED` for efficient concurrent job claiming without blocking. This ensures only one worker processes each job, even with multiple worker instances.

**Orphaned Storage Blobs**: The current implementation doesn't delete storage blobs when photos are deleted or jobs fail, leading to storage bloat over time. **Mitigation**: Add `Delete` method to the storage interface and call it explicitly when deleting photos or cleaning up failed jobs. Implement a periodic cleanup job that identifies orphaned blobs (photos without database records, old failed PDFs) and removes them. Document storage lifecycle policies clearly.

**Large File Uploads**: Base64-encoded image uploads can be large (50MB+), consuming significant memory and potentially causing timeouts. **Mitigation**: Enforce maximum upload size limits (e.g., 10MB per photo) at the HTTP middleware level. Consider implementing streaming uploads for very large files, though base64 encoding is acceptable for typical photo sizes. Monitor memory usage during upload processing and add timeouts to prevent resource exhaustion.

**Cookie Security on HTTP**: The current implementation sets `secure` cookie flag based on request protocol, which may fail on plain HTTP deployments. **Mitigation**: Ensure proper handling of `x-forwarded-proto` header when running behind a reverse proxy (common in production). The cookie middleware should check both `req.Protocol` and `x-forwarded-proto` header to determine if the connection is secure. For local development, allow `secure=false` when explicitly configured, but default to secure in production environments.
