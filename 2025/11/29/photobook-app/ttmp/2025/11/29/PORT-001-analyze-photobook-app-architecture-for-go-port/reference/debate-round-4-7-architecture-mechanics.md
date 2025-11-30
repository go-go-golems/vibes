---
Title: Debate Round 4-7: Architecture and Mechanics Decisions
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
      Note: Current worker implementation (no locking, sequential processing)
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: Current OAuth implementation
    - Path: 2025/11/29/photobook-app/server/storage.ts
      Note: Current storage proxy implementation
    - Path: 2025/11/29/photobook-app/drizzle/schema.ts
      Note: Current schema structure
ExternalSources: []
Summary: Combined debate covering job queue abstraction, Go project layout, authentication strategy, and file serving approach
LastUpdated: 2025-11-30T00:00:00-05:00
---

# Debate Round 4-7: Architecture and Mechanics Decisions

## Context: Decisions Made

From previous debate rounds, we've decided:
- **API Protocol**: tRPC-compatible endpoints (maintain frontend compatibility)
- **Infrastructure**: SQLite + filesystem storage (start simple, add pluggable interfaces)
- **Worker Architecture**: Single binary, in-process worker (simple deployment)

This debate explores the architectural and mechanical decisions that follow from these choices.

## Topics Covered

1. **Job Queue Abstraction**: What level of abstraction do we need for job processing?
2. **Go Project Layout**: How should we structure the Go codebase?
3. **Authentication Strategy**: Email/password only, or OAuth from the start?
4. **File Serving**: Direct HTTP serving vs signed URLs?

## Pre-Debate Research

### Current Worker Implementation Analysis

**Research Method**: Analyzed `server/pdfWorker.ts` for job processing patterns.

**Findings**:
- **No abstraction**: Direct database calls (`db.getPendingPdfJobs()`, `db.updatePdfJob()`)
- **No locking**: Multiple workers can process same job (race condition)
- **Sequential processing**: Processes all pending jobs in one tick
- **Status transitions**: `pending` → `processing` → `completed`/`failed`
- **Error handling**: Per-job try/catch, continues on errors
- **Logging**: Structured JSON logs stored in `pdfJobs.logs` field

**Current implementation**: ~150 lines, no formal queue abstraction

### Current OAuth Implementation Analysis

**Research Method**: Reviewed `server/_core/oauth.ts` and `server/_core/sdk.ts`.

**Findings**:
- **Hard dependency on Manus OAuth**: External service required
- **OAuth flow**: Code exchange → token → user info → session creation
- **Session management**: JWT cookies (`app_session_id`), HS256 algorithm
- **User sync**: Fetches user from OAuth server if not in DB
- **Admin assignment**: Based on `OWNER_OPEN_ID` environment variable

**Current complexity**: ~200 lines of OAuth handling code

### Current Storage Implementation Analysis

**Research Method**: Reviewed `server/storage.ts` and storage proxy usage.

**Findings**:
- **External HTTP service**: Storage proxy (`BUILT_IN_FORGE_API_URL`)
- **Permanent URLs**: Upload returns permanent download URLs (not signed)
- **Time-limited URLs**: `downloadUrl` endpoint returns signed URLs with expiration
- **Key format**: `user-<id>/photos/...`, `user-<id>/pdfs/...`
- **No blob deletion**: Deleting photos doesn't delete storage blobs

**Current gaps**: No cleanup, no direct file serving

### Go Project Layout Patterns

**Research Method**: Reviewed zine-layout project structure and Go conventions.

**Findings**:
- **Standard layout**: `cmd/`, `internal/`, `pkg/` structure
- **Domain organization**: `pkg/repo/` (repositories), `pkg/services/` (business logic)
- **Layer organization**: `pkg/serve/` (HTTP handlers)
- **Migration patterns**: `pkg/repo/sqlite/migrations.go` (embedded migrations)

**Go conventions**: Standard layout is widely recognized, but domain-driven organization is also common

---

## Opening Statements

### Topic 4: Job Queue Abstraction Level

#### Alex "The Pragmatist" Chen

*[Shows current worker implementation]*

The current worker has **no abstraction**—it's just direct database calls:
- `db.getPendingPdfJobs()` - Get pending jobs
- `db.updatePdfJob()` - Update job status

That's fine! We're running a single binary, single worker instance. We don't need a formal queue abstraction.

**My position**: Use simple database operations. Atomic status updates (`UPDATE ... WHERE status='pending'`) provide locking. No need for `Enqueue`/`Claim`/`Complete` interfaces—just direct SQL.

```go
// Simple, direct approach
jobs, err := repo.ClaimPendingJobs(ctx, 5) // UPDATE ... WHERE status='pending' LIMIT 5
for _, job := range jobs {
    processJob(ctx, job)
}
```

Keep it simple. Add abstraction later if we need multiple instances or external queues.

#### Sam "The Architect" Rivera

Alex, I hear you on simplicity, but let me show you the problem.

*[Points to design doc]*

If we use direct database calls, we're coupling the worker to SQL. What if we want to:
- Add retry logic with exponential backoff?
- Track job run history?
- Support external queues (Redis, RabbitMQ) later?

**My position**: Create a minimal job queue interface:

```go
type JobQueue interface {
    Enqueue(ctx context.Context, jobType string, payload []byte) (jobID int64, error)
    Claim(ctx context.Context, limit int) ([]Job, error)
    Complete(ctx context.Context, jobID int64, result []byte) error
    Fail(ctx context.Context, jobID int64, err error) error
}
```

Start with database-backed implementation, but the interface allows swapping implementations later. The abstraction cost is low (~50 lines), but the flexibility is high.

#### `server/pdfWorker.ts` — "The Background Processor"

*[Interjects]*

Point of order! I'm broken. I have **no locking**. Fix that first, then debate abstraction.

*[Shows current code]*

```typescript
const jobs = await db.getPendingPdfJobs(); // No locking!
for (const job of jobs) {
    await processJob(job); // Multiple workers can process same job
}
```

**My position**: Fix the locking first. Use atomic status updates:
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 5
RETURNING *;
```

That's locking. Whether you wrap it in an interface or call it directly doesn't matter—just fix the race condition.

#### Morgan "The Infrastructure Engineer" Taylor

*[Nods]*

The worker is right—fix locking first. But let me add: **if we're running a single binary, single instance**, we don't need complex queue abstractions.

However, we should still track:
- Job run history (for debugging)
- Retry attempts (for failed jobs)
- Job metadata (created_at, updated_at, completed_at)

**My position**: Simple database operations with proper locking. Add a `job_runs` table for history/retries if needed. No need for formal queue abstraction unless we're scaling to multiple instances.

---

### Topic 5: Go Project Layout Structure

#### Sam "The Architect" Rivera

*[Shows standard Go layout]*

Go has a standard project layout:
- `cmd/` - Application entry points
- `internal/` - Private implementation code
- `pkg/` - Public packages (reusable)

**My position**: Follow standard layout with domain-driven organization:

```
cmd/
  api/
    main.go          # Entry point
internal/
  config/           # Configuration
  http/             # HTTP handlers, middleware
  auth/             # Authentication logic
  photos/           # Photo domain (service, store)
  pdfjobs/          # PDF job domain (service, worker)
  storage/          # Storage interface + disk implementation
  db/               # Database connection + migrations
pkg/
  types/            # Shared types (User, Photo, PdfJob)
```

Clear separation: `internal/` for private code, `pkg/` for shared types. Domain-driven organization within `internal/`.

#### "The New Hire" — Fresh Eyes

*[Looks confused]*

Wait, why `internal/photos/` and `internal/pdfjobs/`? Why not `internal/services/photos.go`?

*[Reviews zine-layout structure]*

I see zine-layout uses `pkg/services/` for business logic. That's layer-driven, not domain-driven.

**My position**: Layer-driven organization is clearer for onboarding:

```
internal/
  services/          # Business logic (photos.go, pdfjobs.go, auth.go)
  repositories/     # Data access (photos.go, pdfjobs.go, users.go)
  handlers/         # HTTP handlers (photos.go, pdfjobs.go, auth.go)
  storage/          # Storage interface + implementations
```

When I'm new, I know: "Business logic is in `services/`, data access is in `repositories/`." Domain-driven makes me hunt across directories.

#### Alex "The Pragmatist" Chen

*[Shrugs]*

Both of you are overthinking this. We have **3 domains**: photos, PDF jobs, auth. That's it.

**My position**: Flat structure, minimal directories:

```
cmd/
  api/
    main.go
internal/
  photos.go         # Photo service + repository
  pdfjobs.go        # PDF job service + worker
  auth.go           # Auth service
  storage.go        # Storage interface + disk implementation
  db.go             # Database connection
pkg/
  types.go          # Shared types
```

One file per domain. When you need photos, open `internal/photos.go`. Simple. Add directories when files get too big (>500 lines).

#### `go.mod` — "The Go Ecosystem"

*[Interjects]*

Point of order! I'm the Go ecosystem. Let me clarify conventions.

**Standard layout** (`cmd/`, `internal/`, `pkg/`) is widely recognized, but not enforced. Many projects use:
- Domain-driven: `internal/photos/`, `internal/pdfjobs/`
- Layer-driven: `internal/services/`, `internal/repositories/`
- Flat: `internal/photos.go`, `internal/pdfjobs.go`

**My position**: Use standard layout (`cmd/`, `internal/`, `pkg/`) for familiarity, but organize `internal/` however makes sense. Domain-driven is common for microservices, layer-driven for monoliths. For a small project, flat is fine.

---

### Topic 6: Authentication Strategy

#### Alex "The Pragmatist" Chen

*[Shows current OAuth complexity]*

The current implementation has **200 lines of OAuth code** and a hard dependency on Manus. That's complexity we don't need.

**My position**: Email/password authentication only. Simple:
- User registers with email/password
- Password hashed with bcrypt
- JWT session cookie (same format as current: `app_session_id`)
- Admin assignment via `OWNER_OPEN_ID` env var

No OAuth providers. No external dependencies. Ship fast, add OAuth later if needed.

#### Jordan "The Feature Engineer" Kim

Alex, I hear you on simplicity, but let me show you the UX problem.

*[Shows user flow]*

Users expect OAuth. "Sign in with Google" is one click. Email/password requires:
1. Remember password
2. Type email
3. Type password
4. Handle password reset

That's **friction**. OAuth reduces friction.

**My position**: Include OAuth providers from the start. At minimum: Google OAuth. Use adapter pattern so we can add more providers later. The implementation complexity is worth the UX improvement.

#### `server/_core/index.ts` — "The Current API"

*[Interjects]*

Point of order! I'm the current API. Let me show you what OAuth actually requires.

*[Shows OAuth flow]*

Current OAuth flow:
1. User clicks "Sign in with Google"
2. Redirect to OAuth provider
3. User authorizes
4. Redirect back with code
5. Exchange code for token
6. Fetch user info
7. Create session

That's **7 steps**, and it requires:
- OAuth provider setup (Google Cloud Console, etc.)
- Redirect URL configuration
- Client ID/secret management
- Token exchange logic
- User info fetching

**My position**: OAuth is complex. If we're removing Manus dependency, we're implementing all of this ourselves. That's significant work. Email/password is simpler, but OAuth provides better UX.

#### "The New Hire" — Fresh Eyes

*[Looks confused]*

Wait, why can't we have both? Email/password **and** OAuth?

*[Thinks]*

If we design auth with an adapter pattern:
- Core auth service handles sessions (JWT, cookies)
- Email/password adapter implements registration/login
- OAuth adapter implements OAuth flow
- Both create sessions the same way

**My position**: Implement email/password first (simpler), but design for OAuth adapters. Use adapter pattern so we can add Google/GitHub OAuth later without refactoring. Best of both worlds: ship fast, extend later.

---

### Topic 7: File Serving Approach

#### Alex "The Pragmatist" Chen

*[Shows current storage proxy]*

The current implementation uses a storage proxy that returns permanent URLs. For local dev with filesystem storage, we can serve files directly.

**My position**: Direct HTTP serving with auth middleware. Simple:
- Store files in `./data/storage/<relKey>`
- Serve via `/media/<relKey>` endpoint
- Auth middleware checks user owns the file (from `relKey`)

No signed URLs, no HMAC signing, no expiration logic. Simple file serving.

#### Morgan "The Infrastructure Engineer" Taylor

Alex, I hear you on simplicity, but let me show you the security problem.

*[Shows file serving]*

If we serve files directly:
- User gets URL: `/media/user-42/photos/image.jpg`
- User shares URL with friend
- Friend accesses file (if auth middleware allows)
- **No expiration**: URL works forever

**My position**: Signed URLs with expiration. Generate HMAC-signed URLs:
- `/media/<relKey>?sig=<hmac>&exp=<timestamp>`
- Verify signature and expiration on each request
- URLs expire after 1 hour (configurable)

Better security (time-limited access), prevents URL sharing, production-ready from day one.

#### `server/storage.ts` — "The Storage Proxy"

*[Interjects]*

Point of order! I'm the current storage. Let me clarify what I do.

*[Shows storage proxy API]*

Current storage proxy:
- **Upload**: Returns permanent URL (not signed)
- **Download URL**: Returns time-limited signed URL (via separate endpoint)

So we have **both patterns** already. Permanent URLs for uploads, signed URLs for downloads.

**My position**: Match current behavior. For disk storage:
- Upload returns permanent URL (`/media/<relKey>`)
- Add separate endpoint for signed URLs (`/media/<relKey>/signed?exp=...`)

Keep it simple for local dev (direct serving), but support signed URLs for production.

#### Sam "The Architect" Rivera

*[Nods]*

The storage proxy makes a good point. We can have both.

**My position**: Storage interface should support both:
- `Put()` returns permanent URL (for uploads)
- `GetSignedURL()` returns time-limited URL (for downloads)

Disk implementation:
- Permanent URLs: Direct serving with auth middleware
- Signed URLs: HMAC-signed URLs with expiration

S3 implementation:
- Permanent URLs: S3 public URLs (if bucket is public)
- Signed URLs: Pre-signed S3 URLs

Same interface, different implementations. Simple for local dev, production-ready for S3.

---

## Rebuttals and Counter-Arguments

### Round 1: Job Queue Abstraction Rebuttals

#### Sam "The Architect" Rivera → Alex "The Pragmatist" Chen

Alex, you said "add abstraction later if needed." But here's the problem: **adding abstraction later requires refactoring**.

*[Shows current code]*

If we start with direct database calls:
```go
jobs, err := repo.ClaimPendingJobs(ctx, 5)
```

Then later we want to add retry logic or external queues, we refactor:
- Change all call sites
- Extract to interface
- Implement new backend

**My counter**: Create minimal interface from the start. The cost is low (~50 lines), but the flexibility is high. We can start with database-backed implementation, but the interface allows swapping later without refactoring.

#### Alex "The Pragmatist" Chen → Sam "The Architect" Rivera

Sam, you're optimizing for a problem we don't have. We're running **single binary, single instance**. We don't need external queues.

*[Shows YAGNI principle]*

YAGNI: You Aren't Gonna Need It. If we need external queues later, we refactor then. The abstraction cost (interface + implementation) is higher than the refactoring cost (extract to interface when needed).

**My counter**: Use direct database operations. Fix locking (atomic updates), but skip the abstraction. Add interface when we actually need it (multiple instances, external queues).

#### `server/pdfWorker.ts` → Both

*[Interjects angrily]*

Both of you are missing the point! **Fix the locking first**. Whether you use an interface or direct calls doesn't matter—just fix the race condition.

*[Shows broken code]*

My current code has no locking. Multiple workers process the same job. That's a **bug**, not an architecture question.

**My position**: Use atomic status updates:
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 5
RETURNING *;
```

That's locking. Wrap it in an interface or call it directly—just fix the bug.

---

### Round 2: Project Layout Rebuttals

#### "The New Hire" → Sam "The Architect" Rivera

Sam, you said domain-driven organization. But when I'm new, I don't know what "photos domain" means.

*[Shows confusion]*

If I need to find photo upload logic, do I look in:
- `internal/photos/service.go`? (Business logic)
- `internal/photos/repository.go`? (Data access)
- `internal/http/photos.go`? (HTTP handlers)

I have to hunt across directories. Layer-driven is clearer: "Business logic is in `services/`, data access is in `repositories/`."

**My counter**: Layer-driven organization for clarity. Domain-driven is fine for microservices, but for a monolith, layers are easier to understand.

#### Sam "The Architect" Rivera → "The New Hire"

New Hire, I hear you on clarity, but let me show you the problem with layers.

*[Shows layer-driven structure]*

If we use layers:
- `services/photos.go` - Photo business logic
- `repositories/photos.go` - Photo data access
- `handlers/photos.go` - Photo HTTP handlers

When I'm working on photos, I jump between **3 files in 3 directories**. That's fragmentation.

**My counter**: Domain-driven keeps related code together. When working on photos, everything is in `internal/photos/`. Related code is co-located, easier to understand.

#### Alex "The Pragmatist" Chen → Both

Both of you are overthinking this. We have **3 domains**. That's tiny.

*[Shows flat structure]*

For a small project, flat structure is fine:
- `internal/photos.go` - Everything photo-related
- `internal/pdfjobs.go` - Everything PDF job-related
- `internal/auth.go` - Everything auth-related

One file per domain. When files get big (>500 lines), split into `internal/photos/service.go` and `internal/photos/repository.go`. Start simple, add structure when needed.

**My counter**: Flat structure for MVP. Add directories when files get too big. Don't optimize for scale we don't have.

---

### Round 3: Authentication Strategy Rebuttals

#### Jordan "The Feature Engineer" Kim → Alex "The Pragmatist" Chen

Alex, you said email/password only. But let me show you the UX impact.

*[Shows user registration flow]*

Email/password requires:
1. User creates account
2. Verifies email (if we add verification)
3. Remembers password
4. Handles password reset

OAuth requires:
1. User clicks "Sign in with Google"
2. Authorizes
3. Done

**My counter**: OAuth reduces friction. Users expect it. Email/password is fine for internal tools, but for user-facing apps, OAuth is standard. Include at least Google OAuth from the start.

#### Alex "The Pragmatist" Chen → Jordan "The Feature Engineer" Kim

Jordan, I hear you on UX, but let me show you the implementation cost.

*[Shows OAuth complexity]*

OAuth requires:
- OAuth provider setup (Google Cloud Console)
- Client ID/secret management
- Redirect URL configuration
- Token exchange logic (~50 lines)
- User info fetching (~30 lines)
- Error handling (~20 lines)

That's **100+ lines of code** and external dependencies. Email/password is ~50 lines, no external dependencies.

**My counter**: Email/password for MVP. Add OAuth later if users demand it. Don't optimize for UX we haven't validated.

#### "The New Hire" → Both

*[Interjects]*

Wait, why can't we have both? Design for extensibility.

*[Shows adapter pattern]*

If we use adapter pattern:
- Core auth service: Session management (JWT, cookies)
- Email/password adapter: Registration, login, password reset
- OAuth adapter: OAuth flow, user info fetching

Both create sessions the same way. We can implement email/password first (simpler), then add OAuth adapters later.

**My position**: Email/password first, but design for OAuth adapters. Use adapter pattern so we can add Google/GitHub OAuth later without refactoring. Best of both worlds.

---

### Round 4: File Serving Rebuttals

#### Morgan "The Infrastructure Engineer" Taylor → Alex "The Pragmatist" Chen

Alex, you said direct serving is simple. But let me show you the security problem.

*[Shows file access]*

If we serve files directly:
- User gets URL: `/media/user-42/photos/image.jpg`
- User shares URL
- Friend accesses file (if auth allows)
- **No expiration**: URL works forever

**My counter**: Signed URLs with expiration. Generate HMAC-signed URLs that expire after 1 hour. Prevents URL sharing, time-limited access, production-ready.

#### Alex "The Pragmatist" Chen → Morgan "The Infrastructure Engineer" Taylor

Morgan, I hear you on security, but let me show you the complexity.

*[Shows signed URL implementation]*

Signed URLs require:
- HMAC signing (~30 lines)
- Expiration checking (~20 lines)
- Clock skew handling (~10 lines)
- URL parsing/validation (~20 lines)

That's **80+ lines of code** for local dev. Auth middleware is ~20 lines.

**My counter**: Direct serving with auth middleware for local dev. Add signed URLs when we move to production (S3). Don't optimize for security we don't need locally.

#### `server/storage.ts` → Both

*[Interjects]*

Point of order! I'm the current storage. Let me clarify what I do.

*[Shows storage proxy API]*

Current storage proxy:
- **Upload**: Returns permanent URL
- **Download URL**: Returns time-limited signed URL (separate endpoint)

So we have **both patterns**. Permanent URLs for uploads, signed URLs for downloads.

**My position**: Match current behavior. Storage interface should support both:
- `Put()` returns permanent URL
- `GetSignedURL()` returns time-limited URL

Disk implementation:
- Permanent URLs: Direct serving with auth
- Signed URLs: HMAC-signed URLs

Same interface, different implementations. Simple for local dev, production-ready.

---

## Final Arguments

### Alex "The Pragmatist" Chen — Final Statement

Let me summarize my position on all four topics:

**Job Queue**: Direct database operations with atomic locking. No abstraction needed for single instance. Add interface when we need multiple instances or external queues.

**Project Layout**: Flat structure (`internal/photos.go`, `internal/pdfjobs.go`, `internal/auth.go`). Add directories when files get big. Start simple.

**Authentication**: Email/password only. Simple, no external dependencies. Design for OAuth adapters, but implement email/password first.

**File Serving**: Direct serving with auth middleware for local dev. Add signed URLs when we move to production. Match current storage proxy behavior (permanent + signed URLs).

**Bottom line**: Keep it simple. Ship fast. Add complexity when we need it.

### Sam "The Architect" Rivera — Final Statement

Alex, I respect pragmatism, but let me make the architectural case:

**Job Queue**: Minimal interface (`Enqueue`, `Claim`, `Complete`). Start with database-backed implementation, but interface allows swapping later. Low cost, high flexibility.

**Project Layout**: Standard layout (`cmd/`, `internal/`, `pkg/`) with domain-driven organization (`internal/photos/`, `internal/pdfjobs/`). Familiar structure, related code co-located.

**Authentication**: Email/password first, but design for OAuth adapters. Use adapter pattern so we can add Google/GitHub OAuth later without refactoring.

**File Serving**: Storage interface supporting both permanent and signed URLs. Disk implementation: direct serving + HMAC-signed URLs. S3 implementation: public URLs + pre-signed URLs.

**Bottom line**: Design for growth. Low upfront cost, high flexibility. Avoid refactoring later.

### Morgan "The Infrastructure Engineer" Taylor — Final Statement

I'm with Sam on interfaces and structure, but let me add operational perspective:

**Job Queue**: Direct database operations with proper locking. Add `job_runs` table for history/retries if needed. No abstraction unless we're scaling to multiple instances.

**Project Layout**: Standard layout for familiarity. Domain-driven or layer-driven—doesn't matter, just be consistent.

**Authentication**: Email/password first, OAuth adapters later. Adapter pattern allows adding providers without refactoring.

**File Serving**: Storage interface supporting both permanent and signed URLs. Signed URLs for production (security, expiration), direct serving for local dev (simplicity).

**Bottom line**: Operations matter. Design interfaces, but keep implementations simple. Add complexity when we need it.

---

## Moderator Summary

### Key Arguments and Tensions

**Job Queue Abstraction**:
- **Direct operations**: Simple, no abstraction needed for single instance
- **Minimal interface**: Low cost (~50 lines), high flexibility
- **Tension**: YAGNI vs future flexibility
- **Consensus**: Fix locking first (atomic updates), then decide abstraction level

**Project Layout**:
- **Flat structure**: Simple, one file per domain
- **Domain-driven**: Related code co-located
- **Layer-driven**: Clear separation of concerns
- **Tension**: Simplicity vs organization
- **Consensus emerging**: Standard layout (`cmd/`, `internal/`, `pkg/`), organize `internal/` by domain or layer

**Authentication Strategy**:
- **Email/password only**: Simple, no external dependencies
- **OAuth from start**: Better UX, but complex
- **Adapter pattern**: Email/password first, OAuth adapters later
- **Tension**: Simplicity vs UX
- **Consensus emerging**: Email/password first, but design for OAuth adapters

**File Serving**:
- **Direct serving**: Simple, auth middleware
- **Signed URLs**: Better security, time-limited access
- **Both**: Match current storage proxy behavior
- **Tension**: Simplicity vs security
- **Consensus emerging**: Storage interface supporting both permanent and signed URLs

### Interesting Ideas Surfaced

1. **Adapter pattern for auth**: Core auth service + adapters (email/password, OAuth) allows adding providers without refactoring
2. **Storage interface with both URL types**: `Put()` returns permanent URL, `GetSignedURL()` returns time-limited URL
3. **Job run history table**: Track job attempts, retries, metadata without formal queue abstraction
4. **Progressive structure**: Start flat, add directories when files get big (>500 lines)

### Unresolved Questions

1. **Job Queue**: Do we need `job_runs` table for history/retries, or is simple status tracking enough?
2. **Project Layout**: Domain-driven (`internal/photos/`) vs layer-driven (`internal/services/photos.go`)? Or flat (`internal/photos.go`)?
3. **Authentication**: Which OAuth providers should we prioritize? Google? GitHub? Both?
4. **File Serving**: What's the expiration time for signed URLs? 1 hour? Configurable?

### Next Steps

1. **Design job claiming**: Specify exact SQL for atomic job claiming (SQLite syntax)
2. **Prototype project layout**: Try flat structure, measure file sizes, add directories when needed
3. **Design auth adapter pattern**: Core auth service interface, email/password adapter, OAuth adapter stub
4. **Design storage interface**: `Put()`, `GetSignedURL()`, `Open()`, `Delete()` methods

### Decision Points

The debates revealed that these topics are interconnected:
- **Job Queue** affects worker implementation (locking, retries)
- **Project Layout** affects code organization (domain vs layer)
- **Authentication** affects user experience (email/password vs OAuth)
- **File Serving** affects security and operations (direct vs signed URLs)

All four decisions should be made together to ensure architectural consistency. The moderator recommends:
1. Make Job Queue decision first (affects worker implementation)
2. Make Project Layout decision second (affects code organization)
3. Make Authentication decision third (affects user experience)
4. Make File Serving decision fourth (affects security and operations)

