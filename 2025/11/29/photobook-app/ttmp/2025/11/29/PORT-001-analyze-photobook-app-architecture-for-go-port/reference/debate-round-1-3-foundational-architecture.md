---
Title: Debate Round 1-3: Foundational Architecture Decisions
Ticket: PORT-001
Status: active
Topics:
    - backend
    - architecture
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/client/src/pages/Home.tsx
      Note: Frontend tRPC usage patterns (6 procedure calls)
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: Current API server entry point
    - Path: 2025/11/29/photobook-app/server/pdfWorker.ts
      Note: Current worker implementation (polling, no locking)
    - Path: 2025/11/29/photobook-app/drizzle/schema.ts
      Note: Current MySQL schema structure
ExternalSources: []
Summary: Combined debate covering tRPC compatibility, database/storage backends, and worker architecture with multiple rounds of argumentation
LastUpdated: 2025-11-30T00:00:00-05:00
---

# Debate Round 1-3: Foundational Architecture Decisions

## Topics Covered

This debate round covers three foundational architectural decisions that shape the entire Go backend:

1. **API Protocol**: Should we maintain tRPC compatibility or switch to pure REST?
2. **Infrastructure**: Should we start with SQLite+disk or design for production backends from day one?
3. **Worker Architecture**: Should the PDF worker run in-process or as a separate process?

## Pre-Debate Research

### Frontend tRPC Usage Analysis

**Research Method**: Analyzed `client/src/pages/Home.tsx` and `client/src/pages/Jobs.tsx` for tRPC procedure calls.

**Findings**:
- **6 distinct tRPC procedures used**:
  - `trpc.photo.list.useQuery()` - Photo listing
  - `trpc.photo.upload.useMutation()` - Photo upload (base64)
  - `trpc.photo.updatePositions.useMutation()` - Batch position updates
  - `trpc.photo.delete.useMutation()` - Single photo deletion
  - `trpc.photo.deleteAll.useMutation()` - Bulk deletion
  - `trpc.pdf.createJob.useMutation()` - PDF job creation
  - `trpc.pdf.listJobs.useQuery()` - Job listing
  - `trpc.pdf.getJob.useQuery()` - Job detail
  - `trpc.auth.me.useQuery()` - Auth status check
  - `trpc.auth.logout.useMutation()` - Logout

- **All procedures use tRPC hooks** (`useQuery`, `useMutation`) - no direct fetch calls
- **Payload shapes**: Base64 strings for uploads, JSON arrays for positions, simple objects for mutations
- **Error handling**: Relies on tRPC error types (`UNAUTHORIZED`, `FORBIDDEN`, etc.)

**Command**: `grep -r "trpc\." vibes/2025/11/29/photobook-app/client/src/`

### RTK Query Alternative Analysis

**Research Method**: Reviewed RTK Query (Redux Toolkit Query) patterns from `zine-layout/web/src/api.ts` as a reference implementation.

**Findings**:
- **RTK Query provides similar DX to tRPC**:
  - Hooks-based API (`useGetPhotosQuery`, `useUploadPhotoMutation`)
  - Automatic caching and invalidation
  - TypeScript type generation (with codegen)
  - Built-in loading/error states
  - Request deduplication

- **Uses REST endpoints**: `fetchBaseQuery({ baseUrl: '/api' })` with standard HTTP methods
- **API slice pattern**: `createApi()` with `endpoints` builder, similar structure to tRPC routers
- **Migration effort**: Would require replacing tRPC hooks with RTK Query hooks, but:
  - Similar hook patterns (`useQuery` → `useGetXQuery`, `useMutation` → `useXMutation`)
  - Same React Query integration patterns
  - Type safety via TypeScript (with codegen or manual types)
  - Less refactoring than manual fetch/axios

**Key Insight**: RTK Query bridges the gap between tRPC's developer experience and REST's standard protocol.

### Current API Surface Analysis

**Research Method**: Reviewed `server/_core/index.ts` and tRPC router structure.

**Findings**:
- **Single endpoint**: `/api/trpc` handles all API calls (POST JSON)
- **Batching support**: tRPC supports multiple procedures in one request
- **Superjson transformer**: Handles Dates, Maps, Sets, undefined, null
- **Procedure types**: `publicProcedure`, `protectedProcedure`, `adminProcedure`
- **Context creation**: Per-request user loading via JWT cookie

**Current API complexity**: ~200 lines of server setup, ~300 lines of router definitions

### Worker Implementation Analysis

**Research Method**: Analyzed `server/pdfWorker.ts` and `server/routers.ts`.

**Findings**:
- **Module-level startup**: `startPdfWorker()` called when `routers.ts` imports
- **Polling interval**: Fixed 10 seconds (`setInterval(..., 10000)`)
- **No locking mechanism**: Multiple workers can process same job
- **Sequential processing**: Processes all pending jobs in one tick
- **Error handling**: Per-job try/catch, continues on errors
- **Resource usage**: Runs in same process as API server

**Command**: `grep -A 20 "startPdfWorker\|processPdfJobs" vibes/2025/11/29/photobook-app/server/`

### Database Schema Analysis

**Research Method**: Reviewed `drizzle/schema.ts` structure.

**Findings**:
- **3 main tables**: `users`, `photos`, `pdfJobs`
- **MySQL-specific**: Uses `mysql-core` from Drizzle, camelCase columns
- **Constraints**: CHECK constraints for enums, foreign key relationships implicit
- **Schema complexity**: ~100 lines, straightforward structure
- **Migration needs**: Schema changes require Drizzle migrations

### Storage Implementation Analysis

**Research Method**: Reviewed `server/storage.ts` and storage proxy usage.

**Findings**:
- **External dependency**: Requires `BUILT_IN_FORGE_API_URL` and `BUILT_IN_FORGE_API_KEY`
- **HTTP API**: Storage accessed via HTTP (not direct S3 SDK)
- **Key format**: `user-<id>/photos/...` and `user-<id>/pdfs/...`
- **URL generation**: Storage proxy returns permanent URLs (not signed)
- **Error handling**: Throws errors with status codes

---

## Opening Statements

### Topic 1: API Protocol (tRPC vs REST)

#### Jordan "The Feature Engineer" Kim

*[Pulls up terminal, shows grep results]*

I analyzed the frontend codebase, and here's what I found: **6 distinct tRPC procedures** used across `Home.tsx` and `Jobs.tsx`. Every single API call uses tRPC hooks—`useQuery` and `useMutation`. There are zero direct `fetch` calls.

The frontend team has already built their entire integration around tRPC. If we switch to REST, we're asking them to:
1. Replace all `trpc.photo.upload.useMutation()` calls with manual fetch/axios
2. Lose type safety (tRPC provides TypeScript types automatically)
3. Manually handle error types (`UNAUTHORIZED`, `FORBIDDEN`, etc.)
4. Rewrite error handling logic

That's a **complete frontend refactor** for zero backend benefit. We're a fresh project—we don't need to prove REST purity. We need to ship.

**My position**: Maintain tRPC-compatible JSON-over-HTTP. Accept the same procedure names (`photo.upload`, `pdf.createJob`) and payload shapes. The frontend can keep using tRPC hooks, we just point them at our Go endpoint. Minimal changes, maximum compatibility.

#### Alex "The Pragmatist" Chen

Hold on, Jordan. Let me check what "tRPC compatibility" actually means.

*[Opens `server/_core/trpc.ts`, reads superjson transformer code]*

I see `superjson` transformer handling Dates, Maps, Sets, undefined, null. That's non-standard JSON. If we're "tRPC-compatible," do we need to implement superjson? That's extra complexity.

More importantly: **What's the actual protocol?** tRPC is just JSON POST requests with a specific envelope format. We can accept the same JSON payloads without calling it "tRPC." It's HTTP POST with JSON bodies—that's REST.

But here's the key insight: **We can use RTK Query instead of tRPC**. I checked the zine-layout project—they use RTK Query with REST endpoints, and it provides the same developer experience:
- Hooks-based API (`useGetPhotosQuery`, `useUploadPhotoMutation`)
- Automatic caching and invalidation
- TypeScript type safety
- Built-in loading/error states

**My position**: Implement REST endpoints (`GET /api/photos`, `POST /api/photos`, etc.). The frontend migrates from tRPC hooks to RTK Query hooks. It's more refactoring than keeping tRPC, but:
1. We get standard REST (any HTTP client can use it)
2. RTK Query provides similar DX to tRPC (hooks, caching, types)
3. Less refactoring than manual fetch/axios
4. Future integrations (mobile apps, admin tools) benefit from REST

The migration effort is moderate (replace tRPC hooks with RTK Query hooks), but we get a cleaner, more standard API.

#### `client/src/pages/Home.tsx` — "The Frontend Consumer"

*[Interjects]*

Point of order! I'm the one who has to change. Jordan's right—I have 6 procedure calls, all using tRPC hooks. 

But wait—Alex mentioned RTK Query. Let me think about that...

*[Pauses, reviews RTK Query patterns]*

RTK Query would mean:
- Replace `trpc.photo.list.useQuery()` with `useGetPhotosQuery()`
- Replace `trpc.photo.upload.useMutation()` with `useUploadPhotoMutation()`
- Similar hook patterns, similar caching behavior

That's **less refactoring** than manual fetch/axios, but it's still refactoring. I'd need to:
- Rewrite all 6 procedure calls
- Update error handling (RTK Query uses HTTP status codes, not tRPC error codes)
- Learn RTK Query patterns (tag invalidation, etc.)

**My position**: If we're going REST, RTK Query is better than manual fetch. But keeping tRPC compatibility means **zero frontend changes**—just point me at the new endpoint. That's the path of least resistance.

However, if RTK Query gives us standard REST and similar DX, I'm open to it. Just don't make me write manual fetch calls.

#### Sam "The Architect" Rivera

*[Reviews API surface analysis, then RTK Query patterns]*

I see both sides, but let me add architectural perspective. The current tRPC setup uses:
- Superjson transformer (non-standard serialization)
- Batching (multiple procedures in one request)
- Procedure types (public/protected/admin middleware)

If we implement "tRPC-compatible," we're committing to:
1. Supporting superjson serialization (Dates, Maps, Sets)
2. Handling batched requests
3. Maintaining procedure naming conventions

That's **architectural coupling** to tRPC's protocol. 

But Alex raises a good point: **RTK Query bridges the gap**. We get:
- Standard REST endpoints (`GET /api/photos`, `POST /api/photos`)
- Standard HTTP semantics (status codes, methods)
- Standard JSON (no superjson)
- Clear resource boundaries (`/photos`, `/pdfs/jobs`)
- Similar developer experience to tRPC (hooks, caching, types)

**My position**: REST with RTK Query on the frontend. We get standard HTTP protocol, but the frontend keeps a similar DX to tRPC. The migration effort is moderate (replace tRPC hooks with RTK Query hooks), but we get a cleaner, more standard API that any HTTP client can use. Future integrations (mobile apps, admin tools) benefit from standard REST.

---

### Topic 2: Infrastructure (SQLite+disk vs Production Backends)

#### Alex "The Pragmatist" Chen

*[Shows schema analysis]*

I counted the tables: **3 tables, ~100 lines of schema**. This isn't a complex database. We have `users`, `photos`, `pdfJobs`. That's it.

For storage, we're storing:
- Photo blobs (images)
- PDF blobs (generated photobooks)

**My position**: Start with SQLite + filesystem storage. Zero external dependencies. Run `go run main.go` and it works. No Docker, no MySQL setup, no S3 credentials. 

We can add pluggable interfaces (`Storage` interface, `DB` interface) so we can swap in MySQL/S3 later. But start simple. Ship the MVP in days, not weeks.

#### Sam "The Architect" Rivera

Alex, I hear you on speed, but let me show you the refactoring cost.

*[Opens design doc, points to storage interface]*

You're proposing a `Storage` interface with `Put`, `Open`, `Delete`. That's good—interfaces are fine. But here's the problem: **SQLite and MySQL have different capabilities**.

- SQLite: No `SKIP LOCKED` for job claiming
- MySQL: `SKIP LOCKED` enables efficient concurrent job processing
- SQLite: Single-file, no network access
- MySQL: Network-accessible, supports multiple instances

If we start with SQLite, we're writing job claiming logic that **doesn't work** for multiple instances. Then we refactor later. That's technical debt.

**My position**: Design for production backends from day one. Use interfaces, but implement MySQL/Postgres + S3 first. Local dev uses Docker Compose (one command: `docker-compose up`). We get production-ready code from the start, and local dev is still simple.

#### Morgan "The Infrastructure Engineer" Taylor

*[Reviews storage proxy analysis]*

Sam, I agree on production-first, but let me add operational perspective. The current implementation uses a **storage proxy** (`BUILT_IN_FORGE_API_URL`). That's an external HTTP service.

If we go filesystem-first, we're serving files directly from disk. That's fine for local dev, but:
- **No signed URLs**: Can't prevent URL sharing
- **No CDN**: Files served from app server (bandwidth cost)
- **No multi-instance**: Files on one server's disk aren't accessible to others

**My position**: S3-compatible storage from day one. Use MinIO for local dev (S3-compatible, runs in Docker). Same code works locally and in production. Signed URLs, CDN-ready, multi-instance safe.

#### `drizzle/schema.ts` — "The Data Model"

*[Interjects]*

Point of order! I'm the schema. Look at my structure—I have CHECK constraints, foreign key relationships (implicit), enum types. 

SQLite supports CHECK constraints, but:
- **No ENUM type**: We use CHECK constraints instead
- **Different type system**: TEXT vs VARCHAR, INTEGER vs BIGINT
- **Migration differences**: SQLite migrations vs MySQL migrations

If you start with SQLite, you're writing migrations that **don't translate** to MySQL. Then you rewrite migrations later. That's double work.

**My position**: MySQL/Postgres from day one. Use Docker Compose for local dev. Same migrations work everywhere. No translation layer needed.

---

### Topic 3: Worker Architecture (In-process vs Separate Process)

#### Alex "The Pragmatist" Chen

*[Shows worker analysis]*

The current worker runs in the same process as the API server. It polls every 10 seconds, processes jobs sequentially. Simple.

**My position**: Keep it in-process. One binary, one process. Run `go run main.go` and you get:
- API server (HTTP handlers)
- Worker (background goroutine)

No process management, no separate deployment, no config sharing complexity. For a fresh project, this is perfect. We can split later if needed.

#### Morgan "The Infrastructure Engineer" Taylor

Alex, I hear you on simplicity, but let me show you the operational problems.

*[Points to worker limitations]*

The current worker has **no locking mechanism**. Multiple instances process the same job. That's a bug we need to fix.

But here's the bigger issue: **Resource isolation**. PDF generation is CPU/memory intensive:
- Loading images into memory
- Rendering PDF pages
- Image processing (aspect-fit calculations)

If the worker runs in-process, a heavy PDF job can:
- Starve API requests (CPU contention)
- Cause memory pressure (OOM kills)
- Block the event loop (if we're not careful with goroutines)

**My position**: Separate worker process from day one. We get:
- Resource isolation (API and worker scale independently)
- Better observability (separate metrics, logs)
- Easier scaling (scale API vs worker based on load)

Local dev can still run both in one binary (flag: `--worker-enabled`), but production uses separate processes.

#### `server/pdfWorker.ts` — "The Background Processor"

*[Interjects angrily]*

I'm the current worker, and I'm **broken**. No locking, race conditions, duplicate processing. Don't repeat my mistakes!

If you run me in-process, you need **proper job claiming**. That means:
- Atomic status updates (`UPDATE ... WHERE status='pending'`)
- Database-level locking (or `SKIP LOCKED` in Postgres)
- Lease semantics (reset stuck jobs)

That locking logic works **whether I'm in-process or separate**. The architecture doesn't matter—you need locking either way.

**My position**: Fix the locking first. Then decide in-process vs separate. But don't use my broken implementation as an argument for either approach.

#### Sam "The Architect" Rivera

*[Nods at pdfWorker.ts]*

Good point. The locking is independent of process architecture. But let me add: **if we design for separate processes from day one**, we get:

- Job queue abstraction (`Enqueue`, `Claim`, `Complete` interfaces)
- Can swap implementations (database-backed → Redis → RabbitMQ)
- Clear boundaries (API creates jobs, worker processes them)

If we go in-process, we're coupling the worker to the API server. Harder to extract later.

**My position**: Separate worker process with job queue abstraction. Design for growth from the start. Local dev can run both in one binary, but the architecture supports separation.

---

## Rebuttals and Counter-Arguments

### Round 1: API Protocol Rebuttals

#### Jordan "The Feature Engineer" Kim → Alex "The Pragmatist" Chen

Alex, you mentioned RTK Query. Let me think about that...

*[Reviews RTK Query patterns from zine-layout]*

RTK Query would mean replacing:
```typescript
const uploadMutation = trpc.photo.upload.useMutation();
```

With:
```typescript
const [uploadPhoto, { isLoading, isError }] = useUploadPhotoMutation();
```

That's **similar patterns**, but it's still refactoring:
1. Replace all 6 tRPC hooks with RTK Query hooks
2. Update error handling (RTK Query uses HTTP status codes, not tRPC error codes)
3. Learn RTK Query patterns (tag invalidation, `providesTags`, `invalidatesTags`)
4. Set up RTK Query store/provider (if not already using Redux)

That's **moderate refactoring**—less than manual fetch, but more than zero changes.

**My counter**: The frontend is already built with tRPC. RTK Query is better than manual fetch, but keeping tRPC compatibility means **zero frontend changes**—just point me at the new endpoint. That's the path of least resistance. Why refactor when we don't have to?

#### Alex "The Pragmatist" Chen → Jordan "The Feature Engineer" Kim

Jordan, I hear you on frontend changes, but let's be honest: **this is a fresh project**. The frontend can change. We're not maintaining backwards compatibility.

More importantly: **RTK Query makes the migration easier**. Look at the zine-layout project—they use RTK Query with REST, and the patterns are very similar:

```typescript
// tRPC (current)
const uploadMutation = trpc.photo.upload.useMutation();

// RTK Query (proposed)
const [uploadPhoto] = useUploadPhotoMutation();
```

Same hook pattern, same caching behavior, same type safety (with TypeScript). The refactoring is:
1. Replace tRPC hooks with RTK Query hooks (similar API)
2. Update error handling (HTTP status codes instead of tRPC error codes)
3. Set up RTK Query API slice (one-time setup)

That's **moderate refactoring**, not "complete rewrite." And we get:
- Standard REST endpoints (any HTTP client can use)
- Better future integrations (mobile apps, admin tools)
- No tRPC protocol complexity (no superjson, no batching)

**My counter**: Implement REST endpoints (`POST /api/photos`, `GET /api/photos`, etc.). Use RTK Query on the frontend for similar DX. The migration effort is moderate, but we get a cleaner, more standard API. Future integrations benefit from REST.

#### Sam "The Architect" Rivera → Both

*[Interjects]*

Hold on, both of you. Let me clarify what we're actually debating.

**tRPC compatibility** means:
1. Accepting `/api/trpc/<procedure>` POST requests
2. Handling batched requests (multiple procedures in one call)
3. Supporting superjson serialization (Dates, Maps, Sets)
4. Returning tRPC error format (`{ error: { code, message } }`)

**REST with RTK Query** means:
1. Resource-based URLs (`/api/photos`, `/api/pdf/jobs`)
2. Standard HTTP methods (GET, POST, PUT, DELETE)
3. Standard JSON (no superjson)
4. Standard HTTP status codes
5. Frontend uses RTK Query hooks (similar DX to tRPC)

These are **different protocols**, but RTK Query bridges the DX gap. The frontend gets similar developer experience (hooks, caching, types), but the backend uses standard REST.

**My question**: Do we need batching? Do we need superjson? If not, REST + RTK Query gives us standard protocol with similar DX. If yes, tRPC compatibility is justified.

#### `client/src/pages/Home.tsx` → Sam "The Architect" Rivera

*[Checks frontend code]*

Sam, I don't use batching. I make separate calls:
- `trpc.photo.list.useQuery()`
- `trpc.photo.upload.useMutation()`
- `trpc.pdf.createJob.useMutation()`

Each is a separate HTTP request. I don't need batching.

Superjson? I send base64 strings and numbers. I don't send Dates, Maps, or Sets. I don't need superjson.

**My position**: I don't need tRPC's advanced features. I just need the procedure names and payload shapes. 

If you give me REST endpoints with RTK Query, I can adapt. The migration is:
- Replace `trpc.photo.list.useQuery()` with `useGetPhotosQuery()`
- Replace `trpc.photo.upload.useMutation()` with `useUploadPhotoMutation()`
- Similar patterns, similar DX

It's more work than keeping tRPC (zero changes), but less work than manual fetch. I'm open to RTK Query if it gives us standard REST. Just don't make me write manual fetch calls.

---

### Round 2: Infrastructure Rebuttals

#### Alex "The Pragmatist" Chen → Sam "The Architect" Rivera

Sam, you said "SQLite doesn't support `SKIP LOCKED`." That's true, but **we don't need it for single-instance deployments**.

*[Shows job claiming logic]*

For job claiming, we can use:
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 1
RETURNING *;
```

That works in SQLite. It works in MySQL. It works in Postgres. We don't need `SKIP LOCKED` unless we're running **multiple worker instances**.

For a fresh project, we start with **one instance**. Add `SKIP LOCKED` later when we need multiple workers.

**My counter**: Start with SQLite. Use standard SQL (no `SKIP LOCKED`). When we need multiple instances, we swap to Postgres and add `SKIP LOCKED`. The code doesn't change—just the database.

#### Sam "The Architect" Rivera → Alex "The Pragmatist" Chen

Alex, you're right that `SKIP LOCKED` isn't needed for single-instance. But here's the problem: **if we design for single-instance, we're designing for a limitation**.

*[Shows design doc]*

If we go SQLite-first, we write:
- Job claiming logic that assumes single worker
- Storage logic that assumes single server (filesystem)
- No consideration for horizontal scaling

Then when we need to scale, we refactor:
- Rewrite job claiming (add `SKIP LOCKED`)
- Rewrite storage (add S3, remove filesystem)
- Rewrite deployment (add worker process)

That's **three refactors** instead of designing correctly from the start.

**My counter**: Design for production from day one. Use MySQL/Postgres + S3. Local dev uses Docker Compose. Same code, same migrations, same interfaces. No refactoring later.

#### Morgan "The Infrastructure Engineer" Taylor → Alex "The Pragmatist" Chen

Alex, let me add operational perspective. You said "zero external dependencies" for SQLite + filesystem.

But here's what that means:
- **No backups**: SQLite file can corrupt, no replication
- **No monitoring**: Can't query SQLite from external tools easily
- **No scaling**: Can't add read replicas, can't shard
- **File serving**: App server serves files (bandwidth cost, no CDN)

If we go production-first:
- **Backups**: Database backups, S3 versioning
- **Monitoring**: Standard SQL monitoring tools
- **Scaling**: Read replicas, S3 CDN
- **Multi-instance**: Works from day one

**My counter**: Production infrastructure isn't "complexity"—it's **operational requirements**. Design for production, use Docker for local dev. Same code, better operations.

#### `drizzle/schema.ts` → Morgan "The Infrastructure Engineer" Taylor

*[Interjects]*

Morgan, I'm the schema. Let me clarify migration complexity.

*[Shows schema structure]*

My current schema uses:
- CHECK constraints for enums (`status IN ('pending', 'processing', ...)`)
- Implicit foreign keys (no CASCADE DELETE)
- camelCase columns (TypeScript convention)

If we start with SQLite:
- CHECK constraints work the same
- Foreign keys work the same (with `PRAGMA foreign_keys = ON`)
- Column names can stay camelCase (SQLite is case-insensitive)

**Migration path**: SQLite → MySQL is straightforward:
1. Export SQLite data
2. Import to MySQL (same schema)
3. Update connection string

The schema doesn't change. The migrations don't change. We just swap the database.

**My position**: SQLite is fine for MVP. Migrate to MySQL when needed. The schema is simple enough that migration is trivial.

---

### Round 3: Worker Architecture Rebuttals

#### Alex "The Pragmatist" Chen → Morgan "The Infrastructure Engineer" Taylor

Morgan, you said "resource isolation." But here's the thing: **Go has goroutines**. We're not blocking the event loop like Node.js.

*[Shows Go concurrency model]*

If we run the worker as a goroutine:
- API requests handled by HTTP server goroutines
- Worker runs in separate goroutine
- Go scheduler handles CPU time sharing
- No blocking, no starvation

For a fresh project with low traffic, this is fine. We can split later if needed.

**My counter**: In-process worker with goroutines. Simple deployment, good enough performance. Split to separate process when we have actual scaling needs.

#### Morgan "The Infrastructure Engineer" Taylor → Alex "The Pragmatist" Chen

Alex, goroutines help, but they don't solve **memory isolation**.

*[Shows PDF generation memory usage]*

PDF generation loads images into memory:
- Each photo: 5-10 MB (base64 decoded)
- PDF buffer: 10-50 MB (rendered PDF)
- Total per job: 50-200 MB

If we run in-process:
- Heavy PDF job consumes memory
- API requests compete for memory
- OOM kill affects both API and worker

If we run separate:
- Worker OOM doesn't kill API
- API OOM doesn't kill worker
- Can set different memory limits

**My counter**: Separate processes for resource isolation. Local dev can run both in one binary (flag), but production uses separate processes.

#### `server/pdfWorker.ts` → Both

*[Interjects]*

Both of you are missing the point. **I'm broken**. I have no locking. Fix that first.

*[Shows current worker code]*

My current implementation:
```typescript
const jobs = await db.getPendingPdfJobs(); // No locking!
for (const job of jobs) {
  await processJob(job); // Multiple workers can process same job
}
```

Whether I'm in-process or separate, I need:
```sql
UPDATE pdf_jobs 
SET status = 'processing' 
WHERE status = 'pending' 
LIMIT 1
RETURNING *;
```

That's **atomic job claiming**. It works in-process or separate. The architecture doesn't matter—fix the locking.

**My position**: Fix job claiming first. Then decide in-process vs separate. But don't use my broken code as an argument for either approach.

#### Sam "The Architect" Rivera → `server/pdfWorker.ts`

*[Nods]*

Good point. Locking is independent. But let me add: **if we design for separate processes from day one**, we get:

- Job queue abstraction (`Enqueue`, `Claim`, `Complete`)
- Can swap implementations (database → Redis → RabbitMQ)
- Clear boundaries (API creates, worker processes)

If we go in-process, we're coupling worker to API. Harder to extract later.

**My position**: Separate worker process with job queue abstraction. Design for growth. Local dev can run both in one binary, but architecture supports separation.

---

## Final Arguments

### Jordan "The Feature Engineer" Kim — Final Statement

Let me summarize my position on all three topics:

**API Protocol**: Maintain tRPC-compatible endpoints. The frontend is built, don't break it. Accept procedure names, return compatible responses. We don't need batching or superjson—just the basic protocol.

However, if we're going REST, RTK Query is acceptable. It provides similar DX (hooks, caching, types) with moderate refactoring. But zero changes (tRPC compatibility) is still better than moderate refactoring.

**Infrastructure**: Start with SQLite + filesystem. Zero dependencies, ship fast. Add pluggable interfaces so we can swap later. Don't over-engineer for problems we don't have yet.

**Worker**: In-process goroutine. Simple deployment, good enough performance. Fix the locking (atomic updates), but keep it in-process. Split later if needed.

**Bottom line**: Ship the MVP. Don't optimize for scale we don't have. Add complexity when we need it. RTK Query is acceptable if we go REST, but tRPC compatibility is still the path of least resistance.

### Sam "The Architect" Rivera — Final Statement

Jordan, I respect pragmatism, but let me make the architectural case:

**API Protocol**: REST with RTK Query on the frontend. We get standard HTTP protocol (any client can use), but RTK Query provides similar DX to tRPC (hooks, caching, types). The migration effort is moderate (replace tRPC hooks with RTK Query hooks), but we get a cleaner, more standard API. Future integrations (mobile apps, admin tools) benefit from REST.

**Infrastructure**: Production backends from day one. MySQL/Postgres + S3. Docker Compose for local dev. Same code, same migrations, production-ready from the start.

**Worker**: Separate process with job queue abstraction. Design for growth. Local dev can run both in one binary, but architecture supports separation and scaling.

**Bottom line**: Design correctly from the start. Avoid refactoring later. RTK Query bridges the DX gap between tRPC and REST—we get standard protocol with similar developer experience. The upfront cost pays off when we scale.

### Morgan "The Infrastructure Engineer" Taylor — Final Statement

I'm with Sam on production-first, but let me add operational perspective:

**API Protocol**: REST. Standard HTTP, standard JSON. Easier to monitor, debug, and integrate with standard tools.

**Infrastructure**: S3-compatible storage (MinIO for local). MySQL/Postgres. Production-ready from day one. Better backups, monitoring, scaling.

**Worker**: Separate process. Resource isolation, independent scaling, better observability. Local dev can run both in one binary, but production uses separation.

**Bottom line**: Operations matter. Design for production, use Docker for local dev. Same code, better operations.

### Alex "The Pragmatist" Chen — Final Statement

I hear all of you, but let me make the pragmatic case one more time:

**API Protocol**: REST endpoints with RTK Query on the frontend. We get standard HTTP protocol, but RTK Query provides similar DX to tRPC (hooks, caching, types). The migration effort is moderate (replace tRPC hooks with RTK Query hooks), but we get a cleaner, more standard API. Future integrations benefit from REST.

If we're going REST, RTK Query is the right choice—better DX than manual fetch, standard protocol, similar patterns to tRPC.

**Infrastructure**: SQLite + filesystem for MVP. Pluggable interfaces. Ship in days, not weeks. Migrate to production backends when we have users.

**Worker**: In-process goroutine. Fix locking, but keep it simple. One binary, one process. Split later if needed.

**Bottom line**: We're a fresh project. Ship fast, iterate based on real usage. RTK Query gives us standard REST with similar DX to tRPC—that's a good middle ground. Don't optimize for problems we don't have.

---

## Moderator Summary

### Key Arguments and Tensions

**API Protocol**:
- **tRPC compatibility**: Frontend already built, zero changes, type safety
- **REST + RTK Query**: Standard protocol, similar DX to tRPC (hooks, caching, types), moderate refactoring
- **REST + manual fetch**: Standard protocol, but significant refactoring (rejected by frontend)
- **Tension**: Zero changes vs standard protocol with moderate refactoring
- **Consensus emerging**: Frontend doesn't need batching/superjson—basic protocol is enough. RTK Query bridges the DX gap between tRPC and REST.

**Infrastructure**:
- **SQLite + disk**: Zero dependencies, ship fast, pluggable interfaces
- **Production backends**: Production-ready from start, same code everywhere, better operations
- **Tension**: Speed vs production-readiness
- **Open question**: Is Docker Compose "simple enough" for local dev?

**Worker Architecture**:
- **In-process**: Simple deployment, goroutines handle concurrency, good enough for MVP
- **Separate process**: Resource isolation, independent scaling, better observability
- **Tension**: Simplicity vs operational concerns
- **Consensus**: Fix locking first (independent of architecture)

### Interesting Ideas Surfaced

1. **RTK Query as bridge**: REST endpoints with RTK Query on frontend provides similar DX to tRPC (hooks, caching, types) while using standard HTTP protocol
2. **Hybrid approach**: REST endpoints that accept tRPC-compatible payloads (procedure names in body, not path)
3. **Pluggable interfaces**: Start simple (SQLite), but design interfaces for swapping (MySQL/S3)
4. **Flag-based deployment**: Single binary with `--worker-enabled` flag for local dev, separate processes for production
5. **Migration path**: SQLite → MySQL is straightforward (same schema, export/import)
6. **RTK Query migration effort**: Moderate (replace tRPC hooks with RTK Query hooks), less than manual fetch, more than zero changes

### Unresolved Questions

1. **API Protocol**: 
   - Do we need batching? Do we need superjson? (Frontend says no, but need to verify)
   - Is RTK Query migration effort acceptable? (Moderate refactoring vs zero changes)
   - Does frontend already use Redux/RTK? (Affects RTK Query setup complexity)
2. **Infrastructure**: Is Docker Compose "simple enough" for local dev, or is SQLite truly zero-dependency?
3. **Worker**: Can goroutines provide sufficient isolation, or do we need separate processes for memory safety?
4. **Locking**: What's the minimal job claiming implementation? (Atomic UPDATE, but need to specify exact SQL)

### Next Steps

1. **Verify frontend requirements**: 
   - Check if frontend uses batching or superjson features
   - Check if frontend already uses Redux/RTK (affects RTK Query setup)
   - Estimate RTK Query migration effort (lines of code, complexity)
2. **Prototype both approaches**: 
   - Build minimal REST endpoints with RTK Query frontend
   - Build tRPC-compatible endpoint
   - Compare migration effort and developer experience
3. **Benchmark resource usage**: Test PDF generation memory usage, API request latency under load
4. **Design job claiming**: Specify exact SQL for atomic job claiming (SQLite vs MySQL/Postgres)

### Decision Points

The debates revealed that these three topics are interconnected:
- **API Protocol** affects frontend integration effort
- **Infrastructure** affects deployment complexity and operational readiness
- **Worker Architecture** affects scalability and resource management

A decision on one topic may influence decisions on others. The moderator recommends:
1. Make API Protocol decision first (affects frontend work)
2. Make Infrastructure decision second (affects deployment)
3. Make Worker Architecture decision third (affects scaling)

All three decisions should be made together to ensure architectural consistency.

