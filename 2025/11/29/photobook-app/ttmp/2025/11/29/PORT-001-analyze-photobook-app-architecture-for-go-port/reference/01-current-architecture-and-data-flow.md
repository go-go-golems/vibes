---
Title: Current architecture and data flow
Ticket: PORT-001
Status: active
Topics:
    - backend
    - frontend
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/client/src/pages/Home.tsx
      Note: Frontend photobook flow local only no API calls
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: API server entry Express+tRPC OAuth callback Vite-static serving
    - Path: 2025/11/29/photobook-app/server/db.ts
      Note: Drizzle MySQL data access for users photos pdfJobs
    - Path: 2025/11/29/photobook-app/server/index.ts
      Note: Static-only server used by prod build lacking APIs
    - Path: 2025/11/29/photobook-app/server/pdfRouter.ts
      Note: PDF job CRUD surface for user
    - Path: 2025/11/29/photobook-app/server/pdfWorker.ts
      Note: Background processor polling pending jobs every 10s
    - Path: 2025/11/29/photobook-app/server/photoRouter.ts
      Note: Photo upload list reorder delete API using storage proxy
    - Path: 2025/11/29/photobook-app/server/routers.ts
      Note: tRPC root router bootstrap and PDF worker start
    - Path: 2025/11/29/photobook-app/server/storage.ts
      Note: Forge storage proxy upload download helper
ExternalSources: []
Summary: Deep-dive map of the current Node/Express + tRPC stack, data paths, contracts, and frontend behavior to prep the Go port
LastUpdated: 2025-11-29T20:55:00-05:00
---

# Current architecture and data flow

## Goal

Summarize, in depth, how the photobook app works today (server, frontend, data stores, auth, background jobs) so we can reimplement the backend in Go without losing behaviors or contracts. This is written for a new intern to come up to speed quickly.

## Big-picture orientation (for a new intern)

You’re looking at a “photobook” product: users upload images, reorder them, and request a PDF. The codebase is split into a React client and a Node/Express API that speaks tRPC. It persists users/photos/pdfJobs in MySQL via Drizzle and stores blobs through a Forge storage proxy (S3-like). OAuth with Manus issues a signed session cookie so tRPC procedures know who you are. A background worker polls pending PDF jobs, renders a PDF with jsPDF/canvas, uploads it to storage, and marks the job completed.

Two important wrinkles:
1) The real API server is `server/_core/index.ts` (OAuth + `/api/trpc` + Vite dev proxy).
2) The production build/start pipeline only bundles `server/index.ts`, which serves static assets and **omits the API**. We need to fix that integration before (or while) porting to Go.

## How to run it today

- Install: `pnpm install`
- Dev (client): `pnpm dev` (Vite only). To run the API, start another terminal and run `tsx server/_core/index.ts` (no script exists yet).
- Build: `pnpm build` → emits client to `dist/public` and bundles `server/index.ts` to `dist/index.js` (**static-only server**).
- Start (current prod shape): `pnpm start` → `node dist/index.js` (serves static HTML/JS, no APIs).

## Minimum environment variables

| Scope | Key | Purpose |
| --- | --- | --- |
| Client | `VITE_APP_ID` | Manus app id; must match server expectations |
| Client | `VITE_OAUTH_PORTAL_URL` | Manus portal base for login |
| Client | `VITE_APP_TITLE` | Branding (optional) |
| Client (Maps) | `VITE_FRONTEND_FORGE_API_URL`, `VITE_FRONTEND_FORGE_API_KEY` | Needed if MapView is used |
| Server | `VITE_APP_ID` | Used in session tokens (must match client) |
| Server | `OAUTH_SERVER_URL` | Manus OAuth API base |
| Server | `JWT_SECRET` | HMAC secret for `app_session_id` |
| Server | `OWNER_OPEN_ID` | Marks a user as admin |
| Server | `DATABASE_URL` | MySQL connection string for Drizzle |
| Server | `BUILT_IN_FORGE_API_URL`, `BUILT_IN_FORGE_API_KEY` | Forge storage/notification proxy |

## Components and responsibilities

### Client (Vite/React)

- **Entry point**: `client/src/main.tsx` → `App.tsx` → routes `/` → `Home` component
- **State management**: React Query (via tRPC hooks) for server state, React `useState` for UI state
- **Authentication**: `useAuth()` hook calls `trpc.auth.me.useQuery()` to check auth status
- **File handling**: FileReader API converts files to base64 for upload
- **Drag-and-drop**: `@dnd-kit/core` and `@dnd-kit/sortable` for photo reordering
- **UI libraries**: shadcn/ui components, Lucide icons, Sonner toast notifications

### API server (Express + tRPC)

- **Entry point**: `server/_core/index.ts` - Full-featured server with APIs
- **tRPC setup** (`server/_core/trpc.ts`):
  - Uses `superjson` transformer (handles Dates, Maps, Sets, etc.)
  - Three procedure types:
    - `publicProcedure`: No auth required (`auth.me`, `system.health`)
    - `protectedProcedure`: Requires valid session (`photo.*`, `pdf.*`, `auth.logout`)
    - `adminProcedure`: Requires `role === 'admin'` (`system.notifyOwner`)
- **Context creation** (`server/_core/context.ts`):
  - Runs for every tRPC request
  - Calls `sdk.authenticateRequest()` to verify session and load user
  - Sets `ctx.user = User | null`
  - Errors in auth are caught, user set to null (allows public procedures)

### Routers (`server/routers.ts`)

- **Root router**: Combines all sub-routers into `appRouter`
- **PDF worker startup**: `startPdfWorker()` called at module load time (before server starts)
- **Sub-routers**:
  - `system`: `systemRouter` (health check, admin notifications)
  - `auth`: Inline router (me, logout)
  - `photo`: `photoRouter` (CRUD operations)
  - `pdf`: `pdfRouter` (job management)

### Data access (`server/db.ts`)

- **Lazy connection**: `getDb()` creates connection only when `DATABASE_URL` is set
- **User operations**: `upsertUser()`, `getUserByOpenId()` (with admin role assignment)
- **Photo operations**: `createPhoto()`, `getUserPhotos()`, `updatePhotoPosition()`, `deletePhoto()`, `deleteUserPhotos()`
- **PDF job operations**: `createPdfJob()`, `getPdfJob()`, `getUserPdfJobs()`, `updatePdfJob()`, `getPendingPdfJobs()`
- **Query building**: Uses Drizzle ORM query builder (`db.select()`, `db.insert()`, `db.update()`, `db.delete()`)

### Storage (`server/storage.ts`)

- **Upload**: `storagePut(relKey, data, contentType)` → multipart POST to storage proxy
- **Download URL**: `storageGet(relKey)` → GET request for time-limited URL
- **Key normalization**: Removes leading slashes, preserves path structure
- **Error handling**: Throws errors with status codes and messages

### Auth/OAuth

- **OAuth callback** (`server/_core/oauth.ts`):
  - Handles `/api/oauth/callback` GET route
  - Exchanges code for token, fetches user info, creates session
  - Sets cookie and redirects to `/`
- **SDK** (`server/_core/sdk.ts`):
  - `OAuthService`: Handles OAuth API calls (exchange token, get user info)
  - `SDKServer`: High-level API (create session, verify session, authenticate request)
  - Session JWT signing/verification using `jose` library (HS256)
  - User sync: Fetches from OAuth server if user not in DB

### Background worker (`server/pdfWorker.ts`)

- **Module-level execution**: Worker starts when `routers.ts` is imported
- **Polling**: `setInterval(processPdfJobs, 10000)` + immediate execution
- **Job processing**: Sequential processing of all pending jobs per tick
- **PDF generation**: jsPDF + canvas for image loading and sizing
- **Logging**: Structured JSON logs stored in `pdfJobs.logs` field

### Static server (`server/index.ts`)

- **Production bundle**: This file is bundled by `pnpm build`
- **Limitation**: Only serves static files, no API routes
- **Should be replaced**: Use `server/_core/index.ts` for full functionality

## HTTP surface (what's actually reachable)

- `/api/trpc`: tRPC endpoint (POST; handles JSON-encoded batched calls). Everything API-related is behind this.
- `/api/oauth/callback`: OAuth redirect target. Expects `code` and `state` query params.
- `/*`: Static content (only when served via `server/_core/index.ts` in prod mode or via `server/index.ts` bundle). In dev, Vite middlewares serve `client/index.html`.

### tRPC implementation details

**Middleware setup** (`server/_core/trpc.ts`):
- Uses `superjson` transformer for serialization (handles Dates, undefined, null, etc.)
- Three procedure types:
  - `publicProcedure`: No authentication required
  - `protectedProcedure`: Requires `ctx.user` (throws `UNAUTHORIZED` if null)
  - `adminProcedure`: Requires `ctx.user.role === 'admin'` (throws `FORBIDDEN` if not admin)

**Express adapter** (`server/_core/index.ts`):
- Uses `createExpressMiddleware` from `@trpc/server/adapters/express`
- Mounted at `/api/trpc`
- Context created per request via `createContext()`
- Router: `appRouter` (root router combining all sub-routers)

**Request format**:
- POST requests with JSON body
- Supports batching (multiple procedures in one request)
- Content-Type: `application/json`
- Body limit: 50 MB (configured in Express middleware)

**Error handling**:
- `UNAUTHORIZED`: Thrown by `protectedProcedure` when `ctx.user` is null
- `FORBIDDEN`: Thrown by `adminProcedure` when user is not admin
- `BAD_REQUEST`: Thrown by Zod validation (input schema validation)
- `INTERNAL_SERVER_ERROR`: Unhandled errors
- Errors include message from `@shared/const` (e.g., `UNAUTHED_ERR_MSG`, `NOT_ADMIN_ERR_MSG`)

## Request/data flows (intended)

- **Auth**: User hits Manus portal (`getLoginUrl`), returns to `/api/oauth/callback` with code/state → we exchange token, fetch user info, upsert `users`, sign `app_session_id` cookie (HS256, `JWT_SECRET`, bound to `VITE_APP_ID`), redirect `/`.
- **Authenticated request**: tRPC context parses cookie, verifies JWT, fetches/syncs user from Manus if missing, updates `lastSignedIn`, enforces admin when `openId === OWNER_OPEN_ID`.
- **Photo upload**: Client (once wired) would base64 the file → `photo.upload` decodes, uploads via storage proxy to `user-<id>/photos/<nanoid>-<filename>`, inserts `photos` row with `position`.
- **Photo reorder/delete**: `photo.updatePositions` and `photo.delete`/`deleteAll` mutate rows; `photo.list` returns ordered photos.
- **PDF generation**: `pdf.createJob` inserts `pdfJobs` row with ordered `photoIds` JSON; worker polls → loads photos, renders PDF pages, uploads to `user-<id>/pdfs/<nanoid>-photobook.pdf`, updates `resultUrl` and status `completed|failed`; clients would poll `pdf.getJob`/`listJobs`.

## APIs (tRPC under `/api/trpc`)

| Procedure | Handler | Notes |
| --- | --- | --- |
| `system.health` | `server/_core/systemRouter.ts` | Public; timestamp check; returns `{ok:true}`. |
| `system.notifyOwner` | `systemRouter.ts` | Admin-only; POSTs to Forge notification service. |
| `auth.me` | `routers.ts` | Public; returns `ctx.user` (null when unauthenticated). |
| `auth.logout` | `routers.ts` | Protected; clears `app_session_id` with secure/sameSite settings. |
| `photo.upload` | `server/photoRouter.ts` | Protected; base64 → storagePut → insert `photos`. |
| `photo.list` | `server/photoRouter.ts` | Protected; ordered by `position`. |
| `photo.updatePositions` | `server/photoRouter.ts` | Protected; batch update positions. |
| `photo.delete` | `server/photoRouter.ts` | Protected; delete one photo. |
| `photo.deleteAll` | `server/photoRouter.ts` | Protected; delete all user photos. |
| `pdf.createJob` | `server/pdfRouter.ts` | Protected; insert pending `pdfJobs` row. |
| `pdf.listJobs` | `server/pdfRouter.ts` | Protected; list user jobs. |
| `pdf.getJob` | `server/pdfRouter.ts` | Protected; fetch one job or throw. |

### Detailed endpoint contracts

- `photo.upload`
  - Input: `{ filename: string; mimeType: string; data: string (base64); position: number }`
  - Output: `{ url: string; fileKey: string }`
  - Side effects: uploads blob to storage proxy; inserts `photos` row with `userId`, `fileKey`, `url`, `filename`, `mimeType`, `size`, `position`.
  - Errors: missing DB connection, storage failure, auth failure.
- `photo.list`
  - Input: none
  - Output: `Photo[]` ordered by `position`.
  - Side effects: none (read-only).
- `photo.updatePositions`
  - Input: `{ updates: Array<{ id: number; position: number }> }`
  - Output: `{ success: true }`
  - Side effects: updates `photos.position` per id sequentially.
- `photo.delete`
  - Input: `{ id: number }`
  - Output: `{ success: true }`
  - Side effects: deletes row from `photos` (does not delete blob from storage).
- `photo.deleteAll`
  - Input: none
  - Output: `{ success: true }`
  - Side effects: deletes all `photos` for the current user.
- `pdf.createJob`
  - Input: `{ photoIds: number[] }`
  - Output: `{ jobId: number; message: string }`
  - Side effects: inserts `pdfJobs` row (`status=pending`, `photoIds` JSON string). `insertId` is taken from the driver result.
- `pdf.listJobs`
  - Input: none
  - Output: `PdfJob[]` ordered by `createdAt`.
- `pdf.getJob`
  - Input: `{ jobId: number }`
  - Output: `PdfJob`
  - Errors: throws if not found.
- `system.health`
  - Input: `{ timestamp: number >= 0 }`
  - Output: `{ ok: true }`
- `system.notifyOwner`
  - Input: `{ title: string; content: string }`
  - Output: `{ success: boolean }` (false if Forge notification call fails)
  - Errors: TRPC errors on validation or missing Forge config.
- `auth.me`
  - Input: none
  - Output: `User | null`
- `auth.logout`
  - Input: none
  - Output: `{ success: true }`
  - Side effects: clears `app_session_id` with `sameSite: none`, `secure` determined by request, `httpOnly`.

## Data + external services

### Database (MySQL via Drizzle ORM)

**Connection pattern** (`server/db.ts`):
- **Lazy initialization**: `getDb()` creates Drizzle instance only when `DATABASE_URL` is set
- **Singleton**: Module-level `_db` variable stores connection (reused across requests)
- **Error handling**: Connection failures log warning but don't throw (allows tooling to run without DB)
- **Connection string**: MySQL connection string format: `mysql://user:pass@host:port/database`

**Schema** (`drizzle/schema.ts`):
- Uses `drizzle-orm/mysql-core` for type-safe schema definitions
- Tables use camelCase column names (matches TypeScript conventions)
- Auto-increment primary keys, timestamps with defaults

### External services

**Storage proxy** (`BUILT_IN_FORGE_API_URL`):
- S3-like storage service accessed via HTTP API
- Authentication: Bearer token (`BUILT_IN_FORGE_API_KEY`)
- Endpoints:
  - `POST /v1/storage/upload?path=<key>` - Upload file (multipart/form-data)
  - `GET /v1/storage/downloadUrl?path=<key>` - Get time-limited download URL
- Returns permanent URLs for uploaded files (not time-limited)

**OAuth server** (`OAUTH_SERVER_URL`):
- Manus authentication service
- Endpoints:
  - `POST /webdev.v1.WebDevAuthPublicService/ExchangeToken` - Exchange code for token
  - `POST /webdev.v1.WebDevAuthPublicService/GetUserInfo` - Get user info from access token
  - `POST /webdev.v1.WebDevAuthPublicService/GetUserInfoWithJwt` - Get user info from JWT session
- Timeout: `AXIOS_TIMEOUT_MS` (default not shown, but axios client has timeout)

**OAuth portal** (`VITE_OAUTH_PORTAL_URL`):
- Frontend-facing OAuth authorization page
- Used for initial login redirect (not API calls)

### Unused services

- **LLM service** (`server/_core/llm.ts`): Present but not used in photobook flow
- **Maps service** (`server/_core/map.ts`, `client/src/components/Map.tsx`): Present but not integrated
- **Notification service** (`server/notification.ts`): Used by `system.notifyOwner` but not in main flow

### Table-by-table schema notes (Drizzle)

- `users`
  - `id`: PK, auto-increment.
  - `openId`: unique Manus user id (string).
  - `name`: nullable text.
  - `email`: nullable varchar(320).
  - `loginMethod`: nullable varchar(64) (derived from platforms).
  - `role`: enum `user|admin`, default `user`; admin assigned if `openId === OWNER_OPEN_ID`.
  - `createdAt`, `updatedAt`, `lastSignedIn`: timestamps; `updatedAt` on update.
- `photos`
  - `id`: PK.
  - `userId`: FK-like (no constraint), required.
  - `fileKey`: varchar(512), path in storage.
  - `url`: text, download URL from storage proxy.
  - `filename`: varchar(255).
  - `mimeType`: varchar(100), nullable.
  - `size`: int bytes, nullable.
  - `position`: int, default 0 (ordering).
  - `createdAt`, `updatedAt`: timestamps.
  - Notes: no cascade delete; blobs remain in storage if rows are deleted.
- `pdfJobs`
  - `id`: PK.
  - `userId`: required.
  - `status`: enum `pending|processing|completed|failed`, default `pending`.
  - `photoIds`: text storing JSON array of photo ids.
  - `resultUrl`: text (PDF URL) nullable.
  - `errorMessage`: text nullable.
  - `logs`: text nullable (JSON array of log entries).
  - `createdAt`, `updatedAt`, `completedAt`: timestamps.

## Storage interactions

### Upload flow (`storagePut`)

1. **Key normalization**: Removes leading slashes from `relKey`
2. **URL construction**: `{BUILT_IN_FORGE_API_URL}/v1/storage/upload?path={normalizedKey}`
3. **Form data creation**:
   - Converts `data` (Buffer/Uint8Array/string) to Blob with `contentType`
   - Creates FormData, appends blob as `file` field with filename = last segment of key
4. **Request**:
   - Method: POST
   - Headers: `Authorization: Bearer {BUILT_IN_FORGE_API_KEY}`
   - Body: FormData (multipart/form-data)
5. **Response**: Parses JSON `{ url }` from response body
6. **Error handling**: Throws error with status code and message if `!response.ok`

### Download URL flow (`storageGet`)

1. **Key normalization**: Same as upload (remove leading slashes)
2. **URL construction**: `{BUILT_IN_FORGE_API_URL}/v1/storage/downloadUrl?path={normalizedKey}`
3. **Request**:
   - Method: GET
   - Headers: `Authorization: Bearer {BUILT_IN_FORGE_API_KEY}`
4. **Response**: Parses JSON `{ url }` (time-limited download URL)
5. **Returns**: `{ key, url }`

### Key format conventions

- **Photos**: `user-<userId>/photos/<nanoid()>-<originalFilename>`
  - Example: `user-42/photos/abc123xyz-image.jpg`
  - Generated in `photoRouter.upload` mutation
- **PDFs**: `user-<userId>/pdfs/<nanoid()>-photobook.pdf`
  - Example: `user-42/pdfs/def456uvw-photobook.pdf`
  - Generated in `pdfWorker.generatePdfForJob`
- **Key normalization**: Leading slashes removed, but slashes within path preserved

### Storage proxy API contract

- **Base URL**: `BUILT_IN_FORGE_API_URL` (env var, trailing slash optional)
- **Authentication**: Bearer token in `Authorization` header
- **Upload endpoint**: `/v1/storage/upload?path=<key>`
  - Accepts multipart/form-data with `file` field
  - Returns JSON: `{ url: string }` (permanent download URL)
- **Download URL endpoint**: `/v1/storage/downloadUrl?path=<key>`
  - Returns JSON: `{ url: string }` (time-limited signed URL)

### Current gaps

- **No blob deletion**: Deleting a `photos` row does not delete the blob from storage
- **No cleanup job**: Orphaned blobs accumulate (photos deleted but files remain)
- **No PDF cleanup**: Completed/failed PDF jobs don't delete their result files
- **No storage quota**: No tracking or limits on storage usage per user

## OAuth + session flow (step-by-step)

### Initial authentication

1. **Frontend redirect**: User clicks login → `window.location.href = getLoginUrl()`
   - `getLoginUrl()` constructs: `{OAUTH_PORTAL_URL}/oauth/authorize?client_id={VITE_APP_ID}&redirect_uri={origin}/api/oauth/callback&state={btoa(redirectUri)}`
   - `state` parameter is base64-encoded redirect URI (for CSRF protection)
2. **OAuth callback**: Manus redirects to `/api/oauth/callback?code=...&state=...`
3. **Code exchange** (`server/_core/oauth.ts`):
   - Calls `sdk.exchangeCodeForToken(code, state)`:
     - POSTs to `{OAUTH_SERVER_URL}/webdev.v1.WebDevAuthPublicService/ExchangeToken`
     - Payload: `{ clientId: VITE_APP_ID, grantType: "authorization_code", code, redirectUri: atob(state) }`
     - Returns `{ accessToken }`
4. **User info fetch**:
   - Calls `sdk.getUserInfo(accessToken)`:
     - POSTs to `{OAUTH_SERVER_URL}/webdev.v1.WebDevAuthPublicService/GetUserInfo`
     - Payload: `{ accessToken }`
     - Returns user info with `openId`, `name`, `email`, `platforms`, etc.
5. **User sync**:
   - Calls `db.upsertUser()` with user info
   - Derives `loginMethod` from `platforms` array (email/google/apple/microsoft/github)
   - Sets `lastSignedIn = new Date()`
   - **Admin assignment**: If `openId === OWNER_OPEN_ID`, sets `role = 'admin'` (happens in `db.upsertUser()`)
6. **Session token creation**:
   - Calls `sdk.createSessionToken(openId, { name, expiresInMs: ONE_YEAR_MS })`:
     - Signs JWT using `jose.SignJWT` with HS256 algorithm
     - Payload: `{ openId, appId: VITE_APP_ID, name }`
     - Secret: `JWT_SECRET` (from env) encoded as `TextEncoder().encode(secret)`
     - Expiration: `issuedAt + ONE_YEAR_MS` (1 year = 31,536,000,000 ms)
7. **Cookie setting**:
   - Name: `app_session_id` (from `COOKIE_NAME` constant)
   - Options (via `getSessionCookieOptions(req)`):
     - `httpOnly: true` (prevents JavaScript access)
     - `path: "/"` (available site-wide)
     - `sameSite: "none"` (allows cross-site requests)
     - `secure: true` if `req.protocol === "https"` OR `x-forwarded-proto: https` header present
     - `maxAge: ONE_YEAR_MS` (1 year)
   - Redirects to `/` (home page)

### Authenticated request flow

1. **tRPC context creation** (`server/_core/context.ts`):
   - `createContext()` is called for every tRPC request
   - Calls `sdk.authenticateRequest(req)`:
     - Parses cookies from `req.headers.cookie` using `cookie.parse()`
     - Extracts `app_session_id` cookie value
     - Calls `sdk.verifySession(cookieValue)`:
       - Verifies JWT using `jose.jwtVerify()` with HS256
       - Validates payload contains `openId`, `appId`, `name` (all non-empty strings)
       - Returns `{ openId, appId, name }` or `null` if invalid
2. **User lookup**:
   - Queries DB: `db.getUserByOpenId(session.openId)`
   - **If user not found**: Syncs from OAuth server:
     - Calls `sdk.getUserInfoWithJwt(sessionCookie)`:
       - POSTs to `{OAUTH_SERVER_URL}/webdev.v1.WebDevAuthPublicService/GetUserInfoWithJwt`
       - Payload: `{ jwtToken: sessionCookie, projectId: VITE_APP_ID }`
     - Upserts user with fetched info
3. **Last sign-in update**: Always calls `db.upsertUser({ openId, lastSignedIn: new Date() })` to update timestamp
4. **Context attachment**: Sets `ctx.user = User | null` (null for public procedures)

### Admin role assignment

- **During OAuth callback**: If `userInfo.openId === OWNER_OPEN_ID`, `db.upsertUser()` sets `role = 'admin'`
- **During user sync**: Same check happens in `db.upsertUser()` - if `openId === ENV.ownerOpenId`, role is set to admin
- **Role enforcement**: `adminProcedure` middleware checks `ctx.user.role === 'admin'`, throws `FORBIDDEN` if not

### Session verification details

- **JWT algorithm**: HS256 (HMAC-SHA256)
- **Secret derivation**: `TextEncoder().encode(JWT_SECRET)` (UTF-8 bytes)
- **Payload validation**: Requires `openId`, `appId`, `name` all be non-empty strings
- **Expiration check**: Handled by `jwtVerify()` - expired tokens return verification failure
- **Error handling**: Invalid/missing cookies return `null`, which allows public procedures but blocks protected ones

## Background worker (PDF)

### Worker lifecycle

- **Startup**: `startPdfWorker()` is called immediately when `server/routers.ts` is imported (module-level execution). This happens before the HTTP server starts.
- **Polling interval**: 10 seconds (`setInterval(..., 10000)`)
- **Immediate execution**: Worker processes jobs immediately on startup, then continues polling every 10s
- **Error handling**: Worker catches errors per-job but continues processing other jobs. Errors are logged to console and stored in job `logs` field.

### Job processing algorithm (`generatePdfForJob`)

1. **Status update**: Sets job `status = "processing"`, initializes logger
2. **Job fetch**: Retrieves job from DB by `jobId`
3. **Photo ID parsing**: Parses `photoIds` JSON string to array of numbers
4. **Photo retrieval**: 
   - Fetches ALL photos for the user (`WHERE userId = job.userId`)
   - Filters to only photos matching requested IDs
   - Orders by the requested ID sequence (not by `position` field)
5. **PDF initialization**: Creates jsPDF instance with:
   - Orientation: `portrait`
   - Unit: `mm`
   - Format: `a4`
6. **Page layout calculation**:
   - Page dimensions: A4 = 210mm × 297mm
   - Margins: 10mm on all sides
   - Image area: 190mm × 277mm
7. **Image processing** (per photo):
   - Downloads image from `photo.url` via `fetch()`
   - Converts response to Buffer (`arrayBuffer()` → `Buffer.from()`)
   - Loads image using `canvas.loadImage()` to get dimensions
   - Calculates aspect-fit sizing:
     - If `imgAspectRatio > pageAspectRatio`: fit to width, center vertically
     - Else: fit to height, center horizontally
   - Adds image to PDF using `pdf.addImage(buffer, "JPEG", x, y, width, height)`
   - Adds new page before each subsequent image (`pdf.addPage()`)
   - Updates job logs after each photo (progress tracking)
8. **PDF generation**: Converts to buffer via `pdf.output('arraybuffer')` → `Buffer.from()`
9. **Storage upload**: Uploads PDF buffer to storage proxy:
   - Key: `user-<userId>/pdfs/<nanoid()>-photobook.pdf`
   - Content-Type: `application/pdf`
10. **Completion**: Updates job with:
    - `status = "completed"`
    - `resultUrl = <storage URL>`
    - `logs = <JSON array of log entries>`
    - `completedAt = <current timestamp>`

### Logging system (`PdfJobLogger`)

- Structured logging with timestamp, level (`info`/`warn`/`error`), and message
- Logs stored as JSON array string in `pdfJobs.logs` field
- Console output also emitted for debugging: `[PDF Worker] [LEVEL] message`
- Progress updates: Logs after each photo processed, allowing clients to track progress

### Error handling

- **Per-photo errors**: If a single photo fails to download/process, logs error but continues with next photo
- **Job-level errors**: If job fetch fails or PDF generation fails, sets `status = "failed"`, stores `errorMessage` and `logs`
- **No partial success**: If any photo fails, entire job may fail (depends on jsPDF behavior with missing images)

### Known limitations

- **No locking**: Multiple worker instances can process the same job simultaneously
- **No lease mechanism**: Jobs don't have "claimed by worker X" tracking
- **No backoff**: Fixed 10s interval regardless of load or errors
- **No retry logic**: Failed jobs remain failed (no automatic retry)
- **No cleanup**: Generated PDFs never deleted; failed jobs accumulate
- **No concurrency limit**: Worker processes all pending jobs sequentially in one tick
- **No timeout**: Long-running jobs can block worker indefinitely

## Frontend implementation and integration

**Current state:** The frontend is **fully integrated** with the backend APIs (contrary to earlier documentation that suggested it wasn't wired up).

### Frontend architecture (`client/src/pages/Home.tsx`)

- **Authentication**: Uses `useAuth()` hook that calls `trpc.auth.me.useQuery()` to check authentication status. Redirects to Manus login if unauthenticated.
- **Photo management**: Fully integrated with tRPC:
  - `trpc.photo.list.useQuery()` - Fetches photos on mount (enabled only when user is authenticated)
  - `trpc.photo.upload.useMutation()` - Handles file uploads (converts to base64, sends to server)
  - `trpc.photo.updatePositions.useMutation()` - Updates photo order after drag/drop
  - `trpc.photo.delete.useMutation()` - Deletes individual photos
  - `trpc.photo.deleteAll.useMutation()` - Clears all photos
- **PDF generation**: `trpc.pdf.createJob.useMutation()` - Creates PDF jobs (polling for status not yet implemented in UI)
- **State management**: Uses React Query (via tRPC) for server state, local React state for UI state (selectedIndex, isPlaying, etc.)

### Upload flow (implemented)

1. User selects files via `<input type="file" multiple accept="image/*">`
2. For each file:
   - FileReader converts to base64 (strips data URL prefix, keeps only base64 string)
   - Calls `photo.upload` mutation with `{ filename, mimeType, data: base64, position }`
   - Position is calculated as `currentMaxPosition + index + 1`
3. After all uploads complete, calls `refetch()` to reload photo list
4. Toast notifications show progress (`Uploading X/Y...`) and completion status

### Drag-and-drop reordering (implemented)

- Uses `@dnd-kit/core` and `@dnd-kit/sortable` libraries
- On drag end:
  1. Calculates new positions based on array order
  2. Calls `photo.updatePositions` with `{ updates: [{ id, position }] }`
  3. Refetches photo list
  4. Updates `selectedIndex` to track which image is currently viewed

### Slideshow controls

- Keyboard navigation: Arrow keys (left/right), Space (play/pause)
- Auto-play: 3-second interval between images
- Manual controls: Previous/Next buttons, Play/Pause toggle
- Position display: Shows "Position X of Y" and current index

### Additional features

- **Randomize**: Shuffles photo order via `updatePositions`
- **Move Up/Down**: Individual position adjustments for selected image
- **Delete**: Individual photo deletion with index adjustment
- **Clear All**: Bulk deletion via `deleteAll`

### Unused components (present but not integrated)

- `ManusDialog` component exists but not used (login handled via redirect)
- `MapView` component exists but not used in photobook flow
- Theme toggling disabled in UI

### Missing features (not yet implemented)

- PDF job status polling: Frontend creates jobs but doesn't poll for completion or show download links
- Job history UI: No interface to view past PDF jobs or download completed PDFs

## Dev/prod setup notes

### Development mode (`NODE_ENV !== "production"`)

- **Vite integration** (`server/_core/vite.ts`):
  - Creates Vite dev server in middleware mode
  - HMR (Hot Module Replacement) enabled via WebSocket on HTTP server
  - Serves `client/index.html` template with Vite transformations
  - Injects cache-busting query param: `src="/src/main.tsx?v={nanoid()}"`
  - All routes (`*`) handled by Vite middleware (SPA routing)
- **Port selection** (`server/_core/index.ts`):
  - Preferred port: `process.env.PORT || 3000`
  - Finds first available port starting from preferred (checks ports up to preferred + 20)
  - Logs warning if actual port differs from preferred
- **Body parser**: 50 MB limit for both JSON and URL-encoded (to handle base64 image uploads)

### Production mode (`NODE_ENV === "production"`)

- **Static serving** (`server/_core/vite.ts` → `serveStatic()`):
  - Serves files from `dist/public` directory
  - Fallback: All routes (`*`) serve `dist/public/index.html` (SPA routing)
- **Current build issue**: `server/index.ts` bundle only serves static files, no API routes
  - This is the file bundled by `pnpm build`
  - Should use `server/_core/index.ts` instead for full API support

### Server startup (`server/_core/index.ts`)

1. **Express app creation**
2. **Body parser setup**: `express.json({ limit: "50mb" })` and `express.urlencoded({ limit: "50mb", extended: true })`
3. **OAuth routes**: `registerOAuthRoutes(app)` → mounts `/api/oauth/callback`
4. **tRPC middleware**: `app.use("/api/trpc", createExpressMiddleware({ router: appRouter, createContext }))`
5. **Static/Vite setup**: 
   - Dev: `setupVite(app, server)` (Vite middleware + HMR)
   - Prod: `serveStatic(app)` (static files only)
6. **Port selection and listen**

### CORS and security

- **CORS**: Not explicitly configured (assumes same-origin requests)
- **Cookie security**: 
  - `httpOnly: true` (prevents XSS cookie theft)
  - `sameSite: "none"` (allows cross-site requests, requires `secure: true`)
  - `secure: true` when HTTPS detected (via protocol or `x-forwarded-proto` header)
- **Body size limits**: 50 MB (prevents DoS via large uploads, but allows reasonable image sizes)

### Environment detection

- **Development**: `process.env.NODE_ENV === "development"` → uses Vite
- **Production**: `process.env.NODE_ENV === "production"` → uses static files
- **Port**: `process.env.PORT` or defaults to 3000

## Known gaps / risks (to fix before or during Go port)

### Production deployment

- **API server missing**: `pnpm build` bundles `server/index.ts` (static-only), not `server/_core/index.ts` (full API)
- **Fix needed**: Update build config to bundle `server/_core/index.ts` or create unified entrypoint

### PDF worker concurrency

- **No locking**: Multiple instances can process same job simultaneously (race condition)
- **No lease mechanism**: Jobs don't track which worker is processing them
- **No idempotency**: Same job processed multiple times wastes resources
- **Fix needed**: Add database-level locking (SELECT FOR UPDATE) or distributed lock (Redis)

### Storage lifecycle

- **No blob deletion**: Deleting `photos` row doesn't delete storage blob
- **No cleanup job**: Orphaned blobs accumulate over time
- **No PDF cleanup**: Completed/failed PDFs never deleted
- **Fix needed**: Add cleanup job or cascade deletion logic

### Error handling

- **Worker partial failures**: If one photo fails, job may still complete with missing images (no clear failure state)
- **No retry logic**: Failed jobs remain failed (no automatic retry)
- **No timeout**: Long-running jobs can block worker indefinitely
- **Fix needed**: Define failure semantics (fail job if any photo fails? mark partial success?), add timeouts, add retry logic

### Authentication

- **Cookie security**: `sameSite: "none"` requires `secure: true` (HTTPS)
- **Plain HTTP risk**: Will fail on HTTP unless behind TLS terminator with `x-forwarded-proto: https`
- **Fix needed**: Ensure HTTPS in production or adjust cookie settings for development

### Frontend gaps

- **PDF job polling**: Frontend creates jobs but doesn't poll for completion
- **No job history UI**: Users can't see past jobs or download completed PDFs
- **Fix needed**: Add polling logic and job history view

### Database

- **No foreign key constraints**: `photos.userId` and `pdfJobs.userId` reference `users.id` but no FK constraint
- **No cascade deletes**: Deleting user doesn't delete photos/jobs
- **Fix needed**: Add FK constraints and cascade rules (or handle in application code)

### Performance

- **Sequential photo processing**: Worker processes photos one-by-one (could parallelize)
- **No rate limiting**: API endpoints have no rate limits
- **No pagination**: `photo.list` and `pdf.listJobs` return all records
- **Fix needed**: Add pagination, consider parallel processing, add rate limiting

## Mental model / next steps for a newcomer

### Three core lanes

1. **Auth lane**: OAuth flow → session JWT → cookie → tRPC context → user lookup
2. **Photos lane**: Upload → base64 → storage proxy → DB insert → list/reorder/delete
3. **PDF pipeline**: Job creation → worker polling → image download → PDF generation → storage upload → job completion

### Current state (as of analysis)

- ✅ Frontend fully integrated with backend APIs
- ✅ Photo upload/list/reorder/delete working
- ✅ PDF job creation working
- ❌ PDF job status polling not implemented in frontend
- ❌ Production build missing API server
- ❌ Worker lacks concurrency control

### Immediate unblockers for Go port

1. **Fix production build**: Update build to use `server/_core/index.ts` instead of `server/index.ts`
2. **Add PDF job polling**: Frontend should poll `pdf.getJob` or `pdf.listJobs` to show completion status
3. **Worker concurrency**: Add database locking or distributed lock (Redis) to prevent duplicate processing
4. **Storage lifecycle**: Add cleanup job or cascade deletion for orphaned blobs
5. **Error handling**: Define failure semantics for partial PDF generation failures

### Go port requirements

**Must replicate exactly**:
- JWT signing/verification (HS256, `JWT_SECRET`, payload: `{openId, appId, name}`)
- OAuth exchange flow (code → token → user info → session)
- Storage proxy API calls (multipart upload, download URL generation)
- Database schema (users, photos, pdfJobs tables with exact columns)
- Cookie configuration (`httpOnly`, `sameSite: none`, `secure` based on protocol)
- PDF generation algorithm (A4 portrait, 10mm margins, aspect-fit image sizing)
- Photo ordering (by `position` field, not by creation time)

**Can improve**:
- Worker concurrency (add locking/leases)
- Error handling (retry logic, timeouts)
- Storage cleanup (cascade deletes)
- API pagination (for large photo lists)
- Rate limiting (prevent abuse)

## Related

- Ticket index: [index.md](../index.md)
- Task list: [tasks.md](../tasks.md)
- Backend entrypoint (tRPC): server/_core/index.ts
- Static-only server currently built: server/index.ts
- Primary routers: server/routers.ts, server/photoRouter.ts, server/pdfRouter.ts
- Background worker: server/pdfWorker.ts
- Client entry: client/src/pages/Home.tsx
