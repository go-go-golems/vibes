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

- **Client (Vite/React)**: `client/src/App.tsx` routes `/` → `Home`; `Home` currently runs entirely in-browser (object URLs, dnd-kit sorting, slideshow). Helpers for Manus login (`getLoginUrl`, `ManusDialog`) and Maps (`MapView`) exist but are unused.
- **API server (Express + tRPC)**: `server/_core/index.ts` wires middleware, OAuth callback, and `/api/trpc`. Context creation (`server/_core/context.ts`) authenticates each request.
- **Routers**: `server/routers.ts` is the tRPC root and starts the PDF worker.
  - `systemRouter`: health, admin notify.
  - `auth`: me/logout (session cookie clearing).
  - `photoRouter`: upload/list/updatePositions/delete/deleteAll.
  - `pdfRouter`: createJob/listJobs/getJob.
- **Data access**: `server/db.ts` backed by Drizzle schemas in `drizzle/schema.ts` (`users`, `photos`, `pdfJobs`).
- **Storage**: `server/storage.ts` posts to Forge storage proxy using `BUILT_IN_FORGE_API_KEY`, returns download URLs.
- **Auth/OAuth**: `server/_core/oauth.ts` handles `/api/oauth/callback`; `server/_core/sdk.ts` signs/verifies session JWTs (`app_session_id`), syncs user records, and flags admins if `OWNER_OPEN_ID` matches.
- **Background worker**: `server/pdfWorker.ts` polls pending jobs every 10s, downloads photos by URL, renders PDF pages, uploads the PDF, and updates `pdfJobs.status/resultUrl/logs`.
- **Static server**: `server/index.ts` serves `dist/public` only (no APIs) and is what `pnpm build && pnpm start` currently produces.

## HTTP surface (what’s actually reachable)

- `/api/trpc`: tRPC endpoint (POST; handles JSON-encoded batched calls). Everything API-related is behind this.
- `/api/oauth/callback`: OAuth redirect target. Expects `code` and `state` query params.
- `/*`: Static content (only when served via `server/_core/index.ts` in prod mode or via `server/index.ts` bundle). In dev, Vite middlewares serve `client/index.html`.

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

- **MySQL (Drizzle)**:
  - `users`: `openId` (unique), optional `name/email/loginMethod`, `role` (user/admin), timestamps.
  - `photos`: `userId`, `fileKey`, `url`, `filename`, `mimeType`, `size`, `position`, timestamps.
  - `pdfJobs`: `userId`, `status` (pending/processing/completed/failed), `photoIds` JSON string, `resultUrl`, `errorMessage`, `logs`, timestamps, `completedAt`.
- **Storage proxy**: `BUILT_IN_FORGE_API_URL` + `BUILT_IN_FORGE_API_KEY`; uploads via multipart to `/v1/storage/upload`, download URL via `/v1/storage/downloadUrl`.
- **OAuth/Identity**: Manus auth server (`OAUTH_SERVER_URL`), app ID (`VITE_APP_ID`), `JWT_SECRET` for session signing, `OWNER_OPEN_ID` for admin role.
- **LLM/Maps helpers**: Present (`server/_core/llm.ts`, `server/_core/map.ts`, `client/src/components/Map.tsx`) but unused by the photobook flow.

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

- Uploads: `storagePut(relKey, data, contentType)` → POST multipart to `{BUILT_IN_FORGE_API_URL}/v1/storage/upload?path=<relKey>` with `Authorization: Bearer <apiKey>`. Returns `{ url }`.
- Downloads: `storageGet(relKey)` → GET `{BUILT_IN_FORGE_API_URL}/v1/storage/downloadUrl?path=<relKey>` to retrieve a time-limited URL.
- Key format convention: `user-<userId>/photos/<nanoid>-<filename>` for photos; `user-<userId>/pdfs/<nanoid>-photobook.pdf` for PDFs.
- Current cleanup gap: deleting a photo row does not delete its blob; PDF jobs do not delete intermediate artifacts.

## OAuth + session flow (step-by-step)

1) Frontend sends user to Manus portal (`getLoginUrl` in `client/src/const.ts`), with `state = btoa(redirectUri)` where `redirectUri = <origin>/api/oauth/callback`.
2) Manus redirects to `/api/oauth/callback?code=...&state=...`.
3) Server (`server/_core/oauth.ts`) exchanges code via `sdk.exchangeCodeForToken`, fetches user info, upserts `users` row, signs JWT session (`app_session_id`) using `JWT_SECRET`, `VITE_APP_ID`, and user name.
4) Cookie is set with `httpOnly`, `sameSite: none`, `secure` depending on request protocol, `path=/`, `maxAge = ONE_YEAR_MS`; redirect `/`.
5) Each tRPC call: `createContext` parses cookies, `sdk.verifySession` validates JWT, fetches/syncs user if needed, upserts `lastSignedIn`.
6) Admin gating: if `openId` matches `OWNER_OPEN_ID`, role is set to `admin`.

## Background worker (PDF)

- Startup: `startPdfWorker` runs as soon as `server/routers.ts` is imported; sets a 10s interval and also triggers an immediate run.
- Loop: `getPendingPdfJobs` fetches all `status = pending`.
- For each job:
  - Set `status = processing`, persist logs (JSON array).
  - Fetch job; parse `photoIds` JSON; fetch all photos for the user; order by requested ids.
  - For each photo: download by URL, load via `canvas`, compute aspect fit within A4 minus margins, add to jsPDF (new page per image).
  - After building: `pdf.output('arraybuffer')` → Buffer → `storagePut` to `user-<userId>/pdfs/<nanoid>-photobook.pdf`.
  - Update `pdfJobs` with `status = completed`, `resultUrl`, `logs`, `completedAt`.
  - On any error: set `status = failed`, `errorMessage`, `logs`.
- Gaps: no locking/lease per job; multiple instances could duplicate work; no backoff; relies on periodic polling; no deletion of failed/old artifacts.

## Frontend state today and intended integration

- Current UX (`client/src/pages/Home.tsx`):
  - Local-only image list; object URLs; drag/drop ordering; slideshow controls; delete/clear; keyboard shortcuts.
  - No API calls; no persistence; no auth prompts.
- Provided but unused:
  - `ManusDialog` + `getLoginUrl` (OAuth trigger).
  - `MapView` (Forge Maps proxy).
  - Theme toggling (disabled).
- Integration plan to exercise APIs before Go port:
  1) On upload: read file → base64 → call `photo.upload` → store returned URLs; update local state from `photo.list`.
  2) On reorder: call `photo.updatePositions` with new ordering.
  3) On delete/clear: call `photo.delete`/`deleteAll`.
  4) PDF: call `pdf.createJob` with ordered ids → poll `pdf.getJob`/`listJobs` until `completed` → show download link (`resultUrl`).
  5) Require auth: gate UI behind `auth.me` and redirect to Manus login if unauthenticated.

## Dev/prod setup notes

- Dev server (Vite) runs at `localhost:3000` (or next available). API server picks first free port ≥ 3000 (logs if different).
- Body limits: 50 MB JSON and URL-encoded to allow base64 uploads.
- CORS: implicit (tRPC and OAuth assume same origin).
- Static in prod (when using `_core/index.ts`): serves `dist/public`; fallback `*` to `index.html`.
- Static in current bundle (`server/index.ts`): serves `dist/public` only; fallback `*` to `index.html`.

## Known gaps / risks (to fix before or during Go port)

- API server not included in prod bundle; only static server is built/run.
- Frontend does not call backend; contracts unvalidated (photo/PDF).
- PDF worker lacks locking/lease; risk of duplicate processing with multiple instances.
- Storage cleanup not implemented (orphaned blobs possible).
- Error handling in worker: continues per-photo but does not mark partial success; result may omit failed images silently beyond logs.
- Auth/session cookies require HTTPS for `secure` (will fail on plain HTTP unless behind TLS terminator with `x-forwarded-proto: https`).

## Mental model / next steps for a newcomer

- Think in three lanes: **Auth** (OAuth → `app_session_id`), **Photos** (upload to storage + DB rows), **PDF pipeline** (job insert → worker poll → PDF upload).
- Immediate unblockers:
  1) Change prod entrypoint to serve the API server (or replace entirely with Go service).
  2) Wire frontend to `/api/trpc` to validate contracts before porting.
  3) Decide on job orchestration approach in Go (poller vs queue) with idempotency.
  4) Plan storage lifecycle (delete blobs on photo delete; cleanup PDFs).
- When porting to Go, mirror: JWT signing/verification, OAuth exchange, storage proxy auth headers, DB schema, cookie options, and the PDF generation workflow (including image resizing to fit A4 with margins).

## Related

- Ticket index: [index.md](../index.md)
- Task list: [tasks.md](../tasks.md)
- Backend entrypoint (tRPC): server/_core/index.ts
- Static-only server currently built: server/index.ts
- Primary routers: server/routers.ts, server/photoRouter.ts, server/pdfRouter.ts
- Background worker: server/pdfWorker.ts
- Client entry: client/src/pages/Home.tsx
