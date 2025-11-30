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
Summary: Map of current Node/Express + tRPC stack, data paths, and frontend behavior to prep Go port
LastUpdated: 2025-11-29T20:20:00-05:00
---







# Current architecture and data flow

## Goal

Summarize how the photobook app currently works (server, frontend, data stores, and background jobs) so we can reimplement the backend in Go without missing behaviors or dependencies.

## Context

- Stack: Node 18+/TypeScript with Express + tRPC (`/api/trpc`), Vite React frontend, Drizzle ORM (MySQL), storage proxy via Forge, OAuth against Manus services.
- Production build script only bundles `server/index.ts` (static file server); the richer API server lives in `server/_core/index.ts` and is not wired into the build.
- Frontend currently runs fully client-side (local image objects) and does not call the backend API.

## Quick Reference

### Runtime entry points

- `server/_core/index.ts`: Express app with JSON body limit 50 MB, registers OAuth callback and tRPC router, serves Vite dev or static assets (dist/public) in prod.
- `server/index.ts`: Bare static server used by `pnpm build`/`pnpm start`; **no API routing** (integration gap).
- `server/routers.ts`: tRPC root; starts PDF worker as a side-effect.

### API surface (tRPC under `/api/trpc`)

| Procedure | Handler | Notes |
| --- | --- | --- |
| `system.health` | `server/_core/systemRouter.ts` | Public; validates timestamp ≥0 and returns `{ok:true}`. |
| `system.notifyOwner` | `systemRouter.ts` | Admin-only; posts notification via Forge (`notifyOwner`). |
| `auth.me` | inline in `routers.ts` | Public; echoes authenticated user (null when unauthenticated). |
| `auth.logout` | inline in `routers.ts` | Protected; clears `app_session_id` cookie using request-aware cookie options. |
| `photo.upload` | `server/photoRouter.ts` | Protected; accepts `{filename,mimeType,data (base64),position}` → uploads to storage proxy (`storagePut`), stores record in `photos` with `nanoid` key and size bytes. |
| `photo.list` | `photoRouter.ts` | Protected; lists user photos ordered by `position`. |
| `photo.updatePositions` | `photoRouter.ts` | Protected; batch updates `position` per photo id. |
| `photo.delete` | `photoRouter.ts` | Protected; deletes one photo row. |
| `photo.deleteAll` | `photoRouter.ts` | Protected; deletes all rows for current user. |
| `pdf.createJob` | `server/pdfRouter.ts` | Protected; inserts `pdfJobs` row with `status=pending` and `photoIds` JSON, returns `insertId` (cast from driver result). |
| `pdf.listJobs` | `pdfRouter.ts` | Protected; lists all jobs for user. |
| `pdf.getJob` | `pdfRouter.ts` | Protected; fetches one job by id, throws if missing. |

### Auth/session flow

- OAuth callback at `/api/oauth/callback` (`server/_core/oauth.ts`): exchanges `code` for access token via Manus OAuth API, fetches user info, upserts `users` row, signs session JWT (`app_session_id`) using `JWT_SECRET`, sets httpOnly cookie (`sameSite: "none"`, `secure` based on request), redirects to `/`.
- Request auth (`server/_core/sdk.ts`): parses cookie, verifies JWT (requires `VITE_APP_ID` match), lazily syncs user from Manus via `GetUserInfoWithJwt` if not in DB, upserts `lastSignedIn`, sets admin role when `openId` matches `OWNER_OPEN_ID`. Missing/invalid cookies throw `ForbiddenError`.

### Data + storage

- Database (MySQL via Drizzle, `server/db.ts`, schema in `drizzle/schema.ts`):
  - `users`: `openId` unique, optional `name/email/loginMethod`, `role` enum user/admin, timestamps.
  - `photos`: `userId`, `fileKey`, `url`, `filename`, `mimeType`, `size`, `position`, timestamps.
  - `pdfJobs`: `userId`, `status` enum (pending/processing/completed/failed), `photoIds` JSON string, optional `resultUrl`, `errorMessage`, `logs`, timestamps, `completedAt`.
- Storage proxy (`server/storage.ts`): uploads/downloading via Forge gateway using `BUILT_IN_FORGE_API_URL` + `BUILT_IN_FORGE_API_KEY`; stores blobs under normalized keys like `user-<id>/photos/<nanoid>-<filename>`.

### Background processing

- PDF worker (`server/pdfWorker.ts`) starts automatically when `appRouter` loads; polls DB every 10 s (`getPendingPdfJobs`), marks jobs processing, fetches requested photo records, downloads each image (via stored `url`), adds to jsPDF page sized to A4 with margin, uploads final PDF to storage proxy at `user-<userId>/pdfs/<nanoid>-photobook.pdf`, updates job status/resultUrl/logs/completedAt. On error marks job failed with `errorMessage` and logs.

### Frontend behavior

- Routing (`client/src/App.tsx`): `"/"` → `Home`, fallback → `NotFound`.
- Home (`client/src/pages/Home.tsx`): local-only photobook experience—file input creates object URLs, thumbnails sortable via dnd-kit, slideshow controls, delete/clear actions; no backend calls or persistence.
- Helpers present but unused: `ManusDialog` + `getLoginUrl` (client/src/const.ts) to kick off Manus OAuth; `MapView` wrapper around Forge Maps proxy; Theme context toggles dark mode (disabled by default).

### Integration gaps to address before Go port

- Build/start scripts serve only static assets (`server/index.ts`); the API/tRPC server and OAuth callback in `server/_core/index.ts` are not part of the production bundle.
- Frontend never calls `/api/trpc` for photo upload/list or PDF creation, so backend features are effectively unused.
- No queue or locking around PDF worker; single-process polling could duplicate work if multiple instances run.

## Usage Examples

- Intended photo upload flow (once wired):
  1) Client picks file → encodes to base64 → calls `photo.upload`.
  2) Server decodes, uploads blob via storage proxy, inserts `photos` row with `position`.
  3) Client calls `photo.list` to refresh ordered list.
- PDF generation flow:
  1) Client calls `pdf.createJob` with ordered photo ids.
  2) Worker picks up pending job → downloads photos → builds PDF → uploads to storage → marks job completed with `resultUrl`.
  3) Client polls `pdf.getJob`/`listJobs` for status and download URL.
- Auth handshake:
  1) Client redirects to `getLoginUrl()` (Manus portal) with `state=btoa(redirectUri)`.
  2) Manus redirects back to `/api/oauth/callback?code&state`; server exchanges token, upserts user, sets `app_session_id` cookie, redirects to `/`.
  3) Subsequent tRPC calls include cookie; `createContext` authenticates and exposes `ctx.user`.

## Related

- Ticket index: [index.md](../index.md)
- Task list: [tasks.md](../tasks.md)
- Backend entrypoint (tRPC): server/_core/index.ts
- Static-only server currently built: server/index.ts
- Primary routers: server/routers.ts, server/photoRouter.ts, server/pdfRouter.ts
- Background worker: server/pdfWorker.ts
- Client entry: client/src/pages/Home.tsx
