# Tasks

## TODO

- [x] Initialize docmgr workspace for this sub-repo and create PORT-001
- [x] Capture current architecture/data flow reference
- [x] Map frontend calls to backend APIs (photo upload/list/PDF jobs) - frontend is fully integrated via tRPC
- [ ] Align production entrypoint (static server vs `server/_core/index.ts`) before Go parity work
- [ ] List Go replacements for storage proxy, OAuth/session handling, and PDF worker scheduling
- [ ] Document PDF job status polling implementation (frontend creates jobs but doesn't poll for completion)
- [ ] Phase 1: Set up Go project structure (cmd/, internal/, pkg/ directories)
- [ ] Phase 1: Implement configuration system (env vars, typed config struct, validation)
- [ ] Phase 1: Set up SQLite database connection and Goose migrations
- [ ] Phase 1: Create database migrations (users, photos, pdf_jobs tables)
- [x] Phase 1: Implement storage interface and disk storage implementation
- [x] Phase 1: Set up structured logging with zerolog
- [x] Phase 2: Implement email/password authentication (registration, login, password hashing)
- [x] Phase 2: Implement JWT session management (same format as current: app_session_id cookie)
- [x] Phase 2: Create auth adapter interface (stub for future OAuth providers)
- [x] Phase 2: Implement tRPC auth procedures (auth.me, auth.logout)
- [x] Phase 3: Create photo repository (SQLite implementation with CRUD operations)
- [x] Phase 3: Implement photo service (business logic for upload, list, reorder, delete)
- [x] Phase 3: Implement tRPC photo procedures (photo.list, photo.upload, photo.updatePositions, photo.delete, photo.deleteAll)
- [x] Phase 4: Create PDF job repository with atomic job claiming (UPDATE ... WHERE status='pending' LIMIT 5)
- [ ] Phase 4: Implement PDF worker (in-process goroutine, polls every 10s, proper locking)
- [ ] Phase 4: Implement PDF generation with gofpdf (A4 portrait, 10mm margins, aspect-fit images, match current algorithm)
- [ ] Phase 4: Implement tRPC PDF procedures (pdf.createJob, pdf.listJobs, pdf.getJob)
- [x] Phase 5: Implement tRPC router and handler system (parse JSON, route to procedures, return tRPC-compatible responses)
- [x] Phase 5: Implement HTTP middleware (authentication, logging, error recovery)
- [x] Phase 5: Wire everything together in main.go (dependency injection, server startup, graceful shutdown)
- [ ] Phase 5: Implement system.health tRPC procedure
- [ ] Phase 5: Test all 10 tRPC procedures end-to-end (verify payload shapes match current implementation)
- [ ] Phase 5: Verify frontend integration (point frontend tRPC client to Go backend, test all features)
- [ ] Phase 5: Add error handling with pkg/errors (wrap errors with context, return user-friendly messages)
- [ ] Phase 5: Implement input validation (validate tRPC procedure inputs, return clear error messages)
- [ ] Future: Add signed URL support to storage interface (HMAC signing for disk storage, pre-signed URLs for S3)
- [ ] Future: Implement OAuth adapter (Google OAuth provider, integrate with auth adapter interface)
- [ ] Future: Add S3 storage implementation (implement Storage interface for production deployment)
