---
Title: JS to Go Porting Playbook - Lessons Learned and Best Practices
Ticket: PORT-001
Status: active
Topics:
    - backend
    - frontend
DocType: playbook
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/29/photobook-app/server/_core/index.ts
      Note: Original Node.js implementation for comparison
    - Path: 2025/11/29/photobook-backend-go
      Note: Complete Go backend implementation
ExternalSources: []
Summary: Comprehensive playbook documenting the process, decisions, lessons learned, and best practices for porting Node.js/TypeScript backends to Go
LastUpdated: 2025-11-30T14:00:00-05:00
---


# JS to Go Porting Playbook - Lessons Learned and Best Practices

## Purpose

This playbook documents the complete process of porting a Node.js/TypeScript backend to Go, including architectural decisions, implementation patterns, what worked, what didn't, and best practices for future similar ports. This is designed as a reusable guide for porting similar applications with tRPC APIs, SQL databases, file storage, and background workers.

## Environment Assumptions

- **Source**: Node.js/TypeScript backend with Express, tRPC, MySQL, external storage proxy
- **Target**: Go backend with SQLite (dev) / MySQL (prod), filesystem storage (dev) / S3 (prod)
- **Frontend**: React with tRPC hooks (must remain unchanged)
- **Goal**: Zero frontend changes, maintain API compatibility

---

## Phase-by-Phase Implementation Diary

### Phase 1: Foundation Setup

#### What We Did

1. **Project Structure Setup**
   - Created standard Go layout: `cmd/`, `internal/`, `pkg/`
   - Set up Go module and workspace integration
   - Organized by domain (auth, photos, pdfjobs) not by layer

2. **Configuration System**
   - Implemented typed config struct with environment variable loading
   - Added validation for required fields (JWT_SECRET)
   - Used defaults for optional fields (DATABASE_URL, PORT, etc.)

3. **Database Setup**
   - Chose SQLite for local dev (zero dependencies)
   - Used Goose for migrations (familiar, works well with SQLite)
   - Created migrations for users, photos, pdf_jobs tables
   - Added automatic directory creation for database path

4. **Storage Interface**
   - Defined Storage interface early (Put, Open, Delete, GetSignedURL)
   - Implemented DiskStorage first (simplest)
   - Designed for easy swap to S3 later

5. **Logging**
   - Set up zerolog (structured logging)
   - Simple initialization pattern

6. **CLI Verbs**
   - Created CLI commands using Glazed framework
   - Commands: `config validate`, `db status`, `storage put/list`
   - Used structured output (JSON/table) for validation

#### What Worked Well

✅ **Starting with interfaces**: Defining Storage interface before implementation made it clear what we needed and allowed easy testing

✅ **CLI-first validation**: Creating CLI verbs immediately after each component let us validate functionality in isolation before integration

✅ **SQLite for dev**: Zero external dependencies made local development trivial

✅ **Goose migrations**: Simple, works well, familiar pattern

✅ **Go workspace**: Using `go.work` allowed working with multiple modules (glazed, zine-layout) in same repo

#### What Didn't Work / Challenges

❌ **Database path handling**: Initially forgot to create directory before opening SQLite database, got "out of memory" error (SQLite error code 14). Fixed by adding `os.MkdirAll()` before `sql.Open()`

❌ **Migration path handling**: Goose needs relative paths from module root, had to ensure migrations directory structure was correct

❌ **Go workspace path updates**: After moving project directory, had to manually update `go.work` file

#### What We Learned

- **SQLite error codes**: Error 14 = "out of memory" but often means file/directory issues
- **Go workspace management**: `go work use` and `go work edit` commands are essential when moving modules
- **Interface-first design**: Even for MVP, defining interfaces early pays off
- **CLI validation pattern**: Building CLI verbs immediately catches integration issues early

#### What We'd Do Differently

- **Add database directory creation to config loading**: Should be part of config validation, not database opening
- **Use absolute paths in go.work**: Relative paths break when moving directories
- **Add more validation**: Should validate database URL format, port ranges, etc. in config

---

### Phase 2: Authentication

#### What We Did

1. **JWT Service**
   - Implemented JWT signing/verification with `golang-jwt/jwt/v5`
   - Used HS256 algorithm (matches Node.js implementation)
   - Session claims: `openId`, `appId`, `name` (matches current format)
   - 1-year expiration (matches current)

2. **Auth Adapter Pattern**
   - Created `AuthAdapter` interface for pluggable authentication
   - Implemented `EmailPasswordAdapter` first
   - Designed for future OAuth adapters

3. **Password Hashing**
   - Used `golang.org/x/crypto/bcrypt` for password hashing
   - Stored password hash in users table (added migration)
   - Verified passwords on login

4. **User Repository**
   - Implemented SQLite user repository
   - Methods: Create, GetByOpenID, GetByEmail, GetByID, Update, UpdateLastSignedIn
   - Added GetPasswordHash method

5. **Auth Service**
   - Coordinated adapters and JWT service
   - Implemented Register, Login, Me methods
   - Admin role assignment via OWNER_OPEN_ID env var

6. **CLI Verbs**
   - `auth register`: Create new user, return JWT
   - `auth login`: Authenticate, return JWT
   - `auth me`: Verify token, return user info

#### What Worked Well

✅ **Adapter pattern**: Made it easy to add email/password auth without coupling to JWT or repository

✅ **Repository interface**: Separated data access from business logic

✅ **JWT format matching**: Using same claims structure (`openId`, `appId`, `name`) ensures compatibility

✅ **Bcrypt integration**: Standard library crypto package worked seamlessly

✅ **Migration for password hash**: Adding column via migration was clean

#### What Didn't Work / Challenges

❌ **SQLite ALTER TABLE limitations**: SQLite doesn't support DROP COLUMN, had to use workaround in migration down path (recreate table)

❌ **Password hash storage**: Initially forgot to update repository interface, had to add passwordHash parameter to Create method

❌ **JWT secret encoding**: Node.js uses `TextEncoder().encode(secret)` (UTF-8 bytes), Go uses `[]byte(secret)` directly - had to verify they match (they do)

#### What We Learned

- **SQLite migration limitations**: Need to plan migrations carefully, especially for schema changes
- **Interface evolution**: Adding password hash required updating interface signature - better to plan ahead
- **JWT compatibility**: Same algorithm and claims structure ensures tokens work across implementations
- **Bcrypt cost**: Default cost (10) is fine for MVP, can be configurable later

#### What We'd Do Differently

- **Plan password storage from start**: Should have included password_hash in initial users table migration
- **Add password validation**: Should validate password strength, email format during registration
- **Add rate limiting**: Should implement rate limiting for login attempts
- **Add session management**: Consider storing sessions in database for revocation capability

---

## Key Architectural Decisions

### Decision 1: Maintain tRPC Compatibility

**What**: Keep tRPC-compatible JSON-over-HTTP endpoints instead of switching to REST.

**Why**: 
- Frontend already built with tRPC hooks (10 procedures)
- Zero frontend changes required
- Faster path to MVP

**How**: Accept `/api/trpc/<procedure>` POST requests, return tRPC-compatible JSON responses.

**Result**: ✅ Correct decision - allows incremental migration without frontend changes

**For Future Ports**: If frontend uses tRPC, maintain compatibility. If not, consider REST/GraphQL.

### Decision 2: SQLite + Filesystem Storage (Dev)

**What**: Start with SQLite database and filesystem storage for local development.

**Why**:
- Zero external dependencies
- Simple deployment (`go run main.go`)
- Pluggable interfaces allow swapping implementations

**How**: 
- SQLite: `./data/app.db`
- Filesystem: `./data/storage/`
- Interfaces allow swapping to MySQL/S3 later

**Result**: ✅ Excellent for development, production-ready interfaces

**For Future Ports**: Always start with simplest implementation, design interfaces from day one

### Decision 3: Single Binary, In-Process Worker

**What**: Run PDF worker as in-process goroutine within API server.

**Why**:
- Simple deployment (one binary, one process)
- Good enough for MVP (single instance)
- Go goroutines handle concurrency well

**How**: Worker runs as background goroutine, polls every 10 seconds, proper locking via atomic status updates.

**Result**: ✅ Simple and effective for MVP

**For Future Ports**: Start simple, add queue abstraction only if needed for multiple instances

### Decision 4: Direct Database Operations (No Queue Abstraction)

**What**: Use direct database operations with atomic locking, no formal queue abstraction.

**Why**:
- Single instance doesn't need queue abstraction
- Atomic status updates provide locking
- Simple implementation (~50 lines)

**How**: `UPDATE pdf_jobs SET status = 'processing' WHERE status = 'pending' LIMIT 5 RETURNING *`

**Result**: ✅ Works perfectly for single instance

**For Future Ports**: Add queue abstraction only when scaling to multiple instances

### Decision 5: Email/Password Auth First, Design for OAuth Adapters

**What**: Implement email/password authentication first, design for OAuth adapters.

**Why**:
- Simple, no external dependencies
- Removes OAuth dependency
- Adapter pattern allows adding OAuth providers later

**How**: Core auth service + adapters for different authentication methods.

**Result**: ✅ Clean separation, easy to extend

**For Future Ports**: Always use adapter pattern for authentication - makes adding providers trivial

---

## Implementation Patterns That Worked

### Pattern 1: Interface-First Design

**What**: Define interfaces before implementations.

**Example**:
```go
type Storage interface {
    Put(ctx context.Context, relKey string, r io.Reader, contentType string) (url string, err error)
    Open(ctx context.Context, relKey string) (io.ReadCloser, error)
    Delete(ctx context.Context, relKey string) error
}
```

**Why It Worked**: Made it clear what we needed, allowed testing with mocks, easy to swap implementations.

**For Future Ports**: Always define interfaces first, even for MVP.

### Pattern 2: Repository Pattern

**What**: Separate data access logic from business logic.

**Example**:
```go
type UserRepository interface {
    Create(ctx context.Context, user *types.User, passwordHash *string) (int64, error)
    GetByOpenID(ctx context.Context, openID string) (*types.User, error)
    // ...
}
```

**Why It Worked**: Made code testable, allowed swapping database implementations, clear separation of concerns.

**For Future Ports**: Use repository pattern for all data access.

### Pattern 3: Service Layer

**What**: Business logic lives in service layer, not in handlers or repositories.

**Example**:
```go
type AuthService struct {
    repo    UserRepository
    adapter AuthAdapter
    jwt     *JWTService
}

func (s *AuthService) Register(ctx context.Context, email, password, name string) (*types.User, string, error) {
    // Business logic here
}
```

**Why It Worked**: Keeps handlers thin (just HTTP concerns), repositories focused (just data access), services contain business rules.

**For Future Ports**: Always use service layer pattern.

### Pattern 4: CLI-First Validation

**What**: Create CLI verbs immediately after implementing each component.

**Why It Worked**: 
- Validates functionality in isolation
- Catches integration issues early
- Provides manual testing tools
- Documents usage patterns

**For Future Ports**: Build CLI verbs for every major component - invaluable for debugging and validation.

### Pattern 5: Domain-Driven Organization

**What**: Organize by domain (auth, photos, pdfjobs) not by layer (handlers, services, repos).

**Structure**:
```
internal/
  auth/          # All auth-related code
    jwt.go
    adapter.go
    password.go
    service.go
    repository.go
  photos/        # All photo-related code
    service.go
    repository.go
```

**Why It Worked**: Easier to find related code, clearer boundaries, scales better.

**For Future Ports**: Organize by domain, not by technical layer.

---

## Common Pitfalls and How to Avoid Them

### Pitfall 1: Forgetting Directory Creation

**Problem**: SQLite database or storage directory doesn't exist, get cryptic errors.

**Solution**: Always create directories in config loading or initialization:
```go
if err := os.MkdirAll(cfg.StoragePath, 0755); err != nil {
    return nil, fmt.Errorf("failed to create storage path: %w", err)
}
```

**For Future Ports**: Create all required directories during initialization, not on first use.

### Pitfall 2: Go Workspace Path Issues

**Problem**: Moving project directory breaks `go.work` file.

**Solution**: Use `go work edit -dropuse` and `go work use` to update paths, or use absolute paths.

**For Future Ports**: Keep modules in stable locations, or use absolute paths in go.work.

### Pitfall 3: Interface Evolution

**Problem**: Adding new features requires changing interface signatures, breaking existing code.

**Solution**: Plan interfaces carefully, include optional parameters (pointers for nullable values).

**Example**: `Create(ctx context.Context, user *types.User, passwordHash *string)` - passwordHash is optional.

**For Future Ports**: Design interfaces with extensibility in mind, use pointers for optional parameters.

### Pitfall 4: SQLite Migration Limitations

**Problem**: SQLite doesn't support DROP COLUMN, making migrations complex.

**Solution**: Plan migrations carefully, use table recreation for complex changes, document limitations.

**For Future Ports**: Consider using PostgreSQL for production from start if complex migrations are expected.

### Pitfall 5: JWT Secret Encoding Mismatch

**Problem**: Node.js and Go might encode JWT secrets differently.

**Solution**: Verify encoding matches - Node.js `TextEncoder().encode()` = Go `[]byte()` for UTF-8 strings.

**For Future Ports**: Test JWT tokens across implementations, verify algorithm and encoding match.

---

## Testing Strategy

### What We Did

1. **CLI Verbs for Manual Testing**
   - Created CLI commands for each component
   - Used structured output (JSON/table) for validation
   - Tested end-to-end flows manually

2. **Incremental Validation**
   - Tested each component immediately after implementation
   - Validated integration points early
   - Fixed issues before moving to next phase

### What Worked

✅ **CLI-first testing**: Manual testing via CLI caught issues early

✅ **Incremental validation**: Testing each component prevented cascading failures

✅ **Structured output**: JSON output made it easy to verify data structures

### What We'd Add

- **Unit tests**: Add Go tests for each component
- **Integration tests**: Test database operations, storage operations
- **End-to-end tests**: Test full flows (register → login → upload photo → create PDF job)

**For Future Ports**: 
- Write tests alongside implementation
- Use table-driven tests for Go
- Test interfaces with mocks
- Add integration tests for database operations

---

## Migration Checklist

Use this checklist for future JS-to-Go ports:

### Pre-Implementation

- [ ] Analyze current architecture and data flow
- [ ] Map frontend API calls to backend endpoints
- [ ] Document current authentication/session format
- [ ] Identify external dependencies (databases, storage, OAuth)
- [ ] Decide on API compatibility (maintain tRPC? switch to REST?)
- [ ] Choose initial database (SQLite for dev, MySQL/Postgres for prod?)
- [ ] Choose initial storage (filesystem for dev, S3 for prod?)

### Phase 1: Foundation

- [ ] Set up Go project structure (cmd/, internal/, pkg/)
- [ ] Implement configuration system (env vars, validation)
- [ ] Set up database connection and migrations
- [ ] Create database migrations (all tables)
- [ ] Implement storage interface and initial implementation
- [ ] Set up structured logging
- [ ] Create CLI verbs for validation

### Phase 2: Authentication

- [ ] Implement JWT service (match current format)
- [ ] Create auth adapter interface
- [ ] Implement email/password adapter
- [ ] Create user repository
- [ ] Implement auth service
- [ ] Add password storage to database
- [ ] Create CLI verbs for auth (register, login, me)

### Phase 3: Core Features

- [ ] Create repositories for each domain
- [ ] Implement services for each domain
- [ ] Create CLI verbs for each feature
- [ ] Test each feature in isolation

### Phase 4: Background Workers

- [ ] Implement worker repository with atomic locking
- [ ] Create worker service
- [ ] Implement worker goroutine
- [ ] Test worker with CLI verbs

### Phase 5: API Layer

- [ ] Implement tRPC router/handler
- [ ] Create HTTP middleware (auth, logging, error recovery)
- [ ] Wire everything together in main.go
- [ ] Test all endpoints end-to-end
- [ ] Verify frontend integration

### Post-Implementation

- [ ] Document all decisions and patterns
- [ ] Create playbook for future ports
- [ ] Update changelog and tasks
- [ ] Relate all files to documentation

---

## Technology Mapping

### Node.js → Go Equivalents

| Node.js | Go | Notes |
|---------|-----|-------|
| Express | net/http + custom router | Standard library is sufficient for simple APIs |
| tRPC | Custom tRPC handler | Need to implement tRPC protocol compatibility |
| Drizzle ORM | database/sql + manual queries | Go's standard approach, more verbose but explicit |
| MySQL2 | database/sql + driver | Standard interface, swap drivers easily |
| jose (JWT) | golang-jwt/jwt/v5 | Similar API, same algorithms |
| bcrypt | golang.org/x/crypto/bcrypt | Standard library crypto package |
| Axios | net/http | Standard library HTTP client |
| Winston/Pino | github.com/rs/zerolog | Structured logging, similar API |
| node-cron | time.Ticker + goroutines | Go's concurrency model replaces cron |

### Database

| Node.js | Go | Notes |
|---------|-----|-------|
| Drizzle ORM | database/sql | More verbose but explicit, better type safety |
| MySQL | SQLite (dev) / MySQL (prod) | Use same database/sql interface |
| Migrations | Goose | Simple, works well with SQLite |

### Storage

| Node.js | Go | Notes |
|---------|-----|-------|
| Forge storage proxy | DiskStorage (dev) / S3 (prod) | Interface allows swapping implementations |
| S3 SDK | AWS SDK for Go | Similar API, works with interface |

---

## Performance Considerations

### What We Observed

- **Startup time**: Go binary starts instantly vs Node.js server startup
- **Memory usage**: Go uses less memory (single binary vs Node.js runtime)
- **Concurrency**: Goroutines handle concurrent requests better than Node.js event loop
- **Database connections**: Go's connection pooling is simpler and more efficient

### For Future Ports

- **Connection pooling**: Use `sql.DB` connection pool settings
- **Goroutine limits**: Consider worker pools for background jobs
- **Memory profiling**: Use `go tool pprof` for memory analysis
- **CPU profiling**: Use `go tool pprof` for CPU analysis

---

## Deployment Considerations

### Development

- **Single binary**: `go build` creates single executable
- **Zero dependencies**: SQLite + filesystem = no external services needed
- **Easy testing**: CLI verbs make manual testing trivial

### Production

- **Database**: Swap SQLite for MySQL/Postgres (same interface)
- **Storage**: Swap DiskStorage for S3 (same interface)
- **Deployment**: Single binary deployment vs Node.js + dependencies
- **Scaling**: Add queue abstraction if multiple instances needed

### For Future Ports

- **Containerization**: Use Docker for consistent environments
- **Configuration**: Use environment variables, consider config files for complex setups
- **Monitoring**: Add structured logging, metrics, tracing
- **Health checks**: Implement `/health` endpoint

---

## Lessons Learned Summary

### What Worked Exceptionally Well

1. **Interface-first design**: Made swapping implementations trivial
2. **CLI-first validation**: Caught issues early, provided debugging tools
3. **Domain-driven organization**: Made code easier to navigate and understand
4. **Incremental implementation**: Testing each phase prevented cascading failures
5. **Go workspace**: Made working with multiple modules seamless

### What Was Challenging

1. **SQLite migration limitations**: Need careful planning for schema changes
2. **Go workspace path management**: Moving directories requires manual updates
3. **JWT compatibility**: Had to verify encoding matches between Node.js and Go
4. **Interface evolution**: Adding features sometimes required interface changes

### What We'd Do Differently

1. **Plan password storage from start**: Include in initial migration
2. **Add more validation**: Validate inputs more thoroughly
3. **Write tests earlier**: Add unit tests alongside implementation
4. **Document decisions earlier**: Capture decisions as they're made

### Key Takeaways for Future Ports

1. **Start simple**: Use simplest implementation (SQLite, filesystem) for dev
2. **Design interfaces early**: Even for MVP, define interfaces first
3. **Build CLI verbs**: Create CLI commands for every major component
4. **Test incrementally**: Validate each component before moving forward
5. **Document decisions**: Capture architectural decisions and rationale
6. **Plan for change**: Design interfaces to be extensible
7. **Match existing formats**: Maintain compatibility (JWT format, API responses)

---

## Exit Criteria

This playbook is complete when:

- [x] All phases documented with what worked/didn't work
- [x] Key architectural decisions documented with rationale
- [x] Implementation patterns documented with examples
- [x] Common pitfalls documented with solutions
- [x] Migration checklist created
- [x] Technology mapping documented
- [x] Lessons learned summarized

---

## Future Enhancements

For future ports, consider:

1. **Add unit tests**: Document testing patterns
2. **Add integration tests**: Document integration testing approach
3. **Add performance benchmarks**: Compare Node.js vs Go performance
4. **Add deployment guides**: Document production deployment process
5. **Add monitoring setup**: Document logging, metrics, tracing setup

---

## Related Documentation

- [Go Backend Implementation Guide](../design-doc/02-go-backend-implementation-guide.md)
- [Current Architecture Reference](../reference/01-current-architecture-and-data-flow.md)
- [Debate Synthesis](../reference/04-debate-synthesis-and-decisions.md)
