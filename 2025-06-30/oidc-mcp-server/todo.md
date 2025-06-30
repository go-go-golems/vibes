# OIDC Dynamic Client Registration Server for MCP - Implementation Todo

## Phase 1: Set up Go project structure and dependencies
- [x] Install latest Go toolchain from golang.org
- [x] Create project directory structure
- [x] Initialize Go module
- [x] Add Fosite and other required dependencies
- [x] Create basic project files (main.go, handlers.go, store.go)

## Phase 2: Implement core OIDC server with Fosite library
- [x] Implement in-memory storage for clients, users, and sessions
- [x] Set up Fosite configuration with OAuth2 providers
- [x] Implement basic user authentication (wesen/secret)
- [x] Create authorization endpoint handler
- [x] Create token endpoint handler

## Phase 3: Implement dynamic client registration endpoint
- [x] Create client registration endpoint (/register)
- [x] Implement client validation and storage
- [x] Add proper error handling and responses
- [x] Test client registration flow

## Phase 4: Test REST endpoints with manual HTTP requests
- [x] Test client registration endpoint
- [x] Test authorization flow with registered client
- [x] Test token exchange
- [x] Test protected resource access with tokens
- [x] Verify PKCE implementation

## Phase 5: Search for and test with MCP client
- [x] Search for available MCP clients
- [x] Set up MCP client for testing
- [x] Test full OAuth flow with MCP client
- [x] Verify MCP protocol compliance

## Phase 6: Document and deliver final implementation
- [ ] Create comprehensive documentation
- [ ] Package final code with examples
- [ ] Provide testing instructions
- [ ] Deliver complete implementation

