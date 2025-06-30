# OIDC MCP Server - Quick Start Guide

## Installation

### Prerequisites
- Go 1.21 or later
- Python 3.7+ (for testing scripts)

### Setup Instructions

1. **Extract the project**
   ```bash
   unzip oidc-mcp-server.zip
   cd oidc-mcp-server
   ```

2. **Install Go dependencies**
   ```bash
   go mod tidy
   ```

3. **Build the server**
   ```bash
   go build
   ```

4. **Run the server**
   ```bash
   ./oidc-mcp-server
   ```

The server will start on `http://localhost:8080`

## Quick Test

1. **Test OAuth flow**
   ```bash
   python3 test_mcp_oauth.py
   ```

2. **Test MCP endpoints**
   ```bash
   # Get server info
   curl http://localhost:8080/mcp/info

   # List available tools
   curl http://localhost:8080/mcp/tools

   # List available resources  
   curl http://localhost:8080/mcp/resources
   ```

3. **Test SSE transport**
   ```bash
   curl -N http://localhost:8080/mcp/sse
   ```

## Demo Credentials
- Username: `wesen`
- Password: `secret`

## Key Features
- ✅ OAuth2/OIDC with dynamic client registration
- ✅ MCP server with HTTP and SSE transports
- ✅ Mock tools (calculator, weather, time)
- ✅ OAuth-protected resources and endpoints
- ✅ Comprehensive API documentation

## API Endpoints
- `/.well-known/oauth-authorization-server` - Server metadata
- `/register` - Dynamic client registration
- `/authorize` - Authorization endpoint
- `/token` - Token endpoint
- `/mcp/*` - MCP server endpoints
- `/mcp/sse` - SSE transport for real-time MCP

For complete documentation, see README.md

