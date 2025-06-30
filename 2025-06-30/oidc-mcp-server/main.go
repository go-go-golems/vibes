package main

import (
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
	"github.com/ory/fosite"
	"github.com/ory/fosite/compose"
	"github.com/rs/cors"
	"github.com/rs/zerolog"
)

const (
	serverPort = ":8080"
	baseURL    = "http://localhost:8080"
)

func main() {
	// Global secret for HMAC token signing (32 bytes)
	globalSecret := []byte("this-is-a-very-long-32-byte-secret-key-for-demo-purposes")

	// Fosite configuration
	config := &fosite.Config{
		AccessTokenLifespan:   time.Hour,
		RefreshTokenLifespan:  time.Hour * 24,
		AuthorizeCodeLifespan: time.Minute * 10,
		IDTokenLifespan:       time.Hour,
		GlobalSecret:          globalSecret,
		RotatedGlobalSecrets:  [][]byte{},
		HashCost:              12, // bcrypt cost for password hashing
		SendDebugMessagesToClients: true, // Enable debug messages for development
	}

	// Create in-memory store
	store := NewMemoryStore()

	// Create HMAC strategy for token generation
	hmacStrategy := compose.NewOAuth2HMACStrategy(config)

	// Compose OAuth2 provider with all the grant types and handlers we need
	oauth2Provider := compose.Compose(
		config,
		store,
		hmacStrategy,
		
		// Core OAuth2 grants
		compose.OAuth2AuthorizeExplicitFactory,    // Authorization Code Grant
		compose.OAuth2RefreshTokenGrantFactory,    // Refresh token support
		compose.OAuth2ClientCredentialsGrantFactory, // Client credentials grant
		
		// PKCE support (required for public clients)
		compose.OAuth2PKCEFactory,
		
		// Token introspection and revocation
		compose.OAuth2TokenIntrospectionFactory,
		compose.OAuth2TokenRevocationFactory,
	)

	// Create integrated OAuth + MCP server with SSE support
	logger := zerolog.New(os.Stdout).With().Timestamp().Logger()
	mcpServer := NewSimpleMCPServer(oauth2Provider, store, logger)

	// Set up HTTP router
	router := mux.NewRouter()

	// OAuth2/OIDC endpoints
	router.HandleFunc("/.well-known/oauth-authorization-server", WellKnownHandler(baseURL)).Methods("GET")
	router.HandleFunc("/.well-known/openid_configuration", WellKnownHandler(baseURL)).Methods("GET")
	router.HandleFunc("/register", RegisterClientHandler(store)).Methods("POST")
	router.HandleFunc("/authorize", AuthorizeHandler(oauth2Provider, store)).Methods("GET", "POST")
	router.HandleFunc("/token", TokenHandler(oauth2Provider)).Methods("POST")
	
	// Protected resource endpoint (demonstrates token validation)
	router.HandleFunc("/api/protected", ProtectedResourceHandler(oauth2Provider)).Methods("GET")
	
	// MCP-style protected endpoint
	router.HandleFunc("/v1/contexts", ProtectedResourceHandler(oauth2Provider)).Methods("GET")
	
	// Health check
	router.HandleFunc("/health", HealthHandler()).Methods("GET")
	
	// MCP endpoints (OAuth protected)
	mcpServer.SetupMCPRoutes(router)
	
	// Serve a simple index page with API documentation
	router.HandleFunc("/", IndexHandler()).Methods("GET")

	// Set up CORS to allow cross-origin requests
	c := cors.New(cors.Options{
		AllowedOrigins: []string{"*"}, // In production, specify actual origins
		AllowedMethods: []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders: []string{"*"},
		ExposedHeaders: []string{"*"},
		AllowCredentials: true,
	})

	// Wrap router with CORS
	handler := c.Handler(router)

	// Add logging middleware
	loggedHandler := loggingMiddleware(handler)

	log.Printf("🚀 Starting OIDC MCP Server on %s", serverPort)
	log.Printf("🌐 Server URL: %s", baseURL)
	log.Printf("📋 Available endpoints:")
	log.Printf("   OAuth2/OIDC:")
	log.Printf("     GET  /.well-known/oauth-authorization-server - Server metadata")
	log.Printf("     POST /register - Dynamic client registration")
	log.Printf("     GET  /authorize - Authorization endpoint")
	log.Printf("     POST /token - Token endpoint")
	log.Printf("   Protected Resources:")
	log.Printf("     GET  /api/protected - General protected resource")
	log.Printf("     GET  /v1/contexts - MCP-style protected resource")
	log.Printf("   MCP Server (OAuth Protected):")
	log.Printf("     GET  /mcp/info - MCP server information")
	log.Printf("     GET  /mcp/tools - List available tools")
	log.Printf("     POST /mcp/tools - Execute tools")
	log.Printf("     GET  /mcp/resources - List/get resources")
	log.Printf("     GET  /mcp/prompts - List available prompts")
	log.Printf("     GET  /mcp/sse - SSE transport for MCP (real-time)")
	log.Printf("   Utility:")
	log.Printf("     GET  /health - Health check")
	log.Printf("     GET  / - Documentation page")
	log.Printf("")
	log.Printf("🔐 All MCP endpoints are protected by OAuth2 Bearer tokens")
	log.Printf("👤 Demo user credentials: username=wesen, password=secret")
	log.Printf("🧪 Use the test_mcp_oauth.py script to test the full OAuth + MCP flow")

	// Start the server
	if err := http.ListenAndServe(serverPort, loggedHandler); err != nil {
		log.Fatalf("Server failed to start: %v", err)
	}
}

// IndexHandler serves a simple documentation page
func IndexHandler() http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		html := `
<!DOCTYPE html>
<html>
<head>
    <title>OIDC MCP Server</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 900px; margin: 50px auto; padding: 20px; line-height: 1.6; }
        h1 { color: #333; }
        h2 { color: #666; border-bottom: 1px solid #eee; padding-bottom: 10px; }
        .endpoint { background: #f5f5f5; padding: 15px; margin: 10px 0; border-radius: 5px; }
        .method { font-weight: bold; color: #007cba; }
        .url { font-family: monospace; background: #fff; padding: 5px; border: 1px solid #ddd; }
        .demo { background: #e8f4fd; padding: 15px; border-radius: 5px; margin: 20px 0; }
        .mcp { background: #f0f8ff; padding: 15px; border-radius: 5px; margin: 20px 0; border-left: 4px solid #007cba; }
        code { background: #f0f0f0; padding: 2px 4px; border-radius: 3px; font-family: monospace; }
        pre { background: #f8f8f8; padding: 10px; border-radius: 5px; overflow-x: auto; }
    </style>
</head>
<body>
    <h1>🚀 OIDC MCP Server</h1>
    <p>This is a demonstration OAuth2/OpenID Connect server with dynamic client registration, integrated with MCP (Model Context Protocol) functionality. All MCP endpoints are protected by OAuth2 Bearer tokens.</p>
    
    <div class="demo">
        <h3>🔑 Demo Credentials</h3>
        <p><strong>Username:</strong> wesen<br><strong>Password:</strong> secret</p>
    </div>

    <div class="mcp">
        <h3>🤖 MCP Integration</h3>
        <p>This server provides MCP tools, resources, and prompts that are protected by OAuth2 authentication. Clients must obtain a valid access token before accessing MCP endpoints.</p>
        <p><strong>Available Tools:</strong> calculator, weather, time<br>
        <strong>Available Resources:</strong> server config, stats, OAuth clients<br>
        <strong>Available Prompts:</strong> analyze-data, generate-report</p>
    </div>

    <h2>📡 OAuth2/OIDC Endpoints</h2>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/.well-known/oauth-authorization-server</div>
        <p>OAuth2 server metadata discovery endpoint</p>
    </div>

    <div class="endpoint">
        <div class="method">POST</div>
        <div class="url">/register</div>
        <p>Dynamic client registration endpoint (RFC 7591)</p>
        <p>Example request body:</p>
        <pre><code>{
  "redirect_uris": ["http://localhost:3000/callback"],
  "client_name": "My MCP Client",
  "grant_types": ["authorization_code"],
  "response_types": ["code"],
  "token_endpoint_auth_method": "none"
}</code></pre>
    </div>

    <div class="endpoint">
        <div class="method">GET/POST</div>
        <div class="url">/authorize</div>
        <p>OAuth2 authorization endpoint (with PKCE support)</p>
        <p>Example: <code>/authorize?response_type=code&client_id=CLIENT_ID&redirect_uri=REDIRECT_URI&code_challenge=CHALLENGE&code_challenge_method=S256</code></p>
    </div>

    <div class="endpoint">
        <div class="method">POST</div>
        <div class="url">/token</div>
        <p>OAuth2 token endpoint for exchanging authorization codes for access tokens</p>
    </div>

    <h2>🔒 Protected Resource Endpoints</h2>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/api/protected</div>
        <p>Protected resource requiring valid access token in Authorization header</p>
        <p>Example: <code>Authorization: Bearer ACCESS_TOKEN</code></p>
    </div>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/v1/contexts</div>
        <p>MCP-style protected endpoint (same as /api/protected)</p>
    </div>

    <h2>🤖 MCP Endpoints (OAuth Protected)</h2>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/mcp/info</div>
        <p>MCP server information and capabilities</p>
    </div>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/mcp/tools</div>
        <p>List available MCP tools</p>
    </div>

    <div class="endpoint">
        <div class="method">POST</div>
        <div class="url">/mcp/tools</div>
        <p>Execute MCP tools</p>
        <p>Example request body:</p>
        <pre><code>{
  "name": "calculator",
  "arguments": {
    "operation": "add",
    "a": 5,
    "b": 3
  }
}</code></pre>
    </div>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/mcp/resources</div>
        <p>List available MCP resources or get specific resource with ?uri=mcp://server/config</p>
    </div>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/mcp/prompts</div>
        <p>List available MCP prompts</p>
    </div>

    <h2>🔧 Utility Endpoints</h2>

    <div class="endpoint">
        <div class="method">GET</div>
        <div class="url">/health</div>
        <p>Health check endpoint</p>
    </div>

    <h2>🔄 OAuth2 + MCP Flow Example</h2>
    <ol>
        <li>Register a client using <code>POST /register</code></li>
        <li>Redirect user to <code>/authorize</code> with appropriate parameters</li>
        <li>User logs in with username=wesen, password=secret</li>
        <li>Exchange authorization code for access token at <code>/token</code></li>
        <li>Use access token to access MCP endpoints like <code>/mcp/tools</code></li>
        <li>Execute MCP tools with <code>POST /mcp/tools</code></li>
    </ol>

    <p><a href="/health">Health Check</a> | <a href="/.well-known/oauth-authorization-server">Server Metadata</a> | <a href="/mcp/info">MCP Info</a></p>
</body>
</html>`

		w.Header().Set("Content-Type", "text/html")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(html))
	}
}

// loggingMiddleware logs HTTP requests
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		
		// Create a response writer wrapper to capture status code
		wrapper := &responseWriter{ResponseWriter: w, statusCode: http.StatusOK}
		
		next.ServeHTTP(wrapper, r)
		
		duration := time.Since(start)
		log.Printf("%s %s %d %v", r.Method, r.URL.Path, wrapper.statusCode, duration)
	})
}

// responseWriter wraps http.ResponseWriter to capture status code
type responseWriter struct {
	http.ResponseWriter
	statusCode int
}

func (rw *responseWriter) WriteHeader(code int) {
	rw.statusCode = code
	rw.ResponseWriter.WriteHeader(code)
}

