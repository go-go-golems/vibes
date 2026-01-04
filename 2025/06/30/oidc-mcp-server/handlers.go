package main

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"

	"github.com/google/uuid"
	"github.com/ory/fosite"
)

// ClientRegistrationRequest represents a dynamic client registration request
type ClientRegistrationRequest struct {
	RedirectURIs                []string `json:"redirect_uris"`
	TokenEndpointAuthMethod     string   `json:"token_endpoint_auth_method,omitempty"`
	GrantTypes                  []string `json:"grant_types,omitempty"`
	ResponseTypes               []string `json:"response_types,omitempty"`
	ClientName                  string   `json:"client_name,omitempty"`
	ClientURI                   string   `json:"client_uri,omitempty"`
	LogoURI                     string   `json:"logo_uri,omitempty"`
	Scope                       string   `json:"scope,omitempty"`
	Contacts                    []string `json:"contacts,omitempty"`
	TosURI                      string   `json:"tos_uri,omitempty"`
	PolicyURI                   string   `json:"policy_uri,omitempty"`
	JwksURI                     string   `json:"jwks_uri,omitempty"`
	SoftwareID                  string   `json:"software_id,omitempty"`
	SoftwareVersion             string   `json:"software_version,omitempty"`
}

// ClientRegistrationResponse represents the response to a client registration
type ClientRegistrationResponse struct {
	ClientID                    string   `json:"client_id"`
	ClientSecret                string   `json:"client_secret,omitempty"`
	ClientIDIssuedAt            int64    `json:"client_id_issued_at,omitempty"`
	ClientSecretExpiresAt       int64    `json:"client_secret_expires_at,omitempty"`
	RedirectURIs                []string `json:"redirect_uris"`
	TokenEndpointAuthMethod     string   `json:"token_endpoint_auth_method"`
	GrantTypes                  []string `json:"grant_types"`
	ResponseTypes               []string `json:"response_types"`
	ClientName                  string   `json:"client_name,omitempty"`
	ClientURI                   string   `json:"client_uri,omitempty"`
	LogoURI                     string   `json:"logo_uri,omitempty"`
	Scope                       string   `json:"scope,omitempty"`
	Contacts                    []string `json:"contacts,omitempty"`
	TosURI                      string   `json:"tos_uri,omitempty"`
	PolicyURI                   string   `json:"policy_uri,omitempty"`
	JwksURI                     string   `json:"jwks_uri,omitempty"`
	SoftwareID                  string   `json:"software_id,omitempty"`
	SoftwareVersion             string   `json:"software_version,omitempty"`
}

// RegisterClientHandler handles dynamic client registration
func RegisterClientHandler(store *MemoryStore) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		var req ClientRegistrationRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			http.Error(w, "Invalid JSON", http.StatusBadRequest)
			return
		}

		// Validate required fields
		if len(req.RedirectURIs) == 0 {
			http.Error(w, "redirect_uris is required", http.StatusBadRequest)
			return
		}

		// Set defaults
		if len(req.GrantTypes) == 0 {
			req.GrantTypes = []string{"authorization_code"}
		}
		if len(req.ResponseTypes) == 0 {
			req.ResponseTypes = []string{"code"}
		}
		if req.TokenEndpointAuthMethod == "" {
			req.TokenEndpointAuthMethod = "none" // Public client by default
		}

		// Generate client credentials
		clientID := uuid.New().String()
		var clientSecret string
		isPublic := req.TokenEndpointAuthMethod == "none"

		if !isPublic {
			clientSecret = uuid.New().String()
		}

		// Create client
		client := &fosite.DefaultClient{
			ID:            clientID,
			Secret:        []byte(clientSecret),
			RedirectURIs:  req.RedirectURIs,
			GrantTypes:    req.GrantTypes,
			ResponseTypes: req.ResponseTypes,
			Scopes:        strings.Fields(req.Scope),
			Public:        isPublic,
		}

		// Store the client
		if err := store.RegisterClient(client); err != nil {
			http.Error(w, "Failed to register client", http.StatusInternalServerError)
			return
		}

		// Prepare response
		response := ClientRegistrationResponse{
			ClientID:                clientID,
			RedirectURIs:            req.RedirectURIs,
			TokenEndpointAuthMethod: req.TokenEndpointAuthMethod,
			GrantTypes:              req.GrantTypes,
			ResponseTypes:           req.ResponseTypes,
			ClientName:              req.ClientName,
			ClientURI:               req.ClientURI,
			LogoURI:                 req.LogoURI,
			Scope:                   req.Scope,
			Contacts:                req.Contacts,
			TosURI:                  req.TosURI,
			PolicyURI:               req.PolicyURI,
			JwksURI:                 req.JwksURI,
			SoftwareID:              req.SoftwareID,
			SoftwareVersion:         req.SoftwareVersion,
		}

		if !isPublic {
			response.ClientSecret = clientSecret
		}

		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusCreated)
		json.NewEncoder(w).Encode(response)
	}
}

// AuthorizeHandler handles the OAuth2 authorization endpoint
func AuthorizeHandler(oauth2Provider fosite.OAuth2Provider, store *MemoryStore) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		ctx := context.Background()

		// Parse the authorization request
		ar, err := oauth2Provider.NewAuthorizeRequest(ctx, r)
		if err != nil {
			oauth2Provider.WriteAuthorizeError(ctx, w, ar, err)
			return
		}

		// Handle GET request - show login form
		if r.Method == http.MethodGet {
			showLoginForm(w, r, ar)
			return
		}

		// Handle POST request - process login
		if r.Method == http.MethodPost {
			username := r.FormValue("username")
			password := r.FormValue("password")

			// Authenticate user
			user, err := store.AuthenticateUser(username, password)
			if err != nil {
				showLoginForm(w, r, ar, "Invalid username or password")
				return
			}

			// Create session
			session := &UserSession{
				Username: user.Username,
				Subject:  user.ID,
			}

			// Handle the authorization request
			response, err := oauth2Provider.NewAuthorizeResponse(ctx, ar, session)
			if err != nil {
				oauth2Provider.WriteAuthorizeError(ctx, w, ar, err)
				return
			}

			// Write the response (redirect to client)
			oauth2Provider.WriteAuthorizeResponse(ctx, w, ar, response)
			return
		}

		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

// TokenHandler handles the OAuth2 token endpoint
func TokenHandler(oauth2Provider fosite.OAuth2Provider) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		ctx := context.Background()

		// Create a new session
		session := &UserSession{}

		// Create access token request
		accessRequest, err := oauth2Provider.NewAccessRequest(ctx, r, session)
		if err != nil {
			oauth2Provider.WriteAccessError(ctx, w, accessRequest, err)
			return
		}

		// Create access token response
		response, err := oauth2Provider.NewAccessResponse(ctx, accessRequest)
		if err != nil {
			oauth2Provider.WriteAccessError(ctx, w, accessRequest, err)
			return
		}

		// Write the response
		oauth2Provider.WriteAccessResponse(ctx, w, accessRequest, response)
	}
}

// ProtectedResourceHandler demonstrates a protected resource that requires a valid access token
func ProtectedResourceHandler(oauth2Provider fosite.OAuth2Provider) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		ctx := context.Background()

		// Extract token from Authorization header
		token := fosite.AccessTokenFromRequest(r)
		if token == "" {
			http.Error(w, "Missing access token", http.StatusUnauthorized)
			return
		}

		// Create a session for token validation
		session := &UserSession{}

		// Validate the access token
		_, ar, err := oauth2Provider.IntrospectToken(ctx, token, fosite.AccessToken, session)
		if err != nil {
			http.Error(w, "Invalid access token", http.StatusUnauthorized)
			return
		}

		// Token is valid, return protected resource
		response := map[string]interface{}{
			"message":    "Hello from protected resource!",
			"user":       session.Username,
			"client_id":  ar.GetClient().GetID(),
			"scopes":     ar.GetGrantedScopes(),
			"timestamp":  "2025-06-30T00:00:00Z",
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)
	}
}

// showLoginForm displays the login form
func showLoginForm(w http.ResponseWriter, r *http.Request, ar fosite.AuthorizeRequester, errorMsg ...string) {
	// Build the form action URL with all query parameters preserved
	formAction := "/authorize?" + r.URL.RawQuery

	var errorHTML string
	if len(errorMsg) > 0 && errorMsg[0] != "" {
		errorHTML = fmt.Sprintf(`<div style="color: red; margin-bottom: 10px;">%s</div>`, errorMsg[0])
	}

	loginHTML := fmt.Sprintf(`
<!DOCTYPE html>
<html>
<head>
    <title>Login - OIDC MCP Server</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 400px; margin: 100px auto; padding: 20px; }
        .form-group { margin-bottom: 15px; }
        label { display: block; margin-bottom: 5px; }
        input[type="text"], input[type="password"] { width: 100%%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; }
        button { background: #007cba; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; width: 100%%; }
        button:hover { background: #005a87; }
        .info { background: #f0f8ff; padding: 10px; border-radius: 4px; margin-bottom: 20px; }
    </style>
</head>
<body>
    <h2>Login Required</h2>
    <div class="info">
        <strong>Client:</strong> %s<br>
        <strong>Scopes:</strong> %s<br>
        <strong>Redirect URI:</strong> %s
    </div>
    %s
    <form method="post" action="%s">
        <div class="form-group">
            <label for="username">Username:</label>
            <input type="text" id="username" name="username" required placeholder="Enter username (hint: wesen)">
        </div>
        <div class="form-group">
            <label for="password">Password:</label>
            <input type="password" id="password" name="password" required placeholder="Enter password (hint: secret)">
        </div>
        <button type="submit">Login and Authorize</button>
    </form>
    <p style="color: #666; font-size: 12px; margin-top: 20px;">
        Demo credentials: username=<strong>wesen</strong>, password=<strong>secret</strong>
    </p>
</body>
</html>`, 
		ar.GetClient().GetID(),
		strings.Join(ar.GetRequestedScopes(), " "),
		ar.GetRedirectURI().String(),
		errorHTML,
		formAction)

	w.Header().Set("Content-Type", "text/html")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(loginHTML))
}

// WellKnownHandler provides OAuth2 server metadata
func WellKnownHandler(baseURL string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		metadata := map[string]interface{}{
			"issuer":                 baseURL,
			"authorization_endpoint": baseURL + "/authorize",
			"token_endpoint":         baseURL + "/token",
			"registration_endpoint":  baseURL + "/register",
			"scopes_supported":       []string{"openid", "profile", "email"},
			"response_types_supported": []string{"code"},
			"grant_types_supported": []string{"authorization_code", "refresh_token"},
			"token_endpoint_auth_methods_supported": []string{"none", "client_secret_basic", "client_secret_post"},
			"code_challenge_methods_supported": []string{"S256", "plain"},
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(metadata)
	}
}

// HealthHandler provides a simple health check endpoint
func HealthHandler() http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		response := map[string]string{
			"status":  "healthy",
			"service": "oidc-mcp-server",
			"version": "1.0.0",
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(response)
	}
}

