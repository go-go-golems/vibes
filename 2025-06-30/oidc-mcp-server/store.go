package main

import (
	"context"
	"crypto/sha256"
	"fmt"
	"sync"
	"time"

	"github.com/ory/fosite"
	"github.com/pkg/errors"
)

// User represents a user in our system
type User struct {
	Username     string
	PasswordHash string
	ID           string
}

// MemoryStore implements fosite.Storage interface for in-memory storage
type MemoryStore struct {
	mu            sync.RWMutex
	Clients       map[string]fosite.Client
	Users         map[string]*User
	AuthCodes     map[string]fosite.Requester
	AccessTokens  map[string]fosite.Requester
	RefreshTokens map[string]fosite.Requester
	PKCEs         map[string]fosite.Requester
	IDSessions    map[string]fosite.Requester
}

// NewMemoryStore creates a new in-memory store with default data
func NewMemoryStore() *MemoryStore {
	store := &MemoryStore{
		Clients:       make(map[string]fosite.Client),
		Users:         make(map[string]*User),
		AuthCodes:     make(map[string]fosite.Requester),
		AccessTokens:  make(map[string]fosite.Requester),
		RefreshTokens: make(map[string]fosite.Requester),
		PKCEs:         make(map[string]fosite.Requester),
		IDSessions:    make(map[string]fosite.Requester),
	}

	// Add default user: wesen/secret
	passwordHash := fmt.Sprintf("%x", sha256.Sum256([]byte("secret")))
	store.Users["wesen"] = &User{
		Username:     "wesen",
		PasswordHash: passwordHash,
		ID:           "user-wesen",
	}

	return store
}

// AuthenticateUser checks username/password credentials
func (s *MemoryStore) AuthenticateUser(username, password string) (*User, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	user, exists := s.Users[username]
	if !exists {
		return nil, errors.New("user not found")
	}

	passwordHash := fmt.Sprintf("%x", sha256.Sum256([]byte(password)))
	if user.PasswordHash != passwordHash {
		return nil, errors.New("invalid password")
	}

	return user, nil
}

// RegisterClient adds a new client to the store
func (s *MemoryStore) RegisterClient(client fosite.Client) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.Clients[client.GetID()] = client
	return nil
}

// Client management methods
func (s *MemoryStore) GetClient(ctx context.Context, id string) (fosite.Client, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	client, exists := s.Clients[id]
	if !exists {
		return nil, fosite.ErrNotFound
	}
	return client, nil
}

func (s *MemoryStore) ClientAssertionJWTValid(ctx context.Context, jti string) error {
	return nil
}

func (s *MemoryStore) SetClientAssertionJWT(ctx context.Context, jti string, exp time.Time) error {
	return nil
}

// Authorization code methods
func (s *MemoryStore) CreateAuthorizeCodeSession(ctx context.Context, code string, request fosite.Requester) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.AuthCodes[code] = request
	return nil
}

func (s *MemoryStore) GetAuthorizeCodeSession(ctx context.Context, code string, session fosite.Session) (fosite.Requester, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	request, exists := s.AuthCodes[code]
	if !exists {
		return nil, fosite.ErrNotFound
	}
	return request, nil
}

func (s *MemoryStore) InvalidateAuthorizeCodeSession(ctx context.Context, code string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	delete(s.AuthCodes, code)
	return nil
}

// Access token methods
func (s *MemoryStore) CreateAccessTokenSession(ctx context.Context, signature string, request fosite.Requester) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.AccessTokens[signature] = request
	return nil
}

func (s *MemoryStore) GetAccessTokenSession(ctx context.Context, signature string, session fosite.Session) (fosite.Requester, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	request, exists := s.AccessTokens[signature]
	if !exists {
		return nil, fosite.ErrNotFound
	}
	return request, nil
}

func (s *MemoryStore) DeleteAccessTokenSession(ctx context.Context, signature string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	delete(s.AccessTokens, signature)
	return nil
}

// Refresh token methods
func (s *MemoryStore) CreateRefreshTokenSession(ctx context.Context, signature string, accessSignature string, request fosite.Requester) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.RefreshTokens[signature] = request
	return nil
}

func (s *MemoryStore) GetRefreshTokenSession(ctx context.Context, signature string, session fosite.Session) (fosite.Requester, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	request, exists := s.RefreshTokens[signature]
	if !exists {
		return nil, fosite.ErrNotFound
	}
	return request, nil
}

func (s *MemoryStore) DeleteRefreshTokenSession(ctx context.Context, signature string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	delete(s.RefreshTokens, signature)
	return nil
}

func (s *MemoryStore) RotateRefreshToken(ctx context.Context, requestID string, refreshTokenSignature string) error {
	// For in-memory implementation, we don't need to do anything special for rotation
	// The token will be replaced when a new one is created
	return nil
}

// PKCE methods
func (s *MemoryStore) CreatePKCERequestSession(ctx context.Context, signature string, requester fosite.Requester) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.PKCEs[signature] = requester
	return nil
}

func (s *MemoryStore) GetPKCERequestSession(ctx context.Context, signature string, session fosite.Session) (fosite.Requester, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	request, exists := s.PKCEs[signature]
	if !exists {
		return nil, fosite.ErrNotFound
	}
	return request, nil
}

func (s *MemoryStore) DeletePKCERequestSession(ctx context.Context, signature string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	delete(s.PKCEs, signature)
	return nil
}

// OpenID Connect methods
func (s *MemoryStore) CreateOpenIDConnectSession(ctx context.Context, authorizeCode string, requester fosite.Requester) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.IDSessions[authorizeCode] = requester
	return nil
}

func (s *MemoryStore) GetOpenIDConnectSession(ctx context.Context, authorizeCode string, requester fosite.Requester) (fosite.Requester, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	request, exists := s.IDSessions[authorizeCode]
	if !exists {
		return nil, fosite.ErrNotFound
	}
	return request, nil
}

func (s *MemoryStore) DeleteOpenIDConnectSession(ctx context.Context, authorizeCode string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	delete(s.IDSessions, authorizeCode)
	return nil
}

// Implement additional required interfaces for Fosite
func (s *MemoryStore) RevokeRefreshToken(ctx context.Context, requestID string) error {
	return s.DeleteRefreshTokenSession(ctx, requestID)
}

func (s *MemoryStore) RevokeAccessToken(ctx context.Context, requestID string) error {
	return s.DeleteAccessTokenSession(ctx, requestID)
}

// Implement Implicit grant methods (even if we don't use them)
func (s *MemoryStore) CreateImplicitAccessTokenSession(ctx context.Context, code string, req fosite.Requester) error {
	return s.CreateAccessTokenSession(ctx, code, req)
}

// UserSession represents a user session for OIDC
type UserSession struct {
	Username  string
	Subject   string
	ExpiresAt map[fosite.TokenType]time.Time
}

// GetSubject returns the subject for OIDC ID tokens
func (u *UserSession) GetSubject() string {
	return u.Subject
}

// GetUsername returns the username
func (u *UserSession) GetUsername() string {
	return u.Username
}

// GetExpiresAt returns the expiration time for a token type
func (u *UserSession) GetExpiresAt(tokenType fosite.TokenType) time.Time {
	if u.ExpiresAt == nil {
		return time.Time{}
	}
	return u.ExpiresAt[tokenType]
}

// SetExpiresAt sets the expiration time for a token type
func (u *UserSession) SetExpiresAt(tokenType fosite.TokenType, exp time.Time) {
	if u.ExpiresAt == nil {
		u.ExpiresAt = make(map[fosite.TokenType]time.Time)
	}
	u.ExpiresAt[tokenType] = exp
}

// Clone creates a copy of the session
func (u *UserSession) Clone() fosite.Session {
	clone := &UserSession{
		Username: u.Username,
		Subject:  u.Subject,
	}
	if u.ExpiresAt != nil {
		clone.ExpiresAt = make(map[fosite.TokenType]time.Time)
		for k, v := range u.ExpiresAt {
			clone.ExpiresAt[k] = v
		}
	}
	return clone
}

