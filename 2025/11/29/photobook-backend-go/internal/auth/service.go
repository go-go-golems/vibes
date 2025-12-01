package auth

import (
	"context"
	"fmt"
	"time"

	"photobook-backend-go/internal/config"
	"photobook-backend-go/pkg/types"
)

// AuthService provides authentication functionality
type AuthService struct {
	repo      UserRepository
	adapter   AuthAdapter
	jwt       *JWTService
	config    *config.Config
}

// NewAuthService creates a new auth service
func NewAuthService(repo UserRepository, adapter AuthAdapter, jwtSecret string, cfg *config.Config) *AuthService {
	jwtService := NewJWTService(jwtSecret, cfg.AppID)
	return &AuthService{
		repo:    repo,
		adapter: adapter,
		jwt:     jwtService,
		config:  cfg,
	}
}

// Register registers a new user
func (s *AuthService) Register(ctx context.Context, email, password, name string) (*types.User, string, error) {
	regInfo := &RegisterInfo{
		Email:    email,
		Password: password,
		Name:     name,
	}

	user, err := s.adapter.Register(ctx, regInfo)
	if err != nil {
		return nil, "", fmt.Errorf("failed to register: %w", err)
	}

	// Assign admin role if open_id matches owner
	if s.config.OwnerOpenID != "" && user.OpenID == s.config.OwnerOpenID {
		user.Role = "admin"
		if err := s.repo.Update(ctx, user); err != nil {
			return nil, "", fmt.Errorf("failed to update role: %w", err)
		}
	}

	// Create session token
	token, err := s.jwt.SignSession(user.OpenID, user.Name, 365*24*time.Hour)
	if err != nil {
		return nil, "", fmt.Errorf("failed to create session token: %w", err)
	}

	return user, token, nil
}

// Login authenticates a user and returns a session token
func (s *AuthService) Login(ctx context.Context, email, password string) (*types.User, string, error) {
	creds := &PasswordCredentials{
		Email:    email,
		Password: password,
	}

	user, err := s.adapter.Authenticate(ctx, creds)
	if err != nil {
		return nil, "", fmt.Errorf("failed to authenticate: %w", err)
	}

	// Update last signed in
	if err := s.repo.UpdateLastSignedIn(ctx, user.OpenID); err != nil {
		return nil, "", fmt.Errorf("failed to update last signed in: %w", err)
	}

	// Create session token
	token, err := s.jwt.SignSession(user.OpenID, user.Name, 365*24*time.Hour)
	if err != nil {
		return nil, "", fmt.Errorf("failed to create session token: %w", err)
	}

	return user, token, nil
}

// Me retrieves the current user from a session token
func (s *AuthService) Me(ctx context.Context, sessionToken string) (*types.User, error) {
	claims, err := s.jwt.VerifySession(sessionToken)
	if err != nil {
		return nil, fmt.Errorf("invalid session token: %w", err)
	}

	user, err := s.repo.GetByOpenID(ctx, claims.OpenID)
	if err != nil {
		return nil, fmt.Errorf("failed to get user: %w", err)
	}
	if user == nil {
		return nil, fmt.Errorf("user not found")
	}

	// Update last signed in
	if err := s.repo.UpdateLastSignedIn(ctx, user.OpenID); err != nil {
		return nil, fmt.Errorf("failed to update last signed in: %w", err)
	}

	return user, nil
}

