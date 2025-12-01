package auth

import (
	"context"
	"fmt"
	"time"

	"golang.org/x/crypto/bcrypt"
	"photobook-backend-go/pkg/types"
)

// PasswordCredentials represents email/password login credentials
type PasswordCredentials struct {
	Email    string
	Password string
}

// RegisterInfo represents registration information
type RegisterInfo struct {
	Email    string
	Password string
	Name     string
}

// EmailPasswordAdapter implements AuthAdapter for email/password authentication
type EmailPasswordAdapter struct {
	repo UserRepository
}

// NewEmailPasswordAdapter creates a new email/password adapter
func NewEmailPasswordAdapter(repo UserRepository) *EmailPasswordAdapter {
	return &EmailPasswordAdapter{repo: repo}
}

// Authenticate authenticates a user with email/password
func (a *EmailPasswordAdapter) Authenticate(ctx context.Context, credentials interface{}) (*types.User, error) {
	creds, ok := credentials.(*PasswordCredentials)
	if !ok {
		return nil, fmt.Errorf("invalid credentials type")
	}

	// Get user by email
	user, err := a.repo.GetByEmail(ctx, creds.Email)
	if err != nil {
		return nil, fmt.Errorf("failed to get user: %w", err)
	}
	if user == nil {
		return nil, fmt.Errorf("invalid credentials")
	}

	// Get password hash
	passwordHash, err := a.repo.GetPasswordHash(ctx, creds.Email)
	if err != nil {
		return nil, fmt.Errorf("failed to get password hash: %w", err)
	}
	if passwordHash == nil {
		return nil, fmt.Errorf("user has no password set")
	}

	// Verify password
	if err := bcrypt.CompareHashAndPassword([]byte(*passwordHash), []byte(creds.Password)); err != nil {
		return nil, fmt.Errorf("invalid credentials")
	}

	return user, nil
}

// Register registers a new user with email/password
func (a *EmailPasswordAdapter) Register(ctx context.Context, info interface{}) (*types.User, error) {
	regInfo, ok := info.(*RegisterInfo)
	if !ok {
		return nil, fmt.Errorf("invalid registration info type")
	}

	// Check if user already exists
	existing, err := a.repo.GetByEmail(ctx, regInfo.Email)
	if err != nil {
		return nil, fmt.Errorf("failed to check existing user: %w", err)
	}
	if existing != nil {
		return nil, fmt.Errorf("user with email %s already exists", regInfo.Email)
	}

	// Hash password
	hashedPassword, err := bcrypt.GenerateFromPassword([]byte(regInfo.Password), bcrypt.DefaultCost)
	if err != nil {
		return nil, fmt.Errorf("failed to hash password: %w", err)
	}

	passwordHashStr := string(hashedPassword)

	// Create user
	// For MVP, we'll use email as open_id
	user := &types.User{
		OpenID:      regInfo.Email, // Use email as open_id for email/password auth
		Name:        regInfo.Name,
		Email:       regInfo.Email,
		LoginMethod: "email",
		Role:        "user",
		CreatedAt:   time.Now(),
		UpdatedAt:   time.Now(),
		LastSignedIn: time.Now(),
	}

	id, err := a.repo.Create(ctx, user, &passwordHashStr)
	if err != nil {
		return nil, fmt.Errorf("failed to create user: %w", err)
	}

	user.ID = id
	return user, nil
}

