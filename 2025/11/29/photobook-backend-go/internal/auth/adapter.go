package auth

import (
	"context"
	"photobook-backend-go/pkg/types"
)

// AuthAdapter defines the interface for authentication adapters
type AuthAdapter interface {
	// Authenticate authenticates a user with the given credentials
	Authenticate(ctx context.Context, credentials interface{}) (*types.User, error)
	
	// Register registers a new user with the given information
	Register(ctx context.Context, info interface{}) (*types.User, error)
}

