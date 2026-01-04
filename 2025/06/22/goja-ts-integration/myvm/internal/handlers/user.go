package handlers

import (
	"errors"
	"time"
)

// User represents a user in the system
type User struct {
	ID        string    `json:"id"`
	Username  string    `json:"username"`
	Email     string    `json:"email"`
	CreatedAt time.Time `json:"createdAt"`
	IsActive  bool      `json:"isActive"`
}

// UserCreateParams contains parameters for creating a new user
type UserCreateParams struct {
	Username string `json:"username"`
	Email    string `json:"email"`
}

// CreateUser creates a new user in the system
// This function will be exposed to TypeScript
func CreateUser(params UserCreateParams) (*User, error) {
	// Validate input
	if params.Username == "" {
		return nil, errors.New("username is required")
	}
	if params.Email == "" {
		return nil, errors.New("email is required")
	}

	// In a real app, we would save to database here
	user := &User{
		ID:        generateID(),
		Username:  params.Username,
		Email:     params.Email,
		CreatedAt: time.Now(),
		IsActive:  true,
	}

	return user, nil
}

// Helper function to generate a simple ID
// In production, use a proper UUID library
func generateID() string {
	return "user_" + time.Now().Format("20060102150405")
}
