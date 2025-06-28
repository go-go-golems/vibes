// Package models provides data structures for the demo application.
package models

import (
	"fmt"
	"time"
)

// User represents a user in the system.
// This struct demonstrates various Go language features for LSP testing.
type User struct {
	// ID is the unique identifier for the user
	ID       int       `json:"id"`
	Name     string    `json:"name"`
	Email    string    `json:"email"`
	Age      int       `json:"age"`
	IsActive bool      `json:"is_active"`
	Created  time.Time `json:"created"`
}

// UserRepository defines the interface for user data operations.
// This interface demonstrates Go interface definitions for LSP testing.
type UserRepository interface {
	// GetByID retrieves a user by their ID
	GetByID(id int) (*User, error)
	
	// GetByEmail retrieves a user by their email address
	GetByEmail(email string) (*User, error)
	
	// Create creates a new user in the repository
	Create(user *User) error
	
	// Update updates an existing user
	Update(user *User) error
	
	// Delete removes a user from the repository
	Delete(id int) error
	
	// List returns all users with pagination
	List(offset, limit int) ([]*User, error)
}

// NewUser creates a new User instance with default values.
// This function demonstrates constructor patterns and method calls.
func NewUser(name, email string, age int) *User {
	return &User{
		Name:     name,
		Email:    email,
		Age:      age,
		IsActive: true,
		Created:  time.Now(),
	}
}

// String returns a string representation of the user.
// This method implements the fmt.Stringer interface.
func (u *User) String() string {
	return fmt.Sprintf("User{ID: %d, Name: %s, Email: %s, Age: %d, Active: %t}",
		u.ID, u.Name, u.Email, u.Age, u.IsActive)
}

// IsAdult checks if the user is 18 years or older.
// This method demonstrates simple business logic.
func (u *User) IsAdult() bool {
	return u.Age >= 18
}

// Validate performs basic validation on user data.
// This method demonstrates error handling and validation patterns.
func (u *User) Validate() error {
	if u.Name == "" {
		return fmt.Errorf("name cannot be empty")
	}
	if u.Email == "" {
		return fmt.Errorf("email cannot be empty")
	}
	if u.Age < 0 {
		return fmt.Errorf("age cannot be negative")
	}
	return nil
}

// GetDisplayName returns the user's display name.
// This method demonstrates string manipulation.
func (u *User) GetDisplayName() string {
	if u.Name != "" {
		return u.Name
	}
	return u.Email
}

