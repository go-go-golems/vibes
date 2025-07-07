package auth

import (
	"crypto/rand"
	"encoding/json"
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/charmbracelet/ssh"
)

// Role represents user roles
type Role string

const (
	RoleFacilitator Role = "facilitator"
	RoleParticipant Role = "participant"
)

// UserInfo contains information about an authenticated user
type UserInfo struct {
	ID             string `json:"id"`
	Name           string `json:"name"`
	Role           Role   `json:"role"`
	KeyFingerprint string `json:"key_fingerprint"`
}

// AuthManager handles authentication and authorization
type AuthManager struct {
	roles map[string]Role // fingerprint -> role
	users map[string]*UserInfo // fingerprint -> user info
	mu    sync.RWMutex
}

// NewAuthManager creates a new authentication manager
func NewAuthManager() *AuthManager {
	return &AuthManager{
		roles: make(map[string]Role),
		users: make(map[string]*UserInfo),
	}
}

// LoadRoles loads roles from a JSON file
func (am *AuthManager) LoadRoles(path string) error {
	am.mu.Lock()
	defer am.mu.Unlock()
	
	if _, err := os.Stat(path); os.IsNotExist(err) {
		// Create default roles file if it doesn't exist
		defaultRoles := map[string]string{
			"example_facilitator_key": "facilitator",
			"example_participant_key": "participant",
		}
		
		data, err := json.MarshalIndent(defaultRoles, "", "  ")
		if err != nil {
			return err
		}
		
		if err := os.WriteFile(path, data, 0644); err != nil {
			return err
		}
		
		fmt.Printf("Created default roles file at %s\n", path)
		fmt.Println("Please update it with actual SSH key fingerprints")
	}
	
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	
	roleMap := make(map[string]string)
	if err := json.Unmarshal(data, &roleMap); err != nil {
		return err
	}
	
	// Convert string roles to Role type
	for fingerprint, roleStr := range roleMap {
		switch roleStr {
		case "facilitator":
			am.roles[fingerprint] = RoleFacilitator
		case "participant":
			am.roles[fingerprint] = RoleParticipant
		default:
			return fmt.Errorf("invalid role: %s", roleStr)
		}
	}
	
	return nil
}

// AuthenticateKey authenticates a public key and returns user info
func (am *AuthManager) AuthenticateKey(key ssh.PublicKey) (*UserInfo, bool) {
	am.mu.RLock()
	defer am.mu.RUnlock()
	
	// Use a simple string representation of the key for fingerprinting
	fingerprint := fmt.Sprintf("%x", key.Marshal())
	
	role, exists := am.roles[fingerprint]
	if !exists {
		return nil, false
	}
	
	// Check if user info already exists
	if userInfo, exists := am.users[fingerprint]; exists {
		return userInfo, true
	}
	
	// Create new user info
	userInfo := &UserInfo{
		ID:             generateUserID(),
		Name:           fmt.Sprintf("User_%s", fingerprint[8:16]), // Default name
		Role:           role,
		KeyFingerprint: fingerprint,
	}
	
	am.users[fingerprint] = userInfo
	
	return userInfo, true
}

// GetUserByFingerprint returns user info by key fingerprint
func (am *AuthManager) GetUserByFingerprint(fingerprint string) (*UserInfo, bool) {
	am.mu.RLock()
	defer am.mu.RUnlock()
	
	userInfo, exists := am.users[fingerprint]
	return userInfo, exists
}

// UpdateUserName updates a user's display name
func (am *AuthManager) UpdateUserName(fingerprint, name string) error {
	am.mu.Lock()
	defer am.mu.Unlock()
	
	userInfo, exists := am.users[fingerprint]
	if !exists {
		return fmt.Errorf("user not found")
	}
	
	userInfo.Name = name
	return nil
}

// IsAuthorized checks if a user is authorized for a specific action
func (am *AuthManager) IsAuthorized(userInfo *UserInfo, action string) bool {
	switch action {
	case "create_session", "manage_session", "start_voting", "close_voting", "show_results":
		return userInfo.Role == RoleFacilitator
	case "join_session", "cast_vote", "view_results":
		return userInfo.Role == RoleParticipant || userInfo.Role == RoleFacilitator
	default:
		return false
	}
}

// generateUserID generates a unique user ID
func generateUserID() string {
	// Use a more robust ID generation
	timestamp := time.Now().UnixNano()
	randomBytes := make([]byte, 4)
	rand.Read(randomBytes)
	
	return fmt.Sprintf("user_%d_%x", timestamp, randomBytes)
}

