package session

import (
	"crypto/rand"
	"fmt"
	"math/big"
	"strings"
	"time"
)

// generateID generates a unique ID
func generateID() string {
	timestamp := time.Now().UnixNano()
	randomBytes := make([]byte, 4)
	rand.Read(randomBytes)
	
	return fmt.Sprintf("%d%x", timestamp, randomBytes)
}

// GenerateSessionCode generates a 4-character session code
func GenerateSessionCode() string {
	const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	code := make([]byte, 4)
	
	for i := range code {
		n, _ := rand.Int(rand.Reader, big.NewInt(int64(len(chars))))
		code[i] = chars[n.Int64()]
	}
	
	return string(code)
}

// ValidateSessionCode validates a session code format
func ValidateSessionCode(code string) bool {
	if len(code) != 4 {
		return false
	}
	
	code = strings.ToUpper(code)
	for _, char := range code {
		if !((char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9')) {
			return false
		}
	}
	
	return true
}

// ValidateName validates a participant name
func ValidateName(name string) bool {
	name = strings.TrimSpace(name)
	return len(name) >= 2 && len(name) <= 50
}

