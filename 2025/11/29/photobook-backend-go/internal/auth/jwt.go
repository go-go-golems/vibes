package auth

import (
	"fmt"
	"time"

	"github.com/golang-jwt/jwt/v5"
)

// SessionClaims represents the JWT claims for a session
type SessionClaims struct {
	OpenID string `json:"openId"`
	AppID  string `json:"appId"`
	Name   string `json:"name"`
	jwt.RegisteredClaims
}

// JWTService handles JWT token creation and verification
type JWTService struct {
	secret []byte
	appID  string
}

// NewJWTService creates a new JWT service
func NewJWTService(secret string, appID string) *JWTService {
	return &JWTService{
		secret: []byte(secret),
		appID:  appID,
	}
}

// SignSession creates a JWT token for a session
func (s *JWTService) SignSession(openID, name string, expiresIn time.Duration) (string, error) {
	now := time.Now()
	claims := &SessionClaims{
		OpenID: openID,
		AppID:  s.appID,
		Name:   name,
		RegisteredClaims: jwt.RegisteredClaims{
			IssuedAt:  jwt.NewNumericDate(now),
			ExpiresAt: jwt.NewNumericDate(now.Add(expiresIn)),
		},
	}

	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	return token.SignedString(s.secret)
}

// VerifySession verifies a JWT token and returns the session data
func (s *JWTService) VerifySession(tokenString string) (*SessionClaims, error) {
	token, err := jwt.ParseWithClaims(tokenString, &SessionClaims{}, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		return s.secret, nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to parse token: %w", err)
	}

	if claims, ok := token.Claims.(*SessionClaims); ok && token.Valid {
		// Validate required fields
		if claims.OpenID == "" || claims.AppID == "" || claims.Name == "" {
			return nil, fmt.Errorf("session payload missing required fields")
		}
		return claims, nil
	}

	return nil, fmt.Errorf("invalid token")
}

