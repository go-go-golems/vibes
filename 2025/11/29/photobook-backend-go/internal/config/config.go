package config

import (
	"fmt"
	"os"
	"strconv"
)

// Config holds all configuration for the application
type Config struct {
	DatabaseURL string
	JWTSecret   string
	StoragePath string
	Port        int
	BaseURL     string
	AppID       string
	OwnerOpenID string
}

// LoadConfig loads configuration from environment variables
func LoadConfig() (*Config, error) {
	cfg := &Config{
		DatabaseURL: getEnvOrDefault("DATABASE_URL", "sqlite://./data/app.db"),
		JWTSecret:   os.Getenv("JWT_SECRET"),
		StoragePath: getEnvOrDefault("STORAGE_PATH", "./data/storage"),
		Port:        getEnvIntOrDefault("PORT", 8080),
		BaseURL:     getEnvOrDefault("BASE_URL", "http://localhost:8080"),
		AppID:       getEnvOrDefault("APP_ID", "photobook-app"),
		OwnerOpenID: os.Getenv("OWNER_OPEN_ID"),
	}

	// Validate required fields
	if cfg.JWTSecret == "" {
		return nil, fmt.Errorf("JWT_SECRET is required")
	}

	// Ensure storage path exists
	if err := os.MkdirAll(cfg.StoragePath, 0755); err != nil {
		return nil, fmt.Errorf("failed to create storage path: %w", err)
	}

	return cfg, nil
}

// getEnvOrDefault returns the environment variable value or a default
func getEnvOrDefault(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}

// getEnvIntOrDefault returns the environment variable as an int or a default
func getEnvIntOrDefault(key string, defaultValue int) int {
	if value := os.Getenv(key); value != "" {
		if intValue, err := strconv.Atoi(value); err == nil {
			return intValue
		}
	}
	return defaultValue
}

