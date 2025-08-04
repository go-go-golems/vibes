package config

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"gopkg.in/yaml.v3"
)

// Config represents the application configuration
type Config struct {
	VaultPath    string `yaml:"vault_path"`
	LogsPath     string `yaml:"logs_path"`
	DateFormat   string `yaml:"date_format"`
	DefaultLimit int    `yaml:"default_limit"`
	Editor       string `yaml:"editor,omitempty"`
}

// DefaultConfig returns a configuration with default values
func DefaultConfig() *Config {
	homeDir, _ := os.UserHomeDir()
	return &Config{
		VaultPath:    filepath.Join(homeDir, "obsidian-vault"),
		LogsPath:     "Logs",
		DateFormat:   "2006-01-02",
		DefaultLimit: 10,
		Editor:       "",
	}
}

// Load loads configuration from file or creates default
func Load() (*Config, error) {
	configPath := getConfigPath()
	
	// If config file doesn't exist, create default
	if _, err := os.Stat(configPath); os.IsNotExist(err) {
		cfg := DefaultConfig()
		if err := cfg.Save(); err != nil {
			return nil, fmt.Errorf("failed to save default config: %w", err)
		}
		return cfg, nil
	}

	// Load existing config
	data, err := os.ReadFile(configPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	cfg := DefaultConfig()
	if err := yaml.Unmarshal(data, cfg); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	return cfg, nil
}

// Save saves the configuration to file
func (c *Config) Save() error {
	configPath := getConfigPath()
	
	// Create config directory if it doesn't exist
	configDir := filepath.Dir(configPath)
	if err := os.MkdirAll(configDir, 0755); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}

	data, err := yaml.Marshal(c)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %w", err)
	}

	if err := os.WriteFile(configPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write config file: %w", err)
	}

	return nil
}

// GetLogsDir returns the full path to the logs directory
func (c *Config) GetLogsDir() string {
	return filepath.Join(c.VaultPath, c.LogsPath)
}

// GetTodayFile returns the path to today's diary file
func (c *Config) GetTodayFile() string {
	today := getCurrentDateString(c.DateFormat)
	return c.GetDateFile(today)
}

// GetDateFile returns the path to a specific date's diary file
func (c *Config) GetDateFile(dateStr string) string {
	// For now, assume flat structure in logs directory
	// Could be enhanced to support year/month subdirectories
	return filepath.Join(c.GetLogsDir(), dateStr+".md")
}

// GetEditor returns the editor to use, checking environment variables
func (c *Config) GetEditor() string {
	if c.Editor != "" {
		return c.Editor
	}
	
	if editor := os.Getenv("VISUAL"); editor != "" {
		return editor
	}
	
	if editor := os.Getenv("EDITOR"); editor != "" {
		return editor
	}
	
	return "nano" // fallback
}

// getConfigPath returns the path to the configuration file
func getConfigPath() string {
	homeDir, _ := os.UserHomeDir()
	return filepath.Join(homeDir, ".diary-config.yaml")
}

// getCurrentDateString returns the current date as a string
func getCurrentDateString(format string) string {
	return getCurrentDate().Format(format)
}

// getCurrentDate returns the current date (can be mocked for testing)
var getCurrentDate = func() time.Time {
	return time.Now()
}

