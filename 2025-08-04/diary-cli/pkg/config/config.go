package config

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
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

// ProcessPathTemplate replaces YYYY-MM-DD placeholders in a path with the actual date
func (c *Config) ProcessPathTemplate(path string, date time.Time) string {
	// Replace YYYY-MM-DD with the actual date
	dateStr := date.Format("2006-01-02")
	result := strings.ReplaceAll(path, "YYYY-MM-DD", dateStr)
	
	// Replace YYYY with year
	yearStr := date.Format("2006")
	result = strings.ReplaceAll(result, "YYYY", yearStr)
	
	// Replace MM with month
	monthStr := date.Format("01")
	result = strings.ReplaceAll(result, "MM", monthStr)
	
	// Replace DD with day
	dayStr := date.Format("02")
	result = strings.ReplaceAll(result, "DD", dayStr)
	
	return result
}

// GetLogsDir returns the full path to the logs directory
func (c *Config) GetLogsDir() string {
	return filepath.Join(c.VaultPath, c.LogsPath)
}

// GetLogsDirForDate returns the logs directory path with template processing for a specific date
func (c *Config) GetLogsDirForDate(date time.Time) string {
	processedVaultPath := c.ProcessPathTemplate(c.VaultPath, date)
	processedLogsPath := c.ProcessPathTemplate(c.LogsPath, date)
	return filepath.Join(processedVaultPath, processedLogsPath)
}

// GetTodayFile returns the path to today's diary file
func (c *Config) GetTodayFile() string {
	today := getCurrentDate()
	return c.GetDateFile(today)
}

// GetDateFile returns the path to a specific date's diary file
func (c *Config) GetDateFile(date time.Time) string {
	// Use template processing for the logs directory
	logsDir := c.GetLogsDirForDate(date)
	dateStr := date.Format(c.DateFormat)
	return filepath.Join(logsDir, dateStr+".md")
}

// GetDateFileFromString returns the path to a specific date's diary file from a date string
func (c *Config) GetDateFileFromString(dateStr string) (string, error) {
	// Parse the date string using the configured format
	date, err := time.Parse(c.DateFormat, dateStr)
	if err != nil {
		return "", fmt.Errorf("invalid date format: %w", err)
	}
	return c.GetDateFile(date), nil
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
	return filepath.Join(homeDir, ".config", "diary-cli", "config.yaml")
}

// GetConfigPath returns the path to the configuration file
func GetConfigPath() string {
	return getConfigPath()
}

// getCurrentDateString returns the current date as a string
func getCurrentDateString(format string) string {
	return getCurrentDate().Format(format)
}

// getCurrentDate returns the current date (can be mocked for testing)
var getCurrentDate = func() time.Time {
	return time.Now()
}

