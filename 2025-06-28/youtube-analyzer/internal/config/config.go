package config

import (
	"fmt"
	"strings"
)

// Config holds the application configuration
type Config struct {
	APIKey    string `mapstructure:"api-key"`
	Mode      string `mapstructure:"mode"`
	OutputDir string `mapstructure:"output-dir"`
	LogLevel  string `mapstructure:"log-level"`
	Verbose   bool   `mapstructure:"verbose"`
	Quiet     bool   `mapstructure:"quiet"`
	NoColor   bool   `mapstructure:"no-color"`
}

// Validate validates the configuration
func (c *Config) Validate() error {
	if c.APIKey == "" {
		return fmt.Errorf("API key is required")
	}

	if !strings.HasPrefix(c.APIKey, "AIza") {
		return fmt.Errorf("invalid API key format (should start with 'AIza')")
	}

	validModes := map[string]bool{
		"quick":        true,
		"comprehensive": true,
	}
	if !validModes[c.Mode] {
		return fmt.Errorf("invalid mode '%s', must be 'quick' or 'comprehensive'", c.Mode)
	}

	validLogLevels := map[string]bool{
		"debug": true,
		"info":  true,
		"warn":  true,
		"error": true,
	}
	if !validLogLevels[c.LogLevel] {
		return fmt.Errorf("invalid log level '%s', must be one of: debug, info, warn, error", c.LogLevel)
	}

	if c.OutputDir == "" {
		c.OutputDir = "./analysis_results"
	}

	return nil
}

// GetModelName returns the appropriate Gemini model based on the mode
func (c *Config) GetModelName() string {
	switch c.Mode {
	case "comprehensive":
		return "gemini-2.5-pro"
	default:
		return "gemini-2.5-flash"
	}
}

