package config

import (
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	"gopkg.in/yaml.v3"
)

// Config represents the top-level configuration structure
type Config struct {
	Version      string             `yaml:"version"`
	Connection   ConnectionConfig   `yaml:"connection"`
	Sources      []SourceConfig     `yaml:"sources"`
	Transformations []TransformConfig `yaml:"transformations,omitempty"`
	Output       OutputConfig       `yaml:"output"`
	Logging      *LoggingConfig     `yaml:"logging,omitempty"`
	WebInterface *WebInterfaceConfig `yaml:"web_interface,omitempty"`
}

// ConnectionConfig represents the Usenet server connection settings
type ConnectionConfig struct {
	Server   string         `yaml:"server"`
	Port     int            `yaml:"port"`
	SSL      bool           `yaml:"ssl,omitempty"`
	Timeout  int            `yaml:"timeout,omitempty"`
	Retries  int            `yaml:"retries,omitempty"`
	Auth     *AuthConfig    `yaml:"auth,omitempty"`
}

// AuthConfig represents authentication credentials
type AuthConfig struct {
	Username string `yaml:"username"`
	Password string `yaml:"password"`
}

// SourceConfig represents a Usenet group to scrape
type SourceConfig struct {
	Group      string `yaml:"group"`
	MaxPosts   int    `yaml:"max_posts,omitempty"`
	Since      string `yaml:"since,omitempty"`
	Filter     string `yaml:"filter,omitempty"`
	Concurrent int    `yaml:"concurrent,omitempty"`
	RateLimit  string `yaml:"rate_limit,omitempty"`
}

// TransformConfig represents a data transformation
type TransformConfig struct {
	ID         string   `yaml:"id"`
	Field      string   `yaml:"field"`
	Pattern    string   `yaml:"pattern,omitempty"`
	StoreAs    string   `yaml:"store_as"`
	Operations []string `yaml:"operations,omitempty"`
	Model      string   `yaml:"model,omitempty"`
	Categories []string `yaml:"categories,omitempty"`
	Replace    *ReplaceConfig `yaml:"replace,omitempty"`
	Split      *SplitConfig   `yaml:"split,omitempty"`
	Join       *JoinConfig    `yaml:"join,omitempty"`
}

// ReplaceConfig represents string replacement configuration
type ReplaceConfig struct {
	From string `yaml:"from"`
	To   string `yaml:"to"`
}

// SplitConfig represents string splitting configuration
type SplitConfig struct {
	Delimiter string `yaml:"delimiter"`
}

// JoinConfig represents string joining configuration
type JoinConfig struct {
	Fields    []string `yaml:"fields"`
	Delimiter string   `yaml:"delimiter"`
}

// OutputConfig represents output configuration
type OutputConfig struct {
	Format    string   `yaml:"format"`
	File      string   `yaml:"file"`
	Fields    []string `yaml:"fields,omitempty"`
	Pretty    bool     `yaml:"pretty,omitempty"`
	Append    bool     `yaml:"append,omitempty"`
	BatchSize int      `yaml:"batch_size,omitempty"`
}

// LoggingConfig represents logging configuration
type LoggingConfig struct {
	Level  string `yaml:"level,omitempty"`
	File   string `yaml:"file,omitempty"`
	Format string `yaml:"format,omitempty"`
}

// WebInterfaceConfig represents web interface configuration
type WebInterfaceConfig struct {
	Enabled  bool   `yaml:"enabled,omitempty"`
	Port     int    `yaml:"port,omitempty"`
	Host     string `yaml:"host,omitempty"`
	Auth     *WebAuthConfig `yaml:"auth,omitempty"`
}

// WebAuthConfig represents web interface authentication
type WebAuthConfig struct {
	Username string `yaml:"username"`
	Password string `yaml:"password"`
}

// LoadConfig loads configuration from a YAML file
func LoadConfig(filename string) (*Config, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("error reading config file: %w", err)
	}

	// Process environment variables
	data = replaceEnvVars(data)

	var config Config
	err = yaml.Unmarshal(data, &config)
	if err != nil {
		return nil, fmt.Errorf("error parsing config file: %w", err)
	}

	// Set default values
	setDefaults(&config)

	// Validate configuration
	if err := validateConfig(&config); err != nil {
		return nil, err
	}

	return &config, nil
}

// replaceEnvVars replaces ${ENV_VAR} with environment variable values
func replaceEnvVars(data []byte) []byte {
	re := regexp.MustCompile(`\${([^}]+)}`)
	return re.ReplaceAllFunc(data, func(match []byte) []byte {
		envVar := string(match[2 : len(match)-1])
		envValue := os.Getenv(envVar)
		if envValue == "" {
			return match // Keep original if env var not set
		}
		return []byte(envValue)
	})
}

// setDefaults sets default values for configuration fields
func setDefaults(config *Config) {
	// Connection defaults
	if config.Connection.Port == 0 {
		config.Connection.Port = 119
	}
	if config.Connection.Timeout == 0 {
		config.Connection.Timeout = 30
	}
	if config.Connection.Retries == 0 {
		config.Connection.Retries = 3
	}

	// Source defaults
	for i := range config.Sources {
		if config.Sources[i].MaxPosts == 0 {
			config.Sources[i].MaxPosts = 100
		}
		if config.Sources[i].Since == "" {
			config.Sources[i].Since = "1d"
		}
		if config.Sources[i].Concurrent == 0 {
			config.Sources[i].Concurrent = 1
		}
	}

	// Output defaults
	if config.Output.BatchSize == 0 {
		config.Output.BatchSize = 100
	}

	// Logging defaults
	if config.Logging == nil {
		config.Logging = &LoggingConfig{
			Level:  "info",
			Format: "text",
		}
	} else {
		if config.Logging.Level == "" {
			config.Logging.Level = "info"
		}
		if config.Logging.Format == "" {
			config.Logging.Format = "text"
		}
	}

	// Web interface defaults
	if config.WebInterface != nil && config.WebInterface.Enabled {
		if config.WebInterface.Port == 0 {
			config.WebInterface.Port = 8080
		}
		if config.WebInterface.Host == "" {
			config.WebInterface.Host = "localhost"
		}
	}
}

// validateConfig validates the configuration
func validateConfig(config *Config) error {
	// Validate connection
	if config.Connection.Server == "" {
		return fmt.Errorf("connection.server is required")
	}

	// Validate sources
	if len(config.Sources) == 0 {
		return fmt.Errorf("at least one source is required")
	}
	for i, source := range config.Sources {
		if source.Group == "" {
			return fmt.Errorf("sources[%d].group is required", i)
		}
	}

	// Validate transformations
	for i, transform := range config.Transformations {
		if transform.ID == "" {
			return fmt.Errorf("transformations[%d].id is required", i)
		}
		if transform.Field == "" {
			return fmt.Errorf("transformations[%d].field is required", i)
		}
		if transform.StoreAs == "" {
			return fmt.Errorf("transformations[%d].store_as is required", i)
		}
	}

	// Validate output
	if config.Output.Format == "" {
		return fmt.Errorf("output.format is required")
	}
	if config.Output.File == "" {
		return fmt.Errorf("output.file is required")
	}
	if !isValidOutputFormat(config.Output.Format) {
		return fmt.Errorf("invalid output format: %s", config.Output.Format)
	}

	return nil
}

// isValidOutputFormat checks if the output format is supported
func isValidOutputFormat(format string) bool {
	validFormats := []string{"json", "csv", "xml", "sqlite"}
	for _, f := range validFormats {
		if format == f {
			return true
		}
	}
	return false
}

// ParseDuration parses a duration string like "2d" or "12h"
func ParseDuration(durationStr string) (time.Duration, error) {
	// Check for time unit suffix
	re := regexp.MustCompile(`^(\d+)([mhdw])$`)
	matches := re.FindStringSubmatch(durationStr)
	
	if len(matches) != 3 {
		return 0, fmt.Errorf("invalid duration format: %s", durationStr)
	}
	
	value := matches[1]
	unit := matches[2]
	
	// Parse the numeric value
	var multiplier time.Duration
	switch unit {
	case "m":
		multiplier = time.Minute
	case "h":
		multiplier = time.Hour
	case "d":
		multiplier = 24 * time.Hour
	case "w":
		multiplier = 7 * 24 * time.Hour
	default:
		return 0, fmt.Errorf("invalid time unit: %s", unit)
	}
	
	// Parse the numeric value
	var numericValue int
	_, err := fmt.Sscanf(value, "%d", &numericValue)
	if err != nil {
		return 0, fmt.Errorf("invalid numeric value: %s", value)
	}
	
	return time.Duration(numericValue) * multiplier, nil
}

// ParseRateLimit parses a rate limit string like "10/minute"
func ParseRateLimit(rateStr string) (int, time.Duration, error) {
	parts := strings.Split(rateStr, "/")
	if len(parts) != 2 {
		return 0, 0, fmt.Errorf("invalid rate limit format: %s", rateStr)
	}
	
	var count int
	_, err := fmt.Sscanf(parts[0], "%d", &count)
	if err != nil {
		return 0, 0, fmt.Errorf("invalid rate count: %s", parts[0])
	}
	
	var period time.Duration
	switch strings.ToLower(parts[1]) {
	case "second":
		period = time.Second
	case "minute":
		period = time.Minute
	case "hour":
		period = time.Hour
	default:
		return 0, 0, fmt.Errorf("invalid rate period: %s", parts[1])
	}
	
	return count, period, nil
}
