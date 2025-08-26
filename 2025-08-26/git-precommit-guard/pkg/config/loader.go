package config

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"gopkg.in/yaml.v3"
)

// LoadConfig loads configuration from a YAML file
func LoadConfig(configPath string) (*Config, error) {
	// If no config path provided, look for default locations
	if configPath == "" {
		configPath = findDefaultConfig()
	}

	data, err := os.ReadFile(configPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file %s: %w", configPath, err)
	}

	var config Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse config file %s: %w", configPath, err)
	}

	// Apply defaults and validate
	if err := applyDefaults(&config); err != nil {
		return nil, fmt.Errorf("failed to apply defaults: %w", err)
	}

	if err := validateConfig(&config); err != nil {
		return nil, fmt.Errorf("invalid configuration: %w", err)
	}

	return &config, nil
}

// findDefaultConfig looks for configuration files in common locations
func findDefaultConfig() string {
	candidates := []string{
		".precommit-guard.yml",
		".precommit-guard.yaml",
		".git/precommit-guard.yml",
		".git/precommit-guard.yaml",
	}

	for _, candidate := range candidates {
		if _, err := os.Stat(candidate); err == nil {
			return candidate
		}
	}

	return ".precommit-guard.yml" // Default fallback
}

// applyDefaults sets default values for missing configuration
func applyDefaults(config *Config) error {
	// Set version default
	if config.Version == "" {
		config.Version = "1.0"
	}

	// Set global settings defaults
	if config.Settings.Timeout == 0 {
		var err error
		config.Settings.Timeout, err = time.ParseDuration("10s")
		if err != nil {
			return fmt.Errorf("failed to parse default timeout: %w", err)
		}
	}

	// Set default excludes if empty
	if len(config.Excludes) == 0 {
		config.Excludes = []string{
			".git/*",
			"node_modules/*",
			"vendor/*",
			"*.md",
			"*.txt",
			"*.yml",
			"*.yaml",
			"*.json",
		}
	}

	// Set rule defaults
	setElfDetectionDefaults(&config.Rules.ElfDetection)
	setFileSizeDefaults(&config.Rules.FileSize)
	setMimeDetectionDefaults(&config.Rules.MimeDetection)

	// Set reporting defaults
	if config.Reporting.Format == "" {
		config.Reporting.Format = "console"
	}

	return nil
}

// setElfDetectionDefaults sets defaults for ELF detection rule
func setElfDetectionDefaults(rule *ElfDetectionRule) {
	if rule.Severity == "" {
		rule.Severity = "error"
	}
	if rule.Config.ElfMagic == "" {
		rule.Config.ElfMagic = "7f454c46"
	}
	if len(rule.Config.FilePatterns) == 0 {
		rule.Config.FilePatterns = []string{
			"ELF.*executable",
			"ELF.*shared object",
			"ELF.*relocatable",
		}
	}
	if len(rule.Config.MimeTypes) == 0 {
		rule.Config.MimeTypes = []string{
			"application/x-executable",
			"application/x-sharedlib",
		}
	}
	if rule.Config.Message == "" {
		rule.Config.Message = "ELF binary detected: {{.File}}. Use Git LFS or add to excludes"
	}
}

// setFileSizeDefaults sets defaults for file size rule
func setFileSizeDefaults(rule *FileSizeRule) {
	if rule.Severity == "" {
		rule.Severity = "error"
	}
	if rule.Config.MaxSizeMB == 0 {
		rule.Config.MaxSizeMB = 10
	}
	if rule.Config.WarnSizeMB == 0 {
		rule.Config.WarnSizeMB = 5
	}
	if rule.Config.Message == "" {
		rule.Config.Message = "File {{.File}} ({{.SizeMB}}MB) exceeds size limit ({{.MaxSizeMB}}MB)"
	}
}

// setMimeDetectionDefaults sets defaults for MIME detection rule
func setMimeDetectionDefaults(rule *MimeDetectionRule) {
	if rule.Severity == "" {
		rule.Severity = "warning"
	}
	if len(rule.Config.BlockedTypes) == 0 {
		rule.Config.BlockedTypes = []string{
			"application/octet-stream",
			"application/x-binary",
			"application/x-msdownload",
			"application/x-mach-binary",
			"application/java-archive",
			"application/x-java-archive",
		}
	}
	if len(rule.Config.AllowedTypes) == 0 {
		rule.Config.AllowedTypes = []string{
			"text/*",
			"application/json",
			"application/xml",
			"application/yaml",
			"image/svg+xml",
		}
	}
	if rule.Config.Message == "" {
		rule.Config.Message = "Binary file type detected: {{.File}} ({{.MimeType}})"
	}
}

// validateConfig validates the loaded configuration
func validateConfig(config *Config) error {
	// Validate version
	if config.Version == "" {
		return fmt.Errorf("version is required")
	}

	// Validate timeout
	if config.Settings.Timeout <= 0 {
		return fmt.Errorf("timeout must be positive")
	}

	// Validate reporting format
	validFormats := []string{"console", "json"}
	if !contains(validFormats, config.Reporting.Format) {
		return fmt.Errorf("invalid reporting format: %s (must be one of: %s)",
			config.Reporting.Format, strings.Join(validFormats, ", "))
	}

	return nil
}

// contains checks if a slice contains a string
func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// IsPathExcluded checks if a file path matches any global exclude pattern
func (c *Config) IsPathExcluded(path string) bool {
	for _, pattern := range c.Excludes {
		if matched, _ := filepath.Match(pattern, path); matched {
			return true
		}
		// Also check if the path starts with the pattern (for directory patterns)
		if strings.HasSuffix(pattern, "/*") {
			prefix := strings.TrimSuffix(pattern, "/*")
			if strings.HasPrefix(path, prefix+"/") {
				return true
			}
		}
	}
	return false
}

