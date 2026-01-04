package config

import (
	"path/filepath"
	"strings"
)

// GetDirectoryOverride finds the most specific directory override for a given path
func GetDirectoryOverride(overrides map[string]DirectoryOverride, filePath string) *DirectoryOverride {
	var bestMatch *DirectoryOverride
	var bestMatchLength int

	for pattern, override := range overrides {
		if matchesPattern(pattern, filePath) {
			// Use the most specific (longest) match
			if len(pattern) > bestMatchLength {
				bestMatchLength = len(pattern)
				overrideCopy := override
				bestMatch = &overrideCopy
			}
		}
	}

	return bestMatch
}

// matchesPattern checks if a file path matches a directory pattern
func matchesPattern(pattern, filePath string) bool {
	// Handle glob patterns
	if matched, _ := filepath.Match(pattern, filePath); matched {
		return true
	}

	// Handle directory patterns (ending with /*)
	if strings.HasSuffix(pattern, "/*") {
		prefix := strings.TrimSuffix(pattern, "/*")
		return strings.HasPrefix(filePath, prefix+"/")
	}

	// Handle exact directory matches
	if strings.HasSuffix(pattern, "/") {
		return strings.HasPrefix(filePath, pattern)
	}

	// Handle prefix matches
	return strings.HasPrefix(filePath, pattern+"/")
}

// ApplyElfOverride applies directory overrides to ELF detection configuration
func (c *Config) ApplyElfOverride(filePath string) ElfDetectionConfig {
	config := c.Rules.ElfDetection.Config
	
	override := GetDirectoryOverride(config.DirectoryOverrides, filePath)
	if override == nil {
		return config
	}

	// Create a copy to avoid modifying the original
	result := config
	
	// Apply enabled override if specified
	if override.Enabled != nil {
		// We need to modify the parent rule's enabled state
		// This is handled at the rule level, not config level
	}

	return result
}

// ApplyFileSizeOverride applies directory overrides to file size configuration
func (c *Config) ApplyFileSizeOverride(filePath string) FileSizeConfig {
	config := c.Rules.FileSize.Config
	
	override := GetDirectoryOverride(config.DirectoryOverrides, filePath)
	if override == nil {
		return config
	}

	// Create a copy to avoid modifying the original
	result := config
	
	// Apply max size override if specified
	if override.MaxSizeMB != nil {
		result.MaxSizeMB = *override.MaxSizeMB
	}

	return result
}

// ApplyMimeOverride applies directory overrides to MIME detection configuration
func (c *Config) ApplyMimeOverride(filePath string) MimeDetectionConfig {
	config := c.Rules.MimeDetection.Config
	
	override := GetDirectoryOverride(config.DirectoryOverrides, filePath)
	if override == nil {
		return config
	}

	// Create a copy to avoid modifying the original
	result := config
	
	// Apply blocked types override if specified
	if len(override.BlockedTypes) > 0 {
		result.BlockedTypes = override.BlockedTypes
	}

	return result
}

// IsRuleEnabledForPath checks if a rule is enabled for a specific path
func (c *Config) IsRuleEnabledForPath(ruleName, filePath string) bool {
	var baseEnabled bool
	var overrides map[string]DirectoryOverride

	switch ruleName {
	case "elf_detection":
		baseEnabled = c.Rules.ElfDetection.Enabled
		overrides = c.Rules.ElfDetection.Config.DirectoryOverrides
	case "file_size":
		baseEnabled = c.Rules.FileSize.Enabled
		overrides = c.Rules.FileSize.Config.DirectoryOverrides
	case "mime_detection":
		baseEnabled = c.Rules.MimeDetection.Enabled
		overrides = c.Rules.MimeDetection.Config.DirectoryOverrides
	default:
		return false
	}

	// Check for directory override
	override := GetDirectoryOverride(overrides, filePath)
	if override != nil && override.Enabled != nil {
		return *override.Enabled
	}

	return baseEnabled
}

