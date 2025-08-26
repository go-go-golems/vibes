package config

import (
	"time"
)

// Config represents the main configuration structure
type Config struct {
	Version   string             `yaml:"version"`
	Settings  GlobalSettings     `yaml:"settings"`
	Excludes  []string           `yaml:"global_excludes"`
	Rules     Rules              `yaml:"rules"`
	Reporting ReportingConfig    `yaml:"reporting"`
}

// GlobalSettings contains global configuration options
type GlobalSettings struct {
	FailFast bool          `yaml:"fail_fast"`
	Timeout  time.Duration `yaml:"timeout"`
}

// Rules contains all the detection rules
type Rules struct {
	ElfDetection  ElfDetectionRule  `yaml:"elf_detection"`
	FileSize      FileSizeRule      `yaml:"file_size"`
	MimeDetection MimeDetectionRule `yaml:"mime_detection"`
}

// ElfDetectionRule configures ELF binary detection
type ElfDetectionRule struct {
	Enabled     bool                        `yaml:"enabled"`
	Description string                      `yaml:"description"`
	Severity    string                      `yaml:"severity"`
	Config      ElfDetectionConfig          `yaml:"config"`
}

// ElfDetectionConfig contains ELF detection configuration
type ElfDetectionConfig struct {
	ElfMagic           string                         `yaml:"elf_magic"`
	FilePatterns       []string                       `yaml:"file_patterns"`
	MimeTypes          []string                       `yaml:"mime_types"`
	Message            string                         `yaml:"message"`
	DirectoryOverrides map[string]DirectoryOverride   `yaml:"directory_overrides"`
}

// FileSizeRule configures file size limits
type FileSizeRule struct {
	Enabled     bool                        `yaml:"enabled"`
	Description string                      `yaml:"description"`
	Severity    string                      `yaml:"severity"`
	Config      FileSizeConfig              `yaml:"config"`
}

// FileSizeConfig contains file size configuration
type FileSizeConfig struct {
	MaxSizeMB          int                            `yaml:"max_size_mb"`
	WarnSizeMB         int                            `yaml:"warn_size_mb"`
	Message            string                         `yaml:"message"`
	DirectoryOverrides map[string]DirectoryOverride   `yaml:"directory_overrides"`
}

// MimeDetectionRule configures MIME type detection
type MimeDetectionRule struct {
	Enabled     bool                        `yaml:"enabled"`
	Description string                      `yaml:"description"`
	Severity    string                      `yaml:"severity"`
	Config      MimeDetectionConfig         `yaml:"config"`
}

// MimeDetectionConfig contains MIME detection configuration
type MimeDetectionConfig struct {
	BlockedTypes       []string                       `yaml:"blocked_types"`
	AllowedTypes       []string                       `yaml:"allowed_types"`
	Message            string                         `yaml:"message"`
	DirectoryOverrides map[string]DirectoryOverride   `yaml:"directory_overrides"`
}

// DirectoryOverride allows per-directory rule customization
type DirectoryOverride struct {
	Enabled      *bool    `yaml:"enabled,omitempty"`
	MaxSizeMB    *int     `yaml:"max_size_mb,omitempty"`
	BlockedTypes []string `yaml:"blocked_types,omitempty"`
}

// ReportingConfig configures output formatting
type ReportingConfig struct {
	Format     string `yaml:"format"`
	ShowPassed bool   `yaml:"show_passed"`
	Colors     bool   `yaml:"colors"`
	Summary    bool   `yaml:"summary"`
}

