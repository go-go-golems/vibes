package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/user/git-precommit-guard/pkg/config"
)

// validateConfigCmd represents the validate-config command
var validateConfigCmd = &cobra.Command{
	Use:   "validate-config",
	Short: "Validate configuration file",
	Long: `Validate the configuration file for syntax and logical errors.

This command loads and validates the configuration file, reporting any
issues found in the YAML syntax or configuration values.`,
	Run: runValidateConfig,
}

func runValidateConfig(cmd *cobra.Command, args []string) {
	configPath := getConfigFile()
	
	fmt.Printf("Validating configuration file: %s\n", configPath)
	
	// Load and validate configuration
	cfg, err := config.LoadConfig(configPath)
	if err != nil {
		exitWithError("Configuration validation failed: %v", err)
	}

	// Print configuration summary
	fmt.Println("\n✓ Configuration is valid!")
	fmt.Printf("  Version: %s\n", cfg.Version)
	fmt.Printf("  Timeout: %v\n", cfg.Settings.Timeout)
	fmt.Printf("  Fail Fast: %t\n", cfg.Settings.FailFast)
	
	fmt.Printf("\nGlobal Excludes (%d patterns):\n", len(cfg.Excludes))
	for _, exclude := range cfg.Excludes {
		fmt.Printf("  - %s\n", exclude)
	}

	fmt.Printf("\nRules:\n")
	
	// ELF Detection
	fmt.Printf("  ELF Detection: ")
	if cfg.Rules.ElfDetection.Enabled {
		fmt.Printf("✓ Enabled (%s)\n", cfg.Rules.ElfDetection.Severity)
		fmt.Printf("    Magic: %s\n", cfg.Rules.ElfDetection.Config.ElfMagic)
		fmt.Printf("    File Patterns: %d\n", len(cfg.Rules.ElfDetection.Config.FilePatterns))
		fmt.Printf("    MIME Types: %d\n", len(cfg.Rules.ElfDetection.Config.MimeTypes))
		fmt.Printf("    Directory Overrides: %d\n", len(cfg.Rules.ElfDetection.Config.DirectoryOverrides))
	} else {
		fmt.Printf("✗ Disabled\n")
	}

	// File Size
	fmt.Printf("  File Size: ")
	if cfg.Rules.FileSize.Enabled {
		fmt.Printf("✓ Enabled (%s)\n", cfg.Rules.FileSize.Severity)
		fmt.Printf("    Max Size: %d MB\n", cfg.Rules.FileSize.Config.MaxSizeMB)
		fmt.Printf("    Warn Size: %d MB\n", cfg.Rules.FileSize.Config.WarnSizeMB)
		fmt.Printf("    Directory Overrides: %d\n", len(cfg.Rules.FileSize.Config.DirectoryOverrides))
	} else {
		fmt.Printf("✗ Disabled\n")
	}

	// MIME Detection
	fmt.Printf("  MIME Detection: ")
	if cfg.Rules.MimeDetection.Enabled {
		fmt.Printf("✓ Enabled (%s)\n", cfg.Rules.MimeDetection.Severity)
		fmt.Printf("    Blocked Types: %d\n", len(cfg.Rules.MimeDetection.Config.BlockedTypes))
		fmt.Printf("    Allowed Types: %d\n", len(cfg.Rules.MimeDetection.Config.AllowedTypes))
		fmt.Printf("    Directory Overrides: %d\n", len(cfg.Rules.MimeDetection.Config.DirectoryOverrides))
	} else {
		fmt.Printf("✗ Disabled\n")
	}

	fmt.Printf("\nReporting:\n")
	fmt.Printf("  Format: %s\n", cfg.Reporting.Format)
	fmt.Printf("  Colors: %t\n", cfg.Reporting.Colors)
	fmt.Printf("  Show Passed: %t\n", cfg.Reporting.ShowPassed)
	fmt.Printf("  Summary: %t\n", cfg.Reporting.Summary)
}

