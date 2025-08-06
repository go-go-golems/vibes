package commands

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/spf13/cobra"

	"diary-cli/pkg/config"
)

// NewConfigCommand creates the config command
func NewConfigCommand(cfg *config.Config) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "config [key] [value]",
		Short: "Show or set configuration values",
		Long: `Show current configuration or set configuration values.

Configuration options:
  vault_path     - Path to Obsidian vault directory
  logs_path      - Relative path to logs directory within vault
  date_format    - Date format for file names (default: 2006-01-02)
  default_limit  - Default number of entries to show in lists
  editor         - Preferred editor command

Examples:
  diary config                                    # Show all config
  diary config vault_path                         # Show vault path
  diary config vault_path /path/to/vault          # Set vault path
  diary config default_limit 20                   # Set default limit
  diary config edit                               # Edit config file in editor`,
		RunE: func(cmd *cobra.Command, args []string) error {
			switch len(args) {
			case 0:
				return showAllConfig(cfg)
			case 1:
				if args[0] == "edit" {
					return editConfig(cfg)
				}
				return showConfigValue(cfg, args[0])
			case 2:
				return setConfigValue(cfg, args[0], args[1])
			default:
				return fmt.Errorf("too many arguments")
			}
		},
	}

	return cmd
}

// showAllConfig displays all configuration values
func showAllConfig(cfg *config.Config) error {
	fmt.Println("Current configuration:")
	fmt.Printf("  vault_path:     %s\n", cfg.VaultPath)
	fmt.Printf("  logs_path:      %s\n", cfg.LogsPath)
	fmt.Printf("  date_format:    %s\n", cfg.DateFormat)
	fmt.Printf("  default_limit:  %d\n", cfg.DefaultLimit)
	fmt.Printf("  editor:         %s\n", cfg.Editor)
	fmt.Println()
	
	// Show config file path
	fmt.Println("Configuration file:")
	fmt.Printf("  config_file:    %s\n", config.GetConfigPath())
	fmt.Println()
	
	// Show computed paths
	fmt.Println("Computed paths:")
	fmt.Printf("  logs_dir:       %s\n", cfg.GetLogsDir())
	fmt.Printf("  today_file:     %s\n", cfg.GetTodayFile())
	fmt.Printf("  effective_editor: %s\n", cfg.GetEditor())
	
	// Show file existence
	fmt.Println()
	fmt.Println("Status:")
	if _, err := os.Stat(cfg.VaultPath); os.IsNotExist(err) {
		fmt.Printf("  vault_path:     ❌ Does not exist\n")
	} else {
		fmt.Printf("  vault_path:     ✅ Exists\n")
	}
	
	if _, err := os.Stat(cfg.GetLogsDir()); os.IsNotExist(err) {
		fmt.Printf("  logs_dir:       ❌ Does not exist\n")
	} else {
		fmt.Printf("  logs_dir:       ✅ Exists\n")
	}
	
	if _, err := os.Stat(cfg.GetTodayFile()); os.IsNotExist(err) {
		fmt.Printf("  today_file:     ❌ Does not exist\n")
	} else {
		fmt.Printf("  today_file:     ✅ Exists\n")
	}

	return nil
}

// showConfigValue displays a specific configuration value
func showConfigValue(cfg *config.Config, key string) error {
	switch key {
	case "vault_path":
		fmt.Println(cfg.VaultPath)
	case "logs_path":
		fmt.Println(cfg.LogsPath)
	case "date_format":
		fmt.Println(cfg.DateFormat)
	case "default_limit":
		fmt.Println(cfg.DefaultLimit)
	case "editor":
		fmt.Println(cfg.Editor)
	case "logs_dir":
		fmt.Println(cfg.GetLogsDir())
	case "today_file":
		fmt.Println(cfg.GetTodayFile())
	case "effective_editor":
		fmt.Println(cfg.GetEditor())
	default:
		return fmt.Errorf("unknown config key: %s", key)
	}
	return nil
}

// setConfigValue sets a configuration value and saves the config
func setConfigValue(cfg *config.Config, key, value string) error {
	switch key {
	case "vault_path":
		// Expand path
		if value[0] == '~' {
			homeDir, _ := os.UserHomeDir()
			value = filepath.Join(homeDir, value[1:])
		}
		absPath, err := filepath.Abs(value)
		if err != nil {
			return fmt.Errorf("invalid path: %w", err)
		}
		cfg.VaultPath = absPath
		
	case "logs_path":
		cfg.LogsPath = value
		
	case "date_format":
		cfg.DateFormat = value
		
	case "default_limit":
		var limit int
		if _, err := fmt.Sscanf(value, "%d", &limit); err != nil {
			return fmt.Errorf("invalid number: %s", value)
		}
		if limit <= 0 {
			return fmt.Errorf("default_limit must be positive")
		}
		cfg.DefaultLimit = limit
		
	case "editor":
		cfg.Editor = value
		
	default:
		return fmt.Errorf("unknown config key: %s", key)
	}

	// Save configuration
	if err := cfg.Save(); err != nil {
		return fmt.Errorf("failed to save config: %w", err)
	}

	fmt.Printf("✓ Set %s = %s\n", key, value)
	return nil
}

// editConfig opens the configuration file in the user's preferred editor
func editConfig(cfg *config.Config) error {
	configPath := config.GetConfigPath()
	editor := cfg.GetEditor()
	
	fmt.Printf("Opening config file in %s: %s\n", editor, configPath)
	
	// Check if config file exists, create if not
	if _, err := os.Stat(configPath); os.IsNotExist(err) {
		fmt.Println("Config file does not exist, creating default configuration...")
		if err := cfg.Save(); err != nil {
			return fmt.Errorf("failed to create config file: %w", err)
		}
	}
	
	// Open editor
	cmd := exec.Command(editor, configPath)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to open editor: %w", err)
	}
	
	fmt.Println("✓ Configuration file edited successfully")
	return nil
}

