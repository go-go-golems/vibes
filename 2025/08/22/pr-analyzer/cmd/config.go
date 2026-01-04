package cmd

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
	"pr-analyzer/internal/analysis"
)

// Config represents the configuration file structure
type Config struct {
	Categories map[string][]string `yaml:"categories"`
	Excludes   []string            `yaml:"excludes"`
	Defaults   ConfigDefaults      `yaml:"defaults"`
}

type ConfigDefaults struct {
	OutputFormat string `yaml:"output_format"`
	UseDefaults  bool   `yaml:"use_defaults"`
}

var configCmd = &cobra.Command{
	Use:   "config",
	Short: "Manage configuration files",
	Long: `Create and manage configuration files for pr-analyzer.
Configuration files allow you to define default categories, exclude patterns,
and other settings.

Examples:
  # Create a default configuration file
  pr-analyzer config init

  # Show current configuration
  pr-analyzer config show

  # Validate configuration file
  pr-analyzer config validate`,
}

var configInitCmd = &cobra.Command{
	Use:   "init",
	Short: "Create a default configuration file",
	Long: `Create a default configuration file with common categories and settings.
The file will be created as .pr-analyzer.yaml in the current directory.`,
	RunE: runConfigInit,
}

var configShowCmd = &cobra.Command{
	Use:   "show",
	Short: "Show current configuration",
	Long:  `Display the current configuration that would be used for analysis.`,
	RunE:  runConfigShow,
}

var configValidateCmd = &cobra.Command{
	Use:   "validate",
	Short: "Validate configuration file",
	Long:  `Validate the syntax and content of a configuration file.`,
	RunE:  runConfigValidate,
}

func init() {
	rootCmd.AddCommand(configCmd)
	configCmd.AddCommand(configInitCmd)
	configCmd.AddCommand(configShowCmd)
	configCmd.AddCommand(configValidateCmd)
}

func runConfigInit(cmd *cobra.Command, args []string) error {
	configPath := ".pr-analyzer.yaml"
	
	// Check if file already exists
	if _, err := os.Stat(configPath); err == nil {
		return fmt.Errorf("configuration file already exists: %s", configPath)
	}

	// Create default configuration
	config := Config{
		Categories: analysis.GetDefaultCategories(),
		Excludes: []string{
			"*.md",
			"*.txt",
			"docs/**",
			"*.lock",
			"node_modules/**",
			"vendor/**",
		},
		Defaults: ConfigDefaults{
			OutputFormat: "table",
			UseDefaults:  true,
		},
	}

	// Write to file
	data, err := yaml.Marshal(config)
	if err != nil {
		return fmt.Errorf("failed to marshal configuration: %w", err)
	}

	err = os.WriteFile(configPath, data, 0644)
	if err != nil {
		return fmt.Errorf("failed to write configuration file: %w", err)
	}

	fmt.Printf("Configuration file created: %s\n", configPath)
	fmt.Printf("You can now edit this file to customize your analysis settings.\n")
	
	return nil
}

func runConfigShow(cmd *cobra.Command, args []string) error {
	config, err := loadConfig()
	if err != nil {
		return fmt.Errorf("failed to load configuration: %w", err)
	}

	data, err := yaml.Marshal(config)
	if err != nil {
		return fmt.Errorf("failed to marshal configuration: %w", err)
	}

	fmt.Printf("Current Configuration:\n")
	fmt.Printf("=====================\n\n")
	fmt.Print(string(data))
	
	return nil
}

func runConfigValidate(cmd *cobra.Command, args []string) error {
	configPath := configFile
	if configPath == "" {
		configPath = ".pr-analyzer.yaml"
	}

	// Check if file exists
	if _, err := os.Stat(configPath); os.IsNotExist(err) {
		return fmt.Errorf("configuration file not found: %s", configPath)
	}

	// Try to load and parse
	_, err := loadConfigFromFile(configPath)
	if err != nil {
		return fmt.Errorf("configuration validation failed: %w", err)
	}

	fmt.Printf("Configuration file is valid: %s\n", configPath)
	return nil
}

// loadConfig loads configuration from file or returns defaults
func loadConfig() (*Config, error) {
	configPath := configFile
	if configPath == "" {
		// Look for default config file
		if _, err := os.Stat(".pr-analyzer.yaml"); err == nil {
			configPath = ".pr-analyzer.yaml"
		} else {
			// Return default configuration
			return &Config{
				Categories: analysis.GetDefaultCategories(),
				Excludes:   []string{},
				Defaults: ConfigDefaults{
					OutputFormat: "table",
					UseDefaults:  false,
				},
			}, nil
		}
	}

	return loadConfigFromFile(configPath)
}

// loadConfigFromFile loads configuration from a specific file
func loadConfigFromFile(path string) (*Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var config Config
	err = yaml.Unmarshal(data, &config)
	if err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	return &config, nil
}

// getConfigPath returns the path to the configuration file
func getConfigPath() string {
	if configFile != "" {
		return configFile
	}
	
	// Look in current directory
	if _, err := os.Stat(".pr-analyzer.yaml"); err == nil {
		return ".pr-analyzer.yaml"
	}
	
	// Look in home directory
	home, err := os.UserHomeDir()
	if err == nil {
		homePath := filepath.Join(home, ".pr-analyzer.yaml")
		if _, err := os.Stat(homePath); err == nil {
			return homePath
		}
	}
	
	return ""
}

