package main

import (
    "embed"
	"encoding/json"
	"fmt"
	"os"
	"strings"

    "github.com/go-go-golems/glazed/pkg/help"
    help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/spf13/cobra"
)

var (
	sessionName   string
	setVars       []string
	setJSON       string
	setJSONFile   string
	dryRun        bool
)

//go:embed docs/*.md
var docsFS embed.FS

func main() {
    setupHelpSystem()
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

var rootCmd = &cobra.Command{
	Use:   "tmux-dashboard",
	Short: "A tmux dashboard tool based on YAML configuration",
	Long: `tmux-dashboard is a tool for creating tmux dashboards from YAML configuration files.
It supports template variables, includes, and various tmux layouts.`,
}

var applyCmd = &cobra.Command{
	Use:   "apply <config.yml>",
	Short: "Apply a tmux dashboard configuration",
	Long: `Apply a tmux dashboard configuration file to create a tmux session with
windows, panes, and commands as specified in the YAML file.`,
	Args: cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		configFile := args[0]
		return applyConfig(configFile)
	},
}

var renderCmd = &cobra.Command{
	Use:   "render <config.yml>",
	Short: "Render the resolved configuration after includes and variable substitution",
	Long: `Render the resolved YAML configuration after processing includes and
performing variable substitution. This is useful for debugging configurations.`,
	Args: cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		configFile := args[0]
		return renderConfig(configFile)
	},
}

var validateCmd = &cobra.Command{
	Use:   "validate <config.yml>",
	Short: "Validate a configuration file for syntax and schema errors",
	Long: `Validate a tmux dashboard configuration file for syntax errors,
schema compliance, and logical consistency.`,
	Args: cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		configFile := args[0]
		return validateConfig(configFile)
	},
}

func init() {
	// Add subcommands
	rootCmd.AddCommand(applyCmd)
	rootCmd.AddCommand(renderCmd)
	rootCmd.AddCommand(validateCmd)

	// Add flags to all commands
	for _, cmd := range []*cobra.Command{applyCmd, renderCmd, validateCmd} {
		cmd.Flags().StringVar(&sessionName, "session", "", "Override session name")
		cmd.Flags().StringArrayVar(&setVars, "set", []string{}, "Set variable (key=value)")
		cmd.Flags().StringVar(&setJSON, "set-json", "", "Set variables from JSON string")
		cmd.Flags().StringVar(&setJSONFile, "set-json-file", "", "Set variables from JSON file")
	}

	// Add dry-run flag only to apply command
	applyCmd.Flags().BoolVar(&dryRun, "dry-run", false, "Print tmux commands without executing")
}

func setupHelpSystem() {
    hs := help.NewHelpSystem()
    // Load embedded documentation pages; ignore error if none found
    if err := hs.LoadSectionsFromFS(docsFS, "docs"); err != nil {
        // Best-effort: continue without embedded docs
    }
    help_cmd.SetupCobraRootCommand(hs, rootCmd)
}

func applyConfig(configFile string) error {
	config, err := loadAndProcessConfig(configFile)
	if err != nil {
		return err
	}

	// Override session name if provided
	if sessionName != "" {
		config.Session = sessionName
	}

	// Create tmux manager
	tm, err := NewTmuxManager(dryRun)
	if err != nil {
		return fmt.Errorf("failed to create tmux manager: %w", err)
	}

	// Apply configuration
	if err := tm.ApplyConfig(config); err != nil {
		return fmt.Errorf("failed to apply configuration: %w", err)
	}

	if !dryRun {
		fmt.Printf("Successfully created tmux session '%s'\n", config.Session)
	}

	return nil
}

func renderConfig(configFile string) error {
	config, err := loadAndProcessConfig(configFile)
	if err != nil {
		return err
	}

	// Override session name if provided
	if sessionName != "" {
		config.Session = sessionName
	}

	// Convert back to YAML and print
	output, err := configToYAML(config)
	if err != nil {
		return fmt.Errorf("failed to convert config to YAML: %w", err)
	}

	fmt.Print(output)
	return nil
}

func validateConfig(configFile string) error {
	config, err := loadAndProcessConfig(configFile)
	if err != nil {
		return err
	}

	fmt.Printf("Configuration file '%s' is valid\n", configFile)
	fmt.Printf("Session: %s\n", config.Session)
	fmt.Printf("Tabs: %d\n", len(config.Tabs))

	for i, tab := range config.Tabs {
		fmt.Printf("  Tab %d: %s (%d panes)\n", i+1, tab.Name, len(tab.Panes))
	}

	return nil
}

func loadAndProcessConfig(configFile string) (*Config, error) {
	// Load config with includes
	config, err := LoadConfigWithIncludes(configFile)
	if err != nil {
		return nil, fmt.Errorf("failed to load config: %w", err)
	}

	// Validate config
	if err := config.Validate(); err != nil {
		return nil, fmt.Errorf("config validation failed: %w", err)
	}

	// Parse variables from command line flags
	vars, err := parseVariables()
	if err != nil {
		return nil, fmt.Errorf("failed to parse variables: %w", err)
	}

	// Substitute variables
	if err := config.SubstituteVars(vars); err != nil {
		return nil, fmt.Errorf("variable substitution failed: %w", err)
	}

	return config, nil
}

func parseVariables() (map[string]string, error) {
	vars := make(map[string]string)

	// Parse --set flags
	for _, setVar := range setVars {
		parts := strings.SplitN(setVar, "=", 2)
		if len(parts) != 2 {
			return nil, fmt.Errorf("invalid --set format: %s (expected key=value)", setVar)
		}
		vars[parts[0]] = parts[1]
	}

	// Parse --set-json-file
	if setJSONFile != "" {
		data, err := os.ReadFile(setJSONFile)
		if err != nil {
			return nil, fmt.Errorf("failed to read JSON file %s: %w", setJSONFile, err)
		}

		var jsonVars map[string]interface{}
		if err := json.Unmarshal(data, &jsonVars); err != nil {
			return nil, fmt.Errorf("failed to parse JSON file %s: %w", setJSONFile, err)
		}

		for k, v := range jsonVars {
			vars[k] = fmt.Sprintf("%v", v)
		}
	}

	// Parse --set-json (highest precedence)
	if setJSON != "" {
		var jsonVars map[string]interface{}
		if err := json.Unmarshal([]byte(setJSON), &jsonVars); err != nil {
			return nil, fmt.Errorf("failed to parse JSON string: %w", err)
		}

		for k, v := range jsonVars {
			vars[k] = fmt.Sprintf("%v", v)
		}
	}

	return vars, nil
}

func configToYAML(config *Config) (string, error) {
	// Simple YAML output - in a real implementation you'd use yaml.Marshal
	var sb strings.Builder
	
	sb.WriteString(fmt.Sprintf("version: %d\n", config.Version))
	sb.WriteString(fmt.Sprintf("session: \"%s\"\n", config.Session))
	
	if len(config.Vars) > 0 {
		sb.WriteString("vars:\n")
		for k, v := range config.Vars {
			sb.WriteString(fmt.Sprintf("  %s: \"%s\"\n", k, v))
		}
	}
	
	sb.WriteString("tabs:\n")
	for _, tab := range config.Tabs {
		sb.WriteString(fmt.Sprintf("  - name: \"%s\"\n", tab.Name))
		if tab.Layout != "" {
			sb.WriteString(fmt.Sprintf("    layout: %s\n", tab.Layout))
		}
		sb.WriteString("    panes:\n")
		for _, pane := range tab.Panes {
			sb.WriteString(fmt.Sprintf("      - cmd: \"%s\"\n", pane.Cmd))
			if pane.Refresh > 0 {
				sb.WriteString(fmt.Sprintf("        refresh: %d\n", pane.Refresh))
			}
			if len(pane.Env) > 0 {
				sb.WriteString("        env:\n")
				for k, v := range pane.Env {
					sb.WriteString(fmt.Sprintf("          %s: \"%s\"\n", k, v))
				}
			}
		}
	}
	
	return sb.String(), nil
}

