package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var (
	configFile string
	verbose    bool
	jsonOutput bool
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "git-precommit-guard",
	Short: "A git pre-commit hook utility to detect undesired files",
	Long: `Git Pre-commit Guard is a utility designed to be used as a git pre-commit hook
to detect and prevent committing undesired files such as:

- ELF binaries and executables
- Files with certain MIME types
- Files that exceed size limits

The tool is highly configurable through YAML configuration files and supports
directory-specific overrides for different rules.`,
	Version: "1.0.0",
}

// Execute adds all child commands to the root command and sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	err := rootCmd.Execute()
	if err != nil {
		os.Exit(1)
	}
}

func init() {
	// Global flags
	rootCmd.PersistentFlags().StringVarP(&configFile, "config", "c", "", "config file (default is .precommit-guard.yml)")
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
	rootCmd.PersistentFlags().BoolVarP(&jsonOutput, "json", "j", false, "output results in JSON format")

	// Add subcommands
	rootCmd.AddCommand(checkCmd)
	rootCmd.AddCommand(installCmd)
	rootCmd.AddCommand(validateConfigCmd)
}

// getConfigFile returns the config file path, using default if not specified
func getConfigFile() string {
	if configFile != "" {
		return configFile
	}
	return "" // Let the config loader find the default
}

// exitWithError prints an error message and exits with code 1
func exitWithError(msg string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, "Error: "+msg+"\n", args...)
	os.Exit(1)
}

