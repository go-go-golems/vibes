package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/cmd/ttmp/validate"
	"github.com/scrapybara/ttmp/cmd/ttmp/query"
	"github.com/scrapybara/ttmp/cmd/ttmp/create"
	"github.com/scrapybara/ttmp/cmd/ttmp/list"
	"github.com/scrapybara/ttmp/cmd/ttmp/stats"
	"github.com/scrapybara/ttmp/cmd/ttmp/generate"
	"github.com/scrapybara/ttmp/cmd/ttmp/convert"
	"github.com/scrapybara/ttmp/pkg/parser"
)

var (
	// Global flags
	verbose  bool
	quiet    bool
	jsonOutput bool
	configFile string
	version bool
)

// Version information
const (
	appName    = "ttmp"
	appVersion = "0.1.0"
)

var rootCmd = &cobra.Command{
	Use:   "ttmp",
	Short: "TTMP - Text with Text Metadata Preamble",
	Long: `TTMP is a tool for working with markdown documents that have YAML frontmatter.
It helps create, validate, query, and manage documents with structured metadata.

Examples:
  ttmp create --id "debugging-guide" --title "Debugging Guide" doc.md    Create a new document
  ttmp validate path/to/document.md                                      Validate a document
  ttmp list --format table ./docs                                        List documents in a directory
  ttmp query --tags "guide,debugging" --format detailed ./docs           Find documents by criteria
  ttmp stats ./docs                                                      Show statistics about documents
  ttmp generate toc README.md                                            Generate table of contents
  ttmp convert markdown --to html document.md                            Convert to other formats`,
	Run: func(cmd *cobra.Command, args []string) {
		if version {
			fmt.Printf("%s version %s\n", appName, appVersion)
			return
		}
		
		// If no subcommand is specified, show help
		cmd.Help()
	},
}

func init() {
	// Setup logger
	logger := logrus.New()
	logger.SetOutput(os.Stdout)
	
	// Create parser
	ttmpParser := parser.NewTTMPParser(logger)
	
	// Add commands
	rootCmd.AddCommand(validate.NewValidateCommand(logger, ttmpParser))
	rootCmd.AddCommand(query.NewQueryCommand(logger, ttmpParser))
	rootCmd.AddCommand(create.NewCreateCommand(logger, ttmpParser))
	rootCmd.AddCommand(list.NewListCommand(logger, ttmpParser))
	rootCmd.AddCommand(stats.NewStatsCommand(logger, ttmpParser))
	rootCmd.AddCommand(generate.NewGenerateCommand(logger, ttmpParser))
	rootCmd.AddCommand(convert.NewConvertCommand(logger, ttmpParser))
	
	// Add global flags
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable verbose output")
	rootCmd.PersistentFlags().BoolVarP(&quiet, "quiet", "q", false, "Suppress non-essential output")
	rootCmd.PersistentFlags().BoolVar(&jsonOutput, "json", false, "Output results in JSON format")
	rootCmd.PersistentFlags().StringVar(&configFile, "config", "", "Config file path")
	
	// Version flag
	rootCmd.Flags().BoolVar(&version, "version", false, "Show version information")
	
	// Set log level based on flags
	cobra.OnInitialize(func() {
		if verbose {
			logger.SetLevel(logrus.DebugLevel)
		} else if quiet {
			logger.SetLevel(logrus.ErrorLevel)
		} else {
			logger.SetLevel(logrus.InfoLevel)
		}
	})
}

func main() {
	// Create default config directory if it doesn't exist
	configDir, err := getConfigDir()
	if err == nil {
		os.MkdirAll(configDir, 0755)
	}
	
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

// getConfigDir returns the default configuration directory
func getConfigDir() (string, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	
	return filepath.Join(homeDir, ".ttmp"), nil
}