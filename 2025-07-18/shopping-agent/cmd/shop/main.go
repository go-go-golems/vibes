package main

import (
	"context"
	"os"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"shopping-agent/pkg/agent"
)

func main() {
	// Initialize logging
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnix
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})

	// Create root command
	rootCmd := &cobra.Command{
		Use:   "shop",
		Short: "A shopping agent that can search, compare, and automate shopping tasks",
		Long: `Shopping Agent is a CLI tool built with Go and glazed that provides:
- Product search across multiple e-commerce sites
- Price comparison and monitoring
- Screenshot capture of product pages
- Automated shopping workflows
- Structured data output in multiple formats`,
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			// Set log level based on flag
			logLevel, _ := cmd.Flags().GetString("log-level")
			switch logLevel {
			case "debug":
				zerolog.SetGlobalLevel(zerolog.DebugLevel)
			case "info":
				zerolog.SetGlobalLevel(zerolog.InfoLevel)
			case "warn":
				zerolog.SetGlobalLevel(zerolog.WarnLevel)
			case "error":
				zerolog.SetGlobalLevel(zerolog.ErrorLevel)
			default:
				zerolog.SetGlobalLevel(zerolog.InfoLevel)
			}
		},
	}

	// Add global flags
	rootCmd.PersistentFlags().String("log-level", "info", "Set the logging level (debug, info, warn, error)")

	// Add commands
	rootCmd.AddCommand(agent.NewSearchCommand())
	rootCmd.AddCommand(agent.NewScreenshotCommand())
	rootCmd.AddCommand(agent.NewCompareCommand())
	rootCmd.AddCommand(agent.NewMonitorCommand())

	// Execute
	if err := rootCmd.ExecuteContext(context.Background()); err != nil {
		log.Error().Err(err).Msg("Failed to execute command")
		os.Exit(1)
	}
}

