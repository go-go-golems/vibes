/*
Copyright © 2025 NAME HERE <EMAIL ADDRESS>

*/
package cmd

import (
	"os"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var debugMode bool

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "arxiv-libgen-cli",
	Short: "A CLI tool to search Arxiv, LibGen, Crossref, and OpenAlex for scientific papers.",
	Long: `arxiv-libgen-cli is a command-line tool that allows users to search for scientific papers across multiple academic databases and repositories including Arxiv, Library Genesis, Crossref, and OpenAlex.

It provides specific subcommands for each platform, allowing targeted searches with various filters and options.

Examples:
  arxiv-libgen-cli arxiv -q "all:electron" -n 5
  arxiv-libgen-cli libgen -q "artificial intelligence" -m "https://libgen.is"
  arxiv-libgen-cli crossref -q "climate change mitigation"
  arxiv-libgen-cli openalex -q "machine learning applications"`,
	PersistentPreRun: func(cmd *cobra.Command, args []string) {
		zerolog.TimeFieldFormat = zerolog.TimeFormatUnixMs
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
		if debugMode {
			zerolog.SetGlobalLevel(zerolog.DebugLevel)
			// Use console writer for more readable debug logs
			log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr, TimeFormat: time.RFC3339})
		}
		log.Debug().Msg("Debug mode enabled")
	},
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
	rootCmd.PersistentFlags().BoolVarP(&debugMode, "debug", "d", false, "Enable debug logging")
	// Remove the default toggle flag if it exists from the initial cobra init
	// rootCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}

