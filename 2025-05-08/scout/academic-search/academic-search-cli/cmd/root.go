package cmd

import (
	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "academic-search",
	Short: "A CLI tool for searching academic papers",
	Long: `A command-line interface tool for searching academic papers from various 
sources including arXiv, Crossref, and Libgen.`,
}

// Execute adds all child commands to the root command and sets flags appropriately.
func Execute() error {
	return rootCmd.Execute()
}

func init() {
	rootCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}