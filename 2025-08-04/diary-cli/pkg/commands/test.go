package commands

import (
	"fmt"
	"time"

	"github.com/spf13/cobra"

	"diary-cli/pkg/config"
)

// NewTestCommand creates the test command
func NewTestCommand(cfg *config.Config) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "test",
		Short: "Test configuration and template expansion",
		Long: `Test command for debugging configuration and template expansion.

This command prints various configuration values and computed paths
to help debug template expansion and path resolution.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runTestCommand(cfg)
		},
	}

	return cmd
}

// runTestCommand executes the test command
func runTestCommand(cfg *config.Config) error {
	fmt.Println("=== Configuration Test ===")
	fmt.Println()
	
	// Show current date
	today := time.Now()
	fmt.Printf("Current date: %s\n", today.Format("2006-01-02"))
	fmt.Printf("Date format: %s\n", cfg.DateFormat)
	fmt.Println()
	
	// Show template processing
	fmt.Println("=== Template Processing ===")
	fmt.Printf("Vault path (raw): %s\n", cfg.VaultPath)
	fmt.Printf("Logs path (raw): %s\n", cfg.LogsPath)
	fmt.Println()
	
	// Show processed paths for today
	fmt.Println("=== Today's Paths ===")
	logsDir := cfg.GetLogsDirForDate(today)
	todayFile := cfg.GetDateFile(today)
	
	fmt.Printf("Logs directory (processed): %s\n", logsDir)
	fmt.Printf("Today's file: %s\n", todayFile)
	fmt.Println()
	
	// Show template examples
	fmt.Println("=== Template Examples ===")
	testDate := time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC)
	
	fmt.Printf("Test date: %s\n", testDate.Format("2006-01-02"))
	fmt.Printf("YYYY-MM-DD replacement: %s\n", cfg.ProcessPathTemplate("Logs/YYYY-MM-DD", testDate))
	fmt.Printf("YYYY replacement: %s\n", cfg.ProcessPathTemplate("Logs/YYYY", testDate))
	fmt.Printf("MM replacement: %s\n", cfg.ProcessPathTemplate("Logs/MM", testDate))
	fmt.Printf("DD replacement: %s\n", cfg.ProcessPathTemplate("Logs/DD", testDate))
	fmt.Println()
	
	// Show config file info
	fmt.Println("=== Configuration File ===")
	fmt.Printf("Config file path: %s\n", config.GetConfigPath())
	fmt.Printf("Effective editor: %s\n", cfg.GetEditor())
	
	return nil
} 