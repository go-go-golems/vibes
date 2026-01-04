package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/help"
	"github.com/spf13/cobra"

	"github.com/farm/goat-manager/internal/cli/commands"
	"github.com/farm/goat-manager/internal/database"
)

func main() {
	ctx := context.Background()

	// Create the root command
	rootCmd := &cobra.Command{
		Use:   "goat-manager",
		Short: "Goat Farm Management System",
		Long: `A comprehensive goat farm management system with version control.
Manage goats, track milk production, health records, and farm operations
with built-in version control using embedded Dolt.`,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			// Initialize database connection
			config := database.DefaultConfig()
			client, err := database.OpenEnt(ctx, config)
			if err != nil {
				return fmt.Errorf("failed to connect to database: %w", err)
			}

			// Store client in context for subcommands
			cmd.SetContext(context.WithValue(ctx, "db_client", client))
			return nil
		},
	}

	// Set up help system
	helpSystem := help.NewHelpSystem()
	helpCmd := help.NewCobraHelpCommand(helpSystem)
	rootCmd.AddCommand(helpCmd)

	// Add subcommands
	goatCmd := commands.NewGoatCommand()
	milkCmd := commands.NewMilkCommand()
	healthCmd := commands.NewHealthCommand()
	breedingCmd := commands.NewBreedingCommand()
	feedCmd := commands.NewFeedCommand()
	farmCmd := commands.NewFarmCommand()
	analyticsCmd := commands.NewAnalyticsCommand()
	versionCmd := commands.NewVersionCommand()

	// Convert glazed commands to cobra commands
	cobraGoatCmd, err := cli.BuildCobraCommandFromGlazeCommand(goatCmd)
	if err != nil {
		log.Fatalf("Failed to build goat command: %v", err)
	}

	cobraMilkCmd, err := cli.BuildCobraCommandFromGlazeCommand(milkCmd)
	if err != nil {
		log.Fatalf("Failed to build milk command: %v", err)
	}

	cobraHealthCmd, err := cli.BuildCobraCommandFromGlazeCommand(healthCmd)
	if err != nil {
		log.Fatalf("Failed to build health command: %v", err)
	}

	cobraBreedingCmd, err := cli.BuildCobraCommandFromGlazeCommand(breedingCmd)
	if err != nil {
		log.Fatalf("Failed to build breeding command: %v", err)
	}

	cobraFeedCmd, err := cli.BuildCobraCommandFromGlazeCommand(feedCmd)
	if err != nil {
		log.Fatalf("Failed to build feed command: %v", err)
	}

	cobraFarmCmd, err := cli.BuildCobraCommandFromGlazeCommand(farmCmd)
	if err != nil {
		log.Fatalf("Failed to build farm command: %v", err)
	}

	cobraVersionCmd, err := cli.BuildCobraCommandFromGlazeCommand(versionCmd)
	if err != nil {
		log.Fatalf("Failed to build version command: %v", err)
	}

	cobraAnalyticsCmd, err := cli.BuildCobraCommandFromGlazeCommand(analyticsCmd)
	if err != nil {
		log.Fatalf("Failed to build analytics command: %v", err)
	}

	// Add commands to root
	rootCmd.AddCommand(cobraGoatCmd)
	rootCmd.AddCommand(cobraMilkCmd)
	rootCmd.AddCommand(cobraHealthCmd)
	rootCmd.AddCommand(cobraBreedingCmd)
	rootCmd.AddCommand(cobraFeedCmd)
	rootCmd.AddCommand(cobraFarmCmd)
	rootCmd.AddCommand(cobraAnalyticsCmd)
	rootCmd.AddCommand(cobraVersionCmd)

	// Execute the root command
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

