package main

import (
	"fmt"
	"os"

	clay "github.com/go-go-golems/clay/pkg"
	"github.com/go-go-golems/clay/pkg/doc"
	"github.com/go-go-golems/clay/pkg/repositories"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/help"
	pinocchio_cmds "github.com/go-go-golems/pinocchio/pkg/cmds"
	goagentcmds "github.com/goagent/framework/goagent/cmds"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

func main() {
	helpSystem := help.NewHelpSystem()
	err := doc.AddDocToHelpSystem(helpSystem)
	cobra.CheckErr(err)

	rootCmd := &cobra.Command{
		Use:   "goagent",
		Short: "GoAgent CLI - Execute AI agents from the command line",
		Long: `GoAgent CLI is a tool for executing AI agents from the command line.
It supports various agent types like ReAct, Plan-and-Execute, and File Collection.`,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			// Setup debug logging
			return logging.InitLoggerFromViper()
		},
	}

	// Initialize Viper right after creating the rootCmd
	err = clay.InitViper("pinocchio", rootCmd) // Use "goagent" or a suitable config name
	cobra.CheckErr(err)

	// Setup zerolog
	err = logging.InitLoggerFromViper()
	cobra.CheckErr(err)

	// Setup help system
	helpSystem.SetupCobraRootCommand(rootCmd)

	// --- Load commands from repository ---
	repoPath := "/home/manuel/code/wesen/corporate-headquarters/vibes/2025-04-21/llm-agent-complete/llm-agent-framework/goagent/examples/commands"
	// check if repoPath exists and is a directory
	if fi, err := os.Stat(repoPath); err == nil && fi.IsDir() {
		loader := &goagentcmds.AgentCommandLoader{}
		repo := repositories.NewRepository(
			repositories.WithDirectories(repositories.Directory{
				FS:            os.DirFS(repoPath),
				RootDirectory: ".",
				Name:          "file-agents",
				SourcePrefix:  "file",
			}),
			repositories.WithCommandLoader(loader),
		)

		// Load commands into the repository
		err = repo.LoadCommands(helpSystem)
		if err != nil {
			log.Warn().Err(err).Str("path", repoPath).Msg("Error loading commands from repository")
			// Don't exit, maybe other commands loaded fine
		} else {
			// Collect commands from the repository
			loadedCommands := repo.CollectCommands([]string{}, true)
			log.Info().Int("count", len(loadedCommands)).Str("path", repoPath).Msg("Loaded commands from repository")

			// Add commands from repository to Cobra
			for _, cmd := range loadedCommands {
				cobraCmd, err := pinocchio_cmds.BuildCobraCommandWithGeppettoMiddlewares(cmd)
				if err != nil {
					log.Error().Err(err).Str("command", cmd.Description().Name).Msg("Error building cobra command from repository agent command")
					continue
				}
				rootCmd.AddCommand(cobraCmd)
			}
		}
	} else {
		log.Warn().Str("path", repoPath).Msg("Repository path does not exist or is not a directory, skipping.")
	}

	log.Info().Msg("Starting GoAgent CLI")
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
