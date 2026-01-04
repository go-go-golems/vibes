package main

import (
	"context"
	"io"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"github.com/taskplanner/taskplanner/pkg/commands"
)

var rootCmd = &cobra.Command{
	Use:   "taskplanner",
	Short: "Dynamic hierarchical task planning with Redis and SQLite",
	Long: `TaskPlanner provides a CLI-driven hierarchical task planning system with real-time coordination.

It offers comprehensive task management capabilities:
- Hierarchical task organization with parent-child relationships
- Dynamic task planning and dependency management
- Real-time coordination via Redis for multi-agent environments
- Persistent storage with SQLite for reliability
- Agent-based task assignment and execution tracking

Key features:
- Create and manage complex task hierarchies
- Track task dependencies and status propagation
- Real-time task coordination and monitoring
- Agent registration and task assignment
- Progress tracking and reporting

Each agent identifies itself with AGENT_ID (via --agent flag or env var).
All coordination state is managed through Redis while persistent data is stored in SQLite.`,
	PersistentPreRun: func(cmd *cobra.Command, args []string) {
		// Set up logging
		level, _ := cmd.Flags().GetString("log-level")
		logLevel, err := zerolog.ParseLevel(level)
		if err != nil {
			log.Warn().Str("level", level).Msg("Invalid log level, using info")
			logLevel = zerolog.InfoLevel
		}
		zerolog.SetGlobalLevel(logLevel)
		
		// Set up dual logging: console + file
		logFile, err := os.OpenFile("/tmp/taskplanner.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
		if err != nil {
			log.Warn().Err(err).Msg("Failed to open log file, using console only")
			log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})
		} else {
			consoleWriter := zerolog.ConsoleWriter{Out: os.Stderr}
			multiWriter := io.MultiWriter(consoleWriter, logFile)
			log.Logger = log.Output(multiWriter)
			log.Info().Str("log_file", "/tmp/taskplanner.log").Msg("Logging to file and console")
		}
	},
}

func init() {
	// Global flags
	rootCmd.PersistentFlags().String("agent", "", "Agent ID (can also be set via AGENT_ID env var)")
	rootCmd.PersistentFlags().String("redis-url", "redis://localhost:6379", "Redis connection URL")
	rootCmd.PersistentFlags().String("database", "./taskplanner.db", "SQLite database path")
	rootCmd.PersistentFlags().String("format", "json", "Output format (json, text, table)")
	rootCmd.PersistentFlags().String("log-level", "info", "Log level (debug, info, warn, error)")

	// Bind flags to environment variables
	viper.BindPFlag("agent", rootCmd.PersistentFlags().Lookup("agent"))
	viper.BindPFlag("redis-url", rootCmd.PersistentFlags().Lookup("redis-url"))
	viper.BindPFlag("database", rootCmd.PersistentFlags().Lookup("database"))
	viper.BindPFlag("format", rootCmd.PersistentFlags().Lookup("format"))

	viper.BindEnv("agent", "AGENT_ID")
	viper.BindEnv("redis-url", "REDIS_URL")
	viper.BindEnv("database", "DATABASE_PATH")
}

func main() {
	// Create command groups
	planCommands := createPlanCommands()
	taskCommands := createTaskCommands()
	coordinationCommands := createCoordinationCommands()
	monitoringCommands := createMonitoringCommands()

	// Add all commands to root
	for _, cmd := range planCommands {
		cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
		if err != nil {
			log.Fatal().Err(err).Msg("Failed to build plan command")
		}
		rootCmd.AddCommand(cobraCmd)
	}

	for _, cmd := range taskCommands {
		cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
		if err != nil {
			log.Fatal().Err(err).Msg("Failed to build task command")
		}
		rootCmd.AddCommand(cobraCmd)
	}

	for _, cmd := range coordinationCommands {
		cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
		if err != nil {
			log.Fatal().Err(err).Msg("Failed to build coordination command")
		}
		rootCmd.AddCommand(cobraCmd)
	}

	for _, cmd := range monitoringCommands {
		cobraCmd, err := cli.BuildCobraCommandFromWriterCommand(cmd)
		if err != nil {
			log.Fatal().Err(err).Msg("Failed to build monitoring command")
		}
		rootCmd.AddCommand(cobraCmd)
	}

	// Execute
	ctx := context.Background()
	if err := rootCmd.ExecuteContext(ctx); err != nil {
		os.Exit(1)
	}
}

// createPlanCommands creates all plan-related commands
func createPlanCommands() []cmds.GlazeCommand {
	planCreateCmd, err := commands.NewPlanCreateCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create plan create command")
	}

	planListCmd, err := commands.NewPlanListCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create plan list command")
	}

	planShowCmd, err := commands.NewPlanShowCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create plan show command")
	}

	planDeleteCmd, err := commands.NewPlanDeleteCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create plan delete command")
	}

	return []cmds.GlazeCommand{
		planCreateCmd,
		planListCmd,
		planShowCmd,
		planDeleteCmd,
	}
}

// createTaskCommands creates all task-related commands
func createTaskCommands() []cmds.GlazeCommand {
	taskCreateCmd, err := commands.NewTaskCreateCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task create command")
	}

	taskListCmd, err := commands.NewTaskListCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task list command")
	}

	taskShowCmd, err := commands.NewTaskShowCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task show command")
	}

	taskUpdateCmd, err := commands.NewTaskUpdateCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task update command")
	}

	taskDeleteCmd, err := commands.NewTaskDeleteCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task delete command")
	}

	return []cmds.GlazeCommand{
		taskCreateCmd,
		taskListCmd,
		taskShowCmd,
		taskUpdateCmd,
		taskDeleteCmd,
	}
}

// createCoordinationCommands creates all coordination-related commands
func createCoordinationCommands() []cmds.GlazeCommand {
	taskClaimCmd, err := commands.NewTaskClaimCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task claim command")
	}

	taskReleaseCmd, err := commands.NewTaskReleaseCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task release command")
	}

	taskAssignCmd, err := commands.NewTaskAssignCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create task assign command")
	}

	return []cmds.GlazeCommand{
		taskClaimCmd,
		taskReleaseCmd,
		taskAssignCmd,
	}
}

// createMonitoringCommands creates all monitoring-related commands
func createMonitoringCommands() []cmds.WriterCommand {
	monitorCmd, err := commands.NewMonitorCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create monitor command")
	}

	statusCmd, err := commands.NewStatusCommand()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create status command")
	}

	return []cmds.WriterCommand{
		monitorCmd,
		statusCmd,
	}
}

