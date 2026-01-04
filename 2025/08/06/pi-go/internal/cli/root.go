package cli

import (
	"fmt"

	"github.com/pi-go/pi/internal/cli/commands/config"
	"github.com/pi-go/pi/internal/cli/commands/model"
	"github.com/pi-go/pi/internal/cli/commands/pod"
	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

// NewRootCommand creates the root command for the pi CLI
func NewRootCommand(cfg *types.Config, logger *logrus.Logger, version, commit, date string) *cobra.Command {
	rootCmd := &cobra.Command{
		Use:   "pi",
		Short: "Pi-Go - GPU pod and vLLM model management CLI",
		Long: `Pi-Go is a command-line tool for managing GPU pods and vLLM model deployments.
It provides secure SSH tunneling, resource management, and monitoring capabilities
for distributed AI model serving infrastructure.`,
		Version: fmt.Sprintf("%s (commit: %s, built: %s)", version, commit, date),
		SilenceUsage: true,
	}

	// Add global flags
	rootCmd.PersistentFlags().String("config", "", "config file (default is $HOME/.config/pi-go/pi-go.yaml)")
	rootCmd.PersistentFlags().String("log-level", "info", "log level (debug, info, warn, error)")
	rootCmd.PersistentFlags().String("log-format", "text", "log format (text, json)")

	// Add command groups
	addPodCommands(rootCmd, cfg, logger)
	addModelCommands(rootCmd, cfg, logger)
	addConfigCommands(rootCmd, cfg, logger)

	return rootCmd
}

// addPodCommands adds pod management commands
func addPodCommands(rootCmd *cobra.Command, cfg *types.Config, logger *logrus.Logger) {
	podCmd := &cobra.Command{
		Use:   "pod",
		Short: "Manage GPU pods",
		Long:  "Commands for managing GPU pods including registration, status checking, and shell access.",
	}

	// Add pod subcommands
	podCmd.AddCommand(pod.NewAddCommand(cfg, logger))
	podCmd.AddCommand(pod.NewListCommand(cfg, logger))
	podCmd.AddCommand(pod.NewRemoveCommand(cfg, logger))
	podCmd.AddCommand(pod.NewActivateCommand(cfg, logger))
	podCmd.AddCommand(pod.NewShellCommand(cfg, logger))
	podCmd.AddCommand(pod.NewStatusCommand(cfg, logger))

	rootCmd.AddCommand(podCmd)
}

// addModelCommands adds model management commands
func addModelCommands(rootCmd *cobra.Command, cfg *types.Config, logger *logrus.Logger) {
	modelCmd := &cobra.Command{
		Use:   "model",
		Short: "Manage vLLM models",
		Long:  "Commands for managing vLLM model deployments including starting, stopping, and monitoring.",
	}

	// Add model subcommands
	modelCmd.AddCommand(model.NewStartCommand(cfg, logger))
	modelCmd.AddCommand(model.NewListCommand(cfg, logger))
	modelCmd.AddCommand(model.NewStopCommand(cfg, logger))
	modelCmd.AddCommand(model.NewLogsCommand(cfg, logger))
	modelCmd.AddCommand(model.NewStatusCommand(cfg, logger))

	rootCmd.AddCommand(modelCmd)
}

// addConfigCommands adds configuration management commands
func addConfigCommands(rootCmd *cobra.Command, cfg *types.Config, logger *logrus.Logger) {
	configCmd := &cobra.Command{
		Use:   "config",
		Short: "Manage configuration",
		Long:  "Commands for managing pi-go configuration including viewing and editing settings.",
	}

	// Add config subcommands
	configCmd.AddCommand(config.NewShowCommand(cfg, logger))
	configCmd.AddCommand(config.NewSetCommand(cfg, logger))
	configCmd.AddCommand(config.NewTemplateCommand(cfg, logger))

	rootCmd.AddCommand(configCmd)
}

