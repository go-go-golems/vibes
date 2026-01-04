package pod

import (
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/pi-go/pi/internal/config"
	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

type RemoveCommand struct {
	*cmds.CommandDescription
	config *types.Config
	logger *logrus.Logger
}

func NewRemoveCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &RemoveCommand{
		CommandDescription: cmds.NewCommandDescription(
			"remove",
			cmds.WithShort("Remove a GPU pod"),
			cmds.WithLong(`Remove a GPU pod from the configuration.

This command removes a pod from the configuration. If the pod is currently
active, another pod will be automatically selected as active if available.

Examples:
  # Remove a pod
  pi pod remove my-pod

  # Force remove without confirmation
  pi pod remove my-pod --force

  # Remove and cleanup any running models
  pi pod remove my-pod --cleanup`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"name",
					parameters.ParameterTypeString,
					parameters.WithHelp("Name of the pod to remove"),
					parameters.WithRequired(true),
				),
			),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"force",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Force removal without confirmation"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"cleanup",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Stop any running models on the pod"),
					parameters.WithDefault(false),
				),
			),
		),
		config: cfg,
		logger: logger,
	}

	cobraCmd := &cobra.Command{
		Use:   cmd.Name,
		Short: cmd.Short,
		Long:  cmd.Long,
		Args:  cobra.ExactArgs(1),
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return cmd.RunE(cobraCmd, args)
		},
	}

	// Add flags
	cobraCmd.Flags().Bool("force", false, "Force removal without confirmation")
	cobraCmd.Flags().Bool("cleanup", false, "Stop any running models on the pod")

	return cobraCmd
}

func (c *RemoveCommand) RunE(cobraCmd *cobra.Command, args []string) error {
	name := args[0]

	// Parse flags
	force, _ := cobraCmd.Flags().GetBool("force")
	cleanup, _ := cobraCmd.Flags().GetBool("cleanup")

	// Check if pod exists
	_, exists := c.config.Pods.Pods[name]
	if !exists {
		return fmt.Errorf("pod '%s' not found", name)
	}

	// Check for running models if cleanup is requested
	if cleanup {
		var runningModels []string
		for modelName, model := range c.config.Models.Models {
			if model.PodName == name && model.IsRunning() {
				runningModels = append(runningModels, modelName)
			}
		}

		if len(runningModels) > 0 {
			fmt.Printf("Found %d running models on pod '%s':\n", len(runningModels), name)
			for _, modelName := range runningModels {
				fmt.Printf("  - %s\n", modelName)
			}
			
			// TODO: Implement model stopping logic
			fmt.Printf("Note: Model cleanup not yet implemented. Please stop models manually.\n")
		}
	}

	// Confirmation if not forced
	if !force {
		fmt.Printf("Are you sure you want to remove pod '%s'? [y/N]: ", name)
		var response string
		fmt.Scanln(&response)
		if response != "y" && response != "Y" && response != "yes" {
			fmt.Println("Removal cancelled")
			return nil
		}
	}

	// Remove pod
	if err := c.config.RemovePod(name); err != nil {
		return fmt.Errorf("failed to remove pod: %w", err)
	}

	// Save configuration
	if err := config.Save(c.config); err != nil {
		return fmt.Errorf("failed to save configuration: %w", err)
	}

	fmt.Printf("Successfully removed pod '%s'\n", name)

	// Show new active pod if changed
	if c.config.Pods.Active != "" && c.config.Pods.Active != name {
		fmt.Printf("Active pod is now '%s'\n", c.config.Pods.Active)
	} else if c.config.Pods.Active == "" {
		fmt.Printf("No active pod set\n")
	}

	return nil
}

