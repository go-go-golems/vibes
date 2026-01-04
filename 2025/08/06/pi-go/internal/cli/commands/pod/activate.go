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

type ActivateCommand struct {
	*cmds.CommandDescription
	config *types.Config
	logger *logrus.Logger
}

func NewActivateCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &ActivateCommand{
		CommandDescription: cmds.NewCommandDescription(
			"activate",
			cmds.WithShort("Set the active GPU pod"),
			cmds.WithLong(`Set the active GPU pod for operations.

The active pod is used as the default target for model operations
when no specific pod is specified.

Examples:
  # Activate a pod
  pi pod activate my-pod

  # Show current active pod
  pi pod activate`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"name",
					parameters.ParameterTypeString,
					parameters.WithHelp("Name of the pod to activate"),
					parameters.WithRequired(false),
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
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return cmd.RunE(cobraCmd, args)
		},
	}

	return cobraCmd
}

func (c *ActivateCommand) RunE(cobraCmd *cobra.Command, args []string) error {
	// If no arguments, show current active pod
	if len(args) == 0 {
		if c.config.Pods.Active == "" {
			fmt.Println("No active pod set")
			return nil
		}

		activePod := c.config.GetActivePod()
		if activePod == nil {
			fmt.Printf("Active pod '%s' not found in configuration\n", c.config.Pods.Active)
			return nil
		}

		fmt.Printf("Active pod: %s\n", activePod.Name)
		fmt.Printf("Host: %s\n", activePod.Host)
		fmt.Printf("User: %s\n", activePod.User)
		fmt.Printf("Status: %s\n", activePod.Status)
		fmt.Printf("SSH Command: %s\n", activePod.SSHCommand)
		return nil
	}

	name := args[0]

	// Check if pod exists
	pod, exists := c.config.Pods.Pods[name]
	if !exists {
		return fmt.Errorf("pod '%s' not found", name)
	}

	// Set as active
	if err := c.config.SetActivePod(name); err != nil {
		return fmt.Errorf("failed to set active pod: %w", err)
	}

	// Save configuration
	if err := config.Save(c.config); err != nil {
		return fmt.Errorf("failed to save configuration: %w", err)
	}

	fmt.Printf("Successfully activated pod '%s'\n", name)
	fmt.Printf("Host: %s\n", pod.Host)
	fmt.Printf("User: %s\n", pod.User)
	fmt.Printf("SSH Command: %s\n", pod.SSHCommand)

	return nil
}

