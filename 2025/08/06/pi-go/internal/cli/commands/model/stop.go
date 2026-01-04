package model

import (
	"fmt"
	"time"

	"github.com/pi-go/pi/internal/config"
	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func NewStopCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "stop <model-name>",
		Short: "Stop a vLLM model",
		Long: `Stop a running vLLM model.

This command gracefully stops a running model and cleans up resources.
The model configuration is preserved and can be restarted later.

Examples:
  # Stop a model
  pi model stop my-model

  # Stop a model with custom timeout
  pi model stop my-model --timeout 60s

  # Force stop without graceful shutdown
  pi model stop my-model --force`,
		Args: cobra.ExactArgs(1),
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return runStopCommand(cfg, logger, cobraCmd, args)
		},
	}

	cmd.Flags().Duration("timeout", 30*time.Second, "Timeout for graceful shutdown")
	cmd.Flags().Bool("force", false, "Force stop without graceful shutdown")
	cmd.Flags().Bool("remove", false, "Remove model configuration after stopping")

	return cmd
}

func runStopCommand(cfg *types.Config, logger *logrus.Logger, cmd *cobra.Command, args []string) error {
	modelName := args[0]

	// Parse flags
	timeout, _ := cmd.Flags().GetDuration("timeout")
	force, _ := cmd.Flags().GetBool("force")
	remove, _ := cmd.Flags().GetBool("remove")

	// Check if model exists
	model, exists := cfg.Models.Models[modelName]
	if !exists {
		return fmt.Errorf("model '%s' not found", modelName)
	}

	// Check if model is running
	if !model.IsRunning() && model.Status != types.ModelStatusLoading {
		fmt.Printf("Model '%s' is not running (status: %s)\n", modelName, model.Status)
		if !remove {
			return nil
		}
	}

	pod := cfg.Pods.Pods[model.PodName]
	if pod == nil {
		return fmt.Errorf("pod '%s' not found for model '%s'", model.PodName, modelName)
	}

	fmt.Printf("Stopping model '%s' on pod '%s'\n", modelName, model.PodName)
	fmt.Printf("Model ID: %s\n", model.ModelID)
	fmt.Printf("Port: %d\n", model.Port)

	if force {
		fmt.Printf("Force stopping (no graceful shutdown)\n")
	} else {
		fmt.Printf("Graceful shutdown timeout: %s\n", timeout)
	}

	// Update model status
	model.Status = types.ModelStatusStopping
	model.UpdatedAt = time.Now()

	// TODO: Implement actual model stopping via SSH
	// This would:
	// 1. Send SIGTERM to the vLLM process
	// 2. Wait for graceful shutdown within timeout
	// 3. Send SIGKILL if timeout exceeded or force flag set
	// 4. Clean up any SSH tunnels
	// 5. Update model status

	// Simulate stopping
	time.Sleep(1 * time.Second)

	// Update final status
	now := time.Now()
	model.Status = types.ModelStatusStopped
	model.StoppedAt = &now
	model.UpdatedAt = now

	if remove {
		// Remove model from configuration
		if err := cfg.RemoveModel(modelName); err != nil {
			return fmt.Errorf("failed to remove model: %w", err)
		}
		fmt.Printf("Model '%s' stopped and removed from configuration\n", modelName)
	} else {
		fmt.Printf("Model '%s' stopped successfully\n", modelName)
		if model.StartedAt != nil {
			fmt.Printf("Uptime: %s\n", model.GetUptime().Truncate(time.Second))
		}
	}

	// Save configuration
	if err := config.Save(cfg); err != nil {
		return fmt.Errorf("failed to save configuration: %w", err)
	}

	// TODO: Implement actual stopping logic
	fmt.Printf("\nNote: Actual model stopping not yet implemented.\n")
	fmt.Printf("This would terminate the vLLM process on pod '%s'.\n", model.PodName)

	return nil
}

