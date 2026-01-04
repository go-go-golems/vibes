package model

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
)

func NewStatusCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "status <model-name>",
		Short: "Show model status",
		Long: `Show detailed status information for a vLLM model.

This command displays comprehensive information about a model including
its current status, resource usage, configuration, and API endpoint.

Examples:
  # Show model status
  pi model status my-model

  # Show status as JSON
  pi model status my-model --output json

  # Show status as YAML
  pi model status my-model --output yaml`,
		Args: cobra.ExactArgs(1),
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return runStatusCommand(cfg, logger, cobraCmd, args)
		},
	}

	cmd.Flags().String("output", "text", "Output format (text, json, yaml)")

	return cmd
}

func runStatusCommand(cfg *types.Config, logger *logrus.Logger, cmd *cobra.Command, args []string) error {
	modelName := args[0]
	output, _ := cmd.Flags().GetString("output")

	// Check if model exists
	model, exists := cfg.Models.Models[modelName]
	if !exists {
		return fmt.Errorf("model '%s' not found", modelName)
	}

	pod := cfg.Pods.Pods[model.PodName]
	if pod == nil {
		return fmt.Errorf("pod '%s' not found for model '%s'", model.PodName, modelName)
	}

	// Output based on format
	switch output {
	case "json":
		return outputStatusJSON(model, pod)
	case "yaml":
		return outputStatusYAML(model, pod)
	default:
		return outputStatusText(model, pod)
	}
}

func outputStatusText(model *types.Model, pod *types.Pod) error {
	fmt.Printf("Model Status: %s\n", model.Name)
	fmt.Printf("================\n\n")

	// Basic Information
	fmt.Printf("Model ID:        %s\n", model.ModelID)
	fmt.Printf("Name:            %s\n", model.Name)
	fmt.Printf("Status:          %s\n", model.Status)
	fmt.Printf("Pod:             %s (%s)\n", model.PodName, pod.Host)
	fmt.Printf("Port:            %d\n", model.Port)
	fmt.Printf("API Endpoint:    %s\n", model.GetAPIEndpoint(pod.Host))

	// Resource Information
	fmt.Printf("\nResource Usage:\n")
	fmt.Printf("GPU Memory:      %.1f%%\n", model.GPUMemory*100)
	if model.AllGPUs {
		fmt.Printf("GPU Usage:       All GPUs (tensor parallelism)\n")
	} else if len(model.GPUIDs) > 0 {
		fmt.Printf("GPU IDs:         %v\n", model.GPUIDs)
	} else {
		fmt.Printf("GPU Usage:       Default allocation\n")
	}

	// Configuration
	fmt.Printf("\nConfiguration:\n")
	if model.ToolParser != "" {
		fmt.Printf("Tool Parser:     %s\n", model.ToolParser)
	}
	if len(model.VLLMArgs) > 0 {
		fmt.Printf("vLLM Args:       %v\n", model.VLLMArgs)
	}
	if model.LogFile != "" {
		fmt.Printf("Log File:        %s\n", model.LogFile)
	}

	// Timing Information
	fmt.Printf("\nTiming:\n")
	fmt.Printf("Created:         %s\n", model.CreatedAt.Format("2006-01-02 15:04:05"))
	fmt.Printf("Updated:         %s\n", model.UpdatedAt.Format("2006-01-02 15:04:05"))
	if model.StartedAt != nil {
		fmt.Printf("Started:         %s\n", model.StartedAt.Format("2006-01-02 15:04:05"))
		if model.StoppedAt != nil {
			fmt.Printf("Stopped:         %s\n", model.StoppedAt.Format("2006-01-02 15:04:05"))
		}
		fmt.Printf("Uptime:          %s\n", model.GetUptime().Truncate(1000000000))
	}

	// Process Information
	if model.PID > 0 {
		fmt.Printf("\nProcess:\n")
		fmt.Printf("PID:             %d\n", model.PID)
	}

	// Health Status
	fmt.Printf("\nHealth:\n")
	fmt.Printf("Is Running:      %t\n", model.IsRunning())
	fmt.Printf("Is Healthy:      %t\n", model.IsHealthy())

	// Metadata
	if len(model.Metadata) > 0 {
		fmt.Printf("\nMetadata:\n")
		for key, value := range model.Metadata {
			fmt.Printf("%-15s  %s\n", key+":", value)
		}
	}

	return nil
}

func outputStatusJSON(model *types.Model, pod *types.Pod) error {
	type statusOutput struct {
		*types.Model
		PodHost      string `json:"pod_host"`
		APIEndpoint  string `json:"api_endpoint"`
		IsRunning    bool   `json:"is_running"`
		IsHealthy    bool   `json:"is_healthy"`
		UptimeString string `json:"uptime_string"`
	}

	uptimeStr := ""
	if model.StartedAt != nil {
		uptimeStr = model.GetUptime().Truncate(1000000000).String()
	}

	output := statusOutput{
		Model:        model,
		PodHost:      pod.Host,
		APIEndpoint:  model.GetAPIEndpoint(pod.Host),
		IsRunning:    model.IsRunning(),
		IsHealthy:    model.IsHealthy(),
		UptimeString: uptimeStr,
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(output)
}

func outputStatusYAML(model *types.Model, pod *types.Pod) error {
	type statusOutput struct {
		*types.Model
		PodHost      string `yaml:"pod_host"`
		APIEndpoint  string `yaml:"api_endpoint"`
		IsRunning    bool   `yaml:"is_running"`
		IsHealthy    bool   `yaml:"is_healthy"`
		UptimeString string `yaml:"uptime_string"`
	}

	uptimeStr := ""
	if model.StartedAt != nil {
		uptimeStr = model.GetUptime().Truncate(1000000000).String()
	}

	output := statusOutput{
		Model:        model,
		PodHost:      pod.Host,
		APIEndpoint:  model.GetAPIEndpoint(pod.Host),
		IsRunning:    model.IsRunning(),
		IsHealthy:    model.IsHealthy(),
		UptimeString: uptimeStr,
	}

	encoder := yaml.NewEncoder(os.Stdout)
	defer encoder.Close()
	return encoder.Encode(output)
}

