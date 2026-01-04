package model

import (
	"encoding/json"
	"fmt"
	"os"
	"text/tabwriter"

	piTypes "github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
)

func NewListCommand(cfg *piTypes.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "list",
		Short: "List vLLM models",
		Long: `List all deployed vLLM models with their status and configuration.

This command displays information about all deployed models including their
status, resource usage, and API endpoints.

Examples:
  # List all models
  pi model list

  # List models on a specific pod
  pi model list --pod my-pod

  # List only running models
  pi model list --status running

  # Output as JSON
  pi model list --output json

  # Output as YAML
  pi model list --output yaml`,
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return runListModelsCommand(cfg, logger, cobraCmd, args)
		},
	}

	cmd.Flags().String("pod", "", "Filter by pod name")
	cmd.Flags().String("status", "", "Filter by model status (starting, loading, running, stopping, stopped, error, failed)")
	cmd.Flags().String("output", "table", "Output format (table, json, yaml)")

	return cmd
}

func runListModelsCommand(cfg *piTypes.Config, logger *logrus.Logger, cmd *cobra.Command, args []string) error {
	podName, _ := cmd.Flags().GetString("pod")
	status, _ := cmd.Flags().GetString("status")
	output, _ := cmd.Flags().GetString("output")

	logger.WithFields(logrus.Fields{
		"pod":    podName,
		"status": status,
		"output": output,
	}).Debug("listing models")

	// Get models to display
	var modelsToShow []*piTypes.Model

	for _, model := range cfg.Models.Models {
		modelsToShow = append(modelsToShow, model)
	}

	// Filter by pod if specified
	if podName != "" {
		var filteredModels []*piTypes.Model
		for _, model := range modelsToShow {
			if model.PodName == podName {
				filteredModels = append(filteredModels, model)
			}
		}
		modelsToShow = filteredModels
	}

	// Filter by status if specified
	if status != "" {
		var filteredModels []*piTypes.Model
		for _, model := range modelsToShow {
			if string(model.Status) == status {
				filteredModels = append(filteredModels, model)
			}
		}
		modelsToShow = filteredModels
	}

	logger.WithField("count", len(modelsToShow)).Info("found models")

	// Output results
	switch output {
	case "json":
		return outputModelsJSON(modelsToShow, cfg)
	case "yaml":
		return outputModelsYAML(modelsToShow, cfg)
	default:
		return outputModelsTable(modelsToShow, cfg)
	}
}

func outputModelsTable(models []*piTypes.Model, cfg *piTypes.Config) error {
	if len(models) == 0 {
		fmt.Println("No models found")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "NAME\tMODEL ID\tPOD\tSTATUS\tPORT\tGPU MEM\tUPTIME")

	for _, model := range models {
		uptime := ""
		if model.StartedAt != nil {
			uptime = model.GetUptime().Truncate(1000000000).String() // Truncate to seconds
		}

		fmt.Fprintf(w, "%s\t%s\t%s\t%s\t%d\t%.1f%%\t%s\n",
			model.Name,
			model.ModelID,
			model.PodName,
			model.Status,
			model.Port,
			model.GPUMemory*100,
			uptime,
		)
	}

	return w.Flush()
}

func outputModelsJSON(models []*piTypes.Model, cfg *piTypes.Config) error {
	type modelOutput struct {
		*piTypes.Model
		IsRunning    bool   `json:"is_running"`
		IsHealthy    bool   `json:"is_healthy"`
		DisplayName  string `json:"display_name"`
		APIEndpoint  string `json:"api_endpoint"`
		UptimeString string `json:"uptime_string"`
	}

	var output []modelOutput
	for _, model := range models {
		pod := cfg.Pods.Pods[model.PodName]
		host := "localhost"
		if pod != nil {
			host = pod.Host
		}

		uptimeStr := ""
		if model.StartedAt != nil {
			uptimeStr = model.GetUptime().Truncate(1000000000).String()
		}

		output = append(output, modelOutput{
			Model:        model,
			IsRunning:    model.IsRunning(),
			IsHealthy:    model.IsHealthy(),
			DisplayName:  model.GetDisplayName(),
			APIEndpoint:  model.GetAPIEndpoint(host),
			UptimeString: uptimeStr,
		})
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(output)
}

func outputModelsYAML(models []*piTypes.Model, cfg *piTypes.Config) error {
	type modelOutput struct {
		*piTypes.Model
		IsRunning    bool   `yaml:"is_running"`
		IsHealthy    bool   `yaml:"is_healthy"`
		DisplayName  string `yaml:"display_name"`
		APIEndpoint  string `yaml:"api_endpoint"`
		UptimeString string `yaml:"uptime_string"`
	}

	var output []modelOutput
	for _, model := range models {
		pod := cfg.Pods.Pods[model.PodName]
		host := "localhost"
		if pod != nil {
			host = pod.Host
		}

		uptimeStr := ""
		if model.StartedAt != nil {
			uptimeStr = model.GetUptime().Truncate(1000000000).String()
		}

		output = append(output, modelOutput{
			Model:        model,
			IsRunning:    model.IsRunning(),
			IsHealthy:    model.IsHealthy(),
			DisplayName:  model.GetDisplayName(),
			APIEndpoint:  model.GetAPIEndpoint(host),
			UptimeString: uptimeStr,
		})
	}

	encoder := yaml.NewEncoder(os.Stdout)
	defer encoder.Close()
	return encoder.Encode(output)
}

