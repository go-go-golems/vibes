package pod

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
		Short: "List GPU pods",
		Long: `List all registered GPU pods with their status and configuration.

This command displays information about all registered pods including their
connection status, capabilities, and configuration details.

Examples:
  # List all pods
  pi pod list

  # List only online pods
  pi pod list --status online

  # List only the active pod
  pi pod list --active-only

  # Output as JSON
  pi pod list --output json

  # Output as YAML
  pi pod list --output yaml`,
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return runListCommand(cfg, logger, cobraCmd, args)
		},
	}

	cmd.Flags().String("status", "", "Filter by pod status (unknown, connecting, online, offline, error, maintenance)")
	cmd.Flags().Bool("active-only", false, "Show only the active pod")
	cmd.Flags().String("output", "table", "Output format (table, json, yaml)")

	return cmd
}

func runListCommand(cfg *piTypes.Config, logger *logrus.Logger, cmd *cobra.Command, args []string) error {
	status, _ := cmd.Flags().GetString("status")
	activeOnly, _ := cmd.Flags().GetBool("active-only")
	output, _ := cmd.Flags().GetString("output")

	logger.WithFields(logrus.Fields{
		"status":      status,
		"active_only": activeOnly,
		"output":      output,
	}).Debug("listing pods")

	// Get pods to display
	var podsToShow []*piTypes.Pod

	if activeOnly {
		// Show only active pod
		activePod := cfg.GetActivePod()
		if activePod != nil {
			podsToShow = append(podsToShow, activePod)
		}
	} else {
		// Show all pods
		for _, pod := range cfg.Pods.Pods {
			podsToShow = append(podsToShow, pod)
		}
	}

	// Filter by status if specified
	if status != "" {
		var filteredPods []*piTypes.Pod
		for _, pod := range podsToShow {
			if string(pod.Status) == status {
				filteredPods = append(filteredPods, pod)
			}
		}
		podsToShow = filteredPods
	}

	logger.WithField("count", len(podsToShow)).Info("found pods")

	// Output results
	switch output {
	case "json":
		return outputJSON(podsToShow, cfg)
	case "yaml":
		return outputYAML(podsToShow, cfg)
	default:
		return outputTable(podsToShow, cfg)
	}
}

func outputTable(pods []*piTypes.Pod, cfg *piTypes.Config) error {
	if len(pods) == 0 {
		fmt.Println("No pods found")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(w, "NAME\tHOST\tUSER\tSTATUS\tACTIVE\tSSH COMMAND")

	for _, pod := range pods {
		active := ""
		if pod.Name == cfg.Pods.Active {
			active = "*"
		}
		fmt.Fprintf(w, "%s\t%s\t%s\t%s\t%s\t%s\n",
			pod.Name,
			pod.Host,
			pod.User,
			pod.Status,
			active,
			pod.SSHCommand,
		)
	}

	return w.Flush()
}

func outputJSON(pods []*piTypes.Pod, cfg *piTypes.Config) error {
	type podOutput struct {
		*piTypes.Pod
		IsActive         bool   `json:"is_active"`
		ConnectionString string `json:"connection_string"`
		DisplayName      string `json:"display_name"`
		IsOnline         bool   `json:"is_online"`
		IsHealthy        bool   `json:"is_healthy"`
	}

	var output []podOutput
	for _, pod := range pods {
		output = append(output, podOutput{
			Pod:              pod,
			IsActive:         pod.Name == cfg.Pods.Active,
			ConnectionString: pod.GetConnectionString(),
			DisplayName:      pod.GetDisplayName(),
			IsOnline:         pod.IsOnline(),
			IsHealthy:        pod.IsHealthy(),
		})
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(output)
}

func outputYAML(pods []*piTypes.Pod, cfg *piTypes.Config) error {
	type podOutput struct {
		*piTypes.Pod
		IsActive         bool   `yaml:"is_active"`
		ConnectionString string `yaml:"connection_string"`
		DisplayName      string `yaml:"display_name"`
		IsOnline         bool   `yaml:"is_online"`
		IsHealthy        bool   `yaml:"is_healthy"`
	}

	var output []podOutput
	for _, pod := range pods {
		output = append(output, podOutput{
			Pod:              pod,
			IsActive:         pod.Name == cfg.Pods.Active,
			ConnectionString: pod.GetConnectionString(),
			DisplayName:      pod.GetDisplayName(),
			IsOnline:         pod.IsOnline(),
			IsHealthy:        pod.IsHealthy(),
		})
	}

	encoder := yaml.NewEncoder(os.Stdout)
	defer encoder.Close()
	return encoder.Encode(output)
}

