package monitoring

import (
	"context"
	"fmt"

	"github.com/capsule/capsule/pkg/docker"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

// StatsCommand implements the 'stats' command for monitoring resource usage
type StatsCommand struct {
	*cmds.CommandDescription
}

func NewStatsCommand() (*StatsCommand, error) {
	return &StatsCommand{
		CommandDescription: cmds.NewCommandDescription(
			"stats",
			cmds.WithShort("Stream live resource usage for capsules"),
			cmds.WithLong(`Stream live CPU, memory, and network usage statistics for running capsules.

Examples:
  capsule stats                    # Show stats for all capsule-managed containers
  capsule stats container1         # Show stats for specific container
  capsule stats container1 container2  # Show stats for multiple containers
`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"containers",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Container IDs or names to monitor (optional)"),
				),
			),
		),
	}, nil
}

func (c *StatsCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	containers, _ := parsedLayers.GetStringList("containers")

	// Create Docker client and show stats
	dockerClient := docker.NewClient()
	
	fmt.Println("Streaming container statistics... (Press Ctrl+C to stop)")
	err := dockerClient.GetContainerStats(ctx, containers)
	
	if err != nil {
		return fmt.Errorf("failed to get container stats: %w", err)
	}

	return nil
}

