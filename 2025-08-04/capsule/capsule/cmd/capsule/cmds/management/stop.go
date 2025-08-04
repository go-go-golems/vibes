package management

import (
	"context"
	"fmt"

	"github.com/capsule/capsule/pkg/docker"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

// StopCommand implements the 'stop' command for stopping capsules
type StopCommand struct {
	*cmds.CommandDescription
}

func NewStopCommand() (*StopCommand, error) {
	return &StopCommand{
		CommandDescription: cmds.NewCommandDescription(
			"stop",
			cmds.WithShort("Stop running capsules"),
			cmds.WithLong(`Stop one or more running capsules.

Examples:
  capsule stop container1
  capsule stop container1 container2
  capsule stop $(capsule ls -q)  # Stop all running capsules
`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"containers",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Container IDs or names to stop"),
					parameters.WithRequired(true),
				),
			),
		),
	}, nil
}

func (c *StopCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	containers, _ := parsedLayers.GetStringList("containers")
	
	if len(containers) == 0 {
		return fmt.Errorf("no containers specified")
	}

	// Create Docker client and stop containers
	dockerClient := docker.NewClient()
	
	var errors []error
	for _, containerID := range containers {
		fmt.Printf("Stopping container %s...\n", containerID)
		if err := dockerClient.StopContainer(ctx, containerID); err != nil {
			errors = append(errors, fmt.Errorf("failed to stop container %s: %w", containerID, err))
		} else {
			fmt.Printf("Container %s stopped successfully\n", containerID)
		}
	}
	
	if len(errors) > 0 {
		for _, err := range errors {
			fmt.Printf("Error: %v\n", err)
		}
		return fmt.Errorf("failed to stop %d containers", len(errors))
	}

	return nil
}

