package management

import (
	"context"
	"fmt"

	"github.com/capsule/capsule/pkg/docker"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

// RemoveCommand implements the 'rm' command for removing capsules
type RemoveCommand struct {
	*cmds.CommandDescription
}

func NewRemoveCommand() (*RemoveCommand, error) {
	optionsLayer, err := layers.NewParameterLayer(
		"options",
		"Remove options",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"force",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Force removal of running containers"),
				parameters.WithDefault(false),
			),
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create options parameter layer: %w", err)
	}

	return &RemoveCommand{
		CommandDescription: cmds.NewCommandDescription(
			"rm",
			cmds.WithShort("Remove capsules"),
			cmds.WithLong(`Remove one or more capsules (containers).

Examples:
  capsule rm container1
  capsule rm container1 container2 --force
  capsule rm $(capsule ls -q --filter status=exited)  # Remove all stopped capsules
`),
			cmds.WithLayersList(
				optionsLayer,
			),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"containers",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Container IDs or names to remove"),
					parameters.WithRequired(true),
				),
			),
		),
	}, nil
}

func (c *RemoveCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	containers, _ := parsedLayers.GetStringList("containers")
	force, _ := parsedLayers.GetBool("force")
	
	if len(containers) == 0 {
		return fmt.Errorf("no containers specified")
	}

	// Create Docker client and remove containers
	dockerClient := docker.NewClient()
	
	var errors []error
	for _, containerID := range containers {
		fmt.Printf("Removing container %s...\n", containerID)
		if err := dockerClient.RemoveContainer(ctx, containerID, force); err != nil {
			errors = append(errors, fmt.Errorf("failed to remove container %s: %w", containerID, err))
		} else {
			fmt.Printf("Container %s removed successfully\n", containerID)
		}
	}
	
	if len(errors) > 0 {
		for _, err := range errors {
			fmt.Printf("Error: %v\n", err)
		}
		return fmt.Errorf("failed to remove %d containers", len(errors))
	}

	return nil
}

