package monitoring

import (
	"context"
	"fmt"

	"github.com/capsule/capsule/pkg/docker"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

// ListCommand implements the 'ls' command for listing running capsules
type ListCommand struct {
	*cmds.CommandDescription
}

func NewListCommand() (*ListCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, fmt.Errorf("could not create glazed parameter layer: %w", err)
	}

	optionsLayer, err := layers.NewParameterLayer(
		"options",
		"List options",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"all",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Show all containers (including stopped)"),
				parameters.WithDefault(false),
			),
			parameters.NewParameterDefinition(
				"quiet",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Only show container IDs"),
				parameters.WithDefault(false),
			),
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create options parameter layer: %w", err)
	}

	return &ListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"ls",
			cmds.WithShort("List running capsules"),
			cmds.WithLong(`List all capsule-managed containers with their resource usage and status.

Examples:
  capsule ls                    # List running capsules
  capsule ls --all              # List all capsules (including stopped)
  capsule ls --quiet            # Show only container IDs
  capsule ls --output json      # Output as JSON
`),
			cmds.WithLayersList(
				optionsLayer,
				glazedParameterLayer,
			),
		),
	}, nil
}

func (c *ListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract parameters
	showAll, _ := parsedLayers.GetBool("all")
	quiet, _ := parsedLayers.GetBool("quiet")

	// Create Docker client and list containers
	dockerClient := docker.NewClient()
	
	containers, err := dockerClient.ListContainers(ctx)
	if err != nil {
		return fmt.Errorf("failed to list containers: %w", err)
	}

	if len(containers) == 0 {
		fmt.Println("No capsule-managed containers found")
		return nil
	}

	// Filter containers if not showing all
	var filteredContainers []docker.ContainerInfo
	for _, container := range containers {
		if showAll || container.Status != "Exited" {
			filteredContainers = append(filteredContainers, container)
		}
	}

	// Output results
	for _, container := range filteredContainers {
		if quiet {
			// Just output container ID
			row := types.NewRow(
				types.MRP("id", container.ID),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		} else {
			// Output full information
			row := types.NewRow(
				types.MRP("id", container.ID),
				types.MRP("image", container.Image),
				types.MRP("status", container.Status),
				types.MRP("name", container.Name),
				types.MRP("cpu_limit", container.CPU),
				types.MRP("memory_limit", container.Memory),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	}

	return nil
}

