package container

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

// RunSettings contains the parameters for the run command
type RunSettings struct {
	// Resource constraints
	CPU    float64 `glazed.parameter:"cpu"`
	Memory string  `glazed.parameter:"mem"`
	Pids   int     `glazed.parameter:"pids"`
	Swap   string  `glazed.parameter:"swap"`
	
	// Container options
	Name    string   `glazed.parameter:"name"`
	Remove  bool     `glazed.parameter:"rm"`
	Detach  bool     `glazed.parameter:"detach"`
	Volumes []string `glazed.parameter:"volume"`
	EnvVars []string `glazed.parameter:"env"`
	
	// Arguments
	Image   string   `glazed.parameter:"image"`
	Command []string `glazed.parameter:"command"`
}

// RunCommand implements the 'run' command for executing containers with resource constraints
type RunCommand struct {
	*cmds.CommandDescription
}

func NewRunCommand() (*RunCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, fmt.Errorf("could not create glazed parameter layer: %w", err)
	}

	resourceLayer, err := layers.NewParameterLayer(
		"resource",
		"Resource constraints",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"cpu",
				parameters.ParameterTypeFloat,
				parameters.WithHelp("CPU limit (number of cores)"),
				parameters.WithDefault(1.0),
			),
			parameters.NewParameterDefinition(
				"mem",
				parameters.ParameterTypeString,
				parameters.WithHelp("Memory limit (e.g., 512m, 2g)"),
				parameters.WithDefault("1g"),
			),
			parameters.NewParameterDefinition(
				"pids",
				parameters.ParameterTypeInteger,
				parameters.WithHelp("Process limit"),
				parameters.WithDefault(1024),
			),
			parameters.NewParameterDefinition(
				"swap",
				parameters.ParameterTypeString,
				parameters.WithHelp("Swap limit (e.g., 512m, 1g)"),
				parameters.WithDefault("0"),
			),
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create resource parameter layer: %w", err)
	}

	containerLayer, err := layers.NewParameterLayer(
		"container",
		"Container options",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"name",
				parameters.ParameterTypeString,
				parameters.WithHelp("Container name"),
			),
			parameters.NewParameterDefinition(
				"rm",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Remove container on exit"),
				parameters.WithDefault(true),
			),
			parameters.NewParameterDefinition(
				"detach",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Run container in background"),
				parameters.WithDefault(false),
			),
			parameters.NewParameterDefinition(
				"volume",
				parameters.ParameterTypeStringList,
				parameters.WithHelp("Bind mount volumes (host:container)"),
			),
			parameters.NewParameterDefinition(
				"env",
				parameters.ParameterTypeStringList,
				parameters.WithHelp("Environment variables (KEY=value)"),
			),
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create container parameter layer: %w", err)
	}

	return &RunCommand{
		CommandDescription: cmds.NewCommandDescription(
			"run",
			cmds.WithShort("Execute a command once inside a fresh capsule"),
			cmds.WithLong(`Run a command once inside a Docker container with specified resource constraints.
The container is destroyed on exit by default.

Examples:
  capsule run --cpu 1 --mem 2g ubuntu:latest echo "Hello World"
  capsule run --cpu 0.5 --mem 512m --name test alpine:latest /bin/sh -c "sleep 10"
  capsule run --cpu 1 --mem 2g --volume ./data:/data ubuntu:latest ls /data
`),
			cmds.WithLayersList(
				resourceLayer,
				containerLayer,
				glazedParameterLayer,
			),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"image",
					parameters.ParameterTypeString,
					parameters.WithHelp("Docker image to run"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"command",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Command and arguments to execute"),
				),
			),
		),
	}, nil
}

func (c *RunCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract parameters into struct
	settings := &RunSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings)
	if err != nil {
		return fmt.Errorf("failed to initialize settings from parameters: %w", err)
	}

	// Create Docker client and run container
	dockerClient := docker.NewClient()
	
	result, err := dockerClient.RunContainer(ctx, docker.RunOptions{
		Image:    settings.Image,
		Command:  settings.Command,
		CPU:      settings.CPU,
		Memory:   settings.Memory,
		Pids:     settings.Pids,
		Swap:     settings.Swap,
		Name:     settings.Name,
		Remove:   settings.Remove,
		Detach:   settings.Detach,
		Volumes:  settings.Volumes,
		EnvVars:  settings.EnvVars,
	})
	
	if err != nil {
		return fmt.Errorf("failed to run container: %w", err)
	}

	// Output result as structured data
	row := types.NewRow(
		types.MRP("container_id", result.ContainerID),
		types.MRP("image", settings.Image),
		types.MRP("status", result.Status),
		types.MRP("exit_code", result.ExitCode),
		types.MRP("cpu_limit", settings.CPU),
		types.MRP("memory_limit", settings.Memory),
	)
	
	return gp.AddRow(ctx, row)
}

