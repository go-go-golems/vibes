package container

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/capsule/capsule/pkg/docker"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

// ShellCommand implements the 'shell' command for interactive environments
type ShellCommand struct {
	*cmds.CommandDescription
}

func NewShellCommand() (*ShellCommand, error) {
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
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create resource parameter layer: %w", err)
	}

	return &ShellCommand{
		CommandDescription: cmds.NewCommandDescription(
			"shell",
			cmds.WithShort("Open an interactive shell in a constrained capsule"),
			cmds.WithLong(`Open an interactive shell inside a Docker container with resource constraints.
The shell inherits your current working directory.

Examples:
  capsule shell --cpu 1 --mem 2g ubuntu:24.04
  capsule shell --cpu 0.5 --mem 512m alpine:latest
  capsule shell ubuntu:latest  # Uses default resource limits
`),
			cmds.WithLayersList(
				resourceLayer,
			),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"image",
					parameters.ParameterTypeString,
					parameters.WithHelp("Docker image to run"),
					parameters.WithDefault("ubuntu:24.04"),
				),
			),
		),
	}, nil
}

func (c *ShellCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	// Extract parameters
	cpu, _ := parsedLayers.GetFloat("cpu")
	memory, _ := parsedLayers.GetString("mem")
	image, _ := parsedLayers.GetString("image")

	// Get current working directory to mount
	cwd, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("failed to get current working directory: %w", err)
	}

	// Create Docker client and run interactive shell
	dockerClient := docker.NewClient()
	
	// Determine shell command based on image
	shellCmd := []string{"/bin/bash"}
	if image == "alpine:latest" || image == "alpine" {
		shellCmd = []string{"/bin/sh"}
	}

	_, err = dockerClient.RunContainer(ctx, docker.RunOptions{
		Image:   image,
		Command: shellCmd,
		CPU:     cpu,
		Memory:  memory,
		Remove:  true, // Always remove shell containers
		Volumes: []string{fmt.Sprintf("%s:%s", cwd, filepath.Base(cwd))},
		EnvVars: []string{
			fmt.Sprintf("PS1=capsule:%s$ ", filepath.Base(cwd)),
		},
	})
	
	if err != nil {
		return fmt.Errorf("failed to run shell: %w", err)
	}

	return nil
}

