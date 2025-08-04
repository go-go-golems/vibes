package gocmds

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

// GoCommand implements the 'go' command for building and running Go programs
type GoCommand struct {
	*cmds.CommandDescription
}

func NewGoCommand() (*GoCommand, error) {
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
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create resource parameter layer: %w", err)
	}

	buildLayer, err := layers.NewParameterLayer(
		"build",
		"Build options",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"build-flags",
				parameters.ParameterTypeString,
				parameters.WithHelp("Additional flags for go build"),
			),
			parameters.NewParameterDefinition(
				"ldflags",
				parameters.ParameterTypeString,
				parameters.WithHelp("Linker flags"),
			),
			parameters.NewParameterDefinition(
				"tag",
				parameters.ParameterTypeString,
				parameters.WithHelp("Tag for the built image"),
			),
			parameters.NewParameterDefinition(
				"keep-image",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Keep the built image after run"),
				parameters.WithDefault(false),
			),
		),
	)
	if err != nil {
		return nil, fmt.Errorf("could not create build parameter layer: %w", err)
	}

	return &GoCommand{
		CommandDescription: cmds.NewCommandDescription(
			"go",
			cmds.WithShort("Build and run Go programs in a capsule"),
			cmds.WithLong(`Build a Go program and run it inside a Docker container with resource constraints.
This command builds the Go program, creates a minimal container image, and runs it.

Examples:
  capsule go ./cmd/server --cpu 1 --mem 2g -- -config config.yaml
  capsule go . --cpu 0.5 --mem 512m --tag myapp:test
  capsule go ./cmd/worker --build-flags "-tags prod" --ldflags "-s -w"
`),
			cmds.WithLayersList(
				resourceLayer,
				buildLayer,
				glazedParameterLayer,
			),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"package",
					parameters.ParameterTypeString,
					parameters.WithHelp("Go package or directory to build"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"args",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Arguments to pass to the program"),
				),
			),
		),
	}, nil
}

func (c *GoCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract parameters
	cpu, _ := parsedLayers.GetFloat("cpu")
	memory, _ := parsedLayers.GetString("mem")
	buildFlags, _ := parsedLayers.GetString("build-flags")
	ldflags, _ := parsedLayers.GetString("ldflags")
	tag, _ := parsedLayers.GetString("tag")
	keepImage, _ := parsedLayers.GetBool("keep-image")
	
	packagePath, _ := parsedLayers.GetString("package")
	args, _ := parsedLayers.GetStringList("args")

	// Create Docker client and build/run Go program
	dockerClient := docker.NewClient()
	
	result, err := dockerClient.BuildAndRunGo(ctx, docker.GoBuildOptions{
		PackagePath: packagePath,
		Args:        args,
		CPU:         cpu,
		Memory:      memory,
		BuildFlags:  buildFlags,
		LdFlags:     ldflags,
		Tag:         tag,
		KeepImage:   keepImage,
	})
	
	if err != nil {
		return fmt.Errorf("failed to build and run Go program: %w", err)
	}

	// Output result as structured data
	row := types.NewRow(
		types.MRP("container_id", result.ContainerID),
		types.MRP("image_tag", result.ImageTag),
		types.MRP("package", packagePath),
		types.MRP("status", result.Status),
		types.MRP("exit_code", result.ExitCode),
		types.MRP("cpu_limit", cpu),
		types.MRP("memory_limit", memory),
	)
	
	return gp.AddRow(ctx, row)
}

