package main

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"
)

// RunCommand implements the 'run' command for executing containers with resource constraints
type RunCommand struct {
	*cmds.CommandDescription
}

func NewRunCommand() (*RunCommand, error) {
	glazedParameterLayer, err := layers.NewGlazedParameterLayers()
	if err != nil {
		return nil, fmt.Errorf("could not create glazed parameter layer: %w", err)
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
`),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"resource",
					"Resource constraints",
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
				layers.NewParameterLayer(
					"container",
					"Container options",
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
	// Extract parameters
	cpu, _ := parsedLayers.GetFloat("cpu")
	memory, _ := parsedLayers.GetString("mem")
	pids, _ := parsedLayers.GetInt("pids")
	swap, _ := parsedLayers.GetString("swap")
	name, _ := parsedLayers.GetString("name")
	rm, _ := parsedLayers.GetBool("rm")
	detach, _ := parsedLayers.GetBool("detach")
	volumes, _ := parsedLayers.GetStringList("volume")
	envVars, _ := parsedLayers.GetStringList("env")
	
	image, _ := parsedLayers.GetString("image")
	command, _ := parsedLayers.GetStringList("command")

	// Create Docker client and run container
	docker := NewDockerClient()
	
	result, err := docker.RunContainer(ctx, RunOptions{
		Image:    image,
		Command:  command,
		CPU:      cpu,
		Memory:   memory,
		Pids:     pids,
		Swap:     swap,
		Name:     name,
		Remove:   rm,
		Detach:   detach,
		Volumes:  volumes,
		EnvVars:  envVars,
	})
	
	if err != nil {
		return fmt.Errorf("failed to run container: %w", err)
	}

	// Output result as structured data
	row := types.NewRow(
		types.MRP("container_id", result.ContainerID),
		types.MRP("image", image),
		types.MRP("status", result.Status),
		types.MRP("exit_code", result.ExitCode),
		types.MRP("cpu_limit", cpu),
		types.MRP("memory_limit", memory),
	)
	
	return gp.AddRow(ctx, row)
}

// GoCommand implements the 'go' command for building and running Go programs
type GoCommand struct {
	*cmds.CommandDescription
}

func NewGoCommand() (*GoCommand, error) {
	glazedParameterLayer, err := layers.NewGlazedParameterLayers()
	if err != nil {
		return nil, fmt.Errorf("could not create glazed parameter layer: %w", err)
	}

	return &GoCommand{
		CommandDescription: cmds.NewCommandDescription(
			"go",
			cmds.WithShort("Build and run Go programs in a capsule"),
			cmds.WithLong(`Build a Go program and run it inside a Docker container with resource constraints.
This command builds the Go program, creates a minimal container image, and runs it.

Examples:
  capsule go ./cmd/server --cpu 1 --mem 2g -- -config config.yaml
  capsule go . --preset do-1vcpu-2gb --tag myapp:test
`),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"resource",
					"Resource constraints",
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
				layers.NewParameterLayer(
					"build",
					"Build options",
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
	docker := NewDockerClient()
	
	result, err := docker.BuildAndRunGo(ctx, GoBuildOptions{
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

// ShellCommand implements the 'shell' command for interactive environments
type ShellCommand struct {
	*cmds.CommandDescription
}

func NewShellCommand() (*ShellCommand, error) {
	return &ShellCommand{
		CommandDescription: cmds.NewCommandDescription(
			"shell",
			cmds.WithShort("Open an interactive shell in a constrained capsule"),
			cmds.WithLong(`Open an interactive shell inside a Docker container with resource constraints.
The shell inherits your current working directory.

Examples:
  capsule shell --cpu 1 --mem 2g ubuntu:24.04
  capsule shell --preset do-1vcpu-2gb alpine:latest
`),
		),
	}, nil
}

func (c *ShellCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	// Implementation for shell command
	fmt.Println("Shell command not yet implemented")
	return nil
}

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
  capsule stats
  capsule stats container1 container2
`),
		),
	}, nil
}

func (c *StatsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Implementation for stats command
	fmt.Println("Stats command not yet implemented")
	return nil
}

// ListCommand implements the 'ls' command for listing running capsules
type ListCommand struct {
	*cmds.CommandDescription
}

func NewListCommand() (*ListCommand, error) {
	return &ListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"ls",
			cmds.WithShort("List running capsules"),
			cmds.WithLong(`List all running capsules with their resource usage and status.

Examples:
  capsule ls
  capsule ls --all
`),
		),
	}, nil
}

func (c *ListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Implementation for list command
	fmt.Println("List command not yet implemented")
	return nil
}

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
`),
		),
	}, nil
}

func (c *StopCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	// Implementation for stop command
	fmt.Println("Stop command not yet implemented")
	return nil
}

// RemoveCommand implements the 'rm' command for removing capsules
type RemoveCommand struct {
	*cmds.CommandDescription
}

func NewRemoveCommand() (*RemoveCommand, error) {
	return &RemoveCommand{
		CommandDescription: cmds.NewCommandDescription(
			"rm",
			cmds.WithShort("Remove capsules"),
			cmds.WithLong(`Remove one or more capsules (containers).

Examples:
  capsule rm container1
  capsule rm container1 container2 --force
`),
		),
	}, nil
}

func (c *RemoveCommand) Run(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
) error {
	// Implementation for remove command
	fmt.Println("Remove command not yet implemented")
	return nil
}

