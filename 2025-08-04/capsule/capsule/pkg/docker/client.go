package docker

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// Client wraps Docker CLI operations
type Client struct{}

// NewClient creates a new Docker client
func NewClient() *Client {
	return &Client{}
}

// RunOptions contains options for running a container
type RunOptions struct {
	Image    string
	Command  []string
	CPU      float64
	Memory   string
	Pids     int
	Swap     string
	Name     string
	Remove   bool
	Detach   bool
	Volumes  []string
	EnvVars  []string
}

// RunResult contains the result of running a container
type RunResult struct {
	ContainerID string
	Status      string
	ExitCode    int
	Output      string
}

// GoBuildOptions contains options for building and running Go programs
type GoBuildOptions struct {
	PackagePath string
	Args        []string
	CPU         float64
	Memory      string
	BuildFlags  string
	LdFlags     string
	Tag         string
	KeepImage   bool
}

// GoBuildResult contains the result of building and running a Go program
type GoBuildResult struct {
	ContainerID string
	ImageTag    string
	Status      string
	ExitCode    int
	Output      string
}

// RunContainer runs a Docker container with the specified options
func (d *Client) RunContainer(ctx context.Context, opts RunOptions) (*RunResult, error) {
	args := []string{"run"}
	
	// Add resource constraints
	if opts.CPU > 0 {
		args = append(args, "--cpus", fmt.Sprintf("%.2f", opts.CPU))
	}
	
	if opts.Memory != "" {
		args = append(args, "--memory", opts.Memory)
	}
	
	if opts.Pids > 0 {
		args = append(args, "--pids-limit", strconv.Itoa(opts.Pids))
	}
	
	if opts.Swap != "" && opts.Swap != "0" {
		args = append(args, "--memory-swap", opts.Swap)
	}
	
	// Add container options
	if opts.Name != "" {
		args = append(args, "--name", opts.Name)
	}
	
	if opts.Remove {
		args = append(args, "--rm")
	}
	
	if opts.Detach {
		args = append(args, "--detach")
	}
	
	// Add volumes
	for _, volume := range opts.Volumes {
		args = append(args, "--volume", volume)
	}
	
	// Add environment variables
	for _, env := range opts.EnvVars {
		args = append(args, "--env", env)
	}
	
	// Add capsule labels for identification
	args = append(args, "--label", "capsule.managed=true")
	args = append(args, "--label", fmt.Sprintf("capsule.cpu=%.2f", opts.CPU))
	args = append(args, "--label", fmt.Sprintf("capsule.memory=%s", opts.Memory))
	args = append(args, "--label", fmt.Sprintf("capsule.created=%d", time.Now().Unix()))
	
	// Add image and command
	args = append(args, opts.Image)
	args = append(args, opts.Command...)
	
	// Execute docker command
	cmd := exec.CommandContext(ctx, "docker", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	
	err := cmd.Run()
	
	result := &RunResult{
		Status: "completed",
	}
	
	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			result.ExitCode = exitError.ExitCode()
			result.Status = "failed"
		} else {
			return nil, fmt.Errorf("failed to execute docker command: %w", err)
		}
	}
	
	// If detached, get container ID
	if opts.Detach {
		result.ContainerID = d.getLastContainerID()
		result.Status = "running"
	}
	
	return result, nil
}

// BuildAndRunGo builds a Go program and runs it in a container
func (d *Client) BuildAndRunGo(ctx context.Context, opts GoBuildOptions) (*GoBuildResult, error) {
	// Create temporary directory for build
	tempDir, err := os.MkdirTemp("", "capsule-go-build-*")
	if err != nil {
		return nil, fmt.Errorf("failed to create temp directory: %w", err)
	}
	defer os.RemoveAll(tempDir)
	
	// Build the Go program
	binaryPath := filepath.Join(tempDir, "main")
	buildArgs := []string{"build", "-o", binaryPath}
	
	if opts.BuildFlags != "" {
		buildArgs = append(buildArgs, strings.Fields(opts.BuildFlags)...)
	}
	
	if opts.LdFlags != "" {
		buildArgs = append(buildArgs, "-ldflags", opts.LdFlags)
	}
	
	buildArgs = append(buildArgs, opts.PackagePath)
	
	buildCmd := exec.CommandContext(ctx, "go", buildArgs...)
	buildCmd.Stderr = os.Stderr
	
	if err := buildCmd.Run(); err != nil {
		return nil, fmt.Errorf("failed to build Go program: %w", err)
	}
	
	// Create Dockerfile
	dockerfilePath := filepath.Join(tempDir, "Dockerfile")
	dockerfile := `FROM gcr.io/distroless/static
COPY main /main
ENTRYPOINT ["/main"]
`
	
	if err := os.WriteFile(dockerfilePath, []byte(dockerfile), 0644); err != nil {
		return nil, fmt.Errorf("failed to create Dockerfile: %w", err)
	}
	
	// Generate image tag if not provided
	imageTag := opts.Tag
	if imageTag == "" {
		imageTag = fmt.Sprintf("capsule-go-%d", time.Now().Unix())
	}
	
	// Build Docker image
	buildDockerArgs := []string{"build", "-t", imageTag, tempDir}
	buildDockerCmd := exec.CommandContext(ctx, "docker", buildDockerArgs...)
	buildDockerCmd.Stderr = os.Stderr
	
	if err := buildDockerCmd.Run(); err != nil {
		return nil, fmt.Errorf("failed to build Docker image: %w", err)
	}
	
	// Run the container
	runOpts := RunOptions{
		Image:   imageTag,
		Command: opts.Args,
		CPU:     opts.CPU,
		Memory:  opts.Memory,
		Remove:  !opts.KeepImage, // Don't remove if keeping image
	}
	
	runResult, err := d.RunContainer(ctx, runOpts)
	if err != nil {
		return nil, fmt.Errorf("failed to run container: %w", err)
	}
	
	// Clean up image if not keeping it
	if !opts.KeepImage {
		cleanupCmd := exec.CommandContext(ctx, "docker", "rmi", imageTag)
		cleanupCmd.Run() // Ignore errors for cleanup
	}
	
	return &GoBuildResult{
		ContainerID: runResult.ContainerID,
		ImageTag:    imageTag,
		Status:      runResult.Status,
		ExitCode:    runResult.ExitCode,
		Output:      runResult.Output,
	}, nil
}

// getLastContainerID gets the ID of the last created container
func (d *Client) getLastContainerID() string {
	cmd := exec.Command("docker", "ps", "-lq")
	output, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(output))
}

// ListContainers lists all capsule-managed containers
func (d *Client) ListContainers(ctx context.Context) ([]ContainerInfo, error) {
	cmd := exec.CommandContext(ctx, "docker", "ps", "-a", 
		"--filter", "label=capsule.managed=true",
		"--format", "table {{.ID}}\t{{.Image}}\t{{.Status}}\t{{.Names}}\t{{.Label \"capsule.cpu\"}}\t{{.Label \"capsule.memory\"}}")
	
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to list containers: %w", err)
	}
	
	// Parse output (simplified for now)
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	var containers []ContainerInfo
	
	for i, line := range lines {
		if i == 0 { // Skip header
			continue
		}
		
		fields := strings.Fields(line)
		if len(fields) >= 6 {
			containers = append(containers, ContainerInfo{
				ID:     fields[0],
				Image:  fields[1],
				Status: fields[2],
				Name:   fields[3],
				CPU:    fields[4],
				Memory: fields[5],
			})
		}
	}
	
	return containers, nil
}

// ContainerInfo represents information about a container
type ContainerInfo struct {
	ID     string
	Image  string
	Status string
	Name   string
	CPU    string
	Memory string
}

// StopContainer stops a container
func (d *Client) StopContainer(ctx context.Context, containerID string) error {
	cmd := exec.CommandContext(ctx, "docker", "stop", containerID)
	return cmd.Run()
}

// RemoveContainer removes a container
func (d *Client) RemoveContainer(ctx context.Context, containerID string, force bool) error {
	args := []string{"rm"}
	if force {
		args = append(args, "--force")
	}
	args = append(args, containerID)
	
	cmd := exec.CommandContext(ctx, "docker", args...)
	return cmd.Run()
}

// GetContainerStats gets real-time stats for containers
func (d *Client) GetContainerStats(ctx context.Context, containerIDs []string) error {
	args := []string{"stats"}
	if len(containerIDs) > 0 {
		args = append(args, containerIDs...)
	} else {
		// Show stats for all capsule-managed containers
		args = append(args, "--filter", "label=capsule.managed=true")
	}
	
	cmd := exec.CommandContext(ctx, "docker", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	
	return cmd.Run()
}

