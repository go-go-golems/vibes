package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "capsule",
	Short: "A thin wrapper around Docker for running binaries with resource constraints",
	Long:  `Capsule is a CLI tool that wraps Docker to run binaries and Go programs with repeatable CPU, memory, and network constraints.`,
}

var runCmd = &cobra.Command{
	Use:   "run [flags] IMAGE [COMMAND...]",
	Short: "Execute a command once inside a fresh capsule",
	Long: `Run a command once inside a Docker container with specified resource constraints.
The container is destroyed on exit by default.

Examples:
  capsule run --cpu 1 --mem 2g ubuntu:latest echo "Hello World"
  capsule run --cpu 0.5 --mem 512m alpine:latest /bin/sh -c "sleep 10"`,
	Args: cobra.MinimumNArgs(1),
	RunE: runContainer,
}

var (
	cpu    float64
	memory string
	name   string
	rm     bool
	detach bool
)

func init() {
	runCmd.Flags().Float64Var(&cpu, "cpu", 1.0, "CPU limit (number of cores)")
	runCmd.Flags().StringVar(&memory, "mem", "1g", "Memory limit (e.g., 512m, 2g)")
	runCmd.Flags().StringVar(&name, "name", "", "Container name")
	runCmd.Flags().BoolVar(&rm, "rm", true, "Remove container on exit")
	runCmd.Flags().BoolVar(&detach, "detach", false, "Run container in background")
	
	rootCmd.AddCommand(runCmd)
}

func runContainer(cmd *cobra.Command, args []string) error {
	image := args[0]
	command := args[1:]

	dockerArgs := []string{"run", "--network=host"}
	
	// Add resource constraints
	if cpu > 0 {
		dockerArgs = append(dockerArgs, "--cpus", fmt.Sprintf("%.2f", cpu))
	}
	
	if memory != "" {
		dockerArgs = append(dockerArgs, "--memory", memory)
	}
	
	// Add container options
	if name != "" {
		dockerArgs = append(dockerArgs, "--name", name)
	}
	
	if rm {
		dockerArgs = append(dockerArgs, "--rm")
	}
	
	if detach {
		dockerArgs = append(dockerArgs, "--detach")
	}
	
	// Add capsule labels for identification
	dockerArgs = append(dockerArgs, "--label", "capsule.managed=true")
	dockerArgs = append(dockerArgs, "--label", fmt.Sprintf("capsule.cpu=%.2f", cpu))
	dockerArgs = append(dockerArgs, "--label", fmt.Sprintf("capsule.memory=%s", memory))
	dockerArgs = append(dockerArgs, "--label", fmt.Sprintf("capsule.created=%d", time.Now().Unix()))
	
	// Add image and command
	dockerArgs = append(dockerArgs, image)
	dockerArgs = append(dockerArgs, command...)
	
	fmt.Printf("Running: docker %s\n", strings.Join(dockerArgs, " "))
	
	// Execute docker command
	dockerCmd := exec.Command("docker", dockerArgs...)
	dockerCmd.Stdout = os.Stdout
	dockerCmd.Stderr = os.Stderr
	dockerCmd.Stdin = os.Stdin
	
	return dockerCmd.Run()
}

// Add a simple go command
var goCmd = &cobra.Command{
	Use:   "go [flags] PACKAGE [ARGS...]",
	Short: "Build and run Go programs in a capsule",
	Long: `Build a Go program and run it inside a Docker container with resource constraints.
This command builds the Go program, creates a minimal container image, and runs it.

Examples:
  capsule go ./test-programs/burner.go --cpu 1 --mem 2g -- -cpu-threads 2 -memory-mb 500`,
	Args: cobra.MinimumNArgs(1),
	RunE: runGoProgram,
}

var (
	goCpu    float64
	goMemory string
	keepImage bool
)

func init() {
	goCmd.Flags().Float64Var(&goCpu, "cpu", 1.0, "CPU limit (number of cores)")
	goCmd.Flags().StringVar(&goMemory, "mem", "1g", "Memory limit (e.g., 512m, 2g)")
	goCmd.Flags().BoolVar(&keepImage, "keep-image", false, "Keep the built image after run")
	
	rootCmd.AddCommand(goCmd)
}

func runGoProgram(cmd *cobra.Command, args []string) error {
	packagePath := args[0]
	programArgs := args[1:]

	// Create temporary directory for build
	tempDir, err := os.MkdirTemp("", "capsule-go-build-*")
	if err != nil {
		return fmt.Errorf("failed to create temp directory: %w", err)
	}
	defer os.RemoveAll(tempDir)

	// Build the Go program
	binaryPath := tempDir + "/main"
	buildCmd := exec.Command("go", "build", "-o", binaryPath, packagePath)
	buildCmd.Stderr = os.Stderr
	
	fmt.Printf("Building Go program: %s\n", packagePath)
	if err := buildCmd.Run(); err != nil {
		return fmt.Errorf("failed to build Go program: %w", err)
	}

	// Create Dockerfile
	dockerfilePath := tempDir + "/Dockerfile"
	dockerfile := `FROM gcr.io/distroless/static
COPY main /main
ENTRYPOINT ["/main"]
`
	
	if err := os.WriteFile(dockerfilePath, []byte(dockerfile), 0644); err != nil {
		return fmt.Errorf("failed to create Dockerfile: %w", err)
	}

	// Generate image tag
	imageTag := fmt.Sprintf("capsule-go-%d", time.Now().Unix())

	// Build Docker image
	fmt.Printf("Building Docker image: %s\n", imageTag)
	buildDockerCmd := exec.Command("docker", "build", "-t", imageTag, tempDir)
	buildDockerCmd.Stderr = os.Stderr
	
	if err := buildDockerCmd.Run(); err != nil {
		return fmt.Errorf("failed to build Docker image: %w", err)
	}

	// Run the container
	dockerArgs := []string{"run", "--rm", "--network=host"}
	
	// Add resource constraints
	if goCpu > 0 {
		dockerArgs = append(dockerArgs, "--cpus", fmt.Sprintf("%.2f", goCpu))
	}
	
	if goMemory != "" {
		dockerArgs = append(dockerArgs, "--memory", goMemory)
	}
	
	// Add capsule labels
	dockerArgs = append(dockerArgs, "--label", "capsule.managed=true")
	dockerArgs = append(dockerArgs, "--label", fmt.Sprintf("capsule.cpu=%.2f", goCpu))
	dockerArgs = append(dockerArgs, "--label", fmt.Sprintf("capsule.memory=%s", goMemory))
	
	// Add image and args
	dockerArgs = append(dockerArgs, imageTag)
	dockerArgs = append(dockerArgs, programArgs...)
	
	fmt.Printf("Running container: docker %s\n", strings.Join(dockerArgs, " "))
	
	// Execute docker command
	dockerCmd := exec.Command("docker", dockerArgs...)
	dockerCmd.Stdout = os.Stdout
	dockerCmd.Stderr = os.Stderr
	dockerCmd.Stdin = os.Stdin
	
	err = dockerCmd.Run()
	
	// Clean up image if not keeping it
	if !keepImage {
		cleanupCmd := exec.Command("docker", "rmi", imageTag)
		cleanupCmd.Run() // Ignore errors for cleanup
	}
	
	return err
}

// Add a simple stats command
var statsCmd = &cobra.Command{
	Use:   "stats [CONTAINER...]",
	Short: "Stream live resource usage for capsules",
	Long:  `Stream live CPU, memory, and network usage statistics for running capsules.`,
	RunE:  showStats,
}

func init() {
	rootCmd.AddCommand(statsCmd)
}

func showStats(cmd *cobra.Command, args []string) error {
	dockerArgs := []string{"stats"}
	if len(args) > 0 {
		dockerArgs = append(dockerArgs, args...)
	} else {
		// Show stats for all capsule-managed containers
		dockerArgs = append(dockerArgs, "--filter", "label=capsule.managed=true")
	}
	
	dockerCmd := exec.Command("docker", dockerArgs...)
	dockerCmd.Stdout = os.Stdout
	dockerCmd.Stderr = os.Stderr
	dockerCmd.Stdin = os.Stdin
	
	return dockerCmd.Run()
}

// Add a simple ls command
var lsCmd = &cobra.Command{
	Use:   "ls",
	Short: "List running capsules",
	Long:  `List all capsule-managed containers with their resource usage and status.`,
	RunE:  listContainers,
}

func init() {
	rootCmd.AddCommand(lsCmd)
}

func listContainers(cmd *cobra.Command, args []string) error {
	dockerCmd := exec.Command("docker", "ps", "-a", 
		"--filter", "label=capsule.managed=true",
		"--format", "table {{.ID}}\t{{.Image}}\t{{.Status}}\t{{.Names}}\t{{.Label \"capsule.cpu\"}}\t{{.Label \"capsule.memory\"}}")
	
	dockerCmd.Stdout = os.Stdout
	dockerCmd.Stderr = os.Stderr
	
	return dockerCmd.Run()
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

