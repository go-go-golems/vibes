package model

import (
	"fmt"
	"time"

	"github.com/pi-go/pi/internal/config"
	"github.com/pi-go/pi/pkg/types"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func NewStartCommand(cfg *types.Config, logger *logrus.Logger) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "start <model-id>",
		Short: "Start a vLLM model",
		Long: `Start a vLLM model on a GPU pod.

This command deploys a model using vLLM on the specified or active pod.
The model will be downloaded if not already cached locally.

Examples:
  # Start a model on the active pod
  pi model start microsoft/Phi-3-mini-128k-instruct

  # Start a model with custom name and memory allocation
  pi model start microsoft/Phi-3-mini-128k-instruct --name phi3 --memory 0.3

  # Start a model on a specific pod
  pi model start Qwen/Qwen2.5-7B-Instruct --pod my-pod --memory 0.5

  # Start a model using all GPUs with tensor parallelism
  pi model start meta-llama/Meta-Llama-3-70B-Instruct --all-gpus

  # Start a model from a template
  pi model start --template phi3-mini --name my-phi3`,
		Args: cobra.MaximumNArgs(1),
		RunE: func(cobraCmd *cobra.Command, args []string) error {
			return runStartCommand(cfg, logger, cobraCmd, args)
		},
	}

	cmd.Flags().String("name", "", "Custom name for the model instance")
	cmd.Flags().String("pod", "", "Pod name (uses active pod if not specified)")
	cmd.Flags().Float64("memory", 0.0, "GPU memory allocation (0.0-1.0, uses default if not specified)")
	cmd.Flags().IntSlice("gpu", []int{}, "Specific GPU IDs to use")
	cmd.Flags().Bool("all-gpus", false, "Use all available GPUs with tensor parallelism")
	cmd.Flags().StringSlice("vllm-args", []string{}, "Additional vLLM arguments")
	cmd.Flags().String("tool-parser", "", "Tool parser to use (hermes, llama3_json, etc.)")
	cmd.Flags().String("template", "", "Use a model template instead of specifying model-id")
	cmd.Flags().Int("port", 0, "Port for the model API (auto-assigned if not specified)")

	return cmd
}

func runStartCommand(cfg *types.Config, logger *logrus.Logger, cmd *cobra.Command, args []string) error {
	// Parse flags
	name, _ := cmd.Flags().GetString("name")
	podName, _ := cmd.Flags().GetString("pod")
	memory, _ := cmd.Flags().GetFloat64("memory")
	gpuIDs, _ := cmd.Flags().GetIntSlice("gpu")
	allGPUs, _ := cmd.Flags().GetBool("all-gpus")
	vllmArgs, _ := cmd.Flags().GetStringSlice("vllm-args")
	toolParser, _ := cmd.Flags().GetString("tool-parser")
	templateName, _ := cmd.Flags().GetString("template")
	port, _ := cmd.Flags().GetInt("port")

	var modelID string

	// Determine model ID from template or argument
	if templateName != "" {
		template, exists := cfg.Models.Templates[templateName]
		if !exists {
			return fmt.Errorf("template '%s' not found", templateName)
		}
		modelID = template.ModelID
		
		// Use template defaults if not overridden
		if name == "" {
			name = template.Name
		}
		if memory == 0.0 {
			memory = template.GPUMemory
		}
		if !allGPUs {
			allGPUs = template.AllGPUs
		}
		if len(vllmArgs) == 0 {
			vllmArgs = template.VLLMArgs
		}
		if toolParser == "" {
			toolParser = template.ToolParser
		}
	} else {
		if len(args) == 0 {
			return fmt.Errorf("model-id is required when not using a template")
		}
		modelID = args[0]
	}

	// Use defaults if not specified
	if name == "" {
		name = modelID
	}
	if memory == 0.0 {
		memory = cfg.Global.DefaultGPUMemory
	}
	if podName == "" {
		if cfg.Pods.Active == "" {
			return fmt.Errorf("no active pod set, specify --pod or activate a pod")
		}
		podName = cfg.Pods.Active
	}

	// Validate pod exists
	pod, exists := cfg.Pods.Pods[podName]
	if !exists {
		return fmt.Errorf("pod '%s' not found", podName)
	}

	// Check if model name already exists
	if _, exists := cfg.Models.Models[name]; exists {
		return fmt.Errorf("model '%s' already exists", name)
	}

	// Auto-assign port if not specified
	if port == 0 {
		port = findAvailablePort(cfg)
	}

	// Create model instance
	model := &types.Model{
		Name:        name,
		ModelID:     modelID,
		PodName:     podName,
		Port:        port,
		Status:      types.ModelStatusStarting,
		GPUMemory:   memory,
		GPUIDs:      gpuIDs,
		AllGPUs:     allGPUs,
		VLLMArgs:    vllmArgs,
		ToolParser:  toolParser,
		Metadata:    make(map[string]string),
		CreatedAt:   time.Now(),
		UpdatedAt:   time.Now(),
	}

	// Validate model
	if err := model.Validate(); err != nil {
		return fmt.Errorf("invalid model configuration: %w", err)
	}

	// Add model to configuration
	if err := cfg.AddModel(model); err != nil {
		return fmt.Errorf("failed to add model: %w", err)
	}

	// Save configuration
	if err := config.Save(cfg); err != nil {
		return fmt.Errorf("failed to save configuration: %w", err)
	}

	fmt.Printf("Successfully started model '%s'\n", name)
	fmt.Printf("Model ID: %s\n", modelID)
	fmt.Printf("Pod: %s (%s)\n", podName, pod.Host)
	fmt.Printf("Port: %d\n", port)
	fmt.Printf("GPU Memory: %.1f%%\n", memory*100)
	if allGPUs {
		fmt.Printf("GPU Usage: All GPUs (tensor parallelism)\n")
	} else if len(gpuIDs) > 0 {
		fmt.Printf("GPU IDs: %v\n", gpuIDs)
	}
	if toolParser != "" {
		fmt.Printf("Tool Parser: %s\n", toolParser)
	}

	fmt.Printf("\nAPI Endpoint: %s\n", model.GetAPIEndpoint(pod.Host))
	fmt.Printf("Status: %s\n", model.Status)

	// TODO: Implement actual model deployment via SSH
	fmt.Printf("\nNote: Actual model deployment not yet implemented.\n")
	fmt.Printf("This would execute the vLLM command on pod '%s'.\n", podName)

	return nil
}

// findAvailablePort finds an available port for the model
func findAvailablePort(cfg *types.Config) int {
	usedPorts := make(map[int]bool)
	
	// Collect used ports
	for _, model := range cfg.Models.Models {
		usedPorts[model.Port] = true
	}
	
	// Find first available port in range
	for port := cfg.SSH.TunnelPortRange.Start; port <= cfg.SSH.TunnelPortRange.End; port++ {
		if !usedPorts[port] {
			return port
		}
	}
	
	// Fallback to default range if tunnel range is full
	for port := 8001; port <= 8100; port++ {
		if !usedPorts[port] {
			return port
		}
	}
	
	return 8001 // Last resort
}

