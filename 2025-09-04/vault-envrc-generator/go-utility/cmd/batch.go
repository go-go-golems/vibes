package cmd

import (
	"fmt"
	"os"
	"path/filepath"
	"context"
	"time"
	"encoding/json"
	"sync"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
	"vault-envrc-generator/pkg/envrc"
	"vault-envrc-generator/pkg/vault"
)

// BatchConfig represents the configuration for batch processing
type BatchConfig struct {
	Jobs []BatchJob `yaml:"jobs"`
}

// BatchJob represents a single job in batch processing
type BatchJob struct {
	Name         string            `yaml:"name"`
	Path         string            `yaml:"path"`
	Output       string            `yaml:"output"`
	OutputMode   string            `yaml:"output_mode,omitempty"` // overwrite (default), append, merge
	Prefix       string            `yaml:"prefix,omitempty"`
	ExcludeKeys  []string          `yaml:"exclude_keys,omitempty"`
	IncludeKeys  []string          `yaml:"include_keys,omitempty"`
	Transform    bool              `yaml:"transform_keys,omitempty"`
	Format       string            `yaml:"format,omitempty"`
	Template     string            `yaml:"template,omitempty"`
	Variables    map[string]string `yaml:"variables,omitempty"`
}

var (
	batchConfigFile string
	parallel        bool
	continueOnError bool
)

var outputLocks = struct {
	mu    sync.Mutex
	locks map[string]*sync.Mutex
}{locks: make(map[string]*sync.Mutex)}

func lockForPath(path string) func() {
	outputLocks.mu.Lock()
	m, ok := outputLocks.locks[path]
	if !ok {
		m = &sync.Mutex{}
		outputLocks.locks[path] = m
	}
	outputLocks.mu.Unlock()
	m.Lock()
	return func() { m.Unlock() }
}

// batchCmd represents the batch command
var batchCmd = &cobra.Command{
	Use:   "batch",
	Short: "Process multiple Vault paths in batch mode",
	Long: `Process multiple Vault secret paths using a configuration file.

Batch mode allows you to:
- Process multiple secret paths with different configurations
- Generate multiple .envrc files with different settings
- Use templates and variables for dynamic generation
- Run jobs in parallel for faster processing
- Continue processing even if some jobs fail

The batch configuration file should be in YAML format with the following structure:

jobs:
  - name: "Frontend App"
    path: "secret/frontend"
    output: "frontend/.envrc"
    output_mode: overwrite   # overwrite | append | merge (merge for json/yaml)
    prefix: "FRONTEND_"
    transform_keys: true
    exclude_keys: ["internal_key"]
    
  - name: "Backend API"
    path: "secret/backend"
    output: "backend/config.json"
    format: "json"
    output_mode: merge       # merges JSON objects across jobs
    template: "templates/api.tmpl"
    variables:
      service_name: "api-server"

Examples:
  # Process batch configuration
  vault-envrc-generator batch --config batch-jobs.yaml

  # Run jobs in parallel
  vault-envrc-generator batch --config batch-jobs.yaml --parallel

  # Continue on errors
  vault-envrc-generator batch --config batch-jobs.yaml --continue-on-error`,
	RunE: runBatch,
}

func init() {
	rootCmd.AddCommand(batchCmd)

	batchCmd.Flags().StringVarP(&batchConfigFile, "config", "c", "", "Batch configuration file (required)")
	batchCmd.Flags().BoolVar(&parallel, "parallel", false, "Run jobs in parallel")
	batchCmd.Flags().BoolVar(&continueOnError, "continue-on-error", false, "Continue processing if a job fails")

	batchCmd.MarkFlagRequired("config")
}

func runBatch(cmd *cobra.Command, args []string) error {
	if viper.GetBool("verbose") {
		fmt.Fprintf(os.Stderr, "Loading batch configuration from: %s\n", batchConfigFile)
	}

	// Load batch configuration
	config, err := loadBatchConfig(batchConfigFile)
	if err != nil {
		return fmt.Errorf("failed to load batch configuration: %w", err)
	}

	if len(config.Jobs) == 0 {
		return fmt.Errorf("no jobs found in configuration file")
	}

	fmt.Printf("Loaded %d jobs from configuration\n", len(config.Jobs))

	// Resolve token via loader
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	resolvedToken, err := vault.ResolveToken(
		ctx,
		viper.GetString("vault.token"),
		vault.TokenSource(viper.GetString("vault.token_source")),
		viper.GetString("vault.token_file"),
		viper.GetBool("verbose"),
	)
	if err != nil {
		return fmt.Errorf("failed to resolve Vault token: %w", err)
	}

	// Initialize Vault client
	vaultClient, err := vault.NewClient(viper.GetString("vault.addr"), resolvedToken)
	if err != nil {
		return fmt.Errorf("failed to create Vault client: %w", err)
	}

	// Process jobs
	if parallel {
		return processBatchParallel(vaultClient, config.Jobs)
	} else {
		return processBatchSequential(vaultClient, config.Jobs)
	}
}

func loadBatchConfig(filename string) (*BatchConfig, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var config BatchConfig
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse YAML config: %w", err)
	}

	return &config, nil
}

func processBatchSequential(vaultClient *vault.Client, jobs []BatchJob) error {
	var errors []error

	for i, job := range jobs {
		fmt.Printf("[%d/%d] Processing job: %s\n", i+1, len(jobs), job.Name)
		
		if err := processJob(vaultClient, job); err != nil {
			fmt.Fprintf(os.Stderr, "Job '%s' failed: %v\n", job.Name, err)
			errors = append(errors, err)
			
			if !continueOnError {
				return fmt.Errorf("job '%s' failed: %w", job.Name, err)
			}
		} else {
			fmt.Printf("✓ Job '%s' completed successfully\n", job.Name)
		}
	}

	if len(errors) > 0 {
		fmt.Printf("\nCompleted with %d errors out of %d jobs\n", len(errors), len(jobs))
		return fmt.Errorf("batch processing completed with %d errors", len(errors))
	}

	fmt.Printf("\n✓ All %d jobs completed successfully\n", len(jobs))
	return nil
}

func processBatchParallel(vaultClient *vault.Client, jobs []BatchJob) error {
	// For simplicity, we'll implement a basic parallel processing
	// In a production system, you might want to use worker pools
	
	type jobResult struct {
		job   BatchJob
		error error
	}

	results := make(chan jobResult, len(jobs))

	// Start all jobs
	for _, job := range jobs {
		go func(j BatchJob) {
			err := processJob(vaultClient, j)
			results <- jobResult{job: j, error: err}
		}(job)
	}

	// Collect results
	var errors []error
	for i := 0; i < len(jobs); i++ {
		result := <-results
		if result.error != nil {
			fmt.Fprintf(os.Stderr, "Job '%s' failed: %v\n", result.job.Name, result.error)
			errors = append(errors, result.error)
		} else {
			fmt.Printf("✓ Job '%s' completed successfully\n", result.job.Name)
		}
	}

	if len(errors) > 0 && !continueOnError {
		return fmt.Errorf("batch processing failed with %d errors", len(errors))
	}

	if len(errors) > 0 {
		fmt.Printf("\nCompleted with %d errors out of %d jobs\n", len(errors), len(jobs))
	} else {
		fmt.Printf("\n✓ All %d jobs completed successfully\n", len(jobs))
	}

	return nil
}

func processJob(vaultClient *vault.Client, job BatchJob) error {
	// Retrieve secrets
	secrets, err := vaultClient.GetSecrets(job.Path)
	if err != nil {
		return fmt.Errorf("failed to retrieve secrets from path %s: %w", job.Path, err)
	}

	// Apply job variables to secrets if specified
	if len(job.Variables) > 0 {
		for key, value := range job.Variables {
			secrets[key] = value
		}
	}

	// Create generator options
	options := &envrc.Options{
		Prefix:        job.Prefix,
		ExcludeKeys:   job.ExcludeKeys,
		IncludeKeys:   job.IncludeKeys,
		TransformKeys: job.Transform,
		Format:        job.Format,
		TemplateFile:  job.Template,
		Verbose:       viper.GetBool("verbose"),
	}

	// Set default format if not specified
	if options.Format == "" {
		options.Format = "envrc"
	}

	// Generate content
	generator := envrc.NewGenerator(options)
	content, err := generator.Generate(secrets)
	if err != nil {
		return fmt.Errorf("failed to generate content: %w", err)
	}

	// Ensure output directory exists
	outputDir := filepath.Dir(job.Output)
	if outputDir != "." {
		if err := os.MkdirAll(outputDir, 0755); err != nil {
			return fmt.Errorf("failed to create output directory %s: %w", outputDir, err)
		}
	}

	unlock := lockForPath(job.Output)
	defer unlock()

	mode := job.OutputMode
	if mode == "" { mode = "overwrite" }

	switch mode {
	case "overwrite":
		return os.WriteFile(job.Output, []byte(content), 0644)
	case "append":
		f, err := os.OpenFile(job.Output, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
		if err != nil { return fmt.Errorf("failed to open output file %s: %w", job.Output, err) }
		defer f.Close()
		if _, err := f.WriteString(content); err != nil { return fmt.Errorf("failed to append to %s: %w", job.Output, err) }
		return nil
	case "merge":
		// Only meaningful for json|yaml; envrc falls back to append
		switch options.Format {
		case "json":
			var existing map[string]interface{}
			if b, err := os.ReadFile(job.Output); err == nil && len(b) > 0 {
				_ = json.Unmarshal(b, &existing)
			}
			if existing == nil { existing = map[string]interface{}{} }
			var next map[string]interface{}
			if err := json.Unmarshal([]byte(content), &next); err != nil {
				return fmt.Errorf("failed to parse generated JSON for merge: %w", err)
			}
			for k, v := range next { existing[k] = v }
			buf, err := json.MarshalIndent(existing, "", "  ")
			if err != nil { return fmt.Errorf("failed to marshal merged JSON: %w", err) }
			return os.WriteFile(job.Output, buf, 0644)
		case "yaml":
			var existing map[string]interface{}
			if b, err := os.ReadFile(job.Output); err == nil && len(b) > 0 {
				_ = yaml.Unmarshal(b, &existing)
			}
			if existing == nil { existing = map[string]interface{}{} }
			var next map[string]interface{}
			if err := yaml.Unmarshal([]byte(content), &next); err != nil {
				return fmt.Errorf("failed to parse generated YAML for merge: %w", err)
			}
			for k, v := range next { existing[k] = v }
			buf, err := yaml.Marshal(existing)
			if err != nil { return fmt.Errorf("failed to marshal merged YAML: %w", err) }
			return os.WriteFile(job.Output, buf, 0644)
		default:
			f, err := os.OpenFile(job.Output, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil { return fmt.Errorf("failed to open output file %s: %w", job.Output, err) }
			defer f.Close()
			if _, err := f.WriteString(content); err != nil { return fmt.Errorf("failed to append to %s: %w", job.Output, err) }
			return nil
		}
	default:
		return fmt.Errorf("unknown output_mode: %s", mode)
	}
}

