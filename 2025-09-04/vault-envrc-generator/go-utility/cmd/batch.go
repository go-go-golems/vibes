package cmd

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
	"vault-envrc-generator/pkg/batch"
	"vault-envrc-generator/pkg/vault"
)

// BatchConfig represents the configuration for batch processing
type BatchConfig struct {
	BasePath string     `yaml:"base_path"`
	Jobs     []BatchJob `yaml:"jobs"`
}

// TemplateContext used for rendering templated strings such as paths
type TemplateContext struct {
	Token TokenContext
}

type TokenContext struct {
	Accessor    string
	CreationTTL string
	DisplayName string
	EntityID    string
	ExpireTime  string
	ID          string
	IssueTime   string
	Meta        map[string]string
	Policies    []string
	Path        string
	TTL         string
	Type        string
	OIDCUserID  string
}

// BatchSection represents one logical section emitted by a job
type BatchSection struct {
	Name        string            `yaml:"name,omitempty"`
	Description string            `yaml:"description,omitempty"`
	Path        string            `yaml:"path,omitempty"`
	Prefix      string            `yaml:"prefix,omitempty"`
	ExcludeKeys []string          `yaml:"exclude_keys,omitempty"`
	IncludeKeys []string          `yaml:"include_keys,omitempty"`
	Transform   *bool             `yaml:"transform_keys,omitempty"`
	Template    string            `yaml:"template,omitempty"`
	Variables   map[string]string `yaml:"variables,omitempty"`
	Format      string            `yaml:"format,omitempty"`  // optional override
	Output      string            `yaml:"output,omitempty"`  // optional override
	EnvMap      map[string]string `yaml:"env_map,omitempty"` // explicit ENV_VAR -> source_key mapping
	Fixed       map[string]string `yaml:"fixed,omitempty"`   // fixed key->templated value additions
}

// BatchJob represents a single job in batch processing
type BatchJob struct {
	Name        string            `yaml:"name"`
	Description string            `yaml:"description,omitempty"`
	Path        string            `yaml:"path,omitempty"`        // legacy single-path mode (templated)
	Output      string            `yaml:"output"`                // templated
	OutputMode  string            `yaml:"output_mode,omitempty"` // overwrite (default), append, merge
	Prefix      string            `yaml:"prefix,omitempty"`
	ExcludeKeys []string          `yaml:"exclude_keys,omitempty"`
	IncludeKeys []string          `yaml:"include_keys,omitempty"`
	Transform   *bool             `yaml:"transform_keys,omitempty"`
	Format      string            `yaml:"format,omitempty"`
	Template    string            `yaml:"template,omitempty"`
	Variables   map[string]string `yaml:"variables,omitempty"`
	Sections    []BatchSection    `yaml:"sections,omitempty"`
	BasePath    string            `yaml:"base_path,omitempty"` // optional per-job base path (templated)
	Fixed       map[string]string `yaml:"fixed,omitempty"`     // fixed key->templated value additions
}

var (
	batchConfigFile         string
	continueOnError         bool
	batchBasePath           string
	batchOutputOverride     string
	batchOutputModeOverride string
	batchFormatOverride     string
)

// batchCmd represents the batch command
var batchCmd = &cobra.Command{
	Use:   "batch",
	Short: "Process multiple Vault paths in batch mode",
	Long: `Process multiple Vault secret paths using a configuration file.

Batch mode allows you to:
- Process multiple secret paths with different configurations
- Generate multiple .envrc files with different settings
- Use templates and variables for dynamic generation
- Continue processing even if some jobs fail
- Generate JSON or YAML outputs and optionally merge across sections

Two schemas are supported:

1) Legacy per-job schema

jobs:
  - name: "Frontend App"
    path: "secret/frontend"
    output: "frontend/.envrc"
    output_mode: overwrite   # overwrite | append | merge (merge for json/yaml)
    description: "Exports for frontend app"
    prefix: "FRONTEND_"
    transform_keys: true
    exclude_keys: ["internal_key"]
    
2) Sections schema (recommended)

jobs:
  - name: "Dev envrc"
    description: "Development env aggregation"
    output: "out/dev/.envrc"
    output_mode: append
    format: envrc
    sections:
      - name: db
        description: "Shared DB user/password"
        path: secrets/environments/development/shared/database
        include_keys: [username, password]
        prefix: DATABASE_
        transform_keys: true
      - name: google-oauth
        path: secrets/external-apis/development/google-oauth
        env_map:                 # explicit mapping ENV_VAR -> key
          GOOGLE_CLIENT_ID: client_id
          GOOGLE_CLIENT_SECRET: client_secret

Templating:
- You can template paths and outputs using Go templates with token context.
- Example: path: "secrets/environments/personal/{{ .Token.OIDCUserID }}/manuel/core"
`,
	RunE: runBatch,
}

func init() {
	rootCmd.AddCommand(batchCmd)

	batchCmd.Flags().StringVarP(&batchConfigFile, "config", "c", "", "Batch configuration file (required)")
	batchCmd.Flags().BoolVar(&continueOnError, "continue-on-error", false, "Continue processing if a job fails")
	batchCmd.Flags().StringVar(&batchBasePath, "base-path", "", "Base Vault path to prepend to relative section paths (overrides YAML base_path)")
	batchCmd.Flags().StringVar(&batchOutputOverride, "output", "", "Override output for all jobs; use '-' for stdout")
	batchCmd.Flags().StringVar(&batchOutputModeOverride, "output-mode", "", "Override output mode for all jobs: overwrite|append|merge")
	batchCmd.Flags().StringVar(&batchFormatOverride, "format", "", "Override format for all jobs: envrc|json|yaml")

	viper.BindPFlag("batch.base_path", batchCmd.Flags().Lookup("base-path"))

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

	// Delegate to batch processor
	proc := batch.Processor{Client: vaultClient, Verbose: viper.GetBool("verbose")}
	return proc.Process(config, batch.ProcessorOptions{
		BasePath:           viper.GetString("batch.base_path"),
		OutputOverride:     batchOutputOverride,
		OutputModeOverride: batchOutputModeOverride,
		FormatOverride:     batchFormatOverride,
		ContinueOnError:    continueOnError,
	})
}

func loadBatchConfig(filename string) (*batch.Config, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var config batch.Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse YAML config: %w", err)
	}

	return &config, nil
}
