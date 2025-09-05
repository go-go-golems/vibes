package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"text/template"
	"time"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
	"vault-envrc-generator/pkg/envrc"
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
	Path        string            `yaml:"path"`
	Prefix      string            `yaml:"prefix,omitempty"`
	ExcludeKeys []string          `yaml:"exclude_keys,omitempty"`
	IncludeKeys []string          `yaml:"include_keys,omitempty"`
	Transform   *bool             `yaml:"transform_keys,omitempty"`
	Template    string            `yaml:"template,omitempty"`
	Variables   map[string]string `yaml:"variables,omitempty"`
	Format      string            `yaml:"format,omitempty"` // optional override
	Output      string            `yaml:"output,omitempty"` // optional override
	EnvMap      map[string]string `yaml:"env_map,omitempty"` // explicit ENV_VAR -> source_key mapping
}

// BatchJob represents a single job in batch processing
type BatchJob struct {
	Name         string            `yaml:"name"`
	Description  string            `yaml:"description,omitempty"`
	Path         string            `yaml:"path,omitempty"`   // legacy single-path mode (templated)
	Output       string            `yaml:"output"`           // templated
	OutputMode   string            `yaml:"output_mode,omitempty"` // overwrite (default), append, merge
	Prefix       string            `yaml:"prefix,omitempty"`
	ExcludeKeys  []string          `yaml:"exclude_keys,omitempty"`
	IncludeKeys  []string          `yaml:"include_keys,omitempty"`
	Transform    *bool             `yaml:"transform_keys,omitempty"`
	Format       string            `yaml:"format,omitempty"`
	Template     string            `yaml:"template,omitempty"`
	Variables    map[string]string `yaml:"variables,omitempty"`
	Sections     []BatchSection    `yaml:"sections,omitempty"`
}

var (
	batchConfigFile string
	parallel        bool
	continueOnError bool
	batchBasePath   string
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
	batchCmd.Flags().BoolVar(&parallel, "parallel", false, "Run jobs in parallel")
	batchCmd.Flags().BoolVar(&continueOnError, "continue-on-error", false, "Continue processing if a job fails")
	batchCmd.Flags().StringVar(&batchBasePath, "base-path", "", "Base Vault path to prepend to relative section paths (overrides YAML base_path)")

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

	// Build template context from token
	tctx, err := buildTemplateContext(vaultClient)
	if err != nil {
		return fmt.Errorf("failed to build template context: %w", err)
	}

	// Determine basePath (CLI flag overrides YAML), and render templates
	basePath := strings.TrimSuffix(config.BasePath, "/")
	if v := viper.GetString("batch.base_path"); v != "" { basePath = strings.TrimSuffix(v, "/") }
	if basePath != "" {
		if bp, err := renderTemplateString(basePath, tctx); err == nil { basePath = strings.TrimSuffix(bp, "/") }
	}

	// Process jobs
	if parallel {
		return processBatchParallel(vaultClient, config.Jobs, tctx, basePath)
	} else {
		return processBatchSequential(vaultClient, config.Jobs, tctx, basePath)
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

func processBatchSequential(vaultClient *vault.Client, jobs []BatchJob, tctx TemplateContext, basePath string) error {
	var errors []error

	for i, job := range jobs {
		fmt.Printf("[%d/%d] Processing job: %s\n", i+1, len(jobs), job.Name)
		
		if err := processJob(vaultClient, job, tctx, basePath); err != nil {
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

func processBatchParallel(vaultClient *vault.Client, jobs []BatchJob, tctx TemplateContext, basePath string) error {
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
			err := processJob(vaultClient, j, tctx, basePath)
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

func processJob(vaultClient *vault.Client, job BatchJob, tctx TemplateContext, basePath string) error {
	// If sections are provided, iterate sections using job-level defaults
	if len(job.Sections) > 0 {
		for _, sec := range job.Sections {
			// Join base path and render templated section paths/outputs
			joinedPath := combineBaseAndPath(basePath, sec.Path)
			renderedSourcePath, err := renderTemplateString(joinedPath, tctx)
			if err != nil { return fmt.Errorf("failed to render section path '%s': %w", sec.Path, err) }
			outPath := job.Output
			if sec.Output != "" { outPath = sec.Output }
			renderedOutPath, err := renderTemplateString(outPath, tctx)
			if err != nil { return fmt.Errorf("failed to render section output '%s': %w", outPath, err) }
			format := job.Format
			if sec.Format != "" { format = sec.Format }

			secrets, err := vaultClient.GetSecrets(renderedSourcePath)
			if err != nil {
				return fmt.Errorf("failed to retrieve secrets from path %s: %w", renderedSourcePath, err)
			}

			// Apply variables: job-level then section-level (section overrides)
			if len(job.Variables) > 0 {
				for key, value := range job.Variables { secrets[key] = value }
			}
			if len(sec.Variables) > 0 {
				for key, value := range sec.Variables { secrets[key] = value }
			}

			// Options: section override or fallback to job
			prefix := job.Prefix
			if sec.Prefix != "" { prefix = sec.Prefix }
			exclude := job.ExcludeKeys
			if len(sec.ExcludeKeys) > 0 { exclude = sec.ExcludeKeys }
			include := job.IncludeKeys
			if len(sec.IncludeKeys) > 0 { include = sec.IncludeKeys }
			// effective transform: section overrides job; nil means not specified
			var transform bool
			if sec.Transform != nil {
				transform = *sec.Transform
			} else if job.Transform != nil {
				transform = *job.Transform
			} else {
				transform = false
			}
			templateFile := job.Template
			if sec.Template != "" { templateFile = sec.Template }

			// If env_map is provided, build explicit mapping and disable transform/prefix
			selected := secrets
			useEnvMap := len(sec.EnvMap) > 0
			if useEnvMap {
				mapped := make(map[string]interface{}, len(sec.EnvMap))
				for envName, srcKey := range sec.EnvMap {
					if v, ok := secrets[srcKey]; ok {
						mapped[envName] = v
					} else if viper.GetBool("verbose") {
						fmt.Fprintf(os.Stderr, "[batch] warning: %s missing key '%s'\n", renderedSourcePath, srcKey)
					}
				}
				selected = mapped
				// env_map uses explicit names; do not transform or prefix
				transform = false
				prefix = ""
				// ignore include/exclude when env_map is used
				exclude = nil
				include = nil
			}

			options := &envrc.Options{
				Prefix:        prefix,
				ExcludeKeys:   exclude,
				IncludeKeys:   include,
				TransformKeys: transform,
				Format:        format,
				TemplateFile:  templateFile,
				Verbose:       viper.GetBool("verbose"),
			}

			generator := envrc.NewGenerator(options)
			content, err := generator.Generate(selected)
			if err != nil {
				return fmt.Errorf("failed to generate content: %w", err)
			}

			// Add envrc header with job+section context and trailing newline
			if options.Format == "envrc" {
				header := fmt.Sprintf("# === %s", job.Name)
				if sec.Name != "" { header += fmt.Sprintf(": %s", sec.Name) }
				header += " ===\n"
				header += fmt.Sprintf("# Source path: %s\n", renderedSourcePath)
				if job.Description != "" { header += fmt.Sprintf("# Job: %s\n", job.Description) }
				if sec.Description != "" { header += fmt.Sprintf("# Section: %s\n", sec.Description) }
				header += "\n"
				content = header + content + "\n"
			}

			// Ensure output directory exists
			outputDir := filepath.Dir(renderedOutPath)
			if outputDir != "." {
				if err := os.MkdirAll(outputDir, 0755); err != nil {
					return fmt.Errorf("failed to create output directory %s: %w", outputDir, err)
				}
			}

			unlock := lockForPath(renderedOutPath)
			defer unlock()

			mode := job.OutputMode
			if mode == "" { mode = "overwrite" }

			switch mode {
			case "overwrite":
				if err := os.WriteFile(renderedOutPath, []byte(content), 0644); err != nil {
					return fmt.Errorf("failed to write output file %s: %w", renderedOutPath, err)
				}
			case "append":
				f, err := os.OpenFile(renderedOutPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
				if err != nil { return fmt.Errorf("failed to open output file %s: %w", renderedOutPath, err) }
				defer f.Close()
				if _, err := f.WriteString(content); err != nil { return fmt.Errorf("failed to append to %s: %w", renderedOutPath, err) }
			case "merge":
				// Only meaningful for json|yaml; envrc falls back to append
				switch options.Format {
				case "json":
					var existing map[string]interface{}
					if b, err := os.ReadFile(renderedOutPath); err == nil && len(b) > 0 {
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
					if err := os.WriteFile(renderedOutPath, buf, 0644); err != nil { return fmt.Errorf("failed to write output file %s: %w", renderedOutPath, err) }
				case "yaml":
					var existing map[string]interface{}
					if b, err := os.ReadFile(renderedOutPath); err == nil && len(b) > 0 {
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
					if err := os.WriteFile(renderedOutPath, buf, 0644); err != nil { return fmt.Errorf("failed to write output file %s: %w", renderedOutPath, err) }
				default:
					f, err := os.OpenFile(renderedOutPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
					if err != nil { return fmt.Errorf("failed to open output file %s: %w", renderedOutPath, err) }
					defer f.Close()
					if _, err := f.WriteString(content); err != nil { return fmt.Errorf("failed to append to %s: %w", renderedOutPath, err) }
				}
			default:
				return fmt.Errorf("unknown output_mode: %s", mode)
			}
		}
		return nil
	}

	// Legacy single-path job processing
	// Render templated job paths/outputs
	joinedJobPath := combineBaseAndPath(basePath, job.Path)
	renderedPath, err := renderTemplateString(joinedJobPath, tctx)
	if err != nil { return fmt.Errorf("failed to render job path '%s': %w", job.Path, err) }
	renderedOutput, err := renderTemplateString(job.Output, tctx)
	if err != nil { return fmt.Errorf("failed to render job output '%s': %w", job.Output, err) }

	// Retrieve secrets
	secrets, err := vaultClient.GetSecrets(renderedPath)
	if err != nil {
		return fmt.Errorf("failed to retrieve secrets from path %s: %w", renderedPath, err)
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
		TransformKeys: func() bool { if job.Transform != nil { return *job.Transform }; return false }(),
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

	// If envrc, add a section header with job metadata and a trailing newline
	if options.Format == "envrc" {
		header := fmt.Sprintf("# === %s ===\n# Source path: %s\n", job.Name, renderedPath)
		if job.Description != "" { header += fmt.Sprintf("# Description: %s\n", job.Description) }
		header += "\n"
		content = header + content + "\n"
	}

	// Ensure output directory exists
	outputDir := filepath.Dir(renderedOutput)
	if outputDir != "." {
		if err := os.MkdirAll(outputDir, 0755); err != nil {
			return fmt.Errorf("failed to create output directory %s: %w", outputDir, err)
		}
	}

	unlock := lockForPath(renderedOutput)
	defer unlock()

	mode := job.OutputMode
	if mode == "" { mode = "overwrite" }

	switch mode {
	case "overwrite":
		return os.WriteFile(renderedOutput, []byte(content), 0644)
	case "append":
		f, err := os.OpenFile(renderedOutput, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
		if err != nil { return fmt.Errorf("failed to open output file %s: %w", renderedOutput, err) }
		defer f.Close()
		if _, err := f.WriteString(content); err != nil { return fmt.Errorf("failed to append to %s: %w", renderedOutput, err) }
		return nil
	case "merge":
		// Only meaningful for json|yaml; envrc falls back to append
		switch options.Format {
		case "json":
			var existing map[string]interface{}
			if b, err := os.ReadFile(renderedOutput); err == nil && len(b) > 0 {
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
			return os.WriteFile(renderedOutput, buf, 0644)
		case "yaml":
			var existing map[string]interface{}
			if b, err := os.ReadFile(renderedOutput); err == nil && len(b) > 0 {
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
			return os.WriteFile(renderedOutput, buf, 0644)
		default:
			f, err := os.OpenFile(renderedOutput, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil { return fmt.Errorf("failed to open output file %s: %w", renderedOutput, err) }
			defer f.Close()
			if _, err := f.WriteString(content); err != nil { return fmt.Errorf("failed to append to %s: %w", renderedOutput, err) }
			return nil
		}
	default:
		return fmt.Errorf("unknown output_mode: %s", mode)
	}
}

func buildTemplateContext(vc *vault.Client) (TemplateContext, error) {
	client := vc.GetClient()
	tInfo, err := client.Auth().Token().LookupSelf()
	if err != nil {
		return TemplateContext{}, fmt.Errorf("token lookup failed: %w", err)
	}
	ctx := TemplateContext{Token: TokenContext{}}
	if tInfo != nil && tInfo.Data != nil {
		getStr := func(key string) string {
			if v, ok := tInfo.Data[key]; ok {
				if s, ok := v.(string); ok { return s }
			}
			return ""
		}
		ctx.Token.Accessor = getStr("accessor")
		ctx.Token.CreationTTL = getStr("creation_ttl")
		ctx.Token.DisplayName = getStr("display_name")
		ctx.Token.EntityID = getStr("entity_id")
		ctx.Token.ExpireTime = getStr("expire_time")
		ctx.Token.ID = getStr("id")
		ctx.Token.IssueTime = getStr("issue_time")
		ctx.Token.Path = getStr("path")
		ctx.Token.TTL = getStr("ttl")
		ctx.Token.Type = getStr("type")
		// Policies
		if pv, ok := tInfo.Data["policies"]; ok {
			if arr, ok := pv.([]interface{}); ok {
				for _, it := range arr {
					if s, ok := it.(string); ok { ctx.Token.Policies = append(ctx.Token.Policies, s) }
				}
			}
		}
		// Meta (flatten map[string]string)
		ctx.Token.Meta = map[string]string{}
		if mv, ok := tInfo.Data["meta"]; ok {
			if m, ok := mv.(map[string]interface{}); ok {
				for k, v := range m {
					if s, ok := v.(string); ok { ctx.Token.Meta[k] = s }
				}
			}
		}
		// Derive OIDCUserID from display_name like "oidc-123456"
		if strings.HasPrefix(ctx.Token.DisplayName, "oidc-") {
			re := regexp.MustCompile(`oidc-([0-9A-Za-z_-]+)`) 
			m := re.FindStringSubmatch(ctx.Token.DisplayName)
			if len(m) == 2 { ctx.Token.OIDCUserID = m[1] }
		}
	}
	return ctx, nil
}

func renderTemplateString(s string, tctx TemplateContext) (string, error) {
	// If no template markers, return as-is
	if !strings.Contains(s, "{{") {
		return s, nil
	}
	tmpl, err := template.New("path").Option("missingkey=error").Parse(s)
	if err != nil { return "", err }
	var buf bytes.Buffer
	if err := tmpl.Execute(&buf, tctx); err != nil { return "", err }
	return buf.String(), nil
}

func isVaultAbsolute(p string) bool {
	return strings.HasPrefix(p, "secrets/") || strings.HasPrefix(p, "secret/") || strings.HasPrefix(p, "auth/") || strings.HasPrefix(p, "sys/") || strings.HasPrefix(p, "transit/")
}

func combineBaseAndPath(basePath, p string) string {
	if basePath == "" || isVaultAbsolute(p) {
		return p
	}
	bp := strings.TrimSuffix(basePath, "/")
	pp := strings.TrimPrefix(p, "/")
	return bp + "/" + pp
}

