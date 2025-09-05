package batch

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
	"encoding/json"
	"vault-envrc-generator/pkg/envrc"
	"vault-envrc-generator/pkg/output"
	"vault-envrc-generator/pkg/vault"
)

type Processor struct {
	Client  *vault.Client
	Verbose bool
}

type ProcessorOptions struct {
	BasePath           string
	OutputOverride     string
	OutputModeOverride string
	FormatOverride     string
	ContinueOnError    bool
}

func (p *Processor) Process(cfg *Config, opts ProcessorOptions) error {
	// build template context
	tctx, err := vault.BuildTemplateContext(p.Client)
	if err != nil {
		return fmt.Errorf("failed to build template context: %w", err)
	}

	// determine base path (opts overrides YAML)
	basePath := strings.TrimSuffix(cfg.BasePath, "/")
	if opts.BasePath != "" {
		basePath = strings.TrimSuffix(opts.BasePath, "/")
	}
	if basePath != "" {
		if bp, err := vault.RenderTemplateString(basePath, tctx); err == nil {
			basePath = strings.TrimSuffix(bp, "/")
		}
	}

	return p.processSequential(cfg.Jobs, tctx, basePath, opts)
}

func (p *Processor) processSequential(jobs []Job, tctx vault.TemplateContext, basePath string, opts ProcessorOptions) error {
	var errors []error
	for i, job := range jobs {
		fmt.Printf("[%d/%d] Processing job: %s\n", i+1, len(jobs), job.Name)
		if p.Verbose {
			fmt.Fprintf(os.Stderr, "[batch] job '%s': %d sections\n", job.Name, len(job.Sections))
		}
		if err := p.processJob(job, tctx, basePath, opts); err != nil {
			fmt.Fprintf(os.Stderr, "Job '%s' failed: %v\n", job.Name, err)
			errors = append(errors, err)
			if !opts.ContinueOnError {
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

func (p *Processor) processParallel(jobs []Job, tctx vault.TemplateContext, basePath string, opts ProcessorOptions) error {
	// Parallel processing removed for simplicity and to avoid lock contention.
	return p.processSequential(jobs, tctx, basePath, opts)
}

func (p *Processor) processJob(job Job, tctx vault.TemplateContext, basePath string, opts ProcessorOptions) error {
	if p.Verbose {
		fmt.Fprintf(os.Stderr, "[batch] job '%s': start (sections=%d)\n", job.Name, len(job.Sections))
	}
	// job-level base path override
	effectiveBase := basePath
	if strings.TrimSpace(job.BasePath) != "" {
		effectiveBase = strings.TrimSuffix(job.BasePath, "/")
		if rbp, err := vault.RenderTemplateString(effectiveBase, tctx); err == nil {
			effectiveBase = strings.TrimSuffix(rbp, "/")
		} else {
			return fmt.Errorf("failed to render job base_path '%s': %w", job.BasePath, err)
		}
	}
	if p.Verbose {
		fmt.Fprintf(os.Stderr, "[batch] job '%s': effectiveBase='%s'\n", job.Name, effectiveBase)
	}

	if len(job.Sections) > 0 {
		// stdout aggregations
		var stdoutJSONAgg map[string]interface{}
		var stdoutYAMLAgg map[string]interface{}
		var stdoutYAMLDocs []string

		for _, sec := range job.Sections {
			if p.Verbose {
				fmt.Fprintf(os.Stderr, "[batch] section '%s': start\n", sec.Name)
			}
			joinedPath := vault.JoinBaseAndPath(effectiveBase, sec.Path)
			renderedSourcePath, err := vault.RenderTemplateString(joinedPath, tctx)
			if err != nil {
				return fmt.Errorf("failed to render section path '%s': %w", sec.Path, err)
			}
			outPath := job.Output
			if sec.Output != "" {
				outPath = sec.Output
			}
			if opts.OutputOverride != "" {
				outPath = opts.OutputOverride
			}
			renderedOutPath, err := vault.RenderTemplateString(outPath, tctx)
			if err != nil {
				return fmt.Errorf("failed to render section output '%s': %w", outPath, err)
			}
			format := job.Format
			if sec.Format != "" {
				format = sec.Format
			}
			if opts.FormatOverride != "" {
				format = opts.FormatOverride
			}
			if format == "" {
				format = "envrc"
			}

			if p.Verbose {
				fmt.Fprintf(os.Stderr, "[batch] section '%s': source='%s' output='%s' format='%s'\n", sec.Name, renderedSourcePath, renderedOutPath, format)
			}

			mode := job.OutputMode
			if opts.OutputModeOverride != "" {
				mode = opts.OutputModeOverride
			}
			if mode == "" {
				mode = "overwrite"
			}

			// secrets
			secrets := map[string]interface{}{}
			if strings.TrimSpace(renderedSourcePath) != "" {
				s, err := p.Client.GetSecrets(renderedSourcePath)
				if err != nil {
					return fmt.Errorf("failed to retrieve secrets from path %s: %w", renderedSourcePath, err)
				}
				for k, v := range s {
					secrets[k] = v
				}
				if p.Verbose {
					fmt.Fprintf(os.Stderr, "[batch] fetched %d keys from '%s'\n", len(s), renderedSourcePath)
				}
			}

			// fixed values
			if len(job.Fixed) > 0 {
				for k, tv := range job.Fixed {
					rv, err := vault.RenderTemplateString(tv, tctx)
					if err != nil {
						return fmt.Errorf("failed to render job fixed '%s': %w", k, err)
					}
					secrets[k] = rv
				}
			}
			if len(sec.Fixed) > 0 {
				for k, tv := range sec.Fixed {
					rv, err := vault.RenderTemplateString(tv, tctx)
					if err != nil {
						return fmt.Errorf("failed to render section fixed '%s': %w", k, err)
					}
					secrets[k] = rv
				}
			}

			// variables
			if len(job.Variables) > 0 {
				for key, value := range job.Variables {
					secrets[key] = value
				}
			}
			if len(sec.Variables) > 0 {
				for key, value := range sec.Variables {
					secrets[key] = value
				}
			}

			// options
			prefix := job.Prefix
			if sec.Prefix != "" {
				prefix = sec.Prefix
			}
			exclude := job.ExcludeKeys
			if len(sec.ExcludeKeys) > 0 {
				exclude = sec.ExcludeKeys
			}
			include := job.IncludeKeys
			if len(sec.IncludeKeys) > 0 {
				include = sec.IncludeKeys
			}
			var transform bool
			if sec.Transform != nil {
				transform = *sec.Transform
			} else if job.Transform != nil {
				transform = *job.Transform
			} else {
				transform = false
			}
			templateFile := job.Template
			if sec.Template != "" {
				templateFile = sec.Template
			}

			// env_map explicit mapping
			selected := secrets
			if len(sec.EnvMap) > 0 {
				mapped := make(map[string]interface{}, len(sec.EnvMap))
				for envName, srcKey := range sec.EnvMap {
					if v, ok := secrets[srcKey]; ok {
						mapped[envName] = v
					} else if p.Verbose {
						fmt.Fprintf(os.Stderr, "[batch] warning: %s missing key '%s'\n", renderedSourcePath, srcKey)
					}
				}
				selected = mapped
				transform = false
				prefix = ""
				exclude = nil
				include = nil
			}

			suppressHeader := false
			if format == "envrc" {
				// With aggregation, suppress generic header; per-section header added below
				suppressHeader = true
			}

			options := &envrc.Options{
				Prefix:         prefix,
				ExcludeKeys:    exclude,
				IncludeKeys:    include,
				TransformKeys:  transform,
				Format:         format,
				TemplateFile:   templateFile,
				Verbose:        viper.GetBool("verbose"),
				SuppressHeader: suppressHeader,
			}

			generator := envrc.NewGenerator(options)
			content, err := generator.Generate(selected)
			if err != nil {
				return fmt.Errorf("failed to generate content: %w", err)
			}
			if p.Verbose {
				fmt.Fprintf(os.Stderr, "[batch] generated %d bytes for section '%s'\n", len(content), sec.Name)
			}

			if options.Format == "envrc" {
				header := fmt.Sprintf("# === %s", job.Name)
				if sec.Name != "" {
					header += fmt.Sprintf(": %s", sec.Name)
				}
				header += " ===\n"
				header += fmt.Sprintf("# Source path: %s\n", renderedSourcePath)
				if job.Description != "" {
					header += fmt.Sprintf("# Job: %s\n", job.Description)
				}
				if sec.Description != "" {
					header += fmt.Sprintf("# Section: %s\n", sec.Description)
				}
				header += "\n"
				content = header + content + "\n"
			}

			// get aggregator
			switch format {
			case "json":
				var next map[string]interface{}
				if err := json.Unmarshal([]byte(content), &next); err != nil { return fmt.Errorf("failed to parse generated JSON for aggregation: %w", err) }
				if stdoutJSONAgg == nil { stdoutJSONAgg = map[string]interface{}{} }
				for k, v := range next { stdoutJSONAgg[k] = v }
			case "yaml":
				if mode == "merge" {
					var next map[string]interface{}
					if err := yaml.Unmarshal([]byte(content), &next); err != nil { return fmt.Errorf("failed to parse generated YAML for aggregation: %w", err) }
					if stdoutYAMLAgg == nil { stdoutYAMLAgg = map[string]interface{}{} }
					for k, v := range next { stdoutYAMLAgg[k] = v }
				} else {
					stdoutYAMLDocs = append(stdoutYAMLDocs, content)
				}
			default:
				// This case should ideally not be reached for stdout aggregation
				// but as a fallback, we can accumulate to a strings.Builder
				// For now, we'll just print the content directly if not json/yaml
				fmt.Print(content)
			}
		}

		// flush outputs
		if stdoutJSONAgg != nil {
			b, err := json.MarshalIndent(stdoutJSONAgg, "", "  ")
			if err != nil { return fmt.Errorf("failed to marshal aggregated JSON: %w", err) }
			fmt.Print(string(b))
		}
		if stdoutYAMLAgg != nil {
			if len(stdoutYAMLAgg) > 0 {
				b, err := yaml.Marshal(stdoutYAMLAgg)
				if err != nil { return fmt.Errorf("failed to marshal aggregated YAML: %w", err) }
				fmt.Print(string(b))
			} else if len(stdoutYAMLDocs) > 0 {
				var sb strings.Builder
				for i, doc := range stdoutYAMLDocs {
					if i > 0 { sb.WriteString("---\n") }
					sb.WriteString(doc)
				}
				fmt.Print(sb.String())
			}
		}
		return nil
	}

	// legacy single-path mode
	joinedJobPath := vault.JoinBaseAndPath(effectiveBase, job.Path)
	renderedPath, err := vault.RenderTemplateString(joinedJobPath, tctx)
	if err != nil {
		return fmt.Errorf("failed to render job path '%s': %w", job.Path, err)
	}
	outPath := job.Output
	if opts.OutputOverride != "" {
		outPath = opts.OutputOverride
	}
	renderedOutput, err := vault.RenderTemplateString(outPath, tctx)
	if err != nil {
		return fmt.Errorf("failed to render job output '%s': %w", outPath, err)
	}

	secrets, err := p.Client.GetSecrets(renderedPath)
	if err != nil {
		return fmt.Errorf("failed to retrieve secrets from path %s: %w", renderedPath, err)
	}

	if len(job.Fixed) > 0 {
		for k, tv := range job.Fixed {
			rv, err := vault.RenderTemplateString(tv, tctx)
			if err != nil {
				return fmt.Errorf("failed to render job fixed '%s': %w", k, err)
			}
			secrets[k] = rv
		}
	}
	if len(job.Variables) > 0 {
		for k, v := range job.Variables {
			secrets[k] = v
		}
	}

	options := &envrc.Options{
		Prefix:        job.Prefix,
		ExcludeKeys:   job.ExcludeKeys,
		IncludeKeys:   job.IncludeKeys,
		TransformKeys: func() bool { if job.Transform != nil { return *job.Transform }; return false }(),
		Format:        job.Format,
		TemplateFile:  job.Template,
		Verbose:       viper.GetBool("verbose"),
		SuppressHeader: false,
	}
	if opts.FormatOverride != "" { options.Format = opts.FormatOverride }
	if options.Format == "" { options.Format = "envrc" }

	mode := job.OutputMode
	if opts.OutputModeOverride != "" { mode = opts.OutputModeOverride }
	if mode == "" { mode = "overwrite" }
	if options.Format == "envrc" && renderedOutput != "-" && mode != "overwrite" {
		if fi, err := os.Stat(renderedOutput); err == nil && fi.Size() > 0 { options.SuppressHeader = true }
	}

	generator := envrc.NewGenerator(options)
	content, err := generator.Generate(secrets)
	if err != nil { return fmt.Errorf("failed to generate content: %w", err) }
	if options.Format == "envrc" {
		header := fmt.Sprintf("# === %s ===\n# Source path: %s\n", job.Name, renderedPath)
		if job.Description != "" { header += fmt.Sprintf("# Description: %s\n", job.Description) }
		header += "\n"
		content = header + content + "\n"
	}

	if p.Verbose { fmt.Fprintf(os.Stderr, "[batch] writing job output to '%s' (mode=%s)\n", renderedOutput, mode) }
	return output.Write(renderedOutput, []byte(content), output.WriteOptions{Mode: output.OutputMode(mode), Format: options.Format, Verbose: p.Verbose})
}
