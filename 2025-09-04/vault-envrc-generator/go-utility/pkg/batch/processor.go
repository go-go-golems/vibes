package batch

import (
    "fmt"
    "os"
    "strings"
    "sort"

    "encoding/json"
    "gopkg.in/yaml.v3"
    "github.com/rs/zerolog/log"
    "vault-envrc-generator/pkg/envrc"
    "vault-envrc-generator/pkg/output"
    "vault-envrc-generator/pkg/vault"
)

type Processor struct {
    Client  *vault.Client
}

type ProcessorOptions struct {
    BasePath           string
    OutputOverride     string
    FormatOverride     string
    ContinueOnError    bool
    DryRun             bool
    SortKeys           bool
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
        log.Debug().Int("sections", len(job.Sections)).Str("job", job.Name).Msg("batch job start")
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
    log.Debug().Str("job", job.Name).Int("sections", len(job.Sections)).Msg("process job")
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
    log.Debug().Str("job", job.Name).Str("effectiveBase", effectiveBase).Msg("job base path")

    if len(job.Sections) > 0 {
		// stdout aggregations (only used when output == "-")
		var stdoutJSONAgg map[string]interface{}
		var stdoutYAMLAgg map[string]interface{}
		var stdoutENVRCAgg strings.Builder

        for _, sec := range job.Sections {
        log.Debug().Str("section", sec.Name).Msg("section start")
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
            if opts.DryRun { renderedOutPath = "-" }
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

        log.Debug().Str("section", sec.Name).Str("source", renderedSourcePath).Str("output", renderedOutPath).Str("format", format).Msg("section io")


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
                log.Debug().Int("keys", len(s)).Str("source", renderedSourcePath).Msg("fetched secrets")
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
        } else {
            log.Debug().Str("source", renderedSourcePath).Str("key", srcKey).Msg("missing key in env_map")
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
                Verbose:        false,
                SuppressHeader: suppressHeader,
                SortKeys:       opts.SortKeys,
            }

			generator := envrc.NewGenerator(options)
			content, err := generator.Generate(selected)
			if err != nil {
				return fmt.Errorf("failed to generate content: %w", err)
			}
        log.Debug().Int("bytes", len(content)).Str("section", sec.Name).Msg("generated content")

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

			// stdout vs file handling: aggregate to stdout, otherwise write immediately (merge-only)
			if renderedOutPath == "-" {
				switch format {
				case "json":
					var next map[string]interface{}
					if err := json.Unmarshal([]byte(content), &next); err != nil { return fmt.Errorf("failed to parse generated JSON for aggregation: %w", err) }
					if stdoutJSONAgg == nil { stdoutJSONAgg = map[string]interface{}{} }
					for k, v := range next { stdoutJSONAgg[k] = v }
				case "yaml":
					var next map[string]interface{}
					if err := yaml.Unmarshal([]byte(content), &next); err != nil { return fmt.Errorf("failed to parse generated YAML for aggregation: %w", err) }
					if stdoutYAMLAgg == nil { stdoutYAMLAgg = map[string]interface{}{} }
					for k, v := range next { stdoutYAMLAgg[k] = v }
				default:
					stdoutENVRCAgg.WriteString(content)
				}
			} else {
				if err := output.Write(renderedOutPath, []byte(content), output.WriteOptions{Format: format, SortKeys: opts.SortKeys}); err != nil {
					return err
				}
			}
		}

        // flush outputs to stdout
        if stdoutJSONAgg != nil {
            if opts.SortKeys {
                keys := make([]string, 0, len(stdoutJSONAgg))
                for k := range stdoutJSONAgg { keys = append(keys, k) }
                sort.Strings(keys)
                var bld strings.Builder
                bld.WriteString("{\n")
                for i, k := range keys {
                    kb, _ := json.Marshal(k)
                    vb, err := json.Marshal(stdoutJSONAgg[k]); if err != nil { return fmt.Errorf("failed to marshal aggregated JSON value: %w", err) }
                    if i > 0 { bld.WriteString(",\n") }
                    bld.WriteString("  ")
                    bld.Write(kb)
                    bld.WriteString(": ")
                    bld.Write(vb)
                }
                bld.WriteString("\n}")
                fmt.Print(bld.String())
            } else {
                b, err := json.MarshalIndent(stdoutJSONAgg, "", "  ")
                if err != nil { return fmt.Errorf("failed to marshal aggregated JSON: %w", err) }
                fmt.Print(string(b))
            }
        }
        if stdoutYAMLAgg != nil {
            if len(stdoutYAMLAgg) > 0 {
                if opts.SortKeys {
                    keys := make([]string, 0, len(stdoutYAMLAgg))
                    for k := range stdoutYAMLAgg { keys = append(keys, k) }
                    sort.Strings(keys)
                    node := &yaml.Node{Kind: yaml.MappingNode}
                    for _, k := range keys {
                        kn := &yaml.Node{Kind: yaml.ScalarNode, Tag: "!!str", Value: k}
                        var valueDoc yaml.Node
                        b, err := yaml.Marshal(stdoutYAMLAgg[k]); if err != nil { return fmt.Errorf("failed to marshal yaml value: %w", err) }
                        if err := yaml.Unmarshal(b, &valueDoc); err != nil { return fmt.Errorf("failed to unmarshal yaml value node: %w", err) }
                        var vn *yaml.Node
                        if len(valueDoc.Content) == 0 { vn = &yaml.Node{Kind: yaml.ScalarNode, Tag: "!!null", Value: "~"} } else { vn = valueDoc.Content[0] }
                        node.Content = append(node.Content, kn, vn)
                    }
                    b, err := yaml.Marshal(node); if err != nil { return fmt.Errorf("failed to marshal ordered YAML: %w", err) }
                    fmt.Print(string(b))
                } else {
                    b, err := yaml.Marshal(stdoutYAMLAgg)
                    if err != nil { return fmt.Errorf("failed to marshal aggregated YAML: %w", err) }
                    fmt.Print(string(b))
                }
            }
        }
        if stdoutENVRCAgg.Len() > 0 {
            fmt.Print(stdoutENVRCAgg.String())
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
        Verbose:       false,
        SuppressHeader: false,
        SortKeys:       opts.SortKeys,
    }
	if opts.FormatOverride != "" { options.Format = opts.FormatOverride }
	if options.Format == "" { options.Format = "envrc" }

    // For envrc, suppress header when appending to existing file
    if options.Format == "envrc" && renderedOutput != "-" {
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

    log.Debug().Str("output", renderedOutput).Msg("writing job output")
    if opts.DryRun { renderedOutput = "-" }
    return output.Write(renderedOutput, []byte(content), output.WriteOptions{Format: options.Format, SortKeys: opts.SortKeys})
}
