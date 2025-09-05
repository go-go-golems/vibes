package seed

import (
    "fmt"
    "os"
    "path/filepath"
    "strings"

    "github.com/rs/zerolog/log"
    "github.com/spf13/viper"
    "vault-envrc-generator/pkg/vault"
)

type Options struct{ DryRun bool }

func Run(client *vault.Client, spec *Spec, opts Options) error {
	// Build template context from token for rendering templated paths
	tctx, err := vault.BuildTemplateContext(client)
	if err != nil {
		return fmt.Errorf("failed to build template context: %w", err)
	}

	// Resolve base path: YAML has priority; render templates
	base := strings.TrimSuffix(spec.BasePath, "/")
	if base == "" {
		base = strings.TrimSuffix(viper.GetString("seed.base_path"), "/")
	}
	if base != "" {
		if bp, err := vault.RenderTemplateString(base, tctx); err == nil {
			base = strings.TrimSuffix(bp, "/")
		} else {
			return fmt.Errorf("failed to render base_path template: %w", err)
		}
	}

	for i, set := range spec.Sets {
		// Determine target path: join with base if relative; error if relative without base
		target := set.Path
		if !vault.IsAbsoluteVaultPath(target) && base == "" {
			return fmt.Errorf("set %d: relative path '%s' without base_path", i+1, target)
		}
		target = vault.JoinBaseAndPath(base, target)
		renderedTarget, err := vault.RenderTemplateString(target, tctx)
		if err != nil {
			return fmt.Errorf("set %d: failed to render path '%s': %w", i+1, target, err)
		}

		data := map[string]interface{}{}
		for k, v := range set.Data {
			data[k] = v
		}
		for k, envName := range set.Env {
			if val, ok := os.LookupEnv(envName); ok {
				data[k] = val
			}
		}
		for k, filePath := range set.Files {
			fp := filePath
			if strings.HasPrefix(fp, "~") {
				if home, err := os.UserHomeDir(); err == nil {
					fp = filepath.Join(home, strings.TrimPrefix(fp, "~"))
				}
			}
			if content, err := os.ReadFile(fp); err == nil {
				data[k] = string(content)
			} else {
				return fmt.Errorf("set %d: failed reading %s: %w", i+1, fp, err)
			}
		}

		if len(data) == 0 {
            log.Debug().Str("path", renderedTarget).Msg("seed: skipping (no data)")
            continue
        }

        if opts.DryRun { log.Debug().Str("path", renderedTarget).Interface("keys", keysOf(data)).Msg("seed: dry-run put"); continue }

		if err := client.PutSecrets(renderedTarget, data); err != nil {
			return fmt.Errorf("failed to write %s: %w", renderedTarget, err)
		}
        log.Info().Str("path", renderedTarget).Int("keys", len(data)).Msg("seed: wrote secrets")
    }
    return nil
}

func keysOf(m map[string]interface{}) []string {
	ks := make([]string, 0, len(m))
	for k := range m {
		ks = append(ks, k)
	}
	return ks
}
