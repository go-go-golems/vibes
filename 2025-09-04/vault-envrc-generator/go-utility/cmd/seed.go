package cmd

import (
    "context"
    "fmt"
    "os"
    "path/filepath"
    "strings"
    "time"

    "github.com/spf13/cobra"
    "github.com/spf13/viper"
    "gopkg.in/yaml.v3"
    "vault-envrc-generator/pkg/vault"
)

type SeedSpec struct {
    BasePath string       `yaml:"base_path"`
    Sets     []SeedSet    `yaml:"sets"`
}

type SeedSet struct {
    Path string                   `yaml:"path"`        // relative to BasePath or absolute Vault path
    Data map[string]string        `yaml:"data"`        // key: value (literal)
    Env  map[string]string        `yaml:"env"`         // key: ENV_VAR (read from env)
    Files map[string]string       `yaml:"files"`       // key: /path/to/file (read content)
}

var (
    seedConfig string
    seedDryRun bool
)

var seedCmd = &cobra.Command{
    Use:   "seed",
    Short: "Seed Vault secrets from a YAML spec and environment",
    Long: `Seed KV secrets into Vault using a YAML specification.

YAML format:

base_path: secrets/environments/personal/102454784610416055110/manuel
sets:
  - path: core
    data:
      VAULT_ADDR: https://vault.mento.co/
    env:
      OP_ACCOUNT: OP_ACCOUNT
      OP_VAULT: OP_VAULT
  - path: google
    env:
      client_email: GOOGLE_EMAIL
    files:
      private_key: /path/to/key.pem

Paths in sets are joined to base_path when not absolute.`,
    RunE: runSeed,
}

func init() {
    rootCmd.AddCommand(seedCmd)
    seedCmd.Flags().StringVarP(&seedConfig, "config", "c", "", "Seed YAML file (required)")
    seedCmd.Flags().BoolVar(&seedDryRun, "dry-run", false, "Show actions without writing to Vault")
    seedCmd.MarkFlagRequired("config")
}

func runSeed(cmd *cobra.Command, args []string) error {
    b, err := os.ReadFile(seedConfig)
    if err != nil {
        return fmt.Errorf("failed to read seed config: %w", err)
    }

    var spec SeedSpec
    if err := yaml.Unmarshal(b, &spec); err != nil {
        return fmt.Errorf("failed to parse YAML: %w", err)
    }

    ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
    defer cancel()
    token, err := vault.ResolveToken(
        ctx,
        viper.GetString("vault.token"),
        vault.TokenSource(viper.GetString("vault.token_source")),
        viper.GetString("vault.token_file"),
        viper.GetBool("verbose"),
    )
    if err != nil {
        return fmt.Errorf("failed to resolve Vault token: %w", err)
    }
    client, err := vault.NewClient(viper.GetString("vault.addr"), token)
    if err != nil {
        return fmt.Errorf("failed to create Vault client: %w", err)
    }

    base := strings.TrimSuffix(spec.BasePath, "/")
    if base == "" {
        base = strings.TrimSuffix(viper.GetString("seed.base_path"), "/")
    }

    for i, set := range spec.Sets {
        target := set.Path
        if !strings.HasPrefix(target, "secrets/") {
            if base == "" {
                return fmt.Errorf("set %d: relative path '%s' without base_path", i+1, target)
            }
            target = base + "/" + strings.TrimPrefix(target, "/")
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
            if viper.GetBool("verbose") {
                fmt.Fprintf(os.Stderr, "[seed] skipping %s (no data)\n", target)
            }
            continue
        }

        if seedDryRun {
            fmt.Printf("[seed] DRY-RUN put %s keys=%v\n", target, keysOf(data))
            continue
        }

        if err := client.PutSecrets(target, data); err != nil {
            return fmt.Errorf("failed to write %s: %w", target, err)
        }
        fmt.Printf("[seed] wrote %s (%d keys)\n", target, len(data))
    }
    return nil
}

func keysOf(m map[string]interface{}) []string {
    ks := make([]string, 0, len(m))
    for k := range m { ks = append(ks, k) }
    return ks
}


