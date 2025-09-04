package cmd

import (
    "context"
    "fmt"
    "sort"
    "strings"
    "time"

    "gopkg.in/yaml.v3"
    "github.com/spf13/cobra"
    "github.com/spf13/viper"
    "vault-envrc-generator/pkg/vault"
)

var (
    listPath   string
    listDepth  int
    listPrefix string
    listFormat string
    includeValues bool
    censorString string
)

// listCmd walks a Vault path and prints accessible keys/directories
var listCmd = &cobra.Command{
    Use:   "list",
    Short: "List accessible secrets under a Vault path",
    Long: `List accessible secrets and subpaths under a given Vault path.

This command attempts KV v2-aware listing and gracefully handles permission errors.

Examples:
  vault-envrc-generator list --path secrets/environments/development/
  vault-envrc-generator list -p secrets/environments/staging/ --depth 2
  vault-envrc-generator list -p secrets/environments/development/services/identity-server`,
    RunE: runList,
}

func init() {
    rootCmd.AddCommand(listCmd)
    listCmd.Flags().StringVarP(&listPath, "path", "p", "", "Vault path to list (required)")
    listCmd.Flags().IntVar(&listDepth, "depth", 1, "Depth to recurse when walking subpaths (0 = unlimited)")
    listCmd.Flags().StringVar(&listPrefix, "prefix", "", "Only show entries starting with this prefix")
    listCmd.Flags().StringVar(&listFormat, "format", "yaml", "Output format: yaml or text")
    listCmd.Flags().BoolVar(&includeValues, "include-values", false, "Include values (censored) instead of only keys")
    listCmd.Flags().StringVar(&censorString, "censor", "****", "String to use for censored values when include-values is set")
    listCmd.MarkFlagRequired("path")
}

func runList(cmd *cobra.Command, args []string) error {
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

    fmt.Printf("Listing: %s\n", listPath)
    entries, errs := walkVault(client, normalizePath(listPath), listDepth)

    sort.Strings(entries)

    if listFormat == "text" {
        for _, e := range entries {
            if listPrefix == "" || strings.HasPrefix(e, listPrefix) {
                fmt.Println(e)
            }
        }
    } else {
        // YAML output with keys/children
        result := struct {
            Paths []ListEntry `yaml:"paths"`
        }{Paths: []ListEntry{}}

        for _, e := range entries {
            if listPrefix != "" && !strings.HasPrefix(e, listPrefix) {
                continue
            }

            if strings.HasSuffix(e, "/") {
                // Directory: include immediate children names
                childKeys, err := client.ListSecrets(e)
                if err != nil {
                    if viper.GetBool("verbose") { errs = append(errs, fmt.Errorf("%s: %w", e, err)) }
                    result.Paths = append(result.Paths, ListEntry{Path: e, Type: "directory", Children: []string{}})
                    continue
                }
                result.Paths = append(result.Paths, ListEntry{Path: e, Type: "directory", Children: childKeys})
            } else {
                // Secret: fetch keys, optional censored values
                data, err := client.GetSecrets(e)
                if err != nil {
                    if viper.GetBool("verbose") { errs = append(errs, fmt.Errorf("%s: %w", e, err)) }
                    result.Paths = append(result.Paths, ListEntry{Path: e, Type: "secret", Keys: []string{}})
                    continue
                }
                if includeValues {
                    m := make(map[string]string, len(data))
                    for k := range data {
                        m[k] = censorString
                    }
                    result.Paths = append(result.Paths, ListEntry{Path: e, Type: "secret", Data: m})
                } else {
                    keys := make([]string, 0, len(data))
                    for k := range data { keys = append(keys, k) }
                    sort.Strings(keys)
                    result.Paths = append(result.Paths, ListEntry{Path: e, Type: "secret", Keys: keys})
                }
            }
        }

        out, err := yaml.Marshal(result)
        if err != nil {
            return fmt.Errorf("failed to marshal YAML: %w", err)
        }
        fmt.Print(string(out))
    }

    if len(errs) > 0 && viper.GetBool("verbose") {
        fmt.Fprintf(cmd.ErrOrStderr(), "\nWarnings (%d):\n", len(errs))
        for _, e := range errs {
            fmt.Fprintf(cmd.ErrOrStderr(), "- %v\n", e)
        }
    }
    return nil
}

func normalizePath(p string) string {
    p = strings.TrimSpace(p)
    if p == "" {
        return p
    }
    if !strings.HasSuffix(p, "/") {
        return p + "/"
    }
    return p
}

// walkVault recursively lists keys and subdirectories up to depth
func walkVault(client *vault.Client, path string, depth int) ([]string, []error) {
    var results []string
    var errs []error

    keys, err := client.ListSecrets(path)
    if err != nil {
        errs = append(errs, fmt.Errorf("%s: %w", path, err))
        return results, errs
    }

    for _, k := range keys {
        full := path + k
        results = append(results, full)
        if strings.HasSuffix(k, "/") {
            if depth == 1 {
                continue
            }
            nextDepth := depth
            if nextDepth > 0 {
                nextDepth = depth - 1
            }
            subResults, subErrs := walkVault(client, full, nextDepth)
            results = append(results, subResults...)
            errs = append(errs, subErrs...)
        }
    }
    return results, errs
}


