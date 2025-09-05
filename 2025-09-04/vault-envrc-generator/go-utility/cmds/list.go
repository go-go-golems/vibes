package cmds

import (
    "context"
    "fmt"
    "os"
    "sort"
    "strings"
    "time"

    glzcli "github.com/go-go-golems/glazed/pkg/cli"
    gcmds "github.com/go-go-golems/glazed/pkg/cmds"
    "github.com/go-go-golems/glazed/pkg/cmds/layers"
    glayers "github.com/go-go-golems/glazed/pkg/cmds/layers"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
    "github.com/go-go-golems/glazed/pkg/middlewares"
    "github.com/go-go-golems/glazed/pkg/settings"
    "github.com/go-go-golems/glazed/pkg/types"

    "vault-envrc-generator/pkg/listing"
    "vault-envrc-generator/pkg/vault"
    "vault-envrc-generator/pkg/vaultlayer"
)

type ListCommand struct { *gcmds.CommandDescription }

type ListSettings struct {
    Path          string `glazed.parameter:"path"`
    Depth         int    `glazed.parameter:"depth"`
    Prefix        string `glazed.parameter:"prefix"`
    Format        string `glazed.parameter:"format"`
    IncludeValues bool   `glazed.parameter:"include-values"`
    Censor        string `glazed.parameter:"censor"`
}

func NewListCommand() (*ListCommand, error) {
    // Glazed output layers for structured output
    glazedLayers, err := settings.NewGlazedParameterLayers()
    if err != nil { return nil, err }
    commandLayer, err := glzcli.NewCommandSettingsLayer()
    if err != nil { return nil, err }
    cd := gcmds.NewCommandDescription(
        "list",
        gcmds.WithShort("List accessible secrets and directories under a Vault path"),
        gcmds.WithFlags(
            parameters.NewParameterDefinition("path", parameters.ParameterTypeString, parameters.WithRequired(true), parameters.WithShortFlag("p"), parameters.WithHelp("Vault path to list")),
            parameters.NewParameterDefinition("depth", parameters.ParameterTypeInteger, parameters.WithDefault(1), parameters.WithHelp("Depth to recurse (0 = unlimited)")),
            parameters.NewParameterDefinition("prefix", parameters.ParameterTypeString, parameters.WithHelp("Only show entries starting with this prefix")),
            parameters.NewParameterDefinition("format", parameters.ParameterTypeChoice, parameters.WithChoices("yaml","text"), parameters.WithDefault("yaml"), parameters.WithHelp("Output format")),
            parameters.NewParameterDefinition("include-values", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Include censored values (yaml only)")),
            parameters.NewParameterDefinition("censor", parameters.ParameterTypeString, parameters.WithDefault("****"), parameters.WithHelp("String used for censored values")),
        ),
        gcmds.WithLayersList(glazedLayers, commandLayer),
    )
    _, err = vaultlayer.AddVaultLayerToCommand(cd)
    if err != nil { return nil, err }
    return &ListCommand{cd}, nil
}

// GlazeCommand: output structured rows
func (c *ListCommand) RunIntoGlazeProcessor(ctx context.Context, parsed *glayers.ParsedLayers, gp middlewares.Processor) error {
    s := &ListSettings{}
    if err := parsed.InitializeStruct(layers.DefaultSlug, s); err != nil { return err }
    vs, err := vaultlayer.GetVaultSettings(parsed)
    if err != nil { return err }

    ctx2, cancel := context.WithTimeout(ctx, 15*time.Second)
    defer cancel()
    token, err := vault.ResolveToken(ctx2, vs.VaultToken, vault.TokenSource(vs.VaultTokenSource), vs.VaultTokenFile, false)
    if err != nil { return fmt.Errorf("failed to resolve Vault token: %w", err) }
    client, err := vault.NewClient(vs.VaultAddr, token)
    if err != nil { return fmt.Errorf("failed to create Vault client: %w", err) }

    entries, warns := listing.Walk(client, s.Path, s.Depth)
    for _, e := range entries {
        if s.Prefix != "" && !strings.HasPrefix(e, s.Prefix) { continue }
        if strings.HasSuffix(e, "/") {
            childKeys, err := client.ListSecrets(e)
            if err != nil { childKeys = []string{} }
            row := types.NewRow(
                types.MRP("path", e),
                types.MRP("type", "directory"),
                types.MRP("children", childKeys),
            )
            if err := gp.AddRow(ctx, row); err != nil { return err }
        } else {
            data, err := client.GetSecrets(e)
            if err != nil { data = map[string]interface{}{} }
            if s.IncludeValues {
                m := make(map[string]string, len(data))
                for k := range data { m[k] = s.Censor }
                row := types.NewRow(
                    types.MRP("path", e),
                    types.MRP("type", "secret"),
                    types.MRP("data", m),
                )
                if err := gp.AddRow(ctx, row); err != nil { return err }
            } else {
                ks := make([]string, 0, len(data))
                for k := range data { ks = append(ks, k) }
                sort.Strings(ks)
                row := types.NewRow(
                    types.MRP("path", e),
                    types.MRP("type", "secret"),
                    types.MRP("keys", ks),
                )
                if err := gp.AddRow(ctx, row); err != nil { return err }
            }
        }
    }
    if len(warns) > 0 {
        fmt.Fprintf(os.Stderr, "Warnings (%d) encountered during listing.\n", len(warns))
        for _, w := range warns {
            msg := w.Error()
            fmt.Fprintf(os.Stderr, "- %s\n", msg)
            // Try to extract path before first ': '
            path := ""
            parts := strings.SplitN(msg, ": ", 2)
            if len(parts) == 2 { path = parts[0] }
            row := types.NewRow(
                types.MRP("type", "warning"),
                types.MRP("path", path),
                types.MRP("error", msg),
            )
            if err := gp.AddRow(ctx, row); err != nil { return err }
        }
    }
    return nil
}

var _ gcmds.GlazeCommand = &ListCommand{}
