package cmds

import (
    "bufio"
    "context"
    "fmt"
    "os"
    "sort"
    "strings"
    "time"

    glzcli "github.com/go-go-golems/glazed/pkg/cli"
    gcmds "github.com/go-go-golems/glazed/pkg/cmds"
    glayers "github.com/go-go-golems/glazed/pkg/cmds/layers"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"

    "vault-envrc-generator/pkg/envrc"
    "vault-envrc-generator/pkg/output"
    "vault-envrc-generator/pkg/vault"
    "vault-envrc-generator/pkg/vaultlayer"
)

type InteractiveCommand struct { *gcmds.CommandDescription }

type InteractiveSettings struct {
    Path string `glazed.parameter:"path"`
}

func NewInteractiveCommand() (*InteractiveCommand, error) {
    layer, err := glzcli.NewCommandSettingsLayer()
    if err != nil { return nil, err }
    cd := gcmds.NewCommandDescription(
        "interactive",
        gcmds.WithShort("Interactive mode to preview and write .envrc/json/yaml"),
        gcmds.WithFlags(
            parameters.NewParameterDefinition("path", parameters.ParameterTypeString, parameters.WithHelp("Vault path to fetch (prompt if empty)")),
        ),
        gcmds.WithLayersList(layer),
    )
    _, err = vaultlayer.AddVaultLayerToCommand(cd)
    if err != nil { return nil, err }
    return &InteractiveCommand{cd}, nil
}

func (c *InteractiveCommand) Run(ctx context.Context, parsed *glayers.ParsedLayers) error {
    s := &InteractiveSettings{}
    if err := parsed.InitializeStruct(glayers.DefaultSlug, s); err != nil { return err }
    vs, err := vaultlayer.GetVaultSettings(parsed)
    if err != nil { return err }

    reader := bufio.NewReader(os.Stdin)
    if strings.TrimSpace(s.Path) == "" {
        fmt.Print("Vault path: ")
        p, _ := reader.ReadString('\n')
        s.Path = strings.TrimSpace(p)
    }
    if s.Path == "" { return fmt.Errorf("path is required") }

    ctx2, cancel := context.WithTimeout(ctx, 15*time.Second)
    defer cancel()
    token, err := vault.ResolveToken(ctx2, vs.VaultToken, vault.TokenSource(vs.VaultTokenSource), vs.VaultTokenFile, false)
    if err != nil { return fmt.Errorf("failed to resolve Vault token: %w", err) }
    client, err := vault.NewClient(vs.VaultAddr, token)
    if err != nil { return fmt.Errorf("failed to create Vault client: %w", err) }

    secrets, err := client.GetSecrets(s.Path)
    if err != nil { return fmt.Errorf("failed to read secrets: %w", err) }

    keys := make([]string, 0, len(secrets))
    for k := range secrets { keys = append(keys, k) }
    sort.Strings(keys)
    fmt.Printf("Found %d keys:\n", len(keys))
    for _, k := range keys { fmt.Printf("  - %s\n", k) }

    fmt.Print("Include keys (comma-separated, empty=all): ")
    incS, _ := reader.ReadString('\n')
    inc := splitCSV(incS)
    fmt.Print("Exclude keys (comma-separated, empty=none): ")
    excS, _ := reader.ReadString('\n')
    exc := splitCSV(excS)
    fmt.Print("Prefix (empty=none): ")
    pref, _ := reader.ReadString('\n')
    pref = strings.TrimSpace(pref)
    fmt.Print("Transform keys to UPPER and '_'? (y/N): ")
    trS, _ := reader.ReadString('\n')
    transform := strings.HasPrefix(strings.ToLower(strings.TrimSpace(trS)), "y")
    fmt.Print("Format [envrc|json|yaml] (default envrc): ")
    fmtS, _ := reader.ReadString('\n')
    format := strings.TrimSpace(fmtS)
    if format == "" { format = "envrc" }

    gen := envrc.NewGenerator(&envrc.Options{
        Prefix:        pref,
        ExcludeKeys:   exc,
        IncludeKeys:   inc,
        TransformKeys: transform,
        Format:        format,
        TemplateFile:  "",
        Verbose:       false,
        SortKeys:      true,
    })
    content, err := gen.Generate(secrets)
    if err != nil { return err }

    fmt.Println("\n=== Preview ===")
    fmt.Println(content)
    fmt.Println("=== End Preview ===\n")

    fmt.Print("Write to file? Path (empty to cancel, '-' to stdout): ")
    outS, _ := reader.ReadString('\n')
    out := strings.TrimSpace(outS)
    if out == "" { fmt.Println("Canceled."); return nil }
    if out == "-" { fmt.Print(content); return nil }
    return output.Write(out, []byte(content), output.WriteOptions{Format: format})
}

func splitCSV(s string) []string {
    s = strings.TrimSpace(s)
    if s == "" { return nil }
    parts := strings.Split(s, ",")
    res := make([]string, 0, len(parts))
    for _, p := range parts { p = strings.TrimSpace(p); if p != "" { res = append(res, p) } }
    return res
}

var _ gcmds.BareCommand = &InteractiveCommand{}
