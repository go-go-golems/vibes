package cmds

import (
    "context"
    "fmt"
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

type GenerateCommand struct { *gcmds.CommandDescription }

type GenerateSettings struct {
    Path          string   `glazed.parameter:"path"`
    TemplateFile  string   `glazed.parameter:"template"`
    Prefix        string   `glazed.parameter:"prefix"`
    ExcludeKeys   []string `glazed.parameter:"exclude"`
    IncludeKeys   []string `glazed.parameter:"include"`
    TransformKeys bool     `glazed.parameter:"transform-keys"`
    DryRun        bool     `glazed.parameter:"dry-run"`
    Format        string   `glazed.parameter:"format"`
    Output        string   `glazed.parameter:"output"`
    SortKeys      bool     `glazed.parameter:"sort-keys"`
}

func NewGenerateCommand() (*GenerateCommand, error) {
    layer, err := glzcli.NewCommandSettingsLayer()
    if err != nil { return nil, err }
    cd := gcmds.NewCommandDescription(
        "generate",
        gcmds.WithShort("Generate .envrc/json/yaml from a single Vault path"),
        gcmds.WithFlags(
            parameters.NewParameterDefinition("path", parameters.ParameterTypeString, parameters.WithRequired(true), parameters.WithShortFlag("p"), parameters.WithHelp("Vault path")),
            parameters.NewParameterDefinition("template", parameters.ParameterTypeString, parameters.WithHelp("Custom template file")),
            parameters.NewParameterDefinition("prefix", parameters.ParameterTypeString, parameters.WithHelp("Prefix to add to keys")),
            parameters.NewParameterDefinition("exclude", parameters.ParameterTypeStringList, parameters.WithHelp("Keys to exclude")),
            parameters.NewParameterDefinition("include", parameters.ParameterTypeStringList, parameters.WithHelp("Keys to include (overrides exclude)")),
            parameters.NewParameterDefinition("transform-keys", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Transform keys to UPPER and '-' to '_'")),
            parameters.NewParameterDefinition("dry-run", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Print to stdout instead of writing")),
            parameters.NewParameterDefinition("format", parameters.ParameterTypeChoice, parameters.WithChoices("envrc","json","yaml"), parameters.WithDefault("envrc"), parameters.WithHelp("Output format")),
            parameters.NewParameterDefinition("output", parameters.ParameterTypeString, parameters.WithDefault("-"), parameters.WithHelp("Output path or '-' for stdout")),
            parameters.NewParameterDefinition("sort-keys", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Sort keys in JSON/YAML")),
        ),
        gcmds.WithLayersList(layer),
    )
    _, err = vaultlayer.AddVaultLayerToCommand(cd)
    if err != nil { return nil, err }
    return &GenerateCommand{cd}, nil
}

func (c *GenerateCommand) Run(ctx context.Context, parsed *glayers.ParsedLayers) error {
    s := &GenerateSettings{}
    if err := parsed.InitializeStruct(glayers.DefaultSlug, s); err != nil { return err }
    vs, err := vaultlayer.GetVaultSettings(parsed)
    if err != nil { return err }

    ctx2, cancel := context.WithTimeout(ctx, 10*time.Second)
    defer cancel()
    token, err := vault.ResolveToken(ctx2, vs.VaultToken, vault.TokenSource(vs.VaultTokenSource), vs.VaultTokenFile, false)
    if err != nil { return fmt.Errorf("failed to resolve Vault token: %w", err) }
    client, err := vault.NewClient(vs.VaultAddr, token)
    if err != nil { return fmt.Errorf("failed to create Vault client: %w", err) }

    secrets, err := client.GetSecrets(s.Path)
    if err != nil { return fmt.Errorf("failed to retrieve secrets: %w", err) }

    gen := envrc.NewGenerator(&envrc.Options{
        Prefix:        s.Prefix,
        ExcludeKeys:   s.ExcludeKeys,
        IncludeKeys:   s.IncludeKeys,
        TransformKeys: s.TransformKeys,
        Format:        s.Format,
        TemplateFile:  s.TemplateFile,
        Verbose:       false,
        SortKeys:      s.SortKeys,
    })
    content, err := gen.Generate(secrets)
    if err != nil { return err }

    if s.DryRun || s.Output == "-" {
        fmt.Print(content)
        if s.Format == "envrc" { fmt.Print("\n") }
        return nil
    }
    return output.Write(s.Output, []byte(content), output.WriteOptions{Format: s.Format, SortKeys: s.SortKeys})
}

var _ gcmds.BareCommand = &GenerateCommand{}
