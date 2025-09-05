package cmds

import (
    "context"
    "fmt"
    "os"
    "time"

    glzcli "github.com/go-go-golems/glazed/pkg/cli"
    gcmds "github.com/go-go-golems/glazed/pkg/cmds"
    glayers "github.com/go-go-golems/glazed/pkg/cmds/layers"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
    "gopkg.in/yaml.v3"

    "vault-envrc-generator/pkg/batch"
    "vault-envrc-generator/pkg/vault"
    "vault-envrc-generator/pkg/vaultlayer"
)

type BatchCommand struct { *gcmds.CommandDescription }

type BatchSettings struct {
    Config            string `glazed.parameter:"config"`
    OutputOverride    string `glazed.parameter:"output"`
    Format            string `glazed.parameter:"format"`
    ContinueOnError   bool   `glazed.parameter:"continue-on-error"`
    DryRun            bool   `glazed.parameter:"dry-run"`
    SortKeys          bool   `glazed.parameter:"sort-keys"`
    BasePath          string `glazed.parameter:"base-path"`
}

func NewBatchCommand() (*BatchCommand, error) {
    layer, err := glzcli.NewCommandSettingsLayer()
    if err != nil { return nil, err }

    cd := gcmds.NewCommandDescription(
        "batch",
        gcmds.WithShort("Process multiple Vault paths from a YAML file"),
        gcmds.WithFlags(
            parameters.NewParameterDefinition("config", parameters.ParameterTypeString, parameters.WithRequired(true), parameters.WithHelp("Batch YAML file"), parameters.WithShortFlag("c")),
            parameters.NewParameterDefinition("base-path", parameters.ParameterTypeString, parameters.WithHelp("Base Vault path to prepend to relative section paths")),
            parameters.NewParameterDefinition("output", parameters.ParameterTypeString, parameters.WithHelp("Override output for all jobs; '-' for stdout")),
            parameters.NewParameterDefinition("format", parameters.ParameterTypeString, parameters.WithHelp("envrc|json|yaml")),
            parameters.NewParameterDefinition("continue-on-error", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Continue processing on errors")),
            parameters.NewParameterDefinition("dry-run", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Preview outputs without writing files")),
            parameters.NewParameterDefinition("sort-keys", parameters.ParameterTypeBool, parameters.WithDefault(true), parameters.WithHelp("Sort JSON/YAML keys for deterministic output")),
        ),
        gcmds.WithLayersList(layer),
    )
    // attach vault layer
    _, err = vaultlayer.AddVaultLayerToCommand(cd)
    if err != nil { return nil, err }
    return &BatchCommand{cd}, nil
}

func (c *BatchCommand) Run(ctx context.Context, parsed *glayers.ParsedLayers) error {
    s := &BatchSettings{}
    if err := parsed.InitializeStruct(glayers.DefaultSlug, s); err != nil { return err }
    vs, err := vaultlayer.GetVaultSettings(parsed)
    if err != nil { return err }

    ctx2, cancel := context.WithTimeout(ctx, 10*time.Second)
    defer cancel()
    token, err := vault.ResolveToken(ctx2, vs.VaultToken, vault.TokenSource(vs.VaultTokenSource), vs.VaultTokenFile, false)
    if err != nil { return fmt.Errorf("failed to resolve Vault token: %w", err) }
    client, err := vault.NewClient(vs.VaultAddr, token)
    if err != nil { return fmt.Errorf("failed to create Vault client: %w", err) }

    cfg, err := loadBatchConfig(s.Config)
    if err != nil { return err }
    proc := batch.Processor{Client: client}
    return proc.Process(cfg, batch.ProcessorOptions{
        BasePath:           s.BasePath,
        OutputOverride:     s.OutputOverride,
                FormatOverride:     s.Format,
        ContinueOnError:    s.ContinueOnError,
        DryRun:             s.DryRun,
        SortKeys:           s.SortKeys,
    })
}

var _ gcmds.BareCommand = &BatchCommand{}

func loadBatchConfig(filename string) (*batch.Config, error) {
    data, err := os.ReadFile(filename)
    if err != nil { return nil, fmt.Errorf("failed to read config file: %w", err) }
    var config batch.Config
    if err := yaml.Unmarshal(data, &config); err != nil { return nil, fmt.Errorf("failed to parse YAML config: %w", err) }
    return &config, nil
}
