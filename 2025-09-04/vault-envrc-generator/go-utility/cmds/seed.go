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

    "vault-envrc-generator/pkg/seed"
    "vault-envrc-generator/pkg/vault"
    "vault-envrc-generator/pkg/vaultlayer"
)

type SeedCommand struct { *gcmds.CommandDescription }

type SeedSettings struct {
    Config string `glazed.parameter:"config"`
    DryRun bool   `glazed.parameter:"dry-run"`
}

func NewSeedCommand() (*SeedCommand, error) {
    layer, err := glzcli.NewCommandSettingsLayer()
    if err != nil { return nil, err }

    cd := gcmds.NewCommandDescription(
        "seed",
        gcmds.WithShort("Seed Vault from env/files via YAML spec"),
        gcmds.WithFlags(
            parameters.NewParameterDefinition("config", parameters.ParameterTypeString, parameters.WithRequired(true), parameters.WithHelp("Seed YAML file"), parameters.WithShortFlag("c")),
            parameters.NewParameterDefinition("dry-run", parameters.ParameterTypeBool, parameters.WithDefault(false), parameters.WithHelp("Preview without writing to Vault")),
        ),
        gcmds.WithLayersList(layer),
    )
    _, err = vaultlayer.AddVaultLayerToCommand(cd)
    if err != nil { return nil, err }
    return &SeedCommand{cd}, nil
}

func (c *SeedCommand) Run(ctx context.Context, parsed *glayers.ParsedLayers) error {
    s := &SeedSettings{}
    if err := parsed.InitializeStruct(glayers.DefaultSlug, s); err != nil { return err }
    vs, err := vaultlayer.GetVaultSettings(parsed)
    if err != nil { return err }

    ctx2, cancel := context.WithTimeout(ctx, 15*time.Second)
    defer cancel()
    token, err := vault.ResolveToken(ctx2, vs.VaultToken, vault.TokenSource(vs.VaultTokenSource), vs.VaultTokenFile, false)
    if err != nil { return fmt.Errorf("failed to resolve Vault token: %w", err) }
    client, err := vault.NewClient(vs.VaultAddr, token)
    if err != nil { return fmt.Errorf("failed to create Vault client: %w", err) }

    b, err := os.ReadFile(s.Config)
    if err != nil { return fmt.Errorf("failed to read seed config: %w", err) }
    var spec seed.Spec
    if err := yaml.Unmarshal(b, &spec); err != nil { return fmt.Errorf("failed to parse seed YAML: %w", err) }

    return seed.Run(client, &spec, seed.Options{DryRun: s.DryRun})
}

var _ gcmds.BareCommand = &SeedCommand{}
