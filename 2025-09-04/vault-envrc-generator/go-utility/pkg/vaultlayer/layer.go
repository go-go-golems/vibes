package vaultlayer

import (
    "fmt"

    glzcms "github.com/go-go-golems/glazed/pkg/cmds"
    glzlayers "github.com/go-go-golems/glazed/pkg/cmds/layers"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

const VaultLayerSlug = "vault"

type VaultSettings struct {
    VaultAddr        string `glazed.parameter:"vault-addr"`
    VaultToken       string `glazed.parameter:"vault-token"`
    VaultTokenSource string `glazed.parameter:"vault-token-source"`
    VaultTokenFile   string `glazed.parameter:"vault-token-file"`
}

// NewVaultLayer defines a reusable parameter layer for Vault configuration.
func NewVaultLayer() (glzlayers.ParameterLayer, error) {
    return glzlayers.NewParameterLayer(
        VaultLayerSlug,
        "Vault connection settings",
        glzlayers.WithParameterDefinitions(
            parameters.NewParameterDefinition(
                "vault-addr",
                parameters.ParameterTypeString,
                parameters.WithHelp("Vault server address"),
                parameters.WithDefault("http://127.0.0.1:8200"),
            ),
            parameters.NewParameterDefinition(
                "vault-token",
                parameters.ParameterTypeString,
                parameters.WithHelp("Vault token (optional)"),
                parameters.WithDefault(""),
            ),
            parameters.NewParameterDefinition(
                "vault-token-source",
                parameters.ParameterTypeChoice,
                parameters.WithHelp("Token source: auto|env|file|lookup"),
                parameters.WithDefault("auto"),
                parameters.WithChoices("auto","env","file","lookup"),
            ),
            parameters.NewParameterDefinition(
                "vault-token-file",
                parameters.ParameterTypeString,
                parameters.WithHelp("Path to token file (default ~/.vault-token)"),
                parameters.WithDefault(""),
            ),
        ),
    )
}

// AddVaultLayerToCommand attaches the layer to a Glazed command description.
func AddVaultLayerToCommand(c glzcms.Command) (glzcms.Command, error) {
    l, err := NewVaultLayer()
    if err != nil { return nil, err }
    c.Description().Layers.Set(VaultLayerSlug, l)
    return c, nil
}

// GetVaultSettings returns parsed vault settings from the ParsedLayers.
func GetVaultSettings(parsed *glzlayers.ParsedLayers) (*VaultSettings, error) {
    var s VaultSettings
    if err := parsed.InitializeStruct(VaultLayerSlug, &s); err != nil {
        return nil, fmt.Errorf("failed to parse vault settings: %w", err)
    }
    return &s, nil
}

