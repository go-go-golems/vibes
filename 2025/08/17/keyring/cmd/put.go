package cmd

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"keyring/pkg/keyring"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

type PutCommand struct {
	*cmds.CommandDescription
	ring *keyring.Ring
}

type PutSettings struct {
	Path      string `glazed.parameter:"path"`
	Value     string `glazed.parameter:"value"`
	Profile   string `glazed.parameter:"profile"`
	Metadata  string `glazed.parameter:"metadata"`
	ExpiresAt string `glazed.parameter:"expires-at"`
}

func NewPutCommand(ring *keyring.Ring) (*PutCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"put",
		cmds.WithShort("Store a secret in the keyring"),
		cmds.WithLong(`
Store a secret in the keyring at the specified hierarchical path.

The secret will be stored using the configured writer backend under the
active profile (or specified profile).

Examples:
  keyring put --path openai/api_key --value sk-xxx
  keyring put --path aws/access_key --value AKIA... --profile work
  keyring put --path temp/token --value abc123 --expires-at 2024-12-31T23:59:59Z
  keyring put --path api/key --value secret --metadata '{"source":"manual","version":"1"}'
		`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"path",
				parameters.ParameterTypeString,
				parameters.WithHelp("Hierarchical path for the secret (e.g., openai/api_key)"),
				parameters.WithShortFlag("p"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"value",
				parameters.ParameterTypeString,
				parameters.WithHelp("Secret value to store"),
				parameters.WithShortFlag("v"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"profile",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Profile to store under (defaults to first configured profile)"),
			),
			parameters.NewParameterDefinition(
				"metadata",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("JSON metadata to attach to the secret"),
				parameters.WithShortFlag("m"),
			),
			parameters.NewParameterDefinition(
				"expires-at",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Expiration time in RFC3339 format (e.g., 2024-12-31T23:59:59Z)"),
				parameters.WithShortFlag("e"),
			),
		),

		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &PutCommand{
		CommandDescription: cmdDesc,
		ring:               ring,
	}, nil
}

func (c *PutCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &PutSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	path := keyring.P(settings.Path)

	// Parse metadata if provided
	var metadata map[string]string
	if settings.Metadata != "" {
		if err := json.Unmarshal([]byte(settings.Metadata), &metadata); err != nil {
			return fmt.Errorf("invalid metadata JSON: %w", err)
		}
	}

	// Parse expiration if provided
	var expiresAt time.Time
	if settings.ExpiresAt != "" {
		var err error
		expiresAt, err = time.Parse(time.RFC3339, settings.ExpiresAt)
		if err != nil {
			return fmt.Errorf("invalid expires-at format (use RFC3339): %w", err)
		}
	}

	secret := keyring.Secret{
		Value:     settings.Value,
		Metadata:  metadata,
		ExpiresAt: expiresAt,
	}

	// If specific profile requested, temporarily override ring profiles
	originalRing := c.ring
	if settings.Profile != "" {
		c.ring = keyring.New(
			keyring.WithProfiles(settings.Profile),
			keyring.WithReaders(originalRing.GetReaders()...),
			keyring.WithWriter(originalRing.GetWriter()),
			keyring.WithStateStore(originalRing.GetStateStore()),
			keyring.WithAuditSink(originalRing.GetAuditSink()),
			keyring.WithActor(originalRing.GetActor()),
		)
	}

	if err := c.ring.Put(ctx, path, secret); err != nil {
		return fmt.Errorf("failed to store secret: %w", err)
	}

	// Create output row
	rowData := []types.MapRowPair{
		types.MRP("path", settings.Path),
		types.MRP("status", "stored"),
		types.MRP("profile", func() string {
			if settings.Profile != "" {
				return settings.Profile
			}
			return c.ring.GetProfiles()[0]
		}()),
	}

	if !expiresAt.IsZero() {
		rowData = append(rowData, types.MRP("expires_at", expiresAt.Format("2006-01-02T15:04:05Z07:00")))
	}

	row := types.NewRow(rowData...)
	return gp.AddRow(ctx, row)
}

