package cmd

import (
	"context"
	"fmt"

	"keyring/pkg/keyring"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

type GetCommand struct {
	*cmds.CommandDescription
	ring *keyring.Ring
}

type GetSettings struct {
	Path    string `glazed.parameter:"path"`
	Profile string `glazed.parameter:"profile"`
}

func NewGetCommand(ring *keyring.Ring) (*GetCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"get",
		cmds.WithShort("Retrieve a secret from the keyring"),
		cmds.WithLong(`
Retrieve a secret from the keyring using hierarchical path lookup with profile fallback.

The command will search through configured profiles in order and return the first
matching secret found. If the secret is deprecated, a warning will be displayed.
If the secret is invalidated, an error will be returned.

Examples:
  keyring get --path openai/api_key
  keyring get --path openai/api_key --profile work
  keyring get --path aws/ses/smtp_password --output json
		`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"path",
				parameters.ParameterTypeString,
				parameters.WithHelp("Hierarchical path to the secret (e.g., openai/api_key)"),
				parameters.WithShortFlag("p"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"profile",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Profile to use (defaults to configured profile order)"),
			),
		),

		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &GetCommand{
		CommandDescription: cmdDesc,
		ring:               ring,
	}, nil
}

func (c *GetCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &GetSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	path := keyring.P(settings.Path)

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

	handle, err := c.ring.Acquire(ctx, path)
	if err != nil {
		if keyring.IsNotFound(err) {
			return fmt.Errorf("secret not found at path: %s", settings.Path)
		}
		return fmt.Errorf("failed to retrieve secret: %w", err)
	}

	// Create output row with all data
	rowData := []types.MapRowPair{
		types.MRP("path", settings.Path),
		types.MRP("value", handle.Secret.Value),
		types.MRP("profile", handle.Profile),
		types.MRP("backend", handle.Backend),
	}

	// Add metadata if present
	if handle.Secret.Metadata != nil {
		for k, v := range handle.Secret.Metadata {
			rowData = append(rowData, types.MRP(fmt.Sprintf("meta_%s", k), v))
		}
	}

	// Add expiration if present
	if !handle.Secret.ExpiresAt.IsZero() {
		rowData = append(rowData, types.MRP("expires_at", handle.Secret.ExpiresAt.Format("2006-01-02T15:04:05Z07:00")))
	}

	row := types.NewRow(rowData...)
	return gp.AddRow(ctx, row)
}

