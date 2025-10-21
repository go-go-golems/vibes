package cmd

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/pkg/ticket"
)

// discoverTTMPRoot tries to find a ttmp directory by walking up from cwd to repo root.
func discoverTTMPRoot() (string, error) {
	// 1) Respect TTMP_ROOT if set
	if env := os.Getenv("TTMP_ROOT"); env != "" {
		if fi, err := os.Stat(env); err == nil && fi.IsDir() {
			return env, nil
		}
	}

	cwd, err := os.Getwd()
	if err != nil {
		return "", err
	}

	// Track a preferred candidate if we find go-go-mento/ttmp along the way
	var preferred string

	// Walk up to filesystem root, check for subproject ttmp first, then local ttmp
	for dir := cwd; ; dir = filepath.Dir(dir) {
		// Prefer go-go-mento/ttmp if present under this ancestor
		ggm := filepath.Join(dir, "go-go-mento", "ttmp")
		if fi, err := os.Stat(ggm); err == nil && fi.IsDir() {
			preferred = ggm
			break
		}
		// Fallback to immediate ttmp at this level
		candidate := filepath.Join(dir, "ttmp")
		if fi, err := os.Stat(candidate); err == nil && fi.IsDir() {
			// do not break; keep searching for a preferred go-go-mento/ttmp higher up
			if preferred == "" {
				preferred = candidate
			}
		}
		if dir == filepath.Dir(dir) { // reached root
			break
		}
	}
	if preferred != "" {
		return preferred, nil
	}

	// Try common project subdirectories from cwd
	candidates := []string{
		filepath.Join(cwd, "ttmp"),
		filepath.Join(cwd, "go-go-mento", "ttmp"),
		filepath.Join(cwd, "geppetto", "ttmp"),
		filepath.Join(cwd, "pinocchio", "ttmp"),
		filepath.Join(cwd, "bobatea", "ttmp"),
		filepath.Join(cwd, "glazed", "ttmp"),
	}
	for _, c := range candidates {
		if fi, err := os.Stat(c); err == nil && fi.IsDir() {
			return c, nil
		}
	}
	return "", fmt.Errorf("ttmp root not found from cwd or common subdirectories")
}

type ListTicketsCommand struct {
	*cmds.CommandDescription
}

type ListTicketsSettings struct {
	Root string `glazed.parameter:"root"`
}

func NewListTicketsCommand() (*cobra.Command, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"tickets",
		cmds.WithShort("List all tickets"),
		cmds.WithLong(`List all tickets in the ttmp directory with metadata.

Examples:
  ttmp list tickets
  ttmp list tickets --output json
  ttmp list tickets --fields ticket,status,topics`),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"root",
				parameters.ParameterTypeString,
				parameters.WithHelp("Root directory for ttmp"),
				parameters.WithDefault("./ttmp"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	cmd := &ListTicketsCommand{
		CommandDescription: cmdDesc,
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCmd, nil
}

func (c *ListTicketsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListTicketsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	// If root doesn't exist, try to discover ttmp root from current git repo or cwd
	root := settings.Root
	if _, err := os.Stat(root); err != nil {
		if cwdRoot, err2 := discoverTTMPRoot(); err2 == nil {
			root = cwdRoot
		}
	}

	tickets, err := ticket.FindTickets(root)
	if err != nil {
		return fmt.Errorf("failed to find tickets: %w", err)
	}

	for _, t := range tickets {
		row := types.NewRow(
			types.MRP("ticket", t.Ticket),
			types.MRP("slug", t.Slug),
			types.MRP("status", t.Status),
			types.MRP("has_index", t.HasIndex),
			types.MRP("doc_count", len(t.Documents)),
		)
		
		if len(t.Topics) > 0 {
			row.Set("topics", t.Topics)
		}
		if len(t.Owners) > 0 {
			row.Set("owners", t.Owners)
		}

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

var _ cmds.GlazeCommand = &ListTicketsCommand{}