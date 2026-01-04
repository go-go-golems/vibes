package cmd

import (
	"context"
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/spf13/cobra"
	"github.com/ttmp/ttmp-cli/pkg/metadata"
	"github.com/ttmp/ttmp-cli/pkg/ticket"
)

type DoctorCommand struct {
	*cmds.CommandDescription
}

type DoctorSettings struct {
	Ticket string `glazed.parameter:"ticket"`
	Root   string `glazed.parameter:"root"`
}

func NewDoctorCommand() (*cobra.Command, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"doctor",
		cmds.WithShort("Run health checks on ticket documentation"),
		cmds.WithLong(`Run health checks on ticket documentation.

Checks for:
  - Missing index.md
  - Documents without required metadata
  - Unknown topics or doc types
  - Stale documents (LastUpdated > 14 days)
  - Missing Status field

Examples:
  ttmp doctor
  ttmp doctor --ticket MEN-3475
  ttmp doctor --output json`),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"ticket",
				parameters.ParameterTypeString,
				parameters.WithHelp("Ticket identifier (default: all tickets)"),
				parameters.WithDefault(""),
			),
			parameters.NewParameterDefinition(
				"root",
				parameters.ParameterTypeString,
				parameters.WithHelp("Root directory for ttmp"),
				parameters.WithDefault("./ttmp"),
			),
		),
		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	cmd := &DoctorCommand{
		CommandDescription: cmdDesc,
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCmd, nil
}

func (c *DoctorCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &DoctorSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	tickets, err := ticket.FindTickets(settings.Root)
	if err != nil {
		return fmt.Errorf("failed to find tickets: %w", err)
	}

	var issues []metadata.HealthIssue

	for _, t := range tickets {
		if settings.Ticket != "" && t.Ticket != settings.Ticket {
			continue
		}

		// Check for missing index.md
		if !t.HasIndex {
			issues = append(issues, metadata.HealthIssue{
				Severity: "error",
				Ticket:   t.Ticket,
				File:     "",
				Message:  "Missing index.md",
			})
		}

		// Check each document
		for _, doc := range t.Documents {
			// Check for missing Status
			if doc.Metadata.Status == "" {
				issues = append(issues, metadata.HealthIssue{
					Severity: "warning",
					Ticket:   t.Ticket,
					File:     doc.Filename,
					Message:  "Missing Status field",
				})
			}

			// Check for missing Topics
			if len(doc.Metadata.Topics) == 0 {
				issues = append(issues, metadata.HealthIssue{
					Severity: "warning",
					Ticket:   t.Ticket,
					File:     doc.Filename,
					Message:  "Missing Topics field",
				})
			}

			// Check for stale documents
			if doc.Metadata.LastUpdated != "" {
				if lastUpdate, err := time.Parse("2006-01-02", doc.Metadata.LastUpdated); err == nil {
					if time.Since(lastUpdate) > 14*24*time.Hour {
						issues = append(issues, metadata.HealthIssue{
							Severity: "info",
							Ticket:   t.Ticket,
							File:     doc.Filename,
							Message:  fmt.Sprintf("Stale document (last updated %s)", doc.Metadata.LastUpdated),
						})
					}
				}
			}
		}
	}

	// Output issues as rows
	for _, issue := range issues {
		row := types.NewRow(
			types.MRP("severity", issue.Severity),
			types.MRP("ticket", issue.Ticket),
			types.MRP("file", issue.File),
			types.MRP("message", issue.Message),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

var _ cmds.GlazeCommand = &DoctorCommand{}

