package get

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pr-analyzer/pr-analyzer/internal/github"
	"github.com/spf13/cobra"
)

type DiffSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	PRNumber int    `glazed.parameter:"pr-number"`
}

// DiffCommand gets the diff for a PR
type DiffCommand struct {
	*cmds.CommandDescription
	githubClient *github.Client
}

func (c *DiffCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract settings using InitializeStruct from the github layer
	s := &DiffSettings{}
	if err := parsedLayers.InitializeStruct("github", s); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	// Get the actual diff from GitHub API
	diff, err := c.githubClient.GetPullRequestDiff(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR diff: %w", err)
	}

	row := types.NewRow(
		types.MRP("owner", s.Owner),
		types.MRP("repo", s.Repo),
		types.MRP("pr_number", s.PRNumber),
		types.MRP("diff", diff),
	)

	return gp.AddRow(ctx, row)
}

func NewDiffCommand() (*cobra.Command, error) {
	glazeParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	// Create parameter layer properly
	githubLayer, err := layers.NewParameterLayer(
		"github",
		"GitHub repository parameters",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"owner",
				parameters.ParameterTypeString,
				parameters.WithHelp("GitHub repository owner"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"repo",
				parameters.ParameterTypeString,
				parameters.WithHelp("GitHub repository name"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"pr-number",
				parameters.ParameterTypeInteger,
				parameters.WithHelp("Pull request number"),
				parameters.WithRequired(true),
			),
		),
	)
	if err != nil {
		return nil, err
	}

	cmd := &DiffCommand{
		CommandDescription: cmds.NewCommandDescription(
			"diff",
			cmds.WithShort("Get the diff for a GitHub pull request"),
			cmds.WithLong("Retrieves and displays the diff for a specified GitHub pull request"),
			cmds.WithLayersList(
				glazeParameterLayer,
				githubLayer,
			),
		),
		githubClient: github.NewClient(),
	}

	cobraCommand, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCommand, nil
}
