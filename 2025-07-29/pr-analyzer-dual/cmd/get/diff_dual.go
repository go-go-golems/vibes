package get

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pr-analyzer/pr-analyzer/internal/github"
)

type DiffDualCommand struct {
	*cmds.CommandDescription
}

type DiffDualSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	PRNumber int    `glazed.parameter:"pr-number"`
}

// Implement BareCommand for human-readable output
func (c *DiffDualCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &DiffDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	diff, err := client.GetPullRequestDiff(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR diff: %w", err)
	}

	// Human-readable output
	fmt.Printf("# Pull Request #%d Diff\n\n", s.PRNumber)
	fmt.Printf("**Repository:** %s/%s\n\n", s.Owner, s.Repo)
	fmt.Printf("```diff\n%s\n```\n", diff)

	return nil
}

// Implement GlazeCommand for structured output
func (c *DiffDualCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &DiffDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	diff, err := client.GetPullRequestDiff(ctx, s.Owner, s.Repo, s.PRNumber)
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

// Ensure both interfaces are implemented
var _ cmds.BareCommand = &DiffDualCommand{}
var _ cmds.GlazeCommand = &DiffDualCommand{}

func NewDiffDualCommand() (*DiffDualCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"diff",
		cmds.WithShort("Get the diff for a GitHub pull request"),
		cmds.WithLong("Retrieves and displays the unified diff for a specified GitHub pull request. Use --output for structured data formats."),
		cmds.WithFlags(
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
		cmds.WithLayersList(glazedLayer),
	)

	return &DiffDualCommand{
		CommandDescription: cmdDesc,
	}, nil
}
