package get

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
	"github.com/pr-analyzer/pr-analyzer/internal/github"
	"github.com/spf13/cobra"
)

type CommitsSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	PRNumber int    `glazed.parameter:"pr-number"`
}

// CommitsCommand gets the commit list for a PR
type CommitsCommand struct {
	*cmds.CommandDescription
	githubClient *github.Client
}

func (c *CommitsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract settings using InitializeStruct from the github layer
	s := &CommitsSettings{}
	if err := parsedLayers.InitializeStruct("github", s); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	// Get commits from GitHub API
	commits, err := c.githubClient.GetPullRequestCommits(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR commits: %w", err)
	}

	// Add each commit as a separate row
	for _, commit := range commits {
		var authorName, authorEmail string
		var commitDate time.Time

		if commit.Commit != nil {
			if commit.Commit.Author != nil {
				if commit.Commit.Author.Name != nil {
					authorName = *commit.Commit.Author.Name
				}
				if commit.Commit.Author.Email != nil {
					authorEmail = *commit.Commit.Author.Email
				}
				if commit.Commit.Author.Date != nil {
					commitDate = commit.Commit.Author.Date.Time
				}
			}
		}

		var sha, message string
		if commit.SHA != nil {
			sha = *commit.SHA
		}
		if commit.Commit != nil && commit.Commit.Message != nil {
			message = *commit.Commit.Message
		}

		row := types.NewRow(
			types.MRP("owner", s.Owner),
			types.MRP("repo", s.Repo),
			types.MRP("pr_number", s.PRNumber),
			types.MRP("sha", sha),
			types.MRP("message", message),
			types.MRP("author_name", authorName),
			types.MRP("author_email", authorEmail),
			types.MRP("commit_date", commitDate.Format(time.RFC3339)),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

func NewCommitsCommand() (*cobra.Command, error) {
	glazeParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

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

	cmd := &CommitsCommand{
		CommandDescription: cmds.NewCommandDescription(
			"commits",
			cmds.WithShort("Get commit list for a PR"),
			cmds.WithLong("Retrieves the list of commits in a pull request"),
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
