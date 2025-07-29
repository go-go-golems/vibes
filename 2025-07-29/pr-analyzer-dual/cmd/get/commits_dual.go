package get

import (
	"context"
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pr-analyzer/pr-analyzer/internal/github"
)

type CommitsDualCommand struct {
	*cmds.CommandDescription
}

type CommitsDualSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	PRNumber int    `glazed.parameter:"pr-number"`
}

// Implement BareCommand for human-readable output
func (c *CommitsDualCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &CommitsDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	commits, err := client.GetPullRequestCommits(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR commits: %w", err)
	}

	// Human-readable markdown-style output
	fmt.Printf("# Pull Request #%d Commits\n\n", s.PRNumber)
	fmt.Printf("**Repository:** %s/%s\n", s.Owner, s.Repo)
	fmt.Printf("**Total Commits:** %d\n\n", len(commits))

	for i, commit := range commits {
		var sha, message, authorName, authorEmail string
		var commitDate time.Time

		if commit.SHA != nil {
			sha = *commit.SHA
		}
		if commit.Commit != nil && commit.Commit.Message != nil {
			message = *commit.Commit.Message
		}
		if commit.Commit != nil && commit.Commit.Author != nil {
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

		fmt.Printf("## %d. %s\n\n", i+1, message)
		fmt.Printf("- **SHA:** `%s`\n", sha[:8])
		fmt.Printf("- **Author:** %s <%s>\n", authorName, authorEmail)
		
		if !commitDate.IsZero() {
			fmt.Printf("- **Date:** %s\n", commitDate.Format("2006-01-02 15:04:05"))
		}
		
		fmt.Printf("\n")
	}

	return nil
}

// Implement GlazeCommand for structured output
func (c *CommitsDualCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &CommitsDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	commits, err := client.GetPullRequestCommits(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR commits: %w", err)
	}

	for _, commit := range commits {
		var sha, message, authorName, authorEmail string
		var commitDate time.Time

		if commit.SHA != nil {
			sha = *commit.SHA
		}
		if commit.Commit != nil && commit.Commit.Message != nil {
			message = *commit.Commit.Message
		}
		if commit.Commit != nil && commit.Commit.Author != nil {
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

// Ensure both interfaces are implemented
var _ cmds.BareCommand = &CommitsDualCommand{}
var _ cmds.GlazeCommand = &CommitsDualCommand{}

func NewCommitsDualCommand() (*CommitsDualCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"commits",
		cmds.WithShort("Get commits for a pull request"),
		cmds.WithLong("Retrieves the list of commits in a GitHub pull request with detailed information about each commit. Use --output for structured data formats."),
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

	return &CommitsDualCommand{
		CommandDescription: cmdDesc,
	}, nil
}

