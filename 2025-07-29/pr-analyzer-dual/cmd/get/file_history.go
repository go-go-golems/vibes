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

type FileHistorySettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	FilePath string `glazed.parameter:"file-path"`
}

// FileHistoryCommand gets commit history for a specific file
type FileHistoryCommand struct {
	*cmds.CommandDescription
	githubClient *github.Client
}

func (c *FileHistoryCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract settings using InitializeStruct from the github layer
	s := &FileHistorySettings{}
	if err := parsedLayers.InitializeStruct("github", s); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	// Get file commit history from GitHub API
	commits, err := c.githubClient.GetFileCommits(ctx, s.Owner, s.Repo, s.FilePath)
	if err != nil {
		return fmt.Errorf("failed to get file commits: %w", err)
	}

	// Add each commit as a separate row
	for _, commit := range commits {
		var authorName, authorEmail, committerName, committerEmail string
		var commitDate, committerDate time.Time

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
			if commit.Commit.Committer != nil {
				if commit.Commit.Committer.Name != nil {
					committerName = *commit.Commit.Committer.Name
				}
				if commit.Commit.Committer.Email != nil {
					committerEmail = *commit.Commit.Committer.Email
				}
				if commit.Commit.Committer.Date != nil {
					committerDate = commit.Commit.Committer.Date.Time
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
			types.MRP("file_path", s.FilePath),
			types.MRP("sha", sha),
			types.MRP("message", message),
			types.MRP("author_name", authorName),
			types.MRP("author_email", authorEmail),
			types.MRP("commit_date", commitDate.Format(time.RFC3339)),
			types.MRP("committer_name", committerName),
			types.MRP("committer_email", committerEmail),
			types.MRP("committer_date", committerDate.Format(time.RFC3339)),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

func NewFileHistoryCommand() (*cobra.Command, error) {
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
				"file-path",
				parameters.ParameterTypeString,
				parameters.WithHelp("Path to the file in the repository"),
				parameters.WithRequired(true),
			),
		),
	)
	if err != nil {
		return nil, err
	}

	cmd := &FileHistoryCommand{
		CommandDescription: cmds.NewCommandDescription(
			"file-history",
			cmds.WithShort("Get commit history for a specific file"),
			cmds.WithLong("Retrieves the commit history for a specified file in the repository"),
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
