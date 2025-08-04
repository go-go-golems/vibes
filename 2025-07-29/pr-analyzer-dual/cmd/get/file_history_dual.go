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

type FileHistoryDualCommand struct {
	*cmds.CommandDescription
}

type FileHistoryDualSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	FilePath string `glazed.parameter:"file-path"`
}

// Implement BareCommand for human-readable output
func (c *FileHistoryDualCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &FileHistoryDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	commits, err := client.GetFileCommits(ctx, s.Owner, s.Repo, s.FilePath)
	if err != nil {
		return fmt.Errorf("failed to get file commits: %w", err)
	}

	// Human-readable markdown-style output
	fmt.Printf("# File History: %s\n\n", s.FilePath)
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
func (c *FileHistoryDualCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &FileHistoryDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	commits, err := client.GetFileCommits(ctx, s.Owner, s.Repo, s.FilePath)
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

// Ensure both interfaces are implemented
var _ cmds.BareCommand = &FileHistoryDualCommand{}
var _ cmds.GlazeCommand = &FileHistoryDualCommand{}

func NewFileHistoryDualCommand() (*FileHistoryDualCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"file-history",
		cmds.WithShort("Get commit history for a specific file"),
		cmds.WithLong("Retrieves the commit history for a specified file in the repository. Use --output for structured data formats."),
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
				"file-path",
				parameters.ParameterTypeString,
				parameters.WithHelp("Path to the file in the repository"),
				parameters.WithRequired(true),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &FileHistoryDualCommand{
		CommandDescription: cmdDesc,
	}, nil
}
