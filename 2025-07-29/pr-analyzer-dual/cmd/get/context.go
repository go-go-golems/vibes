package get

import (
	"context"
	"fmt"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pr-analyzer/pr-analyzer/internal/analysis"
	"github.com/pr-analyzer/pr-analyzer/internal/github"
	"github.com/pr-analyzer/pr-analyzer/internal/treesitter"
	"github.com/spf13/cobra"
)

type ContextSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	PRNumber int    `glazed.parameter:"pr-number"`
}

// ContextCommand gets context on files and functions in a PR
type ContextCommand struct {
	*cmds.CommandDescription
	githubClient *github.Client
	parser       *treesitter.Parser
}

func (c *ContextCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract settings using InitializeStruct from the github layer
	s := &ContextSettings{}
	if err := parsedLayers.InitializeStruct("github", s); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	// Get the PR diff
	diff, err := c.githubClient.GetPullRequestDiff(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR diff: %w", err)
	}

	// Parse the diff
	fileDiffs, err := analysis.ParseDiff(diff)
	if err != nil {
		return fmt.Errorf("failed to parse diff: %w", err)
	}

	// Get PR info to get the head SHA
	pr, err := c.githubClient.GetPullRequest(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR info: %w", err)
	}

	headSHA := ""
	if pr.Head != nil && pr.Head.SHA != nil {
		headSHA = *pr.Head.SHA
	}

	// Analyze each Go file
	for _, fileDiff := range fileDiffs {
		if !fileDiff.IsGoFile() {
			continue
		}

		// Get the current file content
		fileContent, err := c.githubClient.GetFileContent(ctx, s.Owner, s.Repo, fileDiff.NewFile, headSHA)
		if err != nil {
			// File might be deleted or renamed, skip it
			continue
		}

		// Extract functions from the file
		functions, err := c.parser.ExtractFunctions([]byte(fileContent))
		if err != nil {
			continue // Skip files that can't be parsed
		}

		// Get changed lines
		changedLines := fileDiff.GetChangedLines()

		// Find functions that were modified
		changedFunctions, err := c.parser.GetChangedFunctions([]byte(fileContent), changedLines)
		if err != nil {
			continue
		}

		// Get file stats
		added, removed, modified := fileDiff.GetStats()

		// Create a row for the file
		row := types.NewRow(
			types.MRP("owner", s.Owner),
			types.MRP("repo", s.Repo),
			types.MRP("pr_number", s.PRNumber),
			types.MRP("file_path", fileDiff.NewFile),
			types.MRP("lines_added", added),
			types.MRP("lines_removed", removed),
			types.MRP("lines_modified", modified),
			types.MRP("total_functions", len(functions)),
			types.MRP("changed_functions", len(changedFunctions)),
			types.MRP("changed_function_names", strings.Join(getFunctionNames(changedFunctions), ", ")),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}

		// Add detailed rows for each changed function
		for _, fn := range changedFunctions {
			funcRow := types.NewRow(
				types.MRP("owner", s.Owner),
				types.MRP("repo", s.Repo),
				types.MRP("pr_number", s.PRNumber),
				types.MRP("file_path", fileDiff.NewFile),
				types.MRP("function_name", fn.Name),
				types.MRP("function_receiver", fn.Receiver),
				types.MRP("start_line", fn.StartLine),
				types.MRP("end_line", fn.EndLine),
				types.MRP("is_exported", fn.IsExported),
				types.MRP("signature", strings.TrimSpace(fn.Signature)),
			)

			if err := gp.AddRow(ctx, funcRow); err != nil {
				return err
			}
		}
	}

	return nil
}

func getFunctionNames(functions []*treesitter.Function) []string {
	names := make([]string, len(functions))
	for i, fn := range functions {
		if fn.Receiver != "" {
			names[i] = fmt.Sprintf("%s.%s", fn.Receiver, fn.Name)
		} else {
			names[i] = fn.Name
		}
	}
	return names
}

func NewContextCommand() (*cobra.Command, error) {
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

	cmd := &ContextCommand{
		CommandDescription: cmds.NewCommandDescription(
			"context",
			cmds.WithShort("Get context on files and functions in a PR"),
			cmds.WithLong("Analyzes the PR diff to provide context on affected files and functions using tree-sitter"),
			cmds.WithLayersList(
				glazeParameterLayer,
				githubLayer,
			),
		),
		githubClient: github.NewClient(),
		parser:       treesitter.NewParser(),
	}

	cobraCommand, err := cli.BuildCobraCommandFromGlazeCommand(cmd)
	if err != nil {
		return nil, err
	}

	return cobraCommand, nil
}
