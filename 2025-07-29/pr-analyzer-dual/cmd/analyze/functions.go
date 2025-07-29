package analyze

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

type FunctionsSettings struct {
	Owner      string `glazed.parameter:"owner"`
	Repo       string `glazed.parameter:"repo"`
	PRNumber   int    `glazed.parameter:"pr-number"`
	ShowBody   bool   `glazed.parameter:"show-body"`
	OnlyChanged bool  `glazed.parameter:"only-changed"`
}

// FunctionsCommand analyzes functions in PR changes
type FunctionsCommand struct {
	*cmds.CommandDescription
	githubClient *github.Client
	parser       *treesitter.Parser
}

func (c *FunctionsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Extract settings using InitializeStruct from the github layer
	s := &FunctionsSettings{}
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

		// Create a map of changed functions for quick lookup
		changedFuncMap := make(map[string]bool)
		for _, fn := range changedFunctions {
			key := fmt.Sprintf("%s:%d", fn.Name, fn.StartLine)
			changedFuncMap[key] = true
		}

		// Process functions based on settings
		functionsToShow := functions
		if s.OnlyChanged {
			functionsToShow = changedFunctions
		}

		// Add each function as a separate row
		for _, fn := range functionsToShow {
			key := fmt.Sprintf("%s:%d", fn.Name, fn.StartLine)
			isChanged := changedFuncMap[key]

			// Prepare function body if requested
			body := ""
			if s.ShowBody {
				body = fn.Body
			}

			// Format receiver for display
			receiverDisplay := fn.Receiver
			if receiverDisplay != "" {
				receiverDisplay = strings.TrimSpace(receiverDisplay)
			}

			row := types.NewRow(
				types.MRP("owner", s.Owner),
				types.MRP("repo", s.Repo),
				types.MRP("pr_number", s.PRNumber),
				types.MRP("file_path", fileDiff.NewFile),
				types.MRP("function_name", fn.Name),
				types.MRP("receiver", receiverDisplay),
				types.MRP("start_line", fn.StartLine),
				types.MRP("end_line", fn.EndLine),
				types.MRP("is_exported", fn.IsExported),
				types.MRP("is_changed", isChanged),
				types.MRP("signature", strings.TrimSpace(fn.Signature)),
				types.MRP("body", body),
			)

			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	}

	return nil
}

func NewFunctionsCommand() (*cobra.Command, error) {
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
			parameters.NewParameterDefinition(
				"show-body",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Include function body in output"),
				parameters.WithDefault(false),
			),
			parameters.NewParameterDefinition(
				"only-changed",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Show only functions that were changed in the PR"),
				parameters.WithDefault(false),
			),
		),
	)
	if err != nil {
		return nil, err
	}

	cmd := &FunctionsCommand{
		CommandDescription: cmds.NewCommandDescription(
			"functions",
			cmds.WithShort("Analyze functions in PR changes"),
			cmds.WithLong("Analyzes Go functions affected by pull request changes using tree-sitter"),
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

