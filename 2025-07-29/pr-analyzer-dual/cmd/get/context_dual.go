package get

import (
	"context"
	"fmt"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pr-analyzer/pr-analyzer/internal/analysis"
	"github.com/pr-analyzer/pr-analyzer/internal/github"
	"github.com/pr-analyzer/pr-analyzer/internal/treesitter"
)

type ContextDualCommand struct {
	*cmds.CommandDescription
}

type ContextDualSettings struct {
	Owner    string `glazed.parameter:"owner"`
	Repo     string `glazed.parameter:"repo"`
	PRNumber int    `glazed.parameter:"pr-number"`
}

// Implement BareCommand for human-readable output
func (c *ContextDualCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &ContextDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	parser := treesitter.NewParser()

	// Get PR diff
	diff, err := client.GetPullRequestDiff(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR diff: %w", err)
	}

	// Parse diff to get changed files
	fileChanges, err := analysis.ParseDiffForAnalysis(diff)
	if err != nil {
		return fmt.Errorf("failed to parse diff: %w", err)
	}

	fmt.Printf("# Pull Request #%d Context Analysis\n\n", s.PRNumber)
	fmt.Printf("**Repository:** %s/%s\n", s.Owner, s.Repo)
	fmt.Printf("**Files Changed:** %d\n\n", len(fileChanges))

	for _, fileChange := range fileChanges {
		if !strings.HasSuffix(fileChange.FilePath, ".go") {
			continue
		}

		fmt.Printf("## 📁 %s\n\n", fileChange.FilePath)
		fmt.Printf("**Changes:**\n")
		fmt.Printf("- Lines Added: %d\n", fileChange.LinesAdded)
		fmt.Printf("- Lines Removed: %d\n", fileChange.LinesRemoved)
		fmt.Printf("- Lines Modified: %d\n\n", fileChange.LinesModified)

		// Get file content and analyze functions
		content, err := client.GetFileContent(ctx, s.Owner, s.Repo, fileChange.FilePath, "")
		if err != nil {
			fmt.Printf("*Could not analyze functions: %v*\n\n", err)
			continue
		}

		functions, err := parser.ExtractFunctions([]byte(content))
		if err != nil {
			fmt.Printf("*Could not parse Go functions: %v*\n\n", err)
			continue
		}

		fmt.Printf("**Functions:**\n")
		fmt.Printf("- Total Functions: %d\n", len(functions))

		// Determine which functions were changed
		changedFunctions := []string{}
		for _, fn := range functions {
			if analysis.IsFunctionChanged(fn, fileChange.ChangedLines) {
				changedFunctions = append(changedFunctions, fn.Name)
			}
		}

		fmt.Printf("- Changed Functions: %d\n", len(changedFunctions))
		if len(changedFunctions) > 0 {
			fmt.Printf("- Changed Function Names: %s\n", strings.Join(changedFunctions, ", "))
		}

		fmt.Printf("\n")
	}

	return nil
}

// Implement GlazeCommand for structured output
func (c *ContextDualCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &ContextDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	parser := treesitter.NewParser()

	// Get PR diff
	diff, err := client.GetPullRequestDiff(ctx, s.Owner, s.Repo, s.PRNumber)
	if err != nil {
		return fmt.Errorf("failed to get PR diff: %w", err)
	}

	// Parse diff to get changed files
	fileChanges, err := analysis.ParseDiffForAnalysis(diff)
	if err != nil {
		return fmt.Errorf("failed to parse diff: %w", err)
	}

	for _, fileChange := range fileChanges {
		totalFunctions := 0
		changedFunctions := 0
		changedFunctionNames := []string{}

		if strings.HasSuffix(fileChange.FilePath, ".go") {
			// Get file content and analyze functions
			content, err := client.GetFileContent(ctx, s.Owner, s.Repo, fileChange.FilePath, "")
			if err == nil {
				functions, err := parser.ExtractFunctions([]byte(content))
				if err == nil {
					totalFunctions = len(functions)

					// Determine which functions were changed
					for _, fn := range functions {
						if analysis.IsFunctionChanged(fn, fileChange.ChangedLines) {
							changedFunctions++
							changedFunctionNames = append(changedFunctionNames, fn.Name)
						}
					}
				}
			}
		}

		row := types.NewRow(
			types.MRP("owner", s.Owner),
			types.MRP("repo", s.Repo),
			types.MRP("pr_number", s.PRNumber),
			types.MRP("file_path", fileChange.FilePath),
			types.MRP("lines_added", fileChange.LinesAdded),
			types.MRP("lines_removed", fileChange.LinesRemoved),
			types.MRP("lines_modified", fileChange.LinesModified),
			types.MRP("total_functions", totalFunctions),
			types.MRP("changed_functions", changedFunctions),
			types.MRP("changed_function_names", strings.Join(changedFunctionNames, ", ")),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

// Ensure both interfaces are implemented
var _ cmds.BareCommand = &ContextDualCommand{}
var _ cmds.GlazeCommand = &ContextDualCommand{}

func NewContextDualCommand() (*ContextDualCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"context",
		cmds.WithShort("Get context analysis for a pull request"),
		cmds.WithLong("Analyzes the PR diff to provide context on affected files and functions using tree-sitter. Use --output for structured data formats."),
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

	return &ContextDualCommand{
		CommandDescription: cmdDesc,
	}, nil
}
