package analyze

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

type FunctionsDualCommand struct {
	*cmds.CommandDescription
}

type FunctionsDualSettings struct {
	Owner       string `glazed.parameter:"owner"`
	Repo        string `glazed.parameter:"repo"`
	PRNumber    int    `glazed.parameter:"pr-number"`
	ShowBody    bool   `glazed.parameter:"show-body"`
	OnlyChanged bool   `glazed.parameter:"only-changed"`
}

// Implement BareCommand for human-readable output
func (c *FunctionsDualCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &FunctionsDualSettings{}
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

	fmt.Printf("# Function Analysis for PR #%d\n\n", s.PRNumber)
	fmt.Printf("**Repository:** %s/%s\n", s.Owner, s.Repo)
	if s.OnlyChanged {
		fmt.Printf("**Filter:** Only showing changed functions\n")
	}
	fmt.Printf("\n")

	totalFunctions := 0
	changedFunctions := 0

	for _, fileChange := range fileChanges {
		if !strings.HasSuffix(fileChange.FilePath, ".go") {
			continue
		}

		// Get file content and analyze functions
		content, err := client.GetFileContent(ctx, s.Owner, s.Repo, fileChange.FilePath, "")
		if err != nil {
			fmt.Printf("## ❌ %s\n*Could not retrieve file content: %v*\n\n", fileChange.FilePath, err)
			continue
		}

		functions, err := parser.ExtractFunctions([]byte(content))
		if err != nil {
			fmt.Printf("## ❌ %s\n*Could not parse Go functions: %v*\n\n", fileChange.FilePath, err)
			continue
		}

		if len(functions) == 0 {
			continue
		}

		fmt.Printf("## 📁 %s\n\n", fileChange.FilePath)

		fileFunctionCount := 0
		fileChangedCount := 0

		for _, fn := range functions {
			isChanged := analysis.IsFunctionChanged(fn, fileChange.ChangedLines)
			totalFunctions++
			if isChanged {
				changedFunctions++
				fileChangedCount++
			}

			if s.OnlyChanged && !isChanged {
				continue
			}

			fileFunctionCount++

			status := "📝"
			if isChanged {
				status = "🔄"
			}

			fmt.Printf("### %s %s\n\n", status, fn.Name)
			fmt.Printf("- **Type:** %s\n", getTypeDescription(fn))
			fmt.Printf("- **Lines:** %d-%d\n", fn.StartLine, fn.EndLine)
			fmt.Printf("- **Exported:** %t\n", fn.IsExported)
			if isChanged {
				fmt.Printf("- **Status:** Changed in this PR\n")
			}
			if fn.Receiver != "" {
				fmt.Printf("- **Receiver:** %s\n", fn.Receiver)
			}

			if s.ShowBody && fn.Body != "" {
				fmt.Printf("\n**Code:**\n```go\n%s\n```\n", fn.Body)
			}

			fmt.Printf("\n")
		}

		if fileFunctionCount > 0 {
			fmt.Printf("*File Summary: %d functions", fileFunctionCount)
			if fileChangedCount > 0 {
				fmt.Printf(" (%d changed)", fileChangedCount)
			}
			fmt.Printf("*\n\n")
		}
	}

	fmt.Printf("---\n\n")
	fmt.Printf("**Overall Summary:**\n")
	fmt.Printf("- Total Functions: %d\n", totalFunctions)
	fmt.Printf("- Changed Functions: %d\n", changedFunctions)
	if totalFunctions > 0 {
		fmt.Printf("- Change Rate: %.1f%%\n", float64(changedFunctions)/float64(totalFunctions)*100)
	}

	return nil
}

// Implement GlazeCommand for structured output
func (c *FunctionsDualCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &FunctionsDualSettings{}
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
		if !strings.HasSuffix(fileChange.FilePath, ".go") {
			continue
		}

		// Get file content and analyze functions
		content, err := client.GetFileContent(ctx, s.Owner, s.Repo, fileChange.FilePath, "")
		if err != nil {
			continue
		}

		functions, err := parser.ExtractFunctions([]byte(content))
		if err != nil {
			continue
		}

		for _, fn := range functions {
			isChanged := analysis.IsFunctionChanged(fn, fileChange.ChangedLines)

			if s.OnlyChanged && !isChanged {
				continue
			}

			row := types.NewRow(
				types.MRP("owner", s.Owner),
				types.MRP("repo", s.Repo),
				types.MRP("pr_number", s.PRNumber),
				types.MRP("file_path", fileChange.FilePath),
				types.MRP("function_name", fn.Name),
				types.MRP("receiver", fn.Receiver),
				types.MRP("start_line", fn.StartLine),
				types.MRP("end_line", fn.EndLine),
				types.MRP("is_exported", fn.IsExported),
				types.MRP("is_changed", isChanged),
				types.MRP("signature", fn.Signature),
			)

			if s.ShowBody {
				row.Set("body", fn.Body)
			}

			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	}

	return nil
}

func getTypeDescription(fn *treesitter.Function) string {
	if fn.Receiver != "" {
		return "Method"
	}
	return "Function"
}

// Ensure both interfaces are implemented
var _ cmds.BareCommand = &FunctionsDualCommand{}
var _ cmds.GlazeCommand = &FunctionsDualCommand{}

func NewFunctionsDualCommand() (*FunctionsDualCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"functions",
		cmds.WithShort("Analyze Go functions affected by pull request changes"),
		cmds.WithLong("Uses tree-sitter to analyze Go functions in files changed by a pull request, showing which functions were modified. Use --output for structured data formats."),
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
			parameters.NewParameterDefinition(
				"show-body",
				parameters.ParameterTypeBool,
				parameters.WithDefault(false),
				parameters.WithHelp("Include function body in output"),
			),
			parameters.NewParameterDefinition(
				"only-changed",
				parameters.ParameterTypeBool,
				parameters.WithDefault(false),
				parameters.WithHelp("Show only functions that were changed in the PR"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &FunctionsDualCommand{
		CommandDescription: cmdDesc,
	}, nil
}
