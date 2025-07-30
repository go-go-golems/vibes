package analyze

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
	"github.com/pr-analyzer/pr-analyzer/internal/treesitter"
)

type FunctionHistoryDualCommand struct {
	*cmds.CommandDescription
}

type FunctionHistoryDualSettings struct {
	Owner        string `glazed.parameter:"owner"`
	Repo         string `glazed.parameter:"repo"`
	FilePath     string `glazed.parameter:"file-path"`
	FunctionName string `glazed.parameter:"function-name"`
	MaxCommits   int    `glazed.parameter:"max-commits"`
	ShowBody     bool   `glazed.parameter:"show-body"`
}

// Implement BareCommand for human-readable output
func (c *FunctionHistoryDualCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &FunctionHistoryDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	parser := treesitter.NewParser()

	// Get commit history for the file
	commits, err := client.GetFileCommits(ctx, s.Owner, s.Repo, s.FilePath)
	if err != nil {
		return fmt.Errorf("failed to get commit history: %w", err)
	}

	// Limit commits if requested
	if s.MaxCommits > 0 && len(commits) > s.MaxCommits {
		commits = commits[:s.MaxCommits]
	}

	fmt.Printf("# Function History: %s in %s\n\n", s.FunctionName, s.FilePath)
	fmt.Printf("**Repository:** %s/%s\n", s.Owner, s.Repo)
	fmt.Printf("**Function:** %s\n", s.FunctionName)
	fmt.Printf("**File:** %s\n", s.FilePath)
	fmt.Printf("**Commits analyzed:** %d\n\n", len(commits))

	functionFound := false
	for i, commit := range commits {
		// Extract commit information safely
		var sha, message, authorName string
		var commitDate string

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
			if commit.Commit.Author.Date != nil {
				commitDate = commit.Commit.Author.Date.Time.Format("2006-01-02 15:04:05")
			}
		}

		// Get file content at this commit
		content, err := client.GetFileContent(ctx, s.Owner, s.Repo, s.FilePath, sha)
		if err != nil {
			fmt.Printf("## 📝 Commit %d/%d: %s\n", i+1, len(commits), sha[:8])
			fmt.Printf("**Date:** %s\n", commitDate)
			fmt.Printf("**Author:** %s\n", authorName)
			fmt.Printf("**Message:** %s\n", message)
			fmt.Printf("❌ *Could not retrieve file content: %v*\n\n", err)
			continue
		}

		// Parse functions from this version
		functions, err := parser.ExtractFunctions([]byte(content))
		if err != nil {
			fmt.Printf("## 📝 Commit %d/%d: %s\n", i+1, len(commits), sha[:8])
			fmt.Printf("**Date:** %s\n", commitDate)
			fmt.Printf("**Author:** %s\n", authorName)
			fmt.Printf("**Message:** %s\n", message)
			fmt.Printf("❌ *Could not parse Go functions: %v*\n\n", err)
			continue
		}

		// Find the specific function
		var targetFunction *treesitter.Function
		for _, fn := range functions {
			if fn.Name == s.FunctionName {
				targetFunction = fn
				break
			}
		}

		fmt.Printf("## 📝 Commit %d/%d: %s\n", i+1, len(commits), sha[:8])
		fmt.Printf("**Date:** %s\n", commitDate)
		fmt.Printf("**Author:** %s\n", authorName)
		fmt.Printf("**Message:** %s\n", message)

		if targetFunction == nil {
			fmt.Printf("❌ *Function '%s' not found in this version*\n\n", s.FunctionName)
		} else {
			functionFound = true
			fmt.Printf("✅ *Function found*\n")
			fmt.Printf("- **Type:** %s\n", getTypeDescription(targetFunction))
			fmt.Printf("- **Lines:** %d-%d\n", targetFunction.StartLine, targetFunction.EndLine)
			fmt.Printf("- **Exported:** %t\n", targetFunction.IsExported)
			if targetFunction.Receiver != "" {
				fmt.Printf("- **Receiver:** %s\n", targetFunction.Receiver)
			}
			fmt.Printf("- **Signature:** `%s`\n", targetFunction.Signature)

			if s.ShowBody && targetFunction.Body != "" {
				fmt.Printf("\n**Code:**\n```go\n%s\n```\n", targetFunction.Body)
			}
		}
		fmt.Printf("\n")
	}

	if !functionFound {
		fmt.Printf("---\n\n")
		fmt.Printf("⚠️  **Warning:** Function '%s' was not found in any of the analyzed commits.\n", s.FunctionName)
		fmt.Printf("This could mean:\n")
		fmt.Printf("- The function name is incorrect\n")
		fmt.Printf("- The function was added after these commits\n")
		fmt.Printf("- The function was renamed or removed\n")
	}

	return nil
}

// Implement GlazeCommand for structured output
func (c *FunctionHistoryDualCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &FunctionHistoryDualSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	client := github.NewClient()
	parser := treesitter.NewParser()

	// Get commit history for the file
	commits, err := client.GetFileCommits(ctx, s.Owner, s.Repo, s.FilePath)
	if err != nil {
		return fmt.Errorf("failed to get commit history: %w", err)
	}

	// Limit commits if requested
	if s.MaxCommits > 0 && len(commits) > s.MaxCommits {
		commits = commits[:s.MaxCommits]
	}

	for i, commit := range commits {
		// Extract commit information safely
		var sha, message, authorName string
		var commitDate string

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
			if commit.Commit.Author.Date != nil {
				commitDate = commit.Commit.Author.Date.Time.Format("2006-01-02 15:04:05")
			}
		}

		// Get file content at this commit
		content, err := client.GetFileContent(ctx, s.Owner, s.Repo, s.FilePath, sha)
		if err != nil {
			// Add row indicating error
			row := types.NewRow(
				types.MRP("owner", s.Owner),
				types.MRP("repo", s.Repo),
				types.MRP("file_path", s.FilePath),
				types.MRP("function_name", s.FunctionName),
				types.MRP("commit_number", i+1),
				types.MRP("commit_sha", sha),
				types.MRP("commit_date", commitDate),
				types.MRP("author", authorName),
				types.MRP("message", message),
				types.MRP("function_found", false),
				types.MRP("error", err.Error()),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
			continue
		}

		// Parse functions from this version
		functions, err := parser.ExtractFunctions([]byte(content))
		if err != nil {
			// Add row indicating parse error
			row := types.NewRow(
				types.MRP("owner", s.Owner),
				types.MRP("repo", s.Repo),
				types.MRP("file_path", s.FilePath),
				types.MRP("function_name", s.FunctionName),
				types.MRP("commit_number", i+1),
				types.MRP("commit_sha", sha),
				types.MRP("commit_date", commitDate),
				types.MRP("author", authorName),
				types.MRP("message", message),
				types.MRP("function_found", false),
				types.MRP("parse_error", err.Error()),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
			continue
		}

		// Find the specific function
		var targetFunction *treesitter.Function
		for _, fn := range functions {
			if fn.Name == s.FunctionName {
				targetFunction = fn
				break
			}
		}

		row := types.NewRow(
			types.MRP("owner", s.Owner),
			types.MRP("repo", s.Repo),
			types.MRP("file_path", s.FilePath),
			types.MRP("function_name", s.FunctionName),
			types.MRP("commit_number", i+1),
			types.MRP("commit_sha", sha),
			types.MRP("commit_date", commitDate),
			types.MRP("author", authorName),
			types.MRP("message", message),
			types.MRP("function_found", targetFunction != nil),
		)

		if targetFunction != nil {
			row.Set("receiver", targetFunction.Receiver)
			row.Set("start_line", targetFunction.StartLine)
			row.Set("end_line", targetFunction.EndLine)
			row.Set("is_exported", targetFunction.IsExported)
			row.Set("signature", targetFunction.Signature)
			row.Set("function_type", getTypeDescription(targetFunction))

			if s.ShowBody {
				row.Set("body", targetFunction.Body)
			}
		}

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

// Ensure both interfaces are implemented
var _ cmds.BareCommand = &FunctionHistoryDualCommand{}
var _ cmds.GlazeCommand = &FunctionHistoryDualCommand{}

func NewFunctionHistoryDualCommand() (*FunctionHistoryDualCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"function-history",
		cmds.WithShort("Show the commit history of a specific Go function"),
		cmds.WithLong("Analyzes the commit history of a specific Go function in a repository, showing how it changed over time. Use --output for structured data formats."),
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
				parameters.WithHelp("Path to the Go file containing the function"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"function-name",
				parameters.ParameterTypeString,
				parameters.WithHelp("Name of the function to analyze"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"max-commits",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(20),
				parameters.WithHelp("Maximum number of commits to analyze"),
			),
			parameters.NewParameterDefinition(
				"show-body",
				parameters.ParameterTypeBool,
				parameters.WithDefault(false),
				parameters.WithHelp("Include function body in output"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &FunctionHistoryDualCommand{
		CommandDescription: cmdDesc,
	}, nil
}
