package commands

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/mdmeta/mdmeta/pkg/metadata"
)

type ValidateCommand struct {
	*cmds.CommandDescription
}

type ValidateSettings struct {
	Paths     []string `glazed.parameter:"paths"`
	Strict    bool     `glazed.parameter:"strict"`
	Recursive bool     `glazed.parameter:"recursive"`
	Schema    string   `glazed.parameter:"schema"`
	Fix       bool     `glazed.parameter:"fix"`
}

func NewValidateCommand() (*ValidateCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	return &ValidateCommand{
		CommandDescription: cmds.NewCommandDescription(
			"validate",
			cmds.WithShort("Validate markdown metadata compliance"),
			cmds.WithLong(`Validate markdown files against metadata schema requirements.

This command checks markdown files for metadata compliance, schema validation,
and organizational policies. It can operate in strict or permissive modes
and optionally fix common issues automatically.

Examples:
  mdmeta validate docs/design/retry-policy.md
  mdmeta validate docs/ --recursive --strict
  mdmeta validate --schema mdmeta/v1 --fix`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"paths",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Files or directories to validate"),
					parameters.WithDefault([]string{"."}),
				),
				parameters.NewParameterDefinition(
					"strict",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Use strict validation mode"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"recursive",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Process directories recursively"),
					parameters.WithDefault(true),
				),
				parameters.NewParameterDefinition(
					"schema",
					parameters.ParameterTypeString,
					parameters.WithHelp("Required schema version"),
					parameters.WithDefault("mdmeta/v1"),
				),
				parameters.NewParameterDefinition(
					"fix",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Automatically fix common issues"),
					parameters.WithDefault(false),
				),
			),
			cmds.WithLayersList(
				glazedParameterLayer,
			),
		),
	}, nil
}

func (c *ValidateCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ValidateSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	parser := metadata.NewParser(settings.Strict)

	var filesToProcess []string

	// Collect files to process
	for _, path := range settings.Paths {
		err := c.collectFiles(path, settings.Recursive, &filesToProcess)
		if err != nil {
			return fmt.Errorf("failed to collect files from %s: %w", path, err)
		}
	}

	// Validate each file
	totalFiles := 0
	validFiles := 0
	errorFiles := 0
	fixedFiles := 0

	for _, file := range filesToProcess {
		totalFiles++
		result, err := c.validateFile(parser, file, settings)
		if err != nil {
			errorFiles++
			row := types.NewRow(
				types.MRP("file", file),
				types.MRP("status", "error"),
				types.MRP("message", err.Error()),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
			continue
		}

		status, _ := result.Get("status")
		switch status {
		case "valid":
			validFiles++
		case "fixed":
			fixedFiles++
		default:
			errorFiles++
		}

		if err := gp.AddRow(ctx, result); err != nil {
			return err
		}
	}

	// Add summary row
	summaryRow := types.NewRow(
		types.MRP("file", "SUMMARY"),
		types.MRP("total_files", totalFiles),
		types.MRP("valid_files", validFiles),
		types.MRP("error_files", errorFiles),
		types.MRP("fixed_files", fixedFiles),
	)
	if err := gp.AddRow(ctx, summaryRow); err != nil {
		return err
	}

	return nil
}

func (c *ValidateCommand) collectFiles(path string, recursive bool, files *[]string) error {
	info, err := os.Stat(path)
	if err != nil {
		return err
	}

	if info.IsDir() {
		if recursive {
			return filepath.Walk(path, func(filePath string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !info.IsDir() && strings.HasSuffix(strings.ToLower(filePath), ".md") {
					*files = append(*files, filePath)
				}
				return nil
			})
		} else {
			entries, err := os.ReadDir(path)
			if err != nil {
				return err
			}
			for _, entry := range entries {
				if !entry.IsDir() && strings.HasSuffix(strings.ToLower(entry.Name()), ".md") {
					*files = append(*files, filepath.Join(path, entry.Name()))
				}
			}
		}
	} else if strings.HasSuffix(strings.ToLower(path), ".md") {
		*files = append(*files, path)
	}

	return nil
}

func (c *ValidateCommand) validateFile(parser *metadata.Parser, filename string, settings *ValidateSettings) (types.Row, error) {
	// Check if file has metadata
	hasMetadata, err := metadata.HasMetadata(filename)
	if err != nil {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "error"),
			types.MRP("message", fmt.Sprintf("failed to check metadata: %v", err)),
		), err
	}

	if !hasMetadata {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "no_metadata"),
			types.MRP("message", "file has no YAML frontmatter"),
		), nil
	}

	// Parse the file
	doc, _, err := parser.ParseFile(filename)
	if err != nil {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "parse_error"),
			types.MRP("message", err.Error()),
		), nil
	}

	// Validate schema version
	if settings.Schema != "" && doc.Schema != settings.Schema {
		message := fmt.Sprintf("schema mismatch: expected %s, got %s", settings.Schema, doc.Schema)
		if settings.Fix {
			doc.Schema = settings.Schema
			if err := parser.WriteFile(filename, doc, ""); err != nil {
				return types.NewRow(
					types.MRP("file", filename),
					types.MRP("status", "fix_failed"),
					types.MRP("message", fmt.Sprintf("failed to fix schema: %v", err)),
				), nil
			}
			return types.NewRow(
				types.MRP("file", filename),
				types.MRP("status", "fixed"),
				types.MRP("message", "schema version updated"),
			), nil
		}
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "invalid"),
			types.MRP("message", message),
		), nil
	}

	// Perform additional validations
	issues := c.validateDocument(doc)
	
	if len(issues) == 0 {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "valid"),
			types.MRP("doc_id", doc.DocID),
			types.MRP("title", doc.Title),
			types.MRP("message", "validation passed"),
		), nil
	}

	// Try to fix issues if requested
	if settings.Fix {
		fixed := c.fixIssues(doc, issues)
		if len(fixed) > 0 {
			if err := parser.WriteFile(filename, doc, ""); err != nil {
				return types.NewRow(
					types.MRP("file", filename),
					types.MRP("status", "fix_failed"),
					types.MRP("message", fmt.Sprintf("failed to fix issues: %v", err)),
				), nil
			}
			return types.NewRow(
				types.MRP("file", filename),
				types.MRP("status", "fixed"),
				types.MRP("issues_fixed", strings.Join(fixed, ", ")),
				types.MRP("message", fmt.Sprintf("fixed %d issues", len(fixed))),
			), nil
		}
	}

	return types.NewRow(
		types.MRP("file", filename),
		types.MRP("status", "invalid"),
		types.MRP("issues", strings.Join(issues, "; ")),
		types.MRP("message", fmt.Sprintf("%d validation issues found", len(issues))),
	), nil
}

func (c *ValidateCommand) validateDocument(doc *metadata.Document) []string {
	var issues []string

	// Check required fields
	if doc.DocID == "" {
		issues = append(issues, "missing doc_id")
	} else if !strings.HasPrefix(doc.DocID, "ulid:") {
		issues = append(issues, "doc_id must start with 'ulid:'")
	}

	if doc.Title == "" {
		issues = append(issues, "missing title")
	}

	if doc.Schema == "" {
		issues = append(issues, "missing schema")
	}

	// Check valid values
	if doc.Status != "" && !metadata.IsValidStatus(doc.Status) {
		issues = append(issues, fmt.Sprintf("invalid status: %s", doc.Status))
	}

	if doc.Visibility != "" && !metadata.IsValidVisibility(doc.Visibility) {
		issues = append(issues, fmt.Sprintf("invalid visibility: %s", doc.Visibility))
	}

	if doc.DataClass != "" && !metadata.IsValidDataClass(doc.DataClass) {
		issues = append(issues, fmt.Sprintf("invalid data_class: %s", doc.DataClass))
	}

	// Check timestamps
	if doc.CreatedAt != nil && doc.UpdatedAt != nil && doc.UpdatedAt.Before(*doc.CreatedAt) {
		issues = append(issues, "updated_at is before created_at")
	}

	// Check review dates
	if doc.LastReviewedAt != nil && doc.NextReviewDueAt != nil && doc.NextReviewDueAt.Before(*doc.LastReviewedAt) {
		issues = append(issues, "next_review_due_at is before last_reviewed_at")
	}

	return issues
}

func (c *ValidateCommand) fixIssues(doc *metadata.Document, issues []string) []string {
	var fixed []string

	for _, issue := range issues {
		switch {
		case issue == "missing schema":
			doc.Schema = "mdmeta/v1"
			fixed = append(fixed, "added schema")
		case issue == "missing doc_id":
			doc.DocID = "ulid:" + "01K2N4YF0NGPW92JTDPBV2DWRS" // Generate new ULID
			fixed = append(fixed, "added doc_id")
		case strings.HasPrefix(issue, "invalid status:"):
			doc.Status = "draft"
			fixed = append(fixed, "reset status to draft")
		case strings.HasPrefix(issue, "invalid visibility:"):
			doc.Visibility = "internal"
			fixed = append(fixed, "reset visibility to internal")
		case strings.HasPrefix(issue, "invalid data_class:"):
			doc.DataClass = "none"
			fixed = append(fixed, "reset data_class to none")
		}
	}

	return fixed
}

