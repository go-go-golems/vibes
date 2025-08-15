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

type InitCommand struct {
	*cmds.CommandDescription
}

type InitSettings struct {
	Files     []string `glazed.parameter:"files"`
	Title     string   `glazed.parameter:"title"`
	Status    string   `glazed.parameter:"status"`
	Recursive bool     `glazed.parameter:"recursive"`
	Force     bool     `glazed.parameter:"force"`
}

func NewInitCommand() (*InitCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	return &InitCommand{
		CommandDescription: cmds.NewCommandDescription(
			"init",
			cmds.WithShort("Initialize metadata in markdown files"),
			cmds.WithLong(`Initialize YAML frontmatter metadata in markdown files.

This command adds structured metadata to existing markdown files or creates
new files with metadata. It preserves existing content and skips files that
already have metadata unless --force is used.

Examples:
  mdmeta init doc.md --title "My Document"
  mdmeta init docs/ --recursive
  mdmeta init new.md --title "New Doc" --status draft`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"files",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Files or directories to initialize"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"title",
					parameters.ParameterTypeString,
					parameters.WithHelp("Document title (defaults to filename)"),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeChoice,
					parameters.WithHelp("Document status"),
					parameters.WithChoices("draft", "in_progress", "review", "final", "archived"),
					parameters.WithDefault("draft"),
				),
				parameters.NewParameterDefinition(
					"recursive",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Process directories recursively"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"force",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Overwrite existing metadata"),
					parameters.WithDefault(false),
				),
			),
			cmds.WithLayersList(
				glazedParameterLayer,
			),
		),
	}, nil
}

func (c *InitCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &InitSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	parser := metadata.NewParser(false) // Use permissive mode for init

	var filesToProcess []string

	// Collect files to process
	for _, file := range settings.Files {
		err := c.collectFiles(file, settings.Recursive, &filesToProcess)
		if err != nil {
			return fmt.Errorf("failed to collect files from %s: %w", file, err)
		}
	}

	// Process each file
	for _, file := range filesToProcess {
		result, err := c.processFile(parser, file, settings)
		if err != nil {
			result = types.NewRow(
				types.MRP("file", file),
				types.MRP("status", "error"),
				types.MRP("message", err.Error()),
			)
		}

		if err := gp.AddRow(ctx, result); err != nil {
			return err
		}
	}

	return nil
}

func (c *InitCommand) collectFiles(path string, recursive bool, files *[]string) error {
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

func (c *InitCommand) processFile(parser *metadata.Parser, filename string, settings *InitSettings) (types.Row, error) {
	// Check if file already has metadata
	hasMetadata, err := metadata.HasMetadata(filename)
	if err != nil {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "error"),
			types.MRP("message", err.Error()),
		), err
	}

	if hasMetadata && !settings.Force {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "skipped"),
			types.MRP("message", "already has metadata (use --force to overwrite)"),
		), nil
	}

	// Determine title
	title := settings.Title
	if title == "" {
		title = strings.TrimSuffix(filepath.Base(filename), ".md")
		title = strings.ReplaceAll(title, "-", " ")
		title = strings.ReplaceAll(title, "_", " ")
		title = strings.Title(title)
	}

	// Create new document
	doc := metadata.NewDocument(title)
	doc.Status = settings.Status
	doc.Path = filename

	// Read existing content if file exists
	content := fmt.Sprintf("# %s\n\nDocument content goes here.\n", title)
	if _, err := os.Stat(filename); err == nil {
		if hasMetadata {
			// Parse existing file to preserve content
			_, existingContent, parseErr := parser.ParseFile(filename)
			if parseErr == nil {
				content = existingContent
			}
		} else {
			// Read entire file as content
			existingContent, readErr := os.ReadFile(filename)
			if readErr == nil {
				content = string(existingContent)
			}
		}
	}

	// Write file with metadata
	err = parser.WriteFile(filename, doc, content)
	if err != nil {
		return types.NewRow(
			types.MRP("file", filename),
			types.MRP("status", "error"),
			types.MRP("message", fmt.Sprintf("failed to write file: %v", err)),
		), err
	}

	status := "initialized"
	if hasMetadata && settings.Force {
		status = "updated"
	}

	return types.NewRow(
		types.MRP("file", filename),
		types.MRP("status", status),
		types.MRP("doc_id", doc.DocID),
		types.MRP("title", doc.Title),
		types.MRP("message", "metadata added successfully"),
	), nil
}

