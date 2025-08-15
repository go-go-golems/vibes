package commands

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/mdmeta/mdmeta/pkg/metadata"
)

type ListCommand struct {
	*cmds.CommandDescription
}

type ListSettings struct {
	Paths        []string `glazed.parameter:"paths"`
	Status       string   `glazed.parameter:"status"`
	Owner        string   `glazed.parameter:"owner"`
	Tags         []string `glazed.parameter:"tags"`
	DueForReview bool     `glazed.parameter:"due-for-review"`
	Recursive    bool     `glazed.parameter:"recursive"`
	ShowContent  bool     `glazed.parameter:"show-content"`
	SortBy       string   `glazed.parameter:"sort-field"`
	Limit        int      `glazed.parameter:"limit"`
}

type DocumentInfo struct {
	Filename string
	Document *metadata.Document
	Content  string
	Error    error
}

func NewListCommand() (*ListCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	return &ListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"list",
			cmds.WithShort("List markdown documents with metadata"),
			cmds.WithLong(`List and filter markdown documents based on metadata criteria.

This command scans directories for markdown files with metadata and displays
them in a filterable, sortable table format. Supports various filtering
options including status, ownership, tags, and review dates.

Examples:
  mdmeta list docs/
  mdmeta list --status draft --owner john@example.com
  mdmeta list --tags design,reliability --sort-by updated_at
  mdmeta list --due-for-review --recursive`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"paths",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Paths to scan for markdown files"),
					parameters.WithDefault([]string{"."}),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeChoice,
					parameters.WithHelp("Filter by document status"),
					parameters.WithChoices("draft", "in_progress", "review", "final", "archived"),
				),
				parameters.NewParameterDefinition(
					"owner",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by document owner"),
				),
				parameters.NewParameterDefinition(
					"tags",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Filter by tags (comma-separated)"),
				),
				parameters.NewParameterDefinition(
					"due-for-review",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Show only documents due for review"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"recursive",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Scan directories recursively"),
					parameters.WithDefault(true),
				),
				parameters.NewParameterDefinition(
					"show-content",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Include content preview"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"sort-field",
					parameters.ParameterTypeChoice,
					parameters.WithHelp("Sort results by field"),
					parameters.WithChoices("title", "status", "updated_at", "created_at", "next_review_due_at"),
					parameters.WithDefault("updated_at"),
				),
				parameters.NewParameterDefinition(
					"limit",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Limit number of results"),
					parameters.WithDefault(0),
				),
			),
			cmds.WithLayersList(
				glazedParameterLayer,
			),
		),
	}, nil
}

func (c *ListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to initialize settings: %w", err)
	}

	parser := metadata.NewParser(false) // Use permissive mode

	var documents []DocumentInfo
	
	// Collect all markdown files
	for _, path := range settings.Paths {
		err := c.collectDocuments(parser, path, settings.Recursive, &documents)
		if err != nil {
			return fmt.Errorf("failed to collect documents from %s: %w", path, err)
		}
	}

	// Filter documents
	filtered := c.filterDocuments(documents, settings)

	// Sort documents
	c.sortDocuments(filtered, settings.SortBy)

	// Apply limit
	if settings.Limit > 0 && len(filtered) > settings.Limit {
		filtered = filtered[:settings.Limit]
	}

	// Output results
	for _, doc := range filtered {
		row := c.documentToRow(doc, settings.ShowContent)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

func (c *ListCommand) collectDocuments(parser *metadata.Parser, path string, recursive bool, documents *[]DocumentInfo) error {
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
					c.processFile(parser, filePath, documents)
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
					filePath := filepath.Join(path, entry.Name())
					c.processFile(parser, filePath, documents)
				}
			}
		}
	} else if strings.HasSuffix(strings.ToLower(path), ".md") {
		c.processFile(parser, path, documents)
	}

	return nil
}

func (c *ListCommand) processFile(parser *metadata.Parser, filename string, documents *[]DocumentInfo) {
	doc, content, err := parser.ParseFile(filename)
	*documents = append(*documents, DocumentInfo{
		Filename: filename,
		Document: doc,
		Content:  content,
		Error:    err,
	})
}

func (c *ListCommand) filterDocuments(documents []DocumentInfo, settings *ListSettings) []DocumentInfo {
	var filtered []DocumentInfo
	now := time.Now()

	for _, doc := range documents {
		// Skip documents with errors unless we want to show them
		if doc.Error != nil {
			continue
		}

		// Skip documents without metadata
		if doc.Document == nil || doc.Document.DocID == "" {
			continue
		}

		// Filter by status
		if settings.Status != "" && doc.Document.Status != settings.Status {
			continue
		}

		// Filter by owner
		if settings.Owner != "" {
			found := false
			for _, owner := range doc.Document.Owners {
				if strings.Contains(strings.ToLower(owner), strings.ToLower(settings.Owner)) {
					found = true
					break
				}
			}
			if !found {
				continue
			}
		}

		// Filter by tags
		if len(settings.Tags) > 0 {
			found := false
			for _, filterTag := range settings.Tags {
				for _, docTag := range doc.Document.Tags {
					if strings.EqualFold(docTag, filterTag) {
						found = true
						break
					}
				}
				if found {
					break
				}
			}
			if !found {
				continue
			}
		}

		// Filter by due for review
		if settings.DueForReview {
			if doc.Document.NextReviewDueAt == nil || doc.Document.NextReviewDueAt.After(now) {
				continue
			}
		}

		filtered = append(filtered, doc)
	}

	return filtered
}

func (c *ListCommand) sortDocuments(documents []DocumentInfo, sortBy string) {
	sort.Slice(documents, func(i, j int) bool {
		switch sortBy {
		case "title":
			return documents[i].Document.Title < documents[j].Document.Title
		case "status":
			return documents[i].Document.Status < documents[j].Document.Status
		case "created_at":
			if documents[i].Document.CreatedAt == nil {
				return false
			}
			if documents[j].Document.CreatedAt == nil {
				return true
			}
			return documents[i].Document.CreatedAt.Before(*documents[j].Document.CreatedAt)
		case "next_review_due_at":
			if documents[i].Document.NextReviewDueAt == nil {
				return false
			}
			if documents[j].Document.NextReviewDueAt == nil {
				return true
			}
			return documents[i].Document.NextReviewDueAt.Before(*documents[j].Document.NextReviewDueAt)
		default: // updated_at
			if documents[i].Document.UpdatedAt == nil {
				return false
			}
			if documents[j].Document.UpdatedAt == nil {
				return true
			}
			return documents[i].Document.UpdatedAt.After(*documents[j].Document.UpdatedAt)
		}
	})
}

func (c *ListCommand) documentToRow(doc DocumentInfo, showContent bool) types.Row {
	row := types.NewRow(
		types.MRP("file", doc.Filename),
		types.MRP("doc_id", doc.Document.DocID),
		types.MRP("title", doc.Document.Title),
		types.MRP("status", doc.Document.Status),
	)

	// Add optional fields
	if doc.Document.Slug != "" {
		row.Set("slug", doc.Document.Slug)
	}

	if len(doc.Document.Tags) > 0 {
		row.Set("tags", strings.Join(doc.Document.Tags, ", "))
	}

	if len(doc.Document.Owners) > 0 {
		row.Set("owners", strings.Join(doc.Document.Owners, ", "))
	}

	if doc.Document.CreatedAt != nil {
		row.Set("created_at", doc.Document.CreatedAt.Format(time.RFC3339))
	}

	if doc.Document.UpdatedAt != nil {
		row.Set("updated_at", doc.Document.UpdatedAt.Format(time.RFC3339))
	}

	if doc.Document.NextReviewDueAt != nil {
		row.Set("next_review_due_at", doc.Document.NextReviewDueAt.Format(time.RFC3339))
		
		// Add review status
		now := time.Now()
		if doc.Document.NextReviewDueAt.Before(now) {
			row.Set("review_status", "overdue")
		} else if doc.Document.NextReviewDueAt.Before(now.AddDate(0, 0, 7)) {
			row.Set("review_status", "due_soon")
		} else {
			row.Set("review_status", "current")
		}
	}

	if doc.Document.Summary != "" {
		row.Set("summary", doc.Document.Summary)
	}

	if showContent && doc.Content != "" {
		// Show first 200 characters of content
		content := doc.Content
		if len(content) > 200 {
			content = content[:200] + "..."
		}
		row.Set("content_preview", content)
	}

	return row
}

