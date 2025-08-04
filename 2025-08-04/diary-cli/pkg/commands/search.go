package commands

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"

	"diary-cli/pkg/config"
	"diary-cli/pkg/storage"
	diarytypes "diary-cli/pkg/types"
)

// SearchCommand implements both BareCommand and GlazeCommand for dual output
type SearchCommand struct {
	*cmds.CommandDescription
	config *config.Config
}

// SearchSettings holds the command parameters
type SearchSettings struct {
	Query     string    `glazed.parameter:"query"`
	Since     time.Time `glazed.parameter:"since"`
	EntryType string    `glazed.parameter:"type"`
	Limit     int       `glazed.parameter:"limit"`
}

// NewSearchCommand creates a new search command
func NewSearchCommand(cfg *config.Config) *SearchCommand {
	return &SearchCommand{
		CommandDescription: buildSearchCommandDescription(),
		config:             cfg,
	}
}

// buildSearchCommandDescription creates the command description with parameters
func buildSearchCommandDescription() *cmds.CommandDescription {
	glazedLayer, _ := settings.NewGlazedParameterLayers()

	return cmds.NewCommandDescription(
		"search",
		cmds.WithShort("Search diary entries by content"),
		cmds.WithLong(`Search diary entries by content with optional filtering.

Supports multiple output formats including JSON, YAML, CSV, and tables.

Examples:
  diary search "authentication"                # Search for authentication
  diary search "go" --type til                # Search TIL entries for "go"
  diary search "microservices" --since "last month"  # Search recent entries
  diary search "api" --output json            # Output as JSON
  diary search "patterns" --limit 5           # Limit to 5 results`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"query",
				parameters.ParameterTypeString,
				parameters.WithHelp("Search query text"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"limit",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(20),
				parameters.WithHelp("Maximum number of results to show"),
				parameters.WithShortFlag("l"),
			),
			parameters.NewParameterDefinition(
				"since",
				parameters.ParameterTypeDate,
				parameters.WithDefault("1 month ago"),
				parameters.WithHelp("Search entries since this date"),
				parameters.WithShortFlag("s"),
			),
			parameters.NewParameterDefinition(
				"type",
				parameters.ParameterTypeChoice,
				parameters.WithChoices("til", "thought", "did", "link", "todo"),
				parameters.WithHelp("Filter by entry type"),
				parameters.WithShortFlag("t"),
			),
		),

		cmds.WithLayersList(glazedLayer),
	)
}

// Run implements BareCommand for human-readable output
func (c *SearchCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	settings := &SearchSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	storage := storage.NewMarkdownStorage(c.config)

	// Get entries
	var entryType diarytypes.EntryType
	if settings.EntryType != "" {
		entryType = diarytypes.EntryType(settings.EntryType)
	}

	entries, err := storage.SearchEntries(settings.Query, settings.Since, entryType, settings.Limit)
	if err != nil {
		return fmt.Errorf("failed to search entries: %w", err)
	}

	// Display human-readable output
	if len(entries) == 0 {
		fmt.Printf("No entries found matching '%s'.\n", settings.Query)
		return nil
	}

	fmt.Printf("Found %d entries matching '%s':\n\n", len(entries), settings.Query)
	for _, entry := range entries {
		displaySearchResult(entry, settings.Query)
		fmt.Println()
	}

	return nil
}

// RunIntoGlazeProcessor implements GlazeCommand for structured output
func (c *SearchCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &SearchSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	storage := storage.NewMarkdownStorage(c.config)

	// Get entries
	var entryType diarytypes.EntryType
	if settings.EntryType != "" {
		entryType = diarytypes.EntryType(settings.EntryType)
	}

	entries, err := storage.SearchEntries(settings.Query, settings.Since, entryType, settings.Limit)
	if err != nil {
		return fmt.Errorf("failed to search entries: %w", err)
	}

	// Output structured data
	for _, entry := range entries {
		row := types.NewRow(
			types.MRP("type", string(entry.Type)),
			types.MRP("title", entry.Title),
			types.MRP("content", entry.Content),
			types.MRP("subtitle", entry.Subtitle),
			types.MRP("subtitle_slug", entry.SubtitleSlug),
			types.MRP("date", entry.Date.Format("2006-01-02 15:04:05")),
			types.MRP("format", string(entry.Format)),
			types.MRP("file", entry.File),
			types.MRP("line_number", entry.LineNum),
			types.MRP("tags", entry.Tags),
			types.MRP("completed", entry.Completed),
		)

		// Add optional fields
		if entry.Priority != "" {
			row.Set("priority", string(entry.Priority))
		}
		if entry.DueDate != nil {
			row.Set("due_date", entry.DueDate.Format("2006-01-02"))
		}
		if entry.TaskID != "" {
			row.Set("task_id", entry.TaskID)
		}
		if entry.URL != "" {
			row.Set("url", entry.URL)
		}

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

// displaySearchResult displays a search result with highlighted query
func displaySearchResult(entry *diarytypes.DiaryEntry, query string) {
	// Header with type and date
	fmt.Printf("🔍 %s", formatEntryType(entry.Type))
	if entry.Title != "" {
		fmt.Printf(": %s", highlightQuery(entry.Title, query))
	}
	fmt.Printf(" (%s)\n", entry.Date.Format("2006-01-02 15:04"))

	// Content with highlighting
	content := entry.Content
	if len(content) > 200 {
		content = content[:200] + "..."
	}
	fmt.Printf("   %s\n", highlightQuery(content, query))

	// Additional info
	if entry.SubtitleSlug != "" {
		fmt.Printf("   📂 Subtitle: %s\n", entry.SubtitleSlug)
	}

	if entry.Type == diarytypes.EntryTypeTodo {
		status := "⏳ Pending"
		if entry.Completed {
			status = "✅ Completed"
		}
		fmt.Printf("   %s", status)
		
		if entry.Priority != "" {
			fmt.Printf(" | Priority: %s", entry.Priority)
		}
		
		if entry.DueDate != nil {
			fmt.Printf(" | Due: %s", entry.DueDate.Format("2006-01-02"))
		}
		fmt.Println()
	}

	if entry.URL != "" {
		fmt.Printf("   🔗 %s\n", entry.URL)
	}

	fmt.Printf("   📁 %s:%d\n", entry.File, entry.LineNum)
}

// highlightQuery highlights the search query in text (simple implementation)
func highlightQuery(text, query string) string {
	if query == "" {
		return text
	}
	
	// Simple case-insensitive highlighting
	lowerText := strings.ToLower(text)
	lowerQuery := strings.ToLower(query)
	
	if strings.Contains(lowerText, lowerQuery) {
		// For terminal output, we could use ANSI colors here
		// For now, just return the original text
		return text
	}
	
	return text
}

