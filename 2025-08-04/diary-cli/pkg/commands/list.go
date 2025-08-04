package commands

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

	"diary-cli/pkg/config"
	"diary-cli/pkg/storage"
	diarytypes "diary-cli/pkg/types"
)

// ListCommand implements both BareCommand and GlazeCommand for dual output
type ListCommand struct {
	*cmds.CommandDescription
	config *config.Config
}

// ListSettings holds the command parameters
type ListSettings struct {
	Limit     int       `glazed.parameter:"limit"`
	Since     time.Time `glazed.parameter:"since"`
	EntryType string    `glazed.parameter:"type"`
}

// NewListCommand creates a new list command
func NewListCommand(cfg *config.Config) *ListCommand {
	return &ListCommand{
		CommandDescription: buildListCommandDescription(),
		config:             cfg,
	}
}

// buildListCommandDescription creates the command description with parameters
func buildListCommandDescription() *cmds.CommandDescription {
	glazedLayer, _ := settings.NewGlazedParameterLayers()

	return cmds.NewCommandDescription(
		"list",
		cmds.WithShort("List recent diary entries"),
		cmds.WithLong(`List recent diary entries with optional filtering.

Supports multiple output formats including JSON, YAML, CSV, and tables.

Examples:
  diary list                           # List last 10 entries as table
  diary list --limit 20                # Show last 20 entries
  diary list --since "last week"       # Show entries since last week
  diary list --type til                # Show only TIL entries
  diary list --output json             # Output as JSON
  diary list --output csv --fields type,content,date  # CSV with specific fields`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"limit",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(10),
				parameters.WithHelp("Maximum number of entries to show"),
				parameters.WithShortFlag("l"),
			),
			parameters.NewParameterDefinition(
				"since",
				parameters.ParameterTypeDate,
				parameters.WithDefault("1 week ago"),
				parameters.WithHelp("Show entries since this date (today, yesterday, last week, YYYY-MM-DD)"),
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
func (c *ListCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	settings := &ListSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	storage := storage.NewMarkdownStorage(c.config)

	// Parse since date
	sinceDate := settings.Since

	// Get entries
	var entryType diarytypes.EntryType
	if settings.EntryType != "" {
		entryType = diarytypes.EntryType(settings.EntryType)
	}

	entries, err := storage.GetEntries(sinceDate, entryType)
	if err != nil {
		return fmt.Errorf("failed to get entries: %w", err)
	}

	// Limit results
	if len(entries) > settings.Limit {
		entries = entries[:settings.Limit]
	}

	// Display human-readable output
	if len(entries) == 0 {
		fmt.Println("No entries found.")
		return nil
	}

	fmt.Printf("Found %d entries:\n\n", len(entries))
	for _, entry := range entries {
		displayEntry(entry)
		fmt.Println()
	}

	return nil
}

// RunIntoGlazeProcessor implements GlazeCommand for structured output
func (c *ListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	storage := storage.NewMarkdownStorage(c.config)

	// Parse since date
	sinceDate := settings.Since

	// Get entries
	var entryType diarytypes.EntryType
	if settings.EntryType != "" {
		entryType = diarytypes.EntryType(settings.EntryType)
	}

	entries, err := storage.GetEntries(sinceDate, entryType)
	if err != nil {
		return fmt.Errorf("failed to get entries: %w", err)
	}

	// Limit results
	if len(entries) > settings.Limit {
		entries = entries[:settings.Limit]
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

// displayEntry displays a single entry in human-readable format
func displayEntry(entry *diarytypes.DiaryEntry) {
	// Header with type and date
	fmt.Printf("📝 %s", formatEntryType(entry.Type))
	if entry.Title != "" {
		fmt.Printf(": %s", entry.Title)
	}
	fmt.Printf(" (%s)\n", entry.Date.Format("2006-01-02 15:04"))

	// Content
	if entry.Title != "" {
		fmt.Printf("   %s\n", entry.Content)
	} else {
		// Show truncated content if no title
		content := entry.Content
		if len(content) > 100 {
			content = content[:100] + "..."
		}
		fmt.Printf("   %s\n", content)
	}

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
		
		if entry.TaskID != "" {
			fmt.Printf(" | ID: %s", entry.TaskID)
		}
		fmt.Println()
	}

	if entry.URL != "" {
		fmt.Printf("   🔗 %s\n", entry.URL)
	}

	if len(entry.Tags) > 0 {
		fmt.Printf("   🏷️  %s\n", formatTags(entry.Tags))
	}

	fmt.Printf("   📁 %s:%d\n", entry.File, entry.LineNum)
}

// formatEntryType formats entry type for display
func formatEntryType(entryType diarytypes.EntryType) string {
	switch entryType {
	case diarytypes.EntryTypeTIL:
		return "TIL"
	case diarytypes.EntryTypeThought:
		return "Thought"
	case diarytypes.EntryTypeDid:
		return "Activity"
	case diarytypes.EntryTypeLink:
		return "Link"
	case diarytypes.EntryTypeTodo:
		return "Todo"
	default:
		return string(entryType)
	}
}

// formatTags formats tags for display
func formatTags(tags []string) string {
	if len(tags) == 0 {
		return ""
	}
	
	result := ""
	for i, tag := range tags {
		if i > 0 {
			result += " "
		}
		result += "#" + tag
	}
	return result
}




