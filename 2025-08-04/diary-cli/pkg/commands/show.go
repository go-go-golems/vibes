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

// ShowCommand implements both BareCommand and GlazeCommand for dual output
type ShowCommand struct {
	*cmds.CommandDescription
	config *config.Config
}

// ShowSettings holds the command parameters
type ShowSettings struct {
	Date      time.Time `glazed.parameter:"date"`
	EntryType string    `glazed.parameter:"type"`
	Limit     int       `glazed.parameter:"limit"`
}

// NewShowCommand creates a new show command
func NewShowCommand(cfg *config.Config) *ShowCommand {
	return &ShowCommand{
		CommandDescription: buildShowCommandDescription(),
		config:             cfg,
	}
}

// buildShowCommandDescription creates the command description with parameters
func buildShowCommandDescription() *cmds.CommandDescription {
	glazedLayer, _ := settings.NewGlazedParameterLayers()

	return cmds.NewCommandDescription(
		"show",
		cmds.WithShort("Show diary entries for a specific date"),
		cmds.WithLong(`Show diary entries for a specific date with optional filtering.

Supports multiple output formats including JSON, YAML, CSV, and tables.

Examples:
  diary show                                   # Show today's entries
  diary show --date yesterday                 # Show yesterday's entries
  diary show --date "2025-08-01"             # Show entries for specific date
  diary show --type todo                     # Show only todos for today
  diary show --date "last friday" --output json  # JSON output for last Friday`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"date",
				parameters.ParameterTypeDate,
				parameters.WithDefault("today"),
				parameters.WithHelp("Date to show entries for"),
				parameters.WithShortFlag("d"),
			),
			parameters.NewParameterDefinition(
				"limit",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(50),
				parameters.WithHelp("Maximum number of entries to show"),
				parameters.WithShortFlag("l"),
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
func (c *ShowCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	settings := &ShowSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	storage := storage.NewMarkdownStorage(c.config)

	// Get entries for the specified date
	var entryType diarytypes.EntryType
	if settings.EntryType != "" {
		entryType = diarytypes.EntryType(settings.EntryType)
	}
	
	entries, err := storage.GetEntries(settings.Date, entryType)
	if err != nil {
		return fmt.Errorf("failed to get entries: %w", err)
	}

	// Apply limit
	if settings.Limit > 0 && len(entries) > settings.Limit {
		entries = entries[:settings.Limit]
	}

	// Display human-readable output
	if len(entries) == 0 {
		fmt.Printf("No entries found for %s.\n", settings.Date.Format("2006-01-02"))
		return nil
	}

	fmt.Printf("📅 Entries for %s (%d total):\n\n", settings.Date.Format("Monday, January 2, 2006"), len(entries))
	
	for _, entry := range entries {
		displayShowEntry(entry)
		fmt.Println()
	}

	return nil
}

// RunIntoGlazeProcessor implements GlazeCommand for structured output
func (c *ShowCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ShowSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	storage := storage.NewMarkdownStorage(c.config)

	// Get entries for the specified date
	var entryType diarytypes.EntryType
	if settings.EntryType != "" {
		entryType = diarytypes.EntryType(settings.EntryType)
	}
	
	entries, err := storage.GetEntries(settings.Date, entryType)
	if err != nil {
		return fmt.Errorf("failed to get entries: %w", err)
	}

	// Apply limit
	if settings.Limit > 0 && len(entries) > settings.Limit {
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

// displayShowEntry displays a single entry in show format
func displayShowEntry(entry *diarytypes.DiaryEntry) {
	// Header with type and title
	fmt.Printf("%s %s", getTypeEmoji(entry.Type), formatEntryType(entry.Type))
	if entry.Title != "" {
		fmt.Printf(": %s", entry.Title)
	}
	fmt.Println()

	// Content
	if entry.Content != "" {
		fmt.Printf("   %s\n", entry.Content)
	}

	// Additional info for todos
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

	// URL for links
	if entry.URL != "" {
		fmt.Printf("   🔗 %s\n", entry.URL)
	}

	// Subtitle info
	if entry.SubtitleSlug != "" {
		fmt.Printf("   📂 Subtitle: %s\n", entry.SubtitleSlug)
	}

	// File location
	fmt.Printf("   📁 %s:%d\n", entry.File, entry.LineNum)
}

// getTypeEmoji returns an emoji for the entry type
func getTypeEmoji(entryType diarytypes.EntryType) string {
	switch entryType {
	case diarytypes.EntryTypeTIL:
		return "💡"
	case diarytypes.EntryTypeThought:
		return "💭"
	case diarytypes.EntryTypeDid:
		return "✅"
	case diarytypes.EntryTypeLink:
		return "🔗"
	case diarytypes.EntryTypeTodo:
		return "📝"
	default:
		return "📄"
	}
}

