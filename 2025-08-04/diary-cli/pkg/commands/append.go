package commands

import (
	"fmt"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"diary-cli/pkg/config"
	"diary-cli/pkg/storage"
	"diary-cli/pkg/types"
	"diary-cli/pkg/ui"
)

// NewAppendCommand creates the append command
func NewAppendCommand(cfg *config.Config) *cobra.Command {
	var (
		subtitleSlug string
		editor       bool
	)

	cmd := &cobra.Command{
		Use:   "append [subtitle-slug] [content]",
		Short: "Append content to existing diary entries",
		Long: `Append additional content to existing diary entries.

You can either use interactive mode to select an entry, or specify
a subtitle slug directly to append to a specific section.

Examples:
  diary append                                    # Interactive mode
  diary append "interface-concepts" "More notes"  # Direct append
  diary append --editor                           # Interactive with editor
  diary append "learning-go" --subtitle "advanced" "Channel patterns"`,
		RunE: func(cmd *cobra.Command, args []string) error {
			storage := storage.NewMarkdownStorage(cfg)

			// Interactive mode if no arguments
			if len(args) == 0 {
				return runInteractiveAppend(storage, editor)
			}

			// Direct mode
			if len(args) < 2 {
				return fmt.Errorf("direct mode requires both subtitle-slug and content")
			}

			targetSlug := args[0]
			content := strings.Join(args[1:], " ")

			return runDirectAppend(storage, targetSlug, content, subtitleSlug, editor)
		},
	}

	cmd.Flags().StringVar(&subtitleSlug, "subtitle", "", "Create new subtitle section")
	cmd.Flags().BoolVarP(&editor, "editor", "e", false, "Open in visual editor")

	return cmd
}

// runInteractiveAppend handles interactive append mode
func runInteractiveAppend(storage *storage.MarkdownStorage, forceEditor bool) error {
	// Get today's entries to show as options
	entries, err := getTodaysEntries(storage)
	if err != nil {
		return fmt.Errorf("failed to get today's entries: %w", err)
	}

	if len(entries) == 0 {
		return fmt.Errorf("no entries found for today to append to")
	}

	// Format entries for selection
	entryOptions := make([]string, len(entries))
	for i, entry := range entries {
		title := fmt.Sprintf("%s: %s", strings.Title(string(entry.Type)), entry.Title)
		if entry.Title == "" {
			title = fmt.Sprintf("%s: %s", strings.Title(string(entry.Type)), entry.Content)
			if len(title) > 60 {
				title = title[:60] + "..."
			}
		}
		entryOptions[i] = title
	}

	// Show interactive form
	form, err := ui.ShowAppendForm(entryOptions)
	if err != nil {
		return fmt.Errorf("interactive form failed: %w", err)
	}

	// Find the selected entry
	selectedEntry := entries[form.SelectedIndex]

	// Handle editor if requested
	content := form.Content
	if form.UseEditor || forceEditor {
		// For now, just use the content as-is
		// In a full implementation, you'd open the editor with context
		fmt.Println("Editor integration for append not yet fully implemented")
	}

	// Perform append operation
	if err := storage.AppendToEntry(selectedEntry, form.SubtitleSlug, content); err != nil {
		return fmt.Errorf("failed to append entry: %w", err)
	}
	// Print appended content and path
	fmt.Println("Appended content:")
	fmt.Println(content)
	fmt.Printf("Appended to: %s\n", selectedEntry.File)
	return nil
}

// runDirectAppend handles direct append mode
func runDirectAppend(storage *storage.MarkdownStorage, targetSlug, content, newSubtitle string, editor bool) error {
	// Handle editor if requested
	if editor {
		fmt.Println("Editor integration for append not yet fully implemented")
	}

	// Find entry by subtitle slug
	entry, err := storage.FindEntryBySubtitleSlug(targetSlug)
	if err != nil {
		return fmt.Errorf("failed to find entry with slug '%s': %w", targetSlug, err)
	}

	// Perform append operation
	if err := storage.AppendToEntry(entry, newSubtitle, content); err != nil {
		return fmt.Errorf("failed to append entry: %w", err)
	}
	// Print appended content and path
	fmt.Println("Appended content:")
	fmt.Println(content)
	fmt.Printf("Appended to: %s\n", entry.File)
	return nil
}

// getTodaysEntries gets today's entries for selection
func getTodaysEntries(storage *storage.MarkdownStorage) ([]*types.DiaryEntry, error) {
	today := time.Now().Truncate(24 * time.Hour)
	return storage.GetEntries(today, "")
}
