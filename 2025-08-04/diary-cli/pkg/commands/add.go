package commands

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/spf13/cobra"
	"github.com/tj/go-naturaldate"

	"diary-cli/pkg/config"
	"diary-cli/pkg/storage"
	"diary-cli/pkg/types"
	"diary-cli/pkg/ui"
)

// NewAddCommand creates the add command
func NewAddCommand(cfg *config.Config) *cobra.Command {
	var (
		entryType    string
		format       string
		title        string
		date         string
		editor       bool
		subtitleSlug string
		url          string
	)

	cmd := &cobra.Command{
		Use:   "add [type] [content]",
		Short: "Add a new diary entry",
		Long: `Add a new diary entry with the specified type and content.

Entry types: til, thought, did, link
Formats: default, markdown, task

Examples:
  diary add                                    # Interactive mode
  diary add til "Learned about Go interfaces"  # Direct mode
  diary add thought "Architecture ideas" --title "System Design"
  diary add link "https://example.com" --title "Interesting Article"
  diary add --format task til "Task format entry"`,
		RunE: func(cmd *cobra.Command, args []string) error {
			storage := storage.NewMarkdownStorage(cfg)

			// Interactive mode if no arguments
			if len(args) == 0 {
				return runInteractiveAdd(storage, editor)
			}

			// Direct mode
			if len(args) < 2 {
				return fmt.Errorf("direct mode requires both type and content")
			}

			entryType = args[0]
			content := strings.Join(args[1:], " ")

			return runDirectAdd(storage, entryType, content, format, title, date, editor, subtitleSlug, url)
		},
	}

	cmd.Flags().StringVarP(&format, "format", "f", "default", "Entry format (default, markdown, task)")
	cmd.Flags().StringVarP(&title, "title", "t", "", "Entry title")
	cmd.Flags().StringVarP(&date, "date", "d", "today", "Entry date (today, yesterday, YYYY-MM-DD)")
	cmd.Flags().BoolVarP(&editor, "editor", "e", false, "Open in visual editor")
	cmd.Flags().StringVar(&subtitleSlug, "subtitle", "", "Subtitle slug for organization")
	cmd.Flags().StringVar(&url, "url", "", "URL for link entries")

	return cmd
}

// runInteractiveAdd handles interactive entry creation
func runInteractiveAdd(storage *storage.MarkdownStorage, forceEditor bool) error {
	form, err := ui.ShowAddForm()
	if err != nil {
		return fmt.Errorf("interactive form failed: %w", err)
	}

	// Parse date
	entryDate, err := parseDate(form.Date)
	if err != nil {
		return fmt.Errorf("invalid date: %w", err)
	}

	// Create entry
	entry := &types.DiaryEntry{
		Type:         types.EntryType(form.EntryType),
		Title:        form.Title,
		Content:      form.Content,
		Date:         entryDate,
		Format:       types.Format(form.Format),
		SubtitleSlug: form.SubtitleSlug,
	}

	// Handle editor if requested
	if form.UseEditor || forceEditor {
		if err := openEditor(entry); err != nil {
			return fmt.Errorf("editor failed: %w", err)
		}
	}

	// Add entry
	if err := storage.AddEntry(entry); err != nil {
		return fmt.Errorf("failed to add entry: %w", err)
	}

	fmt.Printf("✓ Added %s entry: %s\n", entry.Type, getTitleOrContent(entry))
	return nil
}

// runDirectAdd handles direct entry creation
func runDirectAdd(storage *storage.MarkdownStorage, entryType, content, format, title, date string, editor bool, subtitleSlug, url string) error {
	// Validate entry type
	if !types.EntryType(entryType).IsValid() {
		return fmt.Errorf("invalid entry type: %s (valid: til, thought, did, link)", entryType)
	}

	// Validate format
	if !types.Format(format).IsValid() {
		return fmt.Errorf("invalid format: %s (valid: default, markdown, task)", format)
	}

	// Parse date
	entryDate, err := parseDate(date)
	if err != nil {
		return fmt.Errorf("invalid date: %w", err)
	}

	// Create entry
	entry := &types.DiaryEntry{
		Type:         types.EntryType(entryType),
		Title:        title,
		Content:      content,
		Date:         entryDate,
		Format:       types.Format(format),
		SubtitleSlug: subtitleSlug,
		URL:          url,
	}

	// Handle editor if requested
	if editor {
		if err := openEditor(entry); err != nil {
			return fmt.Errorf("editor failed: %w", err)
		}
	}

	// Add entry
	if err := storage.AddEntry(entry); err != nil {
		return fmt.Errorf("failed to add entry: %w", err)
	}

	fmt.Printf("✓ Added %s entry: %s\n", entry.Type, getTitleOrContent(entry))
	return nil
}

// parseDate parses a date string using natural language
func parseDate(dateStr string) (time.Time, error) {
	if dateStr == "" || dateStr == "today" {
		return time.Now(), nil
	}

	// Try natural language parsing first
	if parsed, err := naturaldate.Parse(dateStr, time.Now()); err == nil {
		return parsed, nil
	}

	// Try standard formats
	formats := []string{
		"2006-01-02",
		"01/02/2006",
		"01-02-2006",
		"2006/01/02",
	}

	for _, format := range formats {
		if parsed, err := time.Parse(format, dateStr); err == nil {
			return parsed, nil
		}
	}

	return time.Time{}, fmt.Errorf("unable to parse date: %s", dateStr)
}

// openEditor opens the configured editor with entry content
func openEditor(entry *types.DiaryEntry) error {
	// Create temporary file
	tmpFile, err := os.CreateTemp("", "diary-*.md")
	if err != nil {
		return fmt.Errorf("failed to create temp file: %w", err)
	}
	defer os.Remove(tmpFile.Name())

	// Write current content to temp file
	content := formatEditorContent(entry)
	if _, err := tmpFile.WriteString(content); err != nil {
		return fmt.Errorf("failed to write temp file: %w", err)
	}
	tmpFile.Close()

	// Get editor command
	editorCmd := getEditorCommand()
	
	// Open editor
	cmd := exec.Command(editorCmd, tmpFile.Name())
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("editor command failed: %w", err)
	}

	// Read modified content
	modifiedContent, err := os.ReadFile(tmpFile.Name())
	if err != nil {
		return fmt.Errorf("failed to read modified content: %w", err)
	}

	// Parse modified content back to entry
	parseEditorContent(entry, string(modifiedContent))

	return nil
}

// formatEditorContent formats entry content for editing
func formatEditorContent(entry *types.DiaryEntry) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("# Entry: %s", strings.Title(string(entry.Type))))
	if entry.Title != "" {
		sb.WriteString(fmt.Sprintf(" - %s", entry.Title))
	}
	sb.WriteString("\n")
	sb.WriteString(fmt.Sprintf("# Date: %s\n", entry.Date.Format("2006-01-02")))
	if entry.SubtitleSlug != "" {
		sb.WriteString(fmt.Sprintf("# Subtitle: %s\n", entry.SubtitleSlug))
	}
	sb.WriteString("\n")
	sb.WriteString(entry.Content)
	sb.WriteString("\n\n")
	sb.WriteString("# Add your content above this line\n")
	sb.WriteString("# Lines starting with # will be ignored\n")

	return sb.String()
}

// parseEditorContent parses editor content back to entry
func parseEditorContent(entry *types.DiaryEntry, content string) {
	lines := strings.Split(content, "\n")
	var contentLines []string

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		contentLines = append(contentLines, line)
	}

	if len(contentLines) > 0 {
		entry.Content = strings.Join(contentLines, "\n")
	}
}

// getEditorCommand returns the editor command to use
func getEditorCommand() string {
	if editor := os.Getenv("VISUAL"); editor != "" {
		return editor
	}
	if editor := os.Getenv("EDITOR"); editor != "" {
		return editor
	}
	return "nano"
}

// getTitleOrContent returns title if present, otherwise content (truncated)
func getTitleOrContent(entry *types.DiaryEntry) string {
	if entry.Title != "" {
		return entry.Title
	}
	if len(entry.Content) > 50 {
		return entry.Content[:50] + "..."
	}
	return entry.Content
}

