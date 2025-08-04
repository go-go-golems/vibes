package storage

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/yuin/goldmark/ast"
	
	"diary-cli/pkg/config"
	"diary-cli/pkg/types"
)

// MarkdownStorage handles reading and writing diary entries to markdown files
type MarkdownStorage struct {
	config *config.Config
}

// NewMarkdownStorage creates a new markdown storage instance
func NewMarkdownStorage(cfg *config.Config) *MarkdownStorage {
	return &MarkdownStorage{config: cfg}
}

// AddEntry adds a new entry to the appropriate markdown file
func (ms *MarkdownStorage) AddEntry(entry *types.DiaryEntry) error {
	filePath := ms.config.GetDateFile(entry.Date.Format(ms.config.DateFormat))
	
	// Ensure directory exists
	if err := os.MkdirAll(filepath.Dir(filePath), 0755); err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	// Check if file exists, create if not
	if _, err := os.Stat(filePath); os.IsNotExist(err) {
		if err := ms.createDailyFile(filePath, entry.Date); err != nil {
			return fmt.Errorf("failed to create daily file: %w", err)
		}
	}

	// Format the entry
	entryText := ms.formatEntry(entry)

	// Add to file
	return ms.appendToFile(filePath, entryText)
}

// GetEntries retrieves entries from markdown files
func (ms *MarkdownStorage) GetEntries(since time.Time, entryType types.EntryType) ([]*types.DiaryEntry, error) {
	var entries []*types.DiaryEntry
	
	logsDir := ms.config.GetLogsDir()
	if _, err := os.Stat(logsDir); os.IsNotExist(err) {
		return entries, nil // No logs directory yet
	}

	// Walk through log files
	err := filepath.Walk(logsDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !strings.HasSuffix(path, ".md") {
			return nil
		}

		// Parse date from filename
		filename := strings.TrimSuffix(filepath.Base(path), ".md")
		fileDate, err := time.Parse(ms.config.DateFormat, filename)
		if err != nil {
			return nil // Skip files that don't match date format
		}

		if fileDate.Before(since) {
			return nil
		}

		// Parse entries from file
		fileEntries, err := ms.parseEntriesFromFile(path, fileDate)
		if err != nil {
			return err
		}

		// Filter by type if specified
		for _, entry := range fileEntries {
			if entryType == "" || entry.Type == entryType {
				entries = append(entries, entry)
			}
		}

		return nil
	})

	return entries, err
}

// createDailyFile creates a new daily markdown file with template
func (ms *MarkdownStorage) createDailyFile(filePath string, date time.Time) error {
	template := fmt.Sprintf(`# Log %s/%s

## To Process

`, date.Format("2006/01"), date.Format(ms.config.DateFormat))

	return os.WriteFile(filePath, []byte(template), 0644)
}

// formatEntry formats a diary entry according to its format type
func (ms *MarkdownStorage) formatEntry(entry *types.DiaryEntry) string {
	timestamp := entry.Date.Format("2006-01-02 15:04")
	
	switch entry.Format {
	case types.FormatTask:
		return ms.formatTaskEntry(entry, timestamp)
	case types.FormatMarkdown:
		return ms.formatMarkdownEntry(entry, timestamp)
	default: // FormatDefault
		return ms.formatDefaultEntry(entry, timestamp)
	}
}

// formatDefaultEntry formats entry in simple markdown format
func (ms *MarkdownStorage) formatDefaultEntry(entry *types.DiaryEntry, timestamp string) string {
	var sb strings.Builder
	
	// Title
	title := entry.Title
	if title == "" {
		title = entry.Content
		if len(title) > 50 {
			title = title[:50] + "..."
		}
	}
	
	sb.WriteString(fmt.Sprintf("## %s: %s\n", strings.Title(string(entry.Type)), title))
	
	// Subtitle if present
	if entry.SubtitleSlug != "" {
		sb.WriteString(fmt.Sprintf("### %s\n", entry.SubtitleSlug))
	}
	
	// Content (if different from title)
	if entry.Title != "" {
		sb.WriteString(entry.Content + "\n")
	}
	
	// URL for links
	if entry.Type == types.EntryTypeLink && entry.URL != "" {
		sb.WriteString(fmt.Sprintf("\nURL: %s\n", entry.URL))
	}
	
	sb.WriteString(fmt.Sprintf("\n*Added: %s*\n\n", timestamp))
	
	return sb.String()
}

// formatMarkdownEntry formats entry in enhanced markdown format
func (ms *MarkdownStorage) formatMarkdownEntry(entry *types.DiaryEntry, timestamp string) string {
	var sb strings.Builder
	
	// Title with metadata
	title := entry.Title
	if title == "" {
		title = entry.Content
		if len(title) > 50 {
			title = title[:50] + "..."
		}
	}
	
	sb.WriteString(fmt.Sprintf("## %s: %s\n", strings.Title(string(entry.Type)), title))
	
	// Metadata
	sb.WriteString(fmt.Sprintf("**Type:** %s  \n", entry.Type))
	sb.WriteString(fmt.Sprintf("**Date:** %s  \n", timestamp))
	if len(entry.Tags) > 0 {
		sb.WriteString(fmt.Sprintf("**Tags:** %s  \n", strings.Join(entry.Tags, ", ")))
	}
	sb.WriteString("\n")
	
	// Subtitle if present
	if entry.SubtitleSlug != "" {
		sb.WriteString(fmt.Sprintf("### %s\n", entry.SubtitleSlug))
	}
	
	// Content
	if entry.Title != "" {
		sb.WriteString(entry.Content + "\n")
	}
	
	// URL for links
	if entry.Type == types.EntryTypeLink && entry.URL != "" {
		sb.WriteString(fmt.Sprintf("\n**URL:** [%s](%s)\n", entry.URL, entry.URL))
	}
	
	sb.WriteString("\n---\n\n")
	
	return sb.String()
}

// formatTaskEntry formats entry in Obsidian Tasks format
func (ms *MarkdownStorage) formatTaskEntry(entry *types.DiaryEntry, timestamp string) string {
	var sb strings.Builder
	
	// Task checkbox
	checkbox := "- [ ]"
	if entry.Completed {
		checkbox = "- [x]"
	}
	
	// Main task line
	if entry.Type == types.EntryTypeTodo {
		sb.WriteString(fmt.Sprintf("%s %s", checkbox, entry.Content))
		
		// Due date for todos
		if entry.DueDate != nil {
			sb.WriteString(fmt.Sprintf(" 📅 %s", entry.DueDate.Format(ms.config.DateFormat)))
		}
		
		// Tags
		tags := []string{"#todo", "#toProcess"}
		if len(entry.Tags) > 0 {
			tags = append(tags, entry.Tags...)
		}
		sb.WriteString(fmt.Sprintf(" %s\n", strings.Join(tags, " ")))
		
		// Priority and metadata
		if entry.Priority != "" {
			sb.WriteString(fmt.Sprintf("  - Priority: %s\n", entry.Priority))
		}
		if entry.TaskID != "" {
			sb.WriteString(fmt.Sprintf("  - ID: %s\n", entry.TaskID))
		}
		
	} else {
		// Regular entry in task format
		title := entry.Title
		if title == "" {
			title = entry.Content
			if len(title) > 50 {
				title = title[:50] + "..."
			}
		}
		
		sb.WriteString(fmt.Sprintf("%s **%s**: %s #toProcess #%s\n", 
			checkbox, strings.ToUpper(string(entry.Type)), title, entry.Type))
		
		// Content as sub-item if different from title
		if entry.Title != "" {
			sb.WriteString(fmt.Sprintf("  - %s\n", entry.Content))
		}
		
		// Subtitle as sub-item
		if entry.SubtitleSlug != "" {
			sb.WriteString(fmt.Sprintf("  - **%s**: %s\n", entry.SubtitleSlug, entry.Content))
		}
	}
	
	sb.WriteString(fmt.Sprintf("  - Added: %s\n", timestamp))
	sb.WriteString("\n")
	
	return sb.String()
}

// appendToFile appends content to a file, inserting in the "To Process" section
func (ms *MarkdownStorage) appendToFile(filePath, content string) error {
	// Read existing file
	file, err := os.Open(filePath)
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}

	// Find "To Process" section and insert content
	toProcessIndex := -1
	for i, line := range lines {
		if strings.Contains(line, "## To Process") {
			toProcessIndex = i
			break
		}
	}

	if toProcessIndex == -1 {
		// No "To Process" section, append at end
		lines = append(lines, "", "## To Process", "", content)
	} else {
		// Insert after "To Process" header
		insertIndex := toProcessIndex + 1
		// Skip empty lines after header
		for insertIndex < len(lines) && strings.TrimSpace(lines[insertIndex]) == "" {
			insertIndex++
		}
		
		// Insert content
		newLines := make([]string, 0, len(lines)+strings.Count(content, "\n")+1)
		newLines = append(newLines, lines[:insertIndex]...)
		newLines = append(newLines, strings.Split(content, "\n")...)
		newLines = append(newLines, lines[insertIndex:]...)
		lines = newLines
	}

	// Write back to file
	return os.WriteFile(filePath, []byte(strings.Join(lines, "\n")), 0644)
}

// parseEntriesFromFile parses diary entries from a markdown file
func (ms *MarkdownStorage) parseEntriesFromFile(filePath string, fileDate time.Time) ([]*types.DiaryEntry, error) {
	var entries []*types.DiaryEntry
	
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineNum := 0
	
	for scanner.Scan() {
		lineNum++
		line := strings.TrimSpace(scanner.Text())
		
		// Parse task format entries
		if strings.HasPrefix(line, "- [ ]") || strings.HasPrefix(line, "- [x]") {
			entry := ms.parseTaskLine(line, filePath, lineNum, fileDate)
			if entry != nil {
				entries = append(entries, entry)
			}
		}
		
		// Parse markdown format entries (headers)
		if strings.HasPrefix(line, "## ") && !strings.Contains(line, "To Process") {
			entry := ms.parseMarkdownHeader(line, filePath, lineNum, fileDate)
			if entry != nil {
				entries = append(entries, entry)
			}
		}
	}

	return entries, scanner.Err()
}

// parseTaskLine parses a task format line into a diary entry
func (ms *MarkdownStorage) parseTaskLine(line, filePath string, lineNum int, fileDate time.Time) *types.DiaryEntry {
	// Regex to parse task lines
	// - [ ] **TYPE**: content #tags
	taskRegex := regexp.MustCompile(`^- \[(.)\] \*\*([A-Z]+)\*\*: (.+?)( #.+)?$`)
	todoRegex := regexp.MustCompile(`^- \[(.)\] (.+?)( 📅 \d{4}-\d{2}-\d{2})?( #.+)?$`)
	
	var entry *types.DiaryEntry
	
	if matches := taskRegex.FindStringSubmatch(line); len(matches) >= 4 {
		// Regular entry in task format
		completed := matches[1] == "x"
		entryType := types.EntryType(strings.ToLower(matches[2]))
		content := matches[3]
		tags := parseTags(matches[4])
		
		entry = &types.DiaryEntry{
			Type:      entryType,
			Content:   content,
			Date:      fileDate,
			Tags:      tags,
			File:      filePath,
			LineNum:   lineNum,
			Format:    types.FormatTask,
			Completed: completed,
		}
	} else if matches := todoRegex.FindStringSubmatch(line); len(matches) >= 3 {
		// Todo entry
		completed := matches[1] == "x"
		content := matches[2]
		
		// Parse due date if present
		var dueDate *time.Time
		if len(matches) >= 4 && matches[3] != "" {
			dueDateStr := strings.TrimPrefix(matches[3], " 📅 ")
			if parsed, err := time.Parse("2006-01-02", dueDateStr); err == nil {
				dueDate = &parsed
			}
		}
		
		tags := parseTags(matches[4])
		
		entry = &types.DiaryEntry{
			Type:      types.EntryTypeTodo,
			Content:   content,
			Date:      fileDate,
			Tags:      tags,
			File:      filePath,
			LineNum:   lineNum,
			Format:    types.FormatTask,
			Completed: completed,
			DueDate:   dueDate,
			TaskID:    uuid.New().String(), // Generate ID for existing todos
		}
	}
	
	return entry
}

// parseMarkdownHeader parses a markdown header into a diary entry
func (ms *MarkdownStorage) parseMarkdownHeader(line, filePath string, lineNum int, fileDate time.Time) *types.DiaryEntry {
	// Parse headers like "## TIL: Learning Go"
	headerRegex := regexp.MustCompile(`^## ([A-Za-z]+): (.+)$`)
	
	matches := headerRegex.FindStringSubmatch(line)
	if len(matches) < 3 {
		return nil
	}
	
	entryTypeStr := strings.ToLower(matches[1])
	title := matches[2]
	
	// Map header types to entry types
	var entryType types.EntryType
	switch entryTypeStr {
	case "til":
		entryType = types.EntryTypeTIL
	case "thought":
		entryType = types.EntryTypeThought
	case "did":
		entryType = types.EntryTypeDid
	case "link":
		entryType = types.EntryTypeLink
	default:
		return nil
	}
	
	return &types.DiaryEntry{
		Type:    entryType,
		Title:   title,
		Content: title, // Will be updated if we read more content
		Date:    fileDate,
		File:    filePath,
		LineNum: lineNum,
		Format:  types.FormatDefault, // Assume default for markdown headers
	}
}

// parseTags extracts tags from a tag string
func parseTags(tagStr string) []string {
	if tagStr == "" {
		return nil
	}
	
	tagRegex := regexp.MustCompile(`#\w+`)
	matches := tagRegex.FindAllString(tagStr, -1)
	
	// Remove # prefix
	tags := make([]string, len(matches))
	for i, tag := range matches {
		tags[i] = strings.TrimPrefix(tag, "#")
	}
	
	return tags
}


// parseEntryFromHeading extracts a diary entry from a heading node
func (ms *MarkdownStorage) parseEntryFromHeading(heading *ast.Heading, content []byte, filePath string, fileDate time.Time) *types.DiaryEntry {
	// Get heading text
	headingText := ms.getNodeText(heading, content)
	
	// Parse entry type and title from heading
	entryType, title := ms.parseHeadingContent(headingText)
	if entryType == "" {
		return nil
	}

	// Get the content following this heading
	entryContent := ms.getContentAfterHeading(heading, content)
	
	// Extract metadata like date, URL, etc.
	entry := &types.DiaryEntry{
		Type:    types.EntryType(entryType),
		Title:   title,
		Content: entryContent,
		Date:    fileDate,
		Format:  types.FormatDefault,
		File:    filePath,
		LineNum: ms.getLineNumber(heading, content),
	}

	// Parse additional metadata from content
	ms.parseEntryMetadata(entry, entryContent)
	
	return entry
}

// parseTaskEntry extracts a todo entry from a task list item
func (ms *MarkdownStorage) parseTaskEntry(listItem *ast.ListItem, content []byte, filePath string, fileDate time.Time) *types.DiaryEntry {
	// Get the task text
	taskText := ms.getNodeText(listItem, content)
	
	// Check if it's a task item with checkbox
	if !strings.Contains(taskText, "[ ]") && !strings.Contains(taskText, "[x]") {
		return nil
	}

	// Parse task content
	completed := strings.Contains(taskText, "[x]")
	taskContent := ms.parseTaskContent(taskText)
	
	// Extract task metadata
	entry := &types.DiaryEntry{
		Type:      types.EntryTypeTodo,
		Content:   taskContent,
		Date:      fileDate,
		Format:    types.FormatTask,
		File:      filePath,
		LineNum:   ms.getLineNumber(listItem, content),
		Completed: completed,
		Tags:      []string{"todo", "toProcess"},
	}

	// Parse additional task metadata (priority, due date, ID)
	ms.parseTaskMetadata(entry, taskText)
	
	return entry
}

// getNodeText extracts text content from an AST node using line-based approach
func (ms *MarkdownStorage) getNodeText(node ast.Node, content []byte) string {
	lines := node.Lines()
	if lines.Len() == 0 {
		return ""
	}
	
	var result []string
	for i := 0; i < lines.Len(); i++ {
		line := lines.At(i)
		lineText := string(line.Value(content))
		result = append(result, lineText)
	}
	
	return strings.Join(result, "\n")
}

// parseHeadingContent parses entry type and title from heading text
func (ms *MarkdownStorage) parseHeadingContent(headingText string) (string, string) {
	// Match patterns like "TIL: Title", "Thought: Title", "Link: Title", etc.
	patterns := map[string]string{
		`^TIL:\s*(.*)`:     "til",
		`^Thought:\s*(.*)`: "thought", 
		`^Did:\s*(.*)`:     "did",
		`^Link:\s*(.*)`:    "link",
	}

	for pattern, entryType := range patterns {
		re := regexp.MustCompile(pattern)
		if matches := re.FindStringSubmatch(headingText); len(matches) > 1 {
			return entryType, strings.TrimSpace(matches[1])
		}
	}

	return "", ""
}

// getContentAfterHeading extracts content that follows a heading until the next heading
func (ms *MarkdownStorage) getContentAfterHeading(heading *ast.Heading, content []byte) string {
	// This is a simplified implementation
	// In a full implementation, you'd walk the AST to get all content until the next heading
	lines := strings.Split(string(content), "\n")
	startLine := ms.getLineNumber(heading, content)
	
	var contentLines []string
	for i := startLine; i < len(lines); i++ {
		line := lines[i]
		// Stop at next heading of same or higher level
		if strings.HasPrefix(line, "##") {
			break
		}
		// Skip the heading line itself
		if i == startLine-1 {
			continue
		}
		contentLines = append(contentLines, line)
	}
	
	return strings.TrimSpace(strings.Join(contentLines, "\n"))
}

// getLineNumber gets the line number of a node in the content
func (ms *MarkdownStorage) getLineNumber(node ast.Node, content []byte) int {
	// Use Lines() method to get line information
	lines := node.Lines()
	if lines.Len() > 0 {
		return lines.At(0).Start + 1 // Lines are 0-based, convert to 1-based
	}
	return 1
}

// isTaskItem checks if a list item is a task item
func (ms *MarkdownStorage) isTaskItem(listItem *ast.ListItem, content []byte) bool {
	text := ms.getNodeText(listItem, content)
	return strings.Contains(text, "[ ]") || strings.Contains(text, "[x]")
}

// parseTaskContent extracts the main content from a task item
func (ms *MarkdownStorage) parseTaskContent(taskText string) string {
	lines := strings.Split(taskText, "\n")
	if len(lines) == 0 {
		return taskText
	}
	
	// Get the first line which contains the main task
	firstLine := strings.TrimSpace(lines[0])
	
	// Remove checkbox and extract main content
	re := regexp.MustCompile(`^-?\s*\[[ x]\]\s*(.+?)(?:\s+#.*)?$`)
	if matches := re.FindStringSubmatch(firstLine); len(matches) > 1 {
		// Remove any trailing tags from the main content
		content := strings.TrimSpace(matches[1])
		// Remove tags that might be at the end
		tagRegex := regexp.MustCompile(`\s+#\w+.*$`)
		content = tagRegex.ReplaceAllString(content, "")
		return content
	}
	
	return firstLine
}

// parseEntryMetadata extracts metadata from entry content
func (ms *MarkdownStorage) parseEntryMetadata(entry *types.DiaryEntry, content string) {
	// Extract URL for link entries
	if entry.Type == types.EntryTypeLink {
		urlRegex := regexp.MustCompile(`https?://[^\s]+`)
		if url := urlRegex.FindString(content); url != "" {
			entry.URL = url
		}
	}

	// Extract date from "Added: YYYY-MM-DD HH:MM" pattern
	dateRegex := regexp.MustCompile(`\*Added:\s*([^*]+)\*`)
	if matches := dateRegex.FindStringSubmatch(content); len(matches) > 1 {
		if parsedDate, err := time.Parse("2006-01-02 15:04", strings.TrimSpace(matches[1])); err == nil {
			entry.Date = parsedDate
		}
	}
}

// parseTaskMetadata extracts metadata from task content
func (ms *MarkdownStorage) parseTaskMetadata(entry *types.DiaryEntry, taskText string) {
	lines := strings.Split(taskText, "\n")
	
	// Reset tags to avoid duplicates
	entry.Tags = []string{"todo", "toProcess"}
	
	for _, line := range lines {
		line = strings.TrimSpace(line)
		
		// Extract priority from "Priority: value" format
		if strings.Contains(line, "Priority:") {
			re := regexp.MustCompile(`Priority:\s*(\w+)`)
			if matches := re.FindStringSubmatch(line); len(matches) > 1 {
				entry.Priority = types.Priority(matches[1])
			}
		}
		
		// Extract task ID from "ID: value" format  
		if strings.Contains(line, "ID:") {
			re := regexp.MustCompile(`ID:\s*([a-f0-9-]+)`)
			if matches := re.FindStringSubmatch(line); len(matches) > 1 {
				entry.TaskID = matches[1]
			}
		}
		
		// Extract due date from emoji format
		dueDateRegex := regexp.MustCompile(`📅\s*(\d{4}-\d{2}-\d{2})`)
		if matches := dueDateRegex.FindStringSubmatch(line); len(matches) > 1 {
			if dueDate, err := time.Parse("2006-01-02", matches[1]); err == nil {
				entry.DueDate = &dueDate
			}
		}
		
		// Extract additional tags from first line only (avoid duplicates)
		if strings.HasPrefix(line, "- [") {
			tagRegex := regexp.MustCompile(`#(\w+)`)
			tags := tagRegex.FindAllStringSubmatch(line, -1)
			for _, tag := range tags {
				if len(tag) > 1 {
					tagName := tag[1]
					// Avoid duplicating default tags
					if tagName != "todo" && tagName != "toProcess" {
						// Check if tag already exists
						exists := false
						for _, existingTag := range entry.Tags {
							if existingTag == tagName {
								exists = true
								break
							}
						}
						if !exists {
							entry.Tags = append(entry.Tags, tagName)
						}
					}
				}
			}
		}
	}
}


// AppendToEntry appends content to an existing diary entry
func (ms *MarkdownStorage) AppendToEntry(entry *types.DiaryEntry, subtitleSlug, content string) error {
	filePath := entry.File
	
	// Read the current file
	fileContent, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}
	
	lines := strings.Split(string(fileContent), "\n")
	
	// Find the entry in the file
	entryLineIndex := entry.LineNum - 1 // Convert to 0-based index
	if entryLineIndex >= len(lines) {
		return fmt.Errorf("entry line number %d is out of range", entry.LineNum)
	}
	
	// Find where to insert the new content
	insertIndex := ms.findInsertionPoint(lines, entryLineIndex, subtitleSlug)
	
	// Format the content to append
	var newLines []string
	if subtitleSlug != "" {
		newLines = append(newLines, fmt.Sprintf("### %s", subtitleSlug))
	}
	newLines = append(newLines, content)
	newLines = append(newLines, "") // Add blank line
	
	// Insert the new content
	result := make([]string, 0, len(lines)+len(newLines))
	result = append(result, lines[:insertIndex]...)
	result = append(result, newLines...)
	result = append(result, lines[insertIndex:]...)
	
	// Write back to file
	return os.WriteFile(filePath, []byte(strings.Join(result, "\n")), 0644)
}

// FindEntryBySubtitleSlug finds an entry that has the specified subtitle slug
func (ms *MarkdownStorage) FindEntryBySubtitleSlug(subtitleSlug string) (*types.DiaryEntry, error) {
	today := time.Now().Truncate(24 * time.Hour)
	entries, err := ms.GetEntries(today, "")
	if err != nil {
		return nil, err
	}
	
	for _, entry := range entries {
		if entry.SubtitleSlug == subtitleSlug {
			return entry, nil
		}
	}
	
	return nil, fmt.Errorf("no entry found with subtitle slug: %s", subtitleSlug)
}

// findInsertionPoint finds where to insert new content for an entry
func (ms *MarkdownStorage) findInsertionPoint(lines []string, entryLineIndex int, subtitleSlug string) int {
	// Start looking after the entry header
	searchStart := entryLineIndex + 1
	
	// If we're adding a subtitle, find the end of the entry content
	if subtitleSlug != "" {
		// Look for the next heading or end of file
		for i := searchStart; i < len(lines); i++ {
			line := strings.TrimSpace(lines[i])
			// Stop at next heading of same or higher level
			if strings.HasPrefix(line, "##") {
				return i
			}
		}
		// If no next heading found, append at end
		return len(lines)
	}
	
	// If no subtitle, just append after the entry content
	// Look for the end of the current entry (next heading or blank lines)
	for i := searchStart; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if strings.HasPrefix(line, "##") {
			return i
		}
	}
	
	return len(lines)
}


// SearchEntries searches for entries containing the query text
func (ms *MarkdownStorage) SearchEntries(query string, since time.Time, entryType types.EntryType, limit int) ([]*types.DiaryEntry, error) {
	allEntries, err := ms.GetEntries(since, entryType)
	if err != nil {
		return nil, err
	}
	
	var results []*types.DiaryEntry
	queryLower := strings.ToLower(query)
	
	for _, entry := range allEntries {
		// Search in title and content
		titleMatch := strings.Contains(strings.ToLower(entry.Title), queryLower)
		contentMatch := strings.Contains(strings.ToLower(entry.Content), queryLower)
		
		if titleMatch || contentMatch {
			results = append(results, entry)
			
			// Apply limit
			if limit > 0 && len(results) >= limit {
				break
			}
		}
	}
	
	return results, nil
}

