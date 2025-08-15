package cmd

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"text/tabwriter"
	"time"

	"markdown-manager/pkg/parser"
	"markdown-manager/pkg/metadata"
)

func listMarkdownFiles(directory string, recursive bool, showPath bool, format string) error {
	var files []string
	var err error

	if recursive {
		files, err = parser.FindMarkdownFiles(directory)
	} else {
		matches, err := filepath.Glob(filepath.Join(directory, "*.md"))
		if err != nil {
			return fmt.Errorf("failed to find markdown files: %w", err)
		}
		files = matches
	}

	if err != nil {
		return fmt.Errorf("failed to find markdown files: %w", err)
	}

	var docs []*metadata.DocumentFile
	for _, filePath := range files {
		doc, err := parser.ParseMarkdownFile(filePath)
		if err != nil {
			fmt.Printf("Warning: failed to parse %s: %v\n", filePath, err)
			continue
		}
		docs = append(docs, doc)
	}

	switch format {
	case "json":
		return outputJSON(docs)
	case "table":
		return outputTable(docs, showPath)
	default:
		return outputTable(docs, showPath)
	}
}

func outputJSON(docs []*metadata.DocumentFile) error {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(docs)
}

func outputTable(docs []*metadata.DocumentFile, showPath bool) error {
	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "PATH\tTITLE\tTAGS\tPROJECT\tSTATUS\tMODIFIED")
	
	for _, doc := range docs {
		displayPath := filepath.Base(doc.Path)
		if showPath {
			displayPath = doc.Path
		}
		
		tags := strings.Join(doc.Metadata.Tags, ",")
		if len(tags) > 30 {
			tags = tags[:27] + "..."
		}
		
		fmt.Fprintf(w, "%s\t%s\t%s\t%s\t%s\t%s\n",
			displayPath,
			doc.Metadata.Title,
			tags,
			doc.Metadata.Project,
			doc.Metadata.Status,
			doc.Metadata.Modified.Format("2006-01-02"),
		)
	}
	
	return nil
}

func searchMarkdownFiles(directory, title string, tags []string, category, project, status, priority, author, content string, showContent bool) error {
	files, err := parser.FindMarkdownFiles(directory)
	if err != nil {
		return fmt.Errorf("failed to find markdown files: %w", err)
	}

	var results []*metadata.DocumentFile
	for _, filePath := range files {
		doc, err := parser.ParseMarkdownFile(filePath)
		if err != nil {
			continue
		}

		if matchesSearchCriteria(doc, title, tags, category, project, status, priority, author, content) {
			results = append(results, doc)
		}
	}

	if len(results) == 0 {
		fmt.Println("No files found matching the search criteria.")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	if showContent {
		fmt.Fprintln(w, "PATH\tTITLE\tTAGS\tPROJECT\tSTATUS\tCONTENT_PREVIEW")
		for _, doc := range results {
			contentPreview := strings.TrimSpace(doc.Content)
			if len(contentPreview) > 50 {
				contentPreview = contentPreview[:47] + "..."
			}
			contentPreview = strings.ReplaceAll(contentPreview, "\n", " ")
			
			fmt.Fprintf(w, "%s\t%s\t%s\t%s\t%s\t%s\n",
				filepath.Base(doc.Path),
				doc.Metadata.Title,
				strings.Join(doc.Metadata.Tags, ","),
				doc.Metadata.Project,
				doc.Metadata.Status,
				contentPreview,
			)
		}
	} else {
		fmt.Fprintln(w, "PATH\tTITLE\tTAGS\tPROJECT\tSTATUS\tMODIFIED")
		for _, doc := range results {
			fmt.Fprintf(w, "%s\t%s\t%s\t%s\t%s\t%s\n",
				filepath.Base(doc.Path),
				doc.Metadata.Title,
				strings.Join(doc.Metadata.Tags, ","),
				doc.Metadata.Project,
				doc.Metadata.Status,
				doc.Metadata.Modified.Format("2006-01-02"),
			)
		}
	}

	return nil
}

func matchesSearchCriteria(doc *metadata.DocumentFile, title string, tags []string, category, project, status, priority, author, content string) bool {
	if title != "" && !strings.Contains(strings.ToLower(doc.Metadata.Title), strings.ToLower(title)) {
		return false
	}

	if len(tags) > 0 {
		docTags := make(map[string]bool)
		for _, tag := range doc.Metadata.Tags {
			docTags[strings.ToLower(tag)] = true
		}
		for _, searchTag := range tags {
			if !docTags[strings.ToLower(searchTag)] {
				return false
			}
		}
	}

	if category != "" && !strings.EqualFold(doc.Metadata.Category, category) {
		return false
	}

	if project != "" && !strings.Contains(strings.ToLower(doc.Metadata.Project), strings.ToLower(project)) {
		return false
	}

	if status != "" && !strings.EqualFold(doc.Metadata.Status, status) {
		return false
	}

	if priority != "" && !strings.EqualFold(doc.Metadata.Priority, priority) {
		return false
	}

	if author != "" && !strings.Contains(strings.ToLower(doc.Metadata.Author), strings.ToLower(author)) {
		return false
	}

	if content != "" && !strings.Contains(strings.ToLower(doc.Content), strings.ToLower(content)) {
		return false
	}

	return true
}

func showFileInfo(filePath string, showContent bool, touch bool) error {
	if touch {
		if err := parser.TouchLastUsed(filePath); err != nil {
			return fmt.Errorf("failed to update last_used: %w", err)
		}
	}

	doc, err := parser.ParseMarkdownFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to parse file: %w", err)
	}

	fmt.Printf("File: %s\n", doc.Path)
	fmt.Printf("Title: %s\n", doc.Metadata.Title)
	fmt.Printf("Description: %s\n", doc.Metadata.Description)
	fmt.Printf("Tags: %s\n", strings.Join(doc.Metadata.Tags, ", "))
	fmt.Printf("Category: %s\n", doc.Metadata.Category)
	fmt.Printf("Project: %s\n", doc.Metadata.Project)
	fmt.Printf("Repository: %s\n", doc.Metadata.Repository)
	fmt.Printf("Branch: %s\n", doc.Metadata.Branch)
	fmt.Printf("Status: %s\n", doc.Metadata.Status)
	fmt.Printf("Priority: %s\n", doc.Metadata.Priority)
	fmt.Printf("Version: %s\n", doc.Metadata.Version)
	fmt.Printf("Author: %s\n", doc.Metadata.Author)
	fmt.Printf("Contributors: %s\n", strings.Join(doc.Metadata.Contributors, ", "))
	fmt.Printf("Language: %s\n", doc.Metadata.Language)
	fmt.Printf("Format: %s\n", doc.Metadata.Format)
	fmt.Printf("Template: %s\n", doc.Metadata.Template)
	fmt.Printf("Created: %s\n", doc.Metadata.Created.Format("2006-01-02 15:04:05"))
	fmt.Printf("Modified: %s\n", doc.Metadata.Modified.Format("2006-01-02 15:04:05"))
	
	if doc.Metadata.LastUsed != nil {
		fmt.Printf("Last Used: %s\n", doc.Metadata.LastUsed.Format("2006-01-02 15:04:05"))
	} else {
		fmt.Printf("Last Used: Never\n")
	}
	
	fmt.Printf("File Size: %d bytes\n", doc.Size)
	fmt.Printf("Content Length: %d characters\n", len(doc.Content))

	if len(doc.Metadata.RelatedFiles) > 0 {
		fmt.Printf("Related Files: %s\n", strings.Join(doc.Metadata.RelatedFiles, ", "))
	}
	if len(doc.Metadata.Dependencies) > 0 {
		fmt.Printf("Dependencies: %s\n", strings.Join(doc.Metadata.Dependencies, ", "))
	}
	if len(doc.Metadata.References) > 0 {
		fmt.Printf("References: %s\n", strings.Join(doc.Metadata.References, ", "))
	}

	if showContent {
		fmt.Printf("\nContent:\n%s\n", doc.Content)
	}

	return nil
}

func updateFileMetadata(filePath, title, description string, tags, addTags, removeTags []string, category, project, status, priority, author string, touch bool) error {
	doc, err := parser.ParseMarkdownFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to parse file: %w", err)
	}

	// Update fields if provided
	if title != "" {
		doc.Metadata.Title = title
	}
	if description != "" {
		doc.Metadata.Description = description
	}
	if category != "" {
		doc.Metadata.Category = category
	}
	if project != "" {
		doc.Metadata.Project = project
	}
	if status != "" {
		doc.Metadata.Status = status
	}
	if priority != "" {
		doc.Metadata.Priority = priority
	}
	if author != "" {
		doc.Metadata.Author = author
	}

	// Handle tags
	if len(tags) > 0 {
		doc.Metadata.Tags = tags
	} else {
		if len(addTags) > 0 {
			existingTags := make(map[string]bool)
			for _, tag := range doc.Metadata.Tags {
				existingTags[tag] = true
			}
			for _, tag := range addTags {
				if !existingTags[tag] {
					doc.Metadata.Tags = append(doc.Metadata.Tags, tag)
				}
			}
		}
		if len(removeTags) > 0 {
			removeMap := make(map[string]bool)
			for _, tag := range removeTags {
				removeMap[tag] = true
			}
			var newTags []string
			for _, tag := range doc.Metadata.Tags {
				if !removeMap[tag] {
					newTags = append(newTags, tag)
				}
			}
			doc.Metadata.Tags = newTags
		}
	}

	if touch {
		now := time.Now()
		doc.Metadata.LastUsed = &now
	}

	if err := parser.UpdateMetadata(filePath, doc.Metadata); err != nil {
		return fmt.Errorf("failed to update metadata: %w", err)
	}

	fmt.Printf("Successfully updated metadata for %s\n", filePath)
	return nil
}

func queryMarkdownFiles(directory, queryType string) error {
	files, err := parser.FindMarkdownFiles(directory)
	if err != nil {
		return fmt.Errorf("failed to find markdown files: %w", err)
	}

	var docs []*metadata.DocumentFile
	for _, filePath := range files {
		doc, err := parser.ParseMarkdownFile(filePath)
		if err != nil {
			continue
		}
		docs = append(docs, doc)
	}

	switch queryType {
	case "stats":
		return showStats(docs)
	case "tags":
		return showTagStats(docs)
	case "projects":
		return showProjectStats(docs)
	case "authors":
		return showAuthorStats(docs)
	case "status":
		return showStatusStats(docs)
	case "priority":
		return showPriorityStats(docs)
	case "recent":
		return showRecentFiles(docs)
	case "stale":
		return showStaleFiles(docs)
	default:
		return fmt.Errorf("unknown query type: %s", queryType)
	}
}

func showStats(docs []*metadata.DocumentFile) error {
	totalFiles := len(docs)
	totalSize := int64(0)
	totalContent := 0
	withTags := 0
	withProjects := 0
	withAuthors := 0

	for _, doc := range docs {
		totalSize += doc.Size
		totalContent += len(doc.Content)
		if len(doc.Metadata.Tags) > 0 {
			withTags++
		}
		if doc.Metadata.Project != "" {
			withProjects++
		}
		if doc.Metadata.Author != "" {
			withAuthors++
		}
	}

	fmt.Printf("Total Files: %d\n", totalFiles)
	fmt.Printf("Total Size: %d bytes\n", totalSize)
	fmt.Printf("Total Content: %d characters\n", totalContent)
	if totalFiles > 0 {
		fmt.Printf("Average Size: %d bytes\n", totalSize/int64(totalFiles))
		fmt.Printf("Average Content: %d characters\n", totalContent/totalFiles)
	}
	fmt.Printf("Files with Tags: %d\n", withTags)
	fmt.Printf("Files with Projects: %d\n", withProjects)
	fmt.Printf("Files with Authors: %d\n", withAuthors)

	return nil
}

func showTagStats(docs []*metadata.DocumentFile) error {
	tagCounts := make(map[string]int)

	for _, doc := range docs {
		for _, tag := range doc.Metadata.Tags {
			tagCounts[tag]++
		}
	}

	if len(tagCounts) == 0 {
		fmt.Println("No tags found.")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "TAG\tCOUNT")
	for tag, count := range tagCounts {
		fmt.Fprintf(w, "%s\t%d\n", tag, count)
	}

	return nil
}

func showProjectStats(docs []*metadata.DocumentFile) error {
	projectCounts := make(map[string]int)
	projectSizes := make(map[string]int64)

	for _, doc := range docs {
		if doc.Metadata.Project != "" {
			projectCounts[doc.Metadata.Project]++
			projectSizes[doc.Metadata.Project] += doc.Size
		}
	}

	if len(projectCounts) == 0 {
		fmt.Println("No projects found.")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "PROJECT\tFILES\tTOTAL_SIZE\tAVG_SIZE")
	for project, count := range projectCounts {
		avgSize := projectSizes[project] / int64(count)
		fmt.Fprintf(w, "%s\t%d\t%d\t%d\n", project, count, projectSizes[project], avgSize)
	}

	return nil
}

func showAuthorStats(docs []*metadata.DocumentFile) error {
	authorCounts := make(map[string]int)

	for _, doc := range docs {
		if doc.Metadata.Author != "" {
			authorCounts[doc.Metadata.Author]++
		}
	}

	if len(authorCounts) == 0 {
		fmt.Println("No authors found.")
		return nil
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "AUTHOR\tFILES")
	for author, count := range authorCounts {
		fmt.Fprintf(w, "%s\t%d\n", author, count)
	}

	return nil
}

func showStatusStats(docs []*metadata.DocumentFile) error {
	statusCounts := make(map[string]int)

	for _, doc := range docs {
		status := doc.Metadata.Status
		if status == "" {
			status = "unspecified"
		}
		statusCounts[status]++
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "STATUS\tCOUNT")
	for status, count := range statusCounts {
		fmt.Fprintf(w, "%s\t%d\n", status, count)
	}

	return nil
}

func showPriorityStats(docs []*metadata.DocumentFile) error {
	priorityCounts := make(map[string]int)

	for _, doc := range docs {
		priority := doc.Metadata.Priority
		if priority == "" {
			priority = "unspecified"
		}
		priorityCounts[priority]++
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "PRIORITY\tCOUNT")
	for priority, count := range priorityCounts {
		fmt.Fprintf(w, "%s\t%d\n", priority, count)
	}

	return nil
}

func showRecentFiles(docs []*metadata.DocumentFile) error {
	// Sort by modified time (most recent first)
	for i := 0; i < len(docs)-1; i++ {
		for j := i + 1; j < len(docs); j++ {
			if docs[i].Metadata.Modified.Before(docs[j].Metadata.Modified) {
				docs[i], docs[j] = docs[j], docs[i]
			}
		}
	}

	limit := 10
	if len(docs) < limit {
		limit = len(docs)
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "RANK\tFILE\tTITLE\tMODIFIED\tPROJECT")
	for i := 0; i < limit; i++ {
		doc := docs[i]
		fmt.Fprintf(w, "%d\t%s\t%s\t%s\t%s\n",
			i+1,
			filepath.Base(doc.Path),
			doc.Metadata.Title,
			doc.Metadata.Modified.Format("2006-01-02"),
			doc.Metadata.Project,
		)
	}

	return nil
}

func showStaleFiles(docs []*metadata.DocumentFile) error {
	thirtyDaysAgo := time.Now().AddDate(0, 0, -30)

	var staleFiles []*metadata.DocumentFile
	for _, doc := range docs {
		if doc.Metadata.Modified.Before(thirtyDaysAgo) {
			staleFiles = append(staleFiles, doc)
		}
	}

	if len(staleFiles) == 0 {
		fmt.Println("No stale files found (all files modified within 30 days).")
		return nil
	}

	// Sort by modified time (oldest first)
	for i := 0; i < len(staleFiles)-1; i++ {
		for j := i + 1; j < len(staleFiles); j++ {
			if staleFiles[i].Metadata.Modified.After(staleFiles[j].Metadata.Modified) {
				staleFiles[i], staleFiles[j] = staleFiles[j], staleFiles[i]
			}
		}
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	defer w.Flush()

	fmt.Fprintln(w, "FILE\tTITLE\tMODIFIED\tDAYS_AGO\tPROJECT")
	for _, doc := range staleFiles {
		daysAgo := int(time.Since(doc.Metadata.Modified).Hours() / 24)
		fmt.Fprintf(w, "%s\t%s\t%s\t%d\t%s\n",
			filepath.Base(doc.Path),
			doc.Metadata.Title,
			doc.Metadata.Modified.Format("2006-01-02"),
			daysAgo,
			doc.Metadata.Project,
		)
	}

	return nil
}

