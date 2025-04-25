package query

import (
	"fmt"
	"strings"
	"time"

	"github.com/scrapybara/ttmp/pkg/model"
)

// DocumentResult represents a formatted result from a query
type DocumentResult struct {
	ID          string
	Title       string
	Status      string
	Type        string
	Tags        string
	UpdatedDate string
	FilePath    string
	Abstract    string
}

// FormatDocumentResult formats a TTMP document into a result for display
func FormatDocumentResult(doc *model.TTMPDocument) DocumentResult {
	result := DocumentResult{
		ID:       doc.ID,
		Title:    doc.Title,
		Status:   doc.Status,
		Type:     doc.DocumentType,
		FilePath: doc.FilePath,
		Abstract: doc.Abstract,
	}

	// Format tags
	if len(doc.Tags) > 0 {
		result.Tags = strings.Join(doc.Tags, ", ")
	}

	// Format updated date
	if doc.Updated != nil {
		result.UpdatedDate = doc.Updated.Format("2006-01-02")
	}

	return result
}

// FormatDocumentsTable formats a list of TTMP documents into a table string
func FormatDocumentsTable(docs []*model.TTMPDocument) string {
	if len(docs) == 0 {
		return "No documents found."
	}

	// Format table header
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("%-20s | %-30s | %-10s | %-10s | %-20s | %-10s\n", "ID", "TITLE", "TYPE", "STATUS", "TAGS", "UPDATED"))
	sb.WriteString(strings.Repeat("-", 110) + "\n")

	// Format table rows
	for _, doc := range docs {
		updatedStr := ""
		if doc.Updated != nil {
			updatedStr = doc.Updated.Format("2006-01-02")
		}

		tagsStr := ""
		if len(doc.Tags) > 0 {
			if len(doc.Tags) <= 3 {
				tagsStr = strings.Join(doc.Tags, ", ")
			} else {
				tagsStr = strings.Join(doc.Tags[:3], ", ") + "..."
			}
		}

		title := doc.Title
		if len(title) > 30 {
			title = title[:27] + "..."
		}

		id := doc.ID
		if len(id) > 20 {
			id = id[:17] + "..."
		}

		sb.WriteString(fmt.Sprintf("%-20s | %-30s | %-10s | %-10s | %-20s | %-10s\n",
			id, title, doc.DocumentType, doc.Status, tagsStr, updatedStr))
	}

	return sb.String()
}

// FormatDetailedDocument formats a single TTMP document with full details
func FormatDetailedDocument(doc *model.TTMPDocument) string {
	var sb strings.Builder

	// Core Identity & Lifecycle
	sb.WriteString(fmt.Sprintf("ID:         %s\n", doc.ID))
	sb.WriteString(fmt.Sprintf("Title:      %s\n", doc.Title))
	
	if doc.Created != nil {
		sb.WriteString(fmt.Sprintf("Created:    %s\n", doc.Created.Format(time.RFC3339)))
	}
	
	if doc.Updated != nil {
		sb.WriteString(fmt.Sprintf("Updated:    %s\n", doc.Updated.Format(time.RFC3339)))
	}
	
	if doc.Status != "" {
		sb.WriteString(fmt.Sprintf("Status:     %s\n", doc.Status))
	}
	
	if doc.Owner != "" {
		sb.WriteString(fmt.Sprintf("Owner:      %s\n", doc.Owner))
	}
	
	if doc.Audience != "" {
		sb.WriteString(fmt.Sprintf("Audience:   %s\n", doc.Audience))
	}
	
	sb.WriteString("\n")

	// Discovery & Classification
	if len(doc.Tags) > 0 {
		sb.WriteString(fmt.Sprintf("Tags:       %s\n", strings.Join(doc.Tags, ", ")))
	}
	
	if doc.Category != "" {
		sb.WriteString(fmt.Sprintf("Category:   %s\n", doc.Category))
	}
	
	if doc.Longevity != "" {
		sb.WriteString(fmt.Sprintf("Longevity:  %s\n", doc.Longevity))
	}
	
	if doc.DocumentType != "" {
		sb.WriteString(fmt.Sprintf("Doc Type:   %s\n", doc.DocumentType))
	}
	
	sb.WriteString("\n")

	// Code & Concept Links
	if len(doc.SourceFiles) > 0 {
		sb.WriteString("Source Files:\n")
		for _, file := range doc.SourceFiles {
			sb.WriteString(fmt.Sprintf("  - %s\n", file))
		}
		sb.WriteString("\n")
	}
	
	if len(doc.TrackedFunctions) > 0 {
		sb.WriteString("Tracked Functions:\n")
		for _, fn := range doc.TrackedFunctions {
			sb.WriteString(fmt.Sprintf("  - %s\n", fn))
		}
		sb.WriteString("\n")
	}
	
	if len(doc.Concepts) > 0 {
		sb.WriteString("Concepts:\n")
		for _, concept := range doc.Concepts {
			sb.WriteString(fmt.Sprintf("  - %s\n", concept))
		}
		sb.WriteString("\n")
	}

	// Relationships
	if len(doc.SeeAlso) > 0 {
		sb.WriteString("See Also:\n")
		for _, ref := range doc.SeeAlso {
			sb.WriteString(fmt.Sprintf("  - %s\n", ref))
		}
		sb.WriteString("\n")
	}

	// Abstract
	if doc.Abstract != "" {
		sb.WriteString("Abstract:\n")
		sb.WriteString(doc.Abstract)
		sb.WriteString("\n\n")
	}

	// Preview of content
	if doc.Content != "" {
		contentLines := strings.Split(doc.Content, "\n")
		previewLines := min(len(contentLines), 5)
		
		sb.WriteString("Content Preview:\n")
		for i := 0; i < previewLines; i++ {
			sb.WriteString(contentLines[i])
			sb.WriteString("\n")
		}
		
		if len(contentLines) > 5 {
			sb.WriteString("...\n")
		}
	}

	return sb.String()
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}