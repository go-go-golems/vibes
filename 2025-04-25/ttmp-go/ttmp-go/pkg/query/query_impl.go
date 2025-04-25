package query

import (
	"fmt"
	"strings"

	"github.com/scrapybara/ttmp/pkg/model"
)

// QueryDocuments performs a query against a collection of documents
func QueryDocuments(documents []*model.TTMPDocument, queryExpr string, keywords []string) ([]*model.TTMPDocument, error) {
	// Simple implementation for now
	var results []*model.TTMPDocument

	// If no query expression, just filter by keywords
	if queryExpr == "" {
		if len(keywords) == 0 {
			return documents, nil
		}
		
		// Filter by keywords
		for _, doc := range documents {
			if matchesKeywords(doc, keywords) {
				results = append(results, doc)
			}
		}
		return results, nil
	}

	// Parse and evaluate the query expression for each document
	for _, doc := range documents {
		matches, err := evaluateQuery(doc, queryExpr)
		if err != nil {
			return nil, err
		}
		
		if matches && (len(keywords) == 0 || matchesKeywords(doc, keywords)) {
			results = append(results, doc)
		}
	}

	return results, nil
}

// matchesKeywords checks if a document matches the given keywords
func matchesKeywords(doc *model.TTMPDocument, keywords []string) bool {
	if len(keywords) == 0 {
		return true
	}
	
	// Convert document to a searchable string
	searchText := strings.ToLower(fmt.Sprintf("%s %s %s %s %s %s %s",
		doc.Title,
		doc.ID,
		doc.Content,
		doc.Abstract,
		strings.Join(doc.Tags, " "),
		doc.Category,
		doc.Keywords,
	))
	
	// Check each keyword
	for _, keyword := range keywords {
		if !strings.Contains(searchText, strings.ToLower(keyword)) {
			return false
		}
	}
	
	return true
}

// evaluateQuery evaluates a query expression against a document
func evaluateQuery(doc *model.TTMPDocument, queryExpr string) (bool, error) {
	// Simple implementation for demonstration
	// In a real implementation, we would parse the query expression into a syntax tree
	
	// Handle basic comparisons
	if strings.Contains(queryExpr, "==") {
		parts := strings.Split(queryExpr, "==")
		if len(parts) != 2 {
			return false, fmt.Errorf("invalid query: %s", queryExpr)
		}
		
		field := strings.TrimSpace(parts[0])
		value := strings.Trim(strings.TrimSpace(parts[1]), "\"'")
		
		return compareField(doc, field, value, func(a, b string) bool { return a == b }), nil
	}
	
	if strings.Contains(queryExpr, "contains") {
		parts := strings.Split(queryExpr, "contains")
		if len(parts) != 2 {
			return false, fmt.Errorf("invalid query: %s", queryExpr)
		}
		
		field := strings.TrimSpace(parts[0])
		value := strings.Trim(strings.TrimSpace(parts[1]), "\"'")
		
		return checkContains(doc, field, value), nil
	}
	
	// Handle logical operators
	if strings.Contains(queryExpr, "&&") {
		parts := strings.Split(queryExpr, "&&")
		for _, part := range parts {
			matches, err := evaluateQuery(doc, strings.TrimSpace(part))
			if err != nil {
				return false, err
			}
			if !matches {
				return false, nil
			}
		}
		return true, nil
	}
	
	if strings.Contains(queryExpr, "||") {
		parts := strings.Split(queryExpr, "||")
		for _, part := range parts {
			matches, err := evaluateQuery(doc, strings.TrimSpace(part))
			if err != nil {
				return false, err
			}
			if matches {
				return true, nil
			}
		}
		return false, nil
	}
	
	return false, fmt.Errorf("unsupported query: %s", queryExpr)
}

// compareField compares a document field with a value using the provided comparison function
func compareField(doc *model.TTMPDocument, field, value string, compareFn func(string, string) bool) bool {
	switch field {
	case "id":
		return compareFn(doc.ID, value)
	case "title":
		return compareFn(doc.Title, value)
	case "status":
		return compareFn(doc.Status, value)
	case "owner":
		return compareFn(doc.Owner, value)
	case "audience":
		return compareFn(doc.Audience, value)
	case "category":
		return compareFn(doc.Category, value)
	case "document_type":
		return compareFn(doc.DocumentType, value)
	default:
		return false
	}
}

// checkContains checks if a field contains a value
func checkContains(doc *model.TTMPDocument, field, value string) bool {
	switch field {
	case "tags":
		for _, tag := range doc.Tags {
			if tag == value {
				return true
			}
		}
	case "concepts":
		for _, concept := range doc.Concepts {
			if concept == value {
				return true
			}
		}
	case "content":
		return strings.Contains(strings.ToLower(doc.Content), strings.ToLower(value))
	case "abstract":
		return strings.Contains(strings.ToLower(doc.Abstract), strings.ToLower(value))
	}
	return false
}