package parser

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

	"github.com/user/ttmp-go/pkg/errors"
	"github.com/user/ttmp-go/pkg/model"
)

// TTMPParser is responsible for parsing TTMP documents
type TTMPParser struct {
	// Configuration options can be added here
}

// NewParser creates a new TTMPParser
func NewParser() *TTMPParser {
	return &TTMPParser{}
}

// ParseFile parses a markdown file with YAML frontmatter into a TTMPDocument
func (p *TTMPParser) ParseFile(filePath string) (*model.TTMPDocument, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, errors.NewIOError(fmt.Sprintf("Failed to open file: %s", filePath), err)
	}
	defer file.Close()

	doc, err := p.Parse(file)
	if err != nil {
		return nil, fmt.Errorf("error parsing file %s: %w", filePath, err)
	}

	// If the doc has no filename, set it to the base filename
	if doc.Filename == "" {
		doc.Filename = filePath
	}

	return doc, nil
}

// Parse parses a reader containing markdown with YAML frontmatter into a TTMPDocument
func (p *TTMPParser) Parse(r io.Reader) (*model.TTMPDocument, error) {
	// Read the file
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, errors.NewIOError("Failed to read file content", err)
	}

	// Check if the file begins with YAML frontmatter (---)
	if !bytes.HasPrefix(data, []byte("---\n")) {
		return nil, errors.NewParsingError("File does not begin with YAML frontmatter", nil)
	}

	// Find the end of the frontmatter
	parts := bytes.SplitN(data, []byte("---\n"), 3)
	if len(parts) < 3 {
		return nil, errors.NewParsingError("Invalid YAML frontmatter format", nil)
	}

	// Extract frontmatter and content
	frontmatter := parts[1]
	content := parts[2]

	// Parse the frontmatter
	var metadata map[string]interface{}
	if err := yaml.Unmarshal(frontmatter, &metadata); err != nil {
		return nil, errors.NewParsingError("Failed to parse YAML frontmatter", err)
	}

	// Convert metadata to TTMPDocument
	doc, err := p.convertMetadataToDocument(metadata)
	if err != nil {
		return nil, err
	}

	// Add the content
	doc.Content = string(content)

	return doc, nil
}

// extractStringValue extracts a string value from metadata with error handling
func extractStringValue(metadata map[string]interface{}, key string) (string, error) {
	if val, ok := metadata[key]; ok {
		if strVal, ok := val.(string); ok {
			return strVal, nil
		}
		return "", errors.NewParsingError(fmt.Sprintf("Field '%s' is not a string", key), nil)
	}
	return "", nil // No error if key doesn't exist
}

// extractDateValue extracts a date value from metadata with error handling
func extractDateValue(metadata map[string]interface{}, key string) (*time.Time, error) {
	if val, ok := metadata[key]; ok {
		switch v := val.(type) {
		case string:
			// Parse the date string
			t, err := time.Parse(time.RFC3339, v)
			if err != nil {
				// Try alternative formats
				formats := []string{
					"2006-01-02",
					"2006-01-02 15:04:05",
					"2006-01-02T15:04:05",
					"2006-01-02T15:04:05Z07:00",
				}
				
				for _, format := range formats {
					t, err = time.Parse(format, v)
					if err == nil {
						break
					}
				}
				
				if err != nil {
					return nil, errors.NewParsingError(
						fmt.Sprintf("Invalid date format for '%s': %s", key, v),
						err,
					)
				}
			}
			return &t, nil
		case time.Time:
			return &v, nil
		default:
			return nil, errors.NewParsingError(
				fmt.Sprintf("Field '%s' is not a valid date", key),
				nil,
			)
		}
	}
	return nil, nil // No error if key doesn't exist
}

// extractStringArrayValue extracts a string array from metadata with error handling
func extractStringArrayValue(metadata map[string]interface{}, key string) ([]string, error) {
	if val, ok := metadata[key]; ok {
		switch v := val.(type) {
		case []interface{}:
			result := make([]string, 0, len(v))
			for i, item := range v {
				if strVal, ok := item.(string); ok {
					result = append(result, strVal)
				} else {
					return nil, errors.NewParsingError(
						fmt.Sprintf("Item at index %d in '%s' is not a string", i, key),
						nil,
					)
				}
			}
			return result, nil
		case string:
			// Handle comma-separated string
			items := strings.Split(v, ",")
			for i := range items {
				items[i] = strings.TrimSpace(items[i])
			}
			return items, nil
		default:
			return nil, errors.NewParsingError(
				fmt.Sprintf("Field '%s' is not a string array", key), 
				nil,
			)
		}
	}
	return nil, nil // No error if key doesn't exist
}

// convertMetadataToDocument converts metadata map to a TTMPDocument
func (p *TTMPParser) convertMetadataToDocument(metadata map[string]interface{}) (*model.TTMPDocument, error) {
	doc := &model.TTMPDocument{}
	
	// Extract required fields
	var err error
	
	doc.ID, err = extractStringValue(metadata, "id")
	if err != nil {
		return nil, err
	}
	
	doc.Type, err = extractStringValue(metadata, "type")
	if err != nil {
		return nil, err
	}
	
	doc.Title, err = extractStringValue(metadata, "title")
	if err != nil {
		return nil, err
	}
	
	createdPtr, err := extractDateValue(metadata, "created")
	if err != nil {
		return nil, err
	}
	if createdPtr != nil {
		doc.Created = *createdPtr
	}

	// Extract optional fields
	doc.Description, err = extractStringValue(metadata, "description")
	if err != nil {
		return nil, err
	}
	
	doc.Modified, err = extractDateValue(metadata, "modified")
	if err != nil {
		return nil, err
	}
	
	doc.Tags, err = extractStringArrayValue(metadata, "tags")
	if err != nil {
		return nil, err
	}

	// Extract links if present
	if linksVal, ok := metadata["links"]; ok {
		if linksArray, ok := linksVal.([]interface{}); ok {
			doc.Links = make([]model.Link, 0, len(linksArray))
			
			for i, linkItem := range linksArray {
				if linkMap, ok := linkItem.(map[string]interface{}); ok {
					link := model.Link{}
					
					link.URL, err = extractStringValue(linkMap, "url")
					if err != nil {
						return nil, err
					}
					
					link.Title, err = extractStringValue(linkMap, "title")
					if err != nil {
						return nil, err
					}
					
					link.Type, err = extractStringValue(linkMap, "type")
					if err != nil {
						return nil, err
					}
					
					doc.Links = append(doc.Links, link)
				} else {
					return nil, errors.NewParsingError(
						fmt.Sprintf("Link at index %d is not a valid map", i),
						nil,
					)
				}
			}
		} else {
			return nil, errors.NewParsingError("'links' field is not an array", nil)
		}
	}

	// Extract custom metadata
	doc.Custom = make(map[string]interface{})
	for key, value := range metadata {
		// Skip standard fields
		if isStandardField(key) {
			continue
		}
		
		doc.Custom[key] = value
	}

	return doc, nil
}

// isStandardField checks if a field name is a standard TTMP field
func isStandardField(field string) bool {
	standardFields := map[string]bool{
		"id":          true,
		"type":        true,
		"title":       true,
		"description": true,
		"created":     true,
		"modified":    true,
		"tags":        true,
		"links":       true,
	}
	
	return standardFields[field]
}

// WriteDocumentToFile writes a TTMPDocument to a file
func (p *TTMPParser) WriteDocumentToFile(doc *model.TTMPDocument, filePath string) error {
	// Create the file
	file, err := os.Create(filePath)
	if err != nil {
		return errors.NewIOError(fmt.Sprintf("Failed to create file: %s", filePath), err)
	}
	defer file.Close()

	// Write the document
	if err := p.WriteDocument(doc, file); err != nil {
		return fmt.Errorf("error writing to file %s: %w", filePath, err)
	}

	return nil
}

// WriteDocument writes a TTMPDocument to a writer
func (p *TTMPParser) WriteDocument(doc *model.TTMPDocument, w io.Writer) error {
	// Create the metadata map
	metadata := make(map[string]interface{})
	
	// Add required fields
	metadata["id"] = doc.ID
	metadata["type"] = doc.Type
	metadata["title"] = doc.Title
	metadata["created"] = doc.Created.Format(time.RFC3339)

	// Add optional fields if present
	if doc.Description != "" {
		metadata["description"] = doc.Description
	}
	
	if doc.Modified != nil {
		metadata["modified"] = doc.Modified.Format(time.RFC3339)
	}
	
	if len(doc.Tags) > 0 {
		metadata["tags"] = doc.Tags
	}
	
	if len(doc.Links) > 0 {
		metadata["links"] = doc.Links
	}

	// Add custom metadata
	for key, value := range doc.Custom {
		metadata[key] = value
	}

	// Marshal to YAML
	yamlData, err := yaml.Marshal(metadata)
	if err != nil {
		return errors.NewParsingError("Failed to marshal document to YAML", err)
	}

	// Write the frontmatter and content
	writer := bufio.NewWriter(w)
	writer.WriteString("---\n")
	writer.Write(yamlData)
	writer.WriteString("---\n")
	writer.WriteString(doc.Content)
	
	// Ensure a newline at the end of the file
	if !strings.HasSuffix(doc.Content, "\n") {
		writer.WriteString("\n")
	}

	return writer.Flush()
}