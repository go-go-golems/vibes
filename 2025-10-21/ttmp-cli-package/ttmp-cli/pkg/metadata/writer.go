package metadata

import (
	"fmt"
	"os"
	"strings"

	"gopkg.in/yaml.v3"
)

// WriteFile writes metadata and content to a file
func WriteFile(filepath string, meta *DocumentMetadata, body string) error {
	content := Serialize(meta, body)
	return os.WriteFile(filepath, []byte(content), 0644)
}

// Serialize converts metadata and body to markdown with frontmatter
func Serialize(meta *DocumentMetadata, body string) string {
	var sb strings.Builder
	
	// Write frontmatter
	sb.WriteString("---\n")
	
	yamlData, err := yaml.Marshal(meta)
	if err == nil {
		sb.Write(yamlData)
	}
	
	sb.WriteString("---\n\n")
	
	// Write body
	sb.WriteString(body)
	
	return sb.String()
}

// UpdateField updates a specific field in a document's metadata
func UpdateField(filepath, field, value string) error {
	meta, body, err := ParseFile(filepath)
	if err != nil {
		return err
	}

	if meta == nil {
		meta = &DocumentMetadata{}
	}

	// Update the specified field
	switch field {
	case "Status":
		meta.Status = value
	case "Intent":
		meta.Intent = value
	case "Title":
		meta.Title = value
	case "Ticket":
		meta.Ticket = value
	case "DocType":
		meta.DocType = value
	case "Summary":
		meta.Summary = value
	case "LastUpdated":
		meta.LastUpdated = value
	default:
		return fmt.Errorf("unknown field: %s", field)
	}

	return WriteFile(filepath, meta, body)
}

// AddTopic adds a topic to the document metadata
func AddTopic(filepath, topic string) error {
	meta, body, err := ParseFile(filepath)
	if err != nil {
		return err
	}

	if meta == nil {
		meta = &DocumentMetadata{}
	}

	// Check if topic already exists
	for _, t := range meta.Topics {
		if t == topic {
			return nil // Already exists
		}
	}

	meta.Topics = append(meta.Topics, topic)
	return WriteFile(filepath, meta, body)
}

// SetTopics replaces all topics in the document metadata
func SetTopics(filepath string, topics []string) error {
	meta, body, err := ParseFile(filepath)
	if err != nil {
		return err
	}

	if meta == nil {
		meta = &DocumentMetadata{}
	}

	meta.Topics = topics
	return WriteFile(filepath, meta, body)
}

// AddRelatedFile adds a file to the RelatedFiles list
func AddRelatedFile(filepath, relatedFile string) error {
	meta, body, err := ParseFile(filepath)
	if err != nil {
		return err
	}

	if meta == nil {
		meta = &DocumentMetadata{}
	}

	// Check if file already exists
	for _, f := range meta.RelatedFiles {
		if f == relatedFile {
			return nil // Already exists
		}
	}

	meta.RelatedFiles = append(meta.RelatedFiles, relatedFile)
	return WriteFile(filepath, meta, body)
}

// SetRelatedFiles replaces all related files in the document metadata
func SetRelatedFiles(filepath string, files []string) error {
	meta, body, err := ParseFile(filepath)
	if err != nil {
		return err
	}

	if meta == nil {
		meta = &DocumentMetadata{}
	}

	meta.RelatedFiles = files
	return WriteFile(filepath, meta, body)
}

// AddOwner adds an owner to the document metadata
func AddOwner(filepath, owner string) error {
	meta, body, err := ParseFile(filepath)
	if err != nil {
		return err
	}

	if meta == nil {
		meta = &DocumentMetadata{}
	}

	// Check if owner already exists
	for _, o := range meta.Owners {
		if o == owner {
			return nil // Already exists
		}
	}

	meta.Owners = append(meta.Owners, owner)
	return WriteFile(filepath, meta, body)
}

