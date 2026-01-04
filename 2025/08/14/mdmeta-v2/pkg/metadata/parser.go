package metadata

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/adrg/frontmatter"
	"github.com/oklog/ulid/v2"
	"gopkg.in/yaml.v3"
)

// Parser handles parsing and writing markdown files with metadata
type Parser struct {
	strict bool
}

// NewParser creates a new parser instance
func NewParser(strict bool) *Parser {
	return &Parser{strict: strict}
}

// HasMetadata checks if a file has YAML frontmatter
func HasMetadata(filename string) (bool, error) {
	file, err := os.Open(filename)
	if err != nil {
		return false, err
	}
	defer file.Close()

	// Read first few bytes to check for frontmatter delimiter
	buf := make([]byte, 4)
	n, err := file.Read(buf)
	if err != nil && err != io.EOF {
		return false, err
	}

	return n >= 3 && string(buf[:3]) == "---", nil
}

// ParseFile parses a markdown file and extracts metadata
func (p *Parser) ParseFile(filename string) (*Document, string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, "", fmt.Errorf("failed to open file %s: %w", filename, err)
	}
	defer file.Close()

	return p.Parse(file)
}

// Parse parses markdown content from a reader and extracts metadata
func (p *Parser) Parse(reader io.Reader) (*Document, string, error) {
	content, err := io.ReadAll(reader)
	if err != nil {
		return nil, "", fmt.Errorf("failed to read content: %w", err)
	}

	var doc Document

	// Parse frontmatter
	rest, err := frontmatter.Parse(bytes.NewReader(content), &doc)
	if err != nil {
		if p.strict {
			return nil, "", fmt.Errorf("failed to parse frontmatter: %w", err)
		}
		// If not strict, return empty metadata with full content
		return &Document{}, string(content), nil
	}

	// Validate the parsed document
	if err := p.validateDocument(&doc); err != nil {
		if p.strict {
			return nil, "", fmt.Errorf("validation failed: %w", err)
		}
	}

	return &doc, string(rest), nil
}

// WriteFile writes a document with metadata to a markdown file
func (p *Parser) WriteFile(filename string, doc *Document, content string) error {
	// Update timestamp
	now := time.Now()
	doc.UpdatedAt = &now

	// Ensure required fields
	if doc.Schema == "" {
		doc.Schema = "mdmeta/v1"
	}
	if doc.DocID == "" {
		doc.DocID = generateULID()
	}
	if doc.CreatedAt == nil {
		doc.CreatedAt = &now
	}
	if doc.Visibility == "" {
		doc.Visibility = VisibilityInternal
	}
	if doc.DataClass == "" {
		doc.DataClass = DataClassNone
	}

	// Marshal metadata to YAML
	var buf bytes.Buffer
	buf.WriteString("---\n")
	
	encoder := yaml.NewEncoder(&buf)
	encoder.SetIndent(2)
	if err := encoder.Encode(doc); err != nil {
		return fmt.Errorf("failed to encode metadata: %w", err)
	}
	encoder.Close()
	
	buf.WriteString("---\n")
	buf.WriteString(content)

	// Write to file
	return os.WriteFile(filename, buf.Bytes(), 0644)
}

// validateDocument performs basic validation on a document
func (p *Parser) validateDocument(doc *Document) error {
	var errors []string

	if doc.DocID == "" {
		errors = append(errors, "missing doc_id")
	}

	if doc.Title == "" {
		errors = append(errors, "missing title")
	}

	if doc.Schema == "" {
		errors = append(errors, "missing schema")
	}

	if doc.Status != "" && !IsValidStatus(doc.Status) {
		errors = append(errors, fmt.Sprintf("invalid status: %s", doc.Status))
	}

	if doc.Visibility != "" && !IsValidVisibility(doc.Visibility) {
		errors = append(errors, fmt.Sprintf("invalid visibility: %s", doc.Visibility))
	}

	if doc.DataClass != "" && !IsValidDataClass(doc.DataClass) {
		errors = append(errors, fmt.Sprintf("invalid data_class: %s", doc.DataClass))
	}

	if len(errors) > 0 {
		return fmt.Errorf("validation errors: %s", strings.Join(errors, ", "))
	}

	return nil
}

// generateULID creates a new ULID with the mdmeta prefix
func generateULID() string {
	return "ulid:" + ulid.Make().String()
}

