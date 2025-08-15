package parser

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/adrg/frontmatter"
	"markdown-manager/pkg/metadata"
	"gopkg.in/yaml.v3"
)

// ParseMarkdownFile parses a markdown file and extracts metadata and content
func ParseMarkdownFile(filePath string) (*metadata.DocumentFile, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %w", filePath, err)
	}

	var meta metadata.DocumentMetadata
	body, err := frontmatter.Parse(strings.NewReader(string(content)), &meta)
	if err != nil {
		return nil, fmt.Errorf("failed to parse frontmatter in %s: %w", filePath, err)
	}

	// Get file info
	fileInfo, err := os.Stat(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to get file info for %s: %w", filePath, err)
	}

	// Set default values if not present
	if meta.Created.IsZero() {
		meta.Created = fileInfo.ModTime()
	}
	if meta.Modified.IsZero() {
		meta.Modified = fileInfo.ModTime()
	}
	if meta.Title == "" {
		meta.Title = strings.TrimSuffix(filepath.Base(filePath), ".md")
	}

	return &metadata.DocumentFile{
		Path:     filePath,
		Metadata: meta,
		Content:  string(body),
		Size:     fileInfo.Size(),
		ModTime:  fileInfo.ModTime(),
	}, nil
}

// UpdateMetadata updates the frontmatter in a markdown file
func UpdateMetadata(filePath string, meta metadata.DocumentMetadata) error {
	// Read current content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file %s: %w", filePath, err)
	}

	// Parse existing frontmatter and content
	var existingMeta metadata.DocumentMetadata
	body, err := frontmatter.Parse(strings.NewReader(string(content)), &existingMeta)
	if err != nil {
		return fmt.Errorf("failed to parse existing frontmatter in %s: %w", filePath, err)
	}

	// Update modified time
	meta.Modified = time.Now()

	// Marshal new metadata to YAML
	yamlData, err := yaml.Marshal(meta)
	if err != nil {
		return fmt.Errorf("failed to marshal metadata to YAML: %w", err)
	}

	// Construct new file content
	newContent := fmt.Sprintf("---\n%s---\n%s", string(yamlData), string(body))

	// Write back to file
	err = ioutil.WriteFile(filePath, []byte(newContent), 0644)
	if err != nil {
		return fmt.Errorf("failed to write updated file %s: %w", filePath, err)
	}

	return nil
}

// FindMarkdownFiles recursively finds all markdown files in a directory
func FindMarkdownFiles(rootDir string) ([]string, error) {
	var files []string

	err := filepath.Walk(rootDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() && strings.HasSuffix(strings.ToLower(path), ".md") {
			files = append(files, path)
		}

		return nil
	})

	return files, err
}

// TouchLastUsed updates the last_used timestamp in a file's metadata
func TouchLastUsed(filePath string) error {
	doc, err := ParseMarkdownFile(filePath)
	if err != nil {
		return err
	}

	now := time.Now()
	doc.Metadata.LastUsed = &now

	return UpdateMetadata(filePath, doc.Metadata)
}

