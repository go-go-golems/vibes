package metadata

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strings"

	"gopkg.in/yaml.v3"
)

// ParseFile reads a markdown file and extracts YAML frontmatter
func ParseFile(filepath string) (*DocumentMetadata, string, error) {
	content, err := os.ReadFile(filepath)
	if err != nil {
		return nil, "", fmt.Errorf("failed to read file: %w", err)
	}

	return Parse(content)
}

// Parse extracts YAML frontmatter from markdown content
func Parse(content []byte) (*DocumentMetadata, string, error) {
	scanner := bufio.NewScanner(bytes.NewReader(content))
	
	// Check for frontmatter start
	if !scanner.Scan() {
		return nil, string(content), nil
	}
	
	firstLine := strings.TrimSpace(scanner.Text())
	if firstLine != "---" {
		// No frontmatter
		return nil, string(content), nil
	}

	// Read frontmatter
	var frontmatterLines []string
	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "---" {
			break
		}
		frontmatterLines = append(frontmatterLines, line)
	}

	// Read remaining content
	var bodyLines []string
	for scanner.Scan() {
		bodyLines = append(bodyLines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, "", fmt.Errorf("error scanning file: %w", err)
	}

	// Parse YAML frontmatter
	var meta DocumentMetadata
	if len(frontmatterLines) > 0 {
		frontmatterYAML := strings.Join(frontmatterLines, "\n")
		if err := yaml.Unmarshal([]byte(frontmatterYAML), &meta); err != nil {
			return nil, "", fmt.Errorf("failed to parse YAML frontmatter: %w", err)
		}
	}

	body := strings.Join(bodyLines, "\n")
	return &meta, body, nil
}

// HasFrontmatter checks if content has YAML frontmatter
func HasFrontmatter(content []byte) bool {
	scanner := bufio.NewScanner(bytes.NewReader(content))
	if !scanner.Scan() {
		return false
	}
	return strings.TrimSpace(scanner.Text()) == "---"
}

