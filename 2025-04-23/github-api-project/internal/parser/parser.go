package parser

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/github-api-project/internal/models"
	"gopkg.in/yaml.v3"
)

// Parser handles parsing and validation of YAML files
type Parser struct {
	filePath string
}

// NewParser creates a new parser for the given file path
func NewParser(filePath string) *Parser {
	return &Parser{
		filePath: filePath,
	}
}

// Parse reads and parses the YAML file into the ProjectManagement struct
func (p *Parser) Parse() (*models.ProjectManagement, error) {
	data, err := ioutil.ReadFile(p.filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file: %w", err)
	}

	// Replace environment variables in the YAML content
	content := string(data)
	content = p.replaceEnvVars(content)

	var pm models.ProjectManagement
	err = yaml.Unmarshal([]byte(content), &pm)
	if err != nil {
		return nil, fmt.Errorf("failed to parse YAML: %w", err)
	}

	if err := p.validate(&pm); err != nil {
		return nil, fmt.Errorf("validation failed: %w", err)
	}

	return &pm, nil
}

// replaceEnvVars replaces environment variables in the format ${VAR_NAME} with their values
func (p *Parser) replaceEnvVars(content string) string {
	result := content
	// Find all ${...} patterns
	for {
		start := strings.Index(result, "${")
		if start == -1 {
			break
		}
		end := strings.Index(result[start:], "}")
		if end == -1 {
			break
		}
		end += start

		// Extract the variable name
		varName := result[start+2 : end]
		// Get the environment variable value
		varValue := os.Getenv(varName)
		// Replace the ${...} with the value
		result = result[:start] + varValue + result[end+1:]
	}
	return result
}

// validate performs validation on the parsed ProjectManagement struct
func (p *Parser) validate(pm *models.ProjectManagement) error {
	// Validate config section
	if pm.Config.Organization == "" {
		return fmt.Errorf("organization is required in config section")
	}
	if pm.Config.Auth.Token == "" {
		return fmt.Errorf("auth token is required in config section")
	}

	// Validate project section if present
	if pm.Project.Name == "" && pm.Project.ID == "" {
		return fmt.Errorf("either project name or ID is required in project section")
	}

	// Validate issues section if present
	for i, issue := range pm.Issues {
		if issue.Title == "" && issue.ID == "" {
			return fmt.Errorf("either title or ID is required for issue at index %d", i)
		}
		// Recursively validate sub-issues
		if err := p.validateSubIssues(issue.SubIssues, 0); err != nil {
			return err
		}
	}

	return nil
}

// validateSubIssues recursively validates sub-issues
func (p *Parser) validateSubIssues(issues []models.Issue, depth int) error {
	if depth > 7 {
		return fmt.Errorf("maximum nesting depth of 8 levels exceeded for sub-issues")
	}

	for i, issue := range issues {
		if issue.Title == "" && issue.ID == "" {
			return fmt.Errorf("either title or ID is required for sub-issue at depth %d, index %d", depth, i)
		}
		// Recursively validate sub-issues
		if err := p.validateSubIssues(issue.SubIssues, depth+1); err != nil {
			return err
		}
	}

	return nil
}

// UpdateWithIDs updates the YAML file with IDs from the provided ProjectManagement struct
func (p *Parser) UpdateWithIDs(pm *models.ProjectManagement) error {
	data, err := yaml.Marshal(pm)
	if err != nil {
		return fmt.Errorf("failed to marshal YAML: %w", err)
	}

	err = ioutil.WriteFile(p.filePath, data, 0644)
	if err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	return nil
}
