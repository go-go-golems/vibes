package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/github-api-project/internal/api"
	"github.com/github-api-project/internal/models"
	"github.com/github-api-project/internal/parser"
	"github.com/spf13/cobra"
)

func main() {
	var rootCmd = &cobra.Command{
		Use:   "ghpm",
		Short: "GitHub Project Manager - Manage GitHub projects and issues using YAML",
		Long: `GitHub Project Manager (ghpm) is a tool for managing GitHub projects and issues using YAML.
It allows you to create and update projects, issues, sub-issues, and more using a simple YAML DSL.`,
	}

	var applyCmd = &cobra.Command{
		Use:   "apply [file]",
		Short: "Apply a YAML configuration file",
		Long:  `Apply a YAML configuration file to create or update GitHub projects and issues.`,
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return applyConfig(args[0])
		},
	}

	var validateCmd = &cobra.Command{
		Use:   "validate [file]",
		Short: "Validate a YAML configuration file",
		Long:  `Validate a YAML configuration file without applying any changes.`,
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return validateConfig(args[0])
		},
	}

	var initCmd = &cobra.Command{
		Use:   "init [directory]",
		Short: "Initialize a new YAML configuration file",
		Long:  `Create a new YAML configuration file with a basic structure.`,
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			dir := "."
			if len(args) > 0 {
				dir = args[0]
			}
			return initConfig(dir)
		},
	}

	rootCmd.AddCommand(applyCmd)
	rootCmd.AddCommand(validateCmd)
	rootCmd.AddCommand(initCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func initConfig(dir string) error {
	// Create directory if it doesn't exist
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	// Create a basic YAML configuration file
	configPath := filepath.Join(dir, "github-project.yaml")
	
	// Check if file already exists
	if _, err := os.Stat(configPath); err == nil {
		return fmt.Errorf("file already exists: %s", configPath)
	}

	// Basic template
	template := `# GitHub Project Management Configuration

config:
  auth:
    token: "${GITHUB_TOKEN}"  # Set the GITHUB_TOKEN environment variable
  organization: "your-org"    # Replace with your organization or username

project:
  name: "My Project"
  description: "Project description"
  public: false
  columns:
    - name: "To Do"
    - name: "In Progress"
    - name: "Done"

issues:
  - title: "First Issue"
    body: "Description of the first issue"
    status: "To Do"
    assignees:
      - "username"
    labels:
      - name: "bug"
        color: "d73a4a"
    sub_issues:
      - title: "Sub-task"
        body: "Description of the sub-task"
        status: "To Do"
`

	// Write the file
	if err := os.WriteFile(configPath, []byte(template), 0644); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	fmt.Printf("Created configuration file: %s\n", configPath)
	fmt.Println("Edit this file with your project details, then run:")
	fmt.Printf("  ghpm validate %s\n", configPath)
	fmt.Printf("  ghpm apply %s\n", configPath)
	return nil
}

func validateConfig(filePath string) error {
	// Parse the YAML file
	p := parser.NewParser(filePath)
	_, err := p.Parse()
	if err != nil {
		return fmt.Errorf("validation failed: %w", err)
	}

	fmt.Println("Configuration file is valid")
	return nil
}

func applyConfig(filePath string) error {
	// Parse the YAML file
	p := parser.NewParser(filePath)
	pm, err := p.Parse()
	if err != nil {
		return fmt.Errorf("failed to parse config file: %w", err)
	}

	// Create API client
	client := api.NewClient(pm.Config.Auth.Token, pm.Config.Organization)

	// Get organization ID
	orgID, err := client.GetOrganizationID()
	if err != nil {
		return fmt.Errorf("failed to get organization ID: %w", err)
	}

	// Handle project
	if pm.Project.Name != "" || pm.Project.ID != "" {
		if pm.Project.ID == "" {
			// Create new project
			fmt.Println("Creating project:", pm.Project.Name)
			projectID, err := client.CreateProject(pm.Project, orgID)
			if err != nil {
				return fmt.Errorf("failed to create project: %w", err)
			}
			pm.Project.ID = projectID
			fmt.Println("Project created with ID:", projectID)
		} else {
			// Project exists, update it if needed
			fmt.Println("Using existing project with ID:", pm.Project.ID)
			if err := client.UpdateProject(pm.Project); err != nil {
				return fmt.Errorf("failed to update project: %w", err)
			}
		}
	}

	// Get status field ID and options map for the project
	var statusFieldID string
	var statusOptionsMap map[string]string
	if pm.Project.ID != "" {
		var err error
		statusFieldID, statusOptionsMap, err = client.GetStatusField(pm.Project.ID)
		if err != nil {
			fmt.Println("Warning: Could not get status field:", err)
			// Continue without status updates
		}
	}

	// Process issues
	for i := range pm.Issues {
		if err := processIssue(&pm.Issues[i], pm.Project.ID, "", client, statusFieldID, statusOptionsMap, 0); err != nil {
			return fmt.Errorf("failed to process issue: %w", err)
		}
	}

	// Update the YAML file with new IDs
	if err := p.UpdateWithIDs(pm); err != nil {
		return fmt.Errorf("failed to update YAML file with IDs: %w", err)
	}

	fmt.Println("Configuration applied successfully")
	return nil
}

func processIssue(issue *models.Issue, projectID, parentIssueID string, client *api.Client, statusFieldID string, statusOptionsMap map[string]string, depth int) error {
	if depth > 7 {
		return fmt.Errorf("maximum nesting depth of 8 levels exceeded for sub-issues")
	}

	if issue.ID == "" {
		// Create new issue
		fmt.Println("Creating issue:", issue.Title)
		var issueID string
		var err error

		// Create as draft issue in project
		issueID, err = client.CreateDraftIssue(*issue, projectID)
		if err != nil {
			return fmt.Errorf("failed to create draft issue: %w", err)
		}
		issue.ID = issueID
		fmt.Println("Issue created with ID:", issueID)

		// Update status if specified and status field is available
		if issue.Status != "" && statusFieldID != "" && statusOptionsMap != nil {
			if optionID, ok := statusOptionsMap[issue.Status]; ok {
				if err := client.UpdateIssueStatus(projectID, issueID, statusFieldID, optionID); err != nil {
					return fmt.Errorf("failed to update issue status: %w", err)
				}
				fmt.Println("Updated issue status to:", issue.Status)
			} else {
				fmt.Printf("Warning: Status '%s' not found in project columns\n", issue.Status)
			}
		}

		// Add assignees if specified
		if len(issue.Assignees) > 0 {
			// Get assignee IDs
			assigneeIDs := make([]string, len(issue.Assignees))
			for i, assignee := range issue.Assignees {
				assigneeID, err := client.GetUserID(assignee)
				if err != nil {
					return fmt.Errorf("failed to get user ID for %s: %w", assignee, err)
				}
				assigneeIDs[i] = assigneeID
			}

			// Add assignees to issue
			if err := client.AddAssignees(issueID, assigneeIDs); err != nil {
				return fmt.Errorf("failed to add assignees: %w", err)
			}
			fmt.Println("Added assignees to issue")
		}

		// Add labels if specified
		// Note: This is a simplified implementation. In a complete solution,
		// you would need to get the repository ID and create/get labels.
		if len(issue.Labels) > 0 {
			fmt.Println("Labels specified but not implemented in this version")
		}

		// Add comments if specified
		for _, comment := range issue.Comments {
			if err := client.AddComment(issueID, comment.Body); err != nil {
				return fmt.Errorf("failed to add comment: %w", err)
			}
			fmt.Println("Added comment to issue")
		}
	} else {
		// Issue exists, update it if needed
		fmt.Println("Using existing issue with ID:", issue.ID)
		
		// Update status if specified and status field is available
		if issue.Status != "" && statusFieldID != "" && statusOptionsMap != nil {
			if optionID, ok := statusOptionsMap[issue.Status]; ok {
				if err := client.UpdateIssueStatus(projectID, issue.ID, statusFieldID, optionID); err != nil {
					return fmt.Errorf("failed to update issue status: %w", err)
				}
				fmt.Println("Updated issue status to:", issue.Status)
			} else {
				fmt.Printf("Warning: Status '%s' not found in project columns\n", issue.Status)
			}
		}
	}

	// Link as sub-issue if parent exists
	if parentIssueID != "" {
		if err := client.AddSubIssue(parentIssueID, issue.ID); err != nil {
			return fmt.Errorf("failed to add sub-issue: %w", err)
		}
		fmt.Println("Linked issue as sub-issue to parent:", parentIssueID)
	}

	// Process sub-issues
	for i := range issue.SubIssues {
		if err := processIssue(&issue.SubIssues[i], projectID, issue.ID, client, statusFieldID, statusOptionsMap, depth+1); err != nil {
			return fmt.Errorf("failed to process sub-issue: %w", err)
		}
	}

	return nil
}
