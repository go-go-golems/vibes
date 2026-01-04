package cmd

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/codereview/cli/internal/database"
	"github.com/codereview/cli/internal/git"
	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
)

func newInitCommand() *cobra.Command {
	var reviewer string
	var configFile string
	var force bool

	cmd := &cobra.Command{
		Use:   "init",
		Short: "Initialize a code review repository",
		Long:  "Initialize a .codereview directory with database and configuration files",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runInit(reviewer, configFile, force)
		},
	}

	cmd.Flags().StringVar(&reviewer, "reviewer", "", "Default reviewer email")
	cmd.Flags().StringVar(&configFile, "config-file", "", "Path to config file template")
	cmd.Flags().BoolVar(&force, "force", false, "Force initialization even if directory exists")

	return cmd
}

func runInit(reviewer, configFile string, force bool) error {
	// Check if we're in a git repository
	repo, err := git.NewRepository("")
	if err != nil {
		return fmt.Errorf("not in a git repository: %w", err)
	}

	// Create .codereview directory
	codeReviewDir := ".codereview"
	if _, err := os.Stat(codeReviewDir); err == nil && !force {
		return fmt.Errorf("code review repository already initialized (use --force to reinitialize)")
	}

	if err := os.MkdirAll(codeReviewDir, 0755); err != nil {
		return fmt.Errorf("failed to create .codereview directory: %w", err)
	}

	// Create subdirectories
	dirs := []string{
		filepath.Join(codeReviewDir, "reviews"),
		filepath.Join(codeReviewDir, "templates"),
	}

	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0755); err != nil {
			return fmt.Errorf("failed to create directory %s: %w", dir, err)
		}
	}

	// Initialize database
	dbPath := filepath.Join(codeReviewDir, "reviews.db")
	db, err := database.New(dbPath)
	if err != nil {
		return fmt.Errorf("failed to initialize database: %w", err)
	}
	defer db.Close()

	// Create default configuration
	config := createDefaultConfig(reviewer)
	configPath := filepath.Join(codeReviewDir, "config.yml")
	
	if configFile != "" {
		// Copy from template file
		if err := copyFile(configFile, configPath); err != nil {
			return fmt.Errorf("failed to copy config file: %w", err)
		}
	} else {
		// Create default config
		configData, err := yaml.Marshal(config)
		if err != nil {
			return fmt.Errorf("failed to marshal config: %w", err)
		}

		if err := os.WriteFile(configPath, configData, 0644); err != nil {
			return fmt.Errorf("failed to write config file: %w", err)
		}
	}

	// Create default template
	templatePath := filepath.Join(codeReviewDir, "templates", "default.yml")
	template := createDefaultTemplate()
	templateData, err := yaml.Marshal(template)
	if err != nil {
		return fmt.Errorf("failed to marshal template: %w", err)
	}

	if err := os.WriteFile(templatePath, templateData, 0644); err != nil {
		return fmt.Errorf("failed to write template file: %w", err)
	}

	// Get current branch for output
	branch, _ := repo.GetCurrentBranch()

	// Output success message
	fmt.Printf("✅ Code review repository initialized\n")
	fmt.Printf("   Directory: %s\n", codeReviewDir)
	fmt.Printf("   Database:  %s\n", dbPath)
	fmt.Printf("   Config:    %s\n", configPath)
	fmt.Printf("   Branch:    %s\n", branch)

	return nil
}

func createDefaultConfig(reviewer string) map[string]interface{} {
	return map[string]interface{}{
		"settings": map[string]interface{}{
			"default_reviewer": reviewer,
			"require_approval": false,
			"auto_assign":      false,
			"database_path":    ".codereview/reviews.db",
		},
		"server": map[string]interface{}{
			"port": 8080,
			"host": "localhost",
		},
		"git": map[string]interface{}{
			"default_base": "main",
			"ignore_patterns": []string{
				"*.log",
				"node_modules/",
				".git/",
			},
		},
		"templates": map[string]interface{}{
			"security": map[string]interface{}{
				"tags": []string{"security"},
				"required_checks": []string{
					"Input validation",
					"Authentication",
					"Authorization",
				},
			},
			"performance": map[string]interface{}{
				"tags": []string{"performance"},
				"required_checks": []string{
					"Time complexity",
					"Memory usage",
				},
			},
		},
	}
}

func createDefaultTemplate() map[string]interface{} {
	return map[string]interface{}{
		"review": map[string]interface{}{
			"title":    "Code Review",
			"reviewer": "",
			"status":   "pending",
		},
		"annotations": []interface{}{},
		"summary": map[string]interface{}{
			"files_changed":  0,
			"lines_added":    0,
			"lines_removed":  0,
			"issues_found":   0,
			"suggestions":    0,
		},
		"tags": []string{},
	}
}

func copyFile(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	return os.WriteFile(dst, data, 0644)
}
