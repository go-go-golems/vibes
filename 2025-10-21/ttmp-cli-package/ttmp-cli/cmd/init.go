package cmd

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/spf13/cobra"
	tmpl "github.com/ttmp/ttmp-cli/pkg/templates"
)

type InitSettings struct {
	Ticket string   `glazed.parameter:"ticket"`
	Title  string   `glazed.parameter:"title"`
	Topics string   `glazed.parameter:"topics"`
	Owners string   `glazed.parameter:"owners"`
	Intent string   `glazed.parameter:"intent"`
	Root   string   `glazed.parameter:"root"`
}

func NewInitCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:   "init [ticket]",
		Short: "Initialize a new ticket directory",
		Long: `Initialize a new ticket directory with standard structure and metadata.

Creates the following structure:
  ttmp/MEN-XXX-slug/
    index.md           - Canonical landing page
    tasks.md           - Task list
    changelog.md       - Decision log
    various/           - Working notes
    design/            - Design documents
    reference/         - Prompt packs and references
    playbooks/         - Command sequences
    scripts/           - Temporary code

If ticket is omitted, attempts to derive from git branch name.`,
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			settings := &InitSettings{}
			
			// Get flags
			ticket, _ := cmd.Flags().GetString("ticket")
			if len(args) > 0 {
				ticket = args[0]
			}
			settings.Ticket = ticket
			settings.Title, _ = cmd.Flags().GetString("title")
			settings.Topics, _ = cmd.Flags().GetString("topics")
			settings.Owners, _ = cmd.Flags().GetString("owners")
			settings.Intent, _ = cmd.Flags().GetString("intent")
			settings.Root, _ = cmd.Flags().GetString("root")

			return runInit(context.Background(), settings)
		},
	}

	cmd.Flags().String("ticket", "", "Ticket identifier (e.g., MEN-3475)")
	cmd.Flags().StringP("title", "t", "", "Human-readable title for the ticket (required)")
	cmd.MarkFlagRequired("title")
	cmd.Flags().String("topics", "", "Comma-separated list of topics (required)")
	cmd.MarkFlagRequired("topics")
	cmd.Flags().String("owners", "", "Comma-separated list of owners")
	cmd.Flags().String("intent", "short-term", "Intent: short-term, long-term, or throwaway")
	cmd.Flags().String("root", "./ttmp", "Root directory for ttmp")

	return cmd, nil
}

func runInit(ctx context.Context, settings *InitSettings) error {
	// Validate ticket format
	if settings.Ticket == "" {
		return fmt.Errorf("ticket identifier is required")
	}

	// Parse topics
	topics := strings.Split(settings.Topics, ",")
	for i := range topics {
		topics[i] = strings.TrimSpace(topics[i])
	}

	// Parse owners
	var owners []string
	if settings.Owners != "" {
		owners = strings.Split(settings.Owners, ",")
		for i := range owners {
			owners[i] = strings.TrimSpace(owners[i])
		}
	}

	// Create ticket directory
	slug := strings.ToLower(strings.ReplaceAll(settings.Title, " ", "-"))
	ticketDir := filepath.Join(settings.Root, fmt.Sprintf("%s-%s", settings.Ticket, slug))

	if err := os.MkdirAll(ticketDir, 0755); err != nil {
		return fmt.Errorf("failed to create ticket directory: %w", err)
	}

	// Create subdirectories
	subdirs := []string{"various", "design", "reference", "playbooks", "scripts"}
	for _, dir := range subdirs {
		path := filepath.Join(ticketDir, dir)
		if err := os.MkdirAll(path, 0755); err != nil {
			return fmt.Errorf("failed to create directory %s: %w", dir, err)
		}
		// Create .gitkeep
		gitkeep := filepath.Join(path, ".gitkeep")
		if err := os.WriteFile(gitkeep, []byte(""), 0644); err != nil {
			return fmt.Errorf("failed to create .gitkeep: %w", err)
		}
	}

	// Prepare template data
	now := time.Now().Format("2006-01-02")
	data := map[string]interface{}{
		"Ticket":  settings.Ticket,
		"Title":   settings.Title,
		"Topics":  topics,
		"Owners":  owners,
		"Intent":  settings.Intent,
		"Date":    now,
		"Status":  "draft",
		"Summary": settings.Title,
	}

	// Create index.md
	if err := createFromTemplate("index", filepath.Join(ticketDir, "index.md"), data); err != nil {
		return fmt.Errorf("failed to create index.md: %w", err)
	}

	// Create tasks.md
	if err := createFromTemplate("tasks", filepath.Join(ticketDir, "tasks.md"), data); err != nil {
		return fmt.Errorf("failed to create tasks.md: %w", err)
	}

	// Create changelog.md
	if err := createFromTemplate("changelog", filepath.Join(ticketDir, "changelog.md"), data); err != nil {
		return fmt.Errorf("failed to create changelog.md: %w", err)
	}

	fmt.Printf("✓ Created ticket directory: %s\n", ticketDir)
	fmt.Printf("✓ Created index.md, tasks.md, changelog.md\n")
	fmt.Printf("✓ Created subdirectories: %s\n", strings.Join(subdirs, ", "))
	fmt.Printf("\nNext steps:\n")
	fmt.Printf("  cd %s\n", ticketDir)
	fmt.Printf("  ttmp add working-note \"initial context\"\n")

	return nil
}

func createFromTemplate(templateName, outputPath string, data interface{}) error {
	tmplContent, err := tmpl.GetTemplate(templateName)
	if err != nil {
		return err
	}

	var buf bytes.Buffer
	if err := tmplContent.Execute(&buf, data); err != nil {
		return fmt.Errorf("failed to execute template: %w", err)
	}

	return os.WriteFile(outputPath, buf.Bytes(), 0644)
}



