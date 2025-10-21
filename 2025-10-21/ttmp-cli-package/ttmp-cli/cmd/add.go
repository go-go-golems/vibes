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
	"github.com/ttmp/ttmp-cli/pkg/ticket"
)

type AddSettings struct {
	DocType string `glazed.parameter:"doc-type"`
	Name    string `glazed.parameter:"name"`
	Ticket  string `glazed.parameter:"ticket"`
	Topics  string `glazed.parameter:"topics"`
	Root    string `glazed.parameter:"root"`
}

func NewAddCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:   "add <doc-type> <name>",
		Short: "Create a new document from template",
		Long: `Create a new document from a template.

Valid doc types:
  working-note  - Free-form notes and meeting logs
  design-doc    - Structured architecture and design documents
  reference     - Prompt packs and API references
  playbook      - Command sequences and operational procedures
  script        - Temporary code with documentation

Examples:
  ttmp add working-note "initial context"
  ttmp add design-doc "api-architecture" --ticket MEN-3475
  ttmp add reference "websocket-triage" --topics chat,debugging`,
		Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			settings := &AddSettings{
				DocType: args[0],
				Name:    args[1],
			}
			settings.Ticket, _ = cmd.Flags().GetString("ticket")
			settings.Topics, _ = cmd.Flags().GetString("topics")
			settings.Root, _ = cmd.Flags().GetString("root")

			return runAdd(context.Background(), settings)
		},
	}

	cmd.Flags().String("ticket", "", "Ticket identifier (default: infer from current directory)")
	cmd.Flags().String("topics", "", "Comma-separated topics (default: inherit from ticket)")
	cmd.Flags().String("root", "./ttmp", "Root directory for ttmp")

	return cmd, nil
}

func runAdd(ctx context.Context, settings *AddSettings) error {
	// Validate doc type
	validTypes := map[string]string{
		"working-note": "various",
		"design-doc":   "design",
		"reference":    "reference",
		"playbook":     "playbooks",
		"script":       "scripts",
	}

	subdir, ok := validTypes[settings.DocType]
	if !ok {
		return fmt.Errorf("invalid doc type: %s (valid: %v)", settings.DocType, getKeys(validTypes))
	}

	// Determine ticket
	ticketID := settings.Ticket
	if ticketID == "" {
		// Try to infer from current directory
		var err error
		ticketID, err = ticket.GetCurrentTicket(settings.Root)
		if err != nil {
			return fmt.Errorf("could not determine ticket: %w (use --ticket flag)", err)
		}
	}

	// Find ticket directory
	tickets, err := ticket.FindTickets(settings.Root)
	if err != nil {
		return fmt.Errorf("failed to find tickets: %w", err)
	}

	var ticketDir string
	for _, t := range tickets {
		if t.Ticket == ticketID {
			ticketDir = t.Path
			break
		}
	}

	if ticketDir == "" {
		return fmt.Errorf("ticket not found: %s", ticketID)
	}

	// Get topics (from flag or inherit from ticket)
	var topics []string
	if settings.Topics != "" {
		topics = strings.Split(settings.Topics, ",")
		for i := range topics {
			topics[i] = strings.TrimSpace(topics[i])
		}
	} else {
		// Inherit from ticket index
		for _, t := range tickets {
			if t.Ticket == ticketID {
				topics = t.Topics
				break
			}
		}
	}

	// Generate filename
	slug := strings.ToLower(strings.ReplaceAll(settings.Name, " ", "-"))
	
	// Find next number in subdirectory
	targetDir := filepath.Join(ticketDir, subdir)
	entries, _ := os.ReadDir(targetDir)
	nextNum := 1
	for _, entry := range entries {
		if strings.HasSuffix(entry.Name(), ".md") {
			nextNum++
		}
	}

	filename := fmt.Sprintf("%02d-%s.md", nextNum, slug)
	outputPath := filepath.Join(targetDir, filename)

	// Prepare template data
	now := time.Now().Format("2006-01-02")
	data := map[string]interface{}{
		"Ticket": ticketID,
		"Title":  settings.Name,
		"Topics": topics,
		"Date":   now,
		"Intent": "short-term",
	}

	// Create from template
	tmplContent, err := tmpl.GetTemplate(settings.DocType)
	if err != nil {
		return err
	}

	var buf bytes.Buffer
	if err := tmplContent.Execute(&buf, data); err != nil {
		return fmt.Errorf("failed to execute template: %w", err)
	}

	if err := os.WriteFile(outputPath, buf.Bytes(), 0644); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	fmt.Printf("✓ Created %s: %s\n", settings.DocType, outputPath)
	fmt.Printf("\nEdit the file to add content:\n")
	fmt.Printf("  $EDITOR %s\n", outputPath)

	return nil
}

func getKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

