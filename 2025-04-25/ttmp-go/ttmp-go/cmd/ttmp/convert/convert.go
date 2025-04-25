package convert

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
)

// NewConvertCommand creates a new convert command
func NewConvertCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "convert [subcommand]",
		Short: "Convert TTMP documents to other formats",
		Long:  `Convert TTMP documents to various output formats like HTML, PDF, etc.`,
	}

	// Add subcommands
	cmd.AddCommand(newMarkdownCommand(logger, parser))
	cmd.AddCommand(newFrontmatterCommand(logger, parser))

	return cmd
}

// newMarkdownCommand creates a command to convert markdown documents
func newMarkdownCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		toFormat string
		outputPath string
		includeFrontmatter bool
	)

	cmd := &cobra.Command{
		Use:   "markdown [file]",
		Short: "Convert markdown documents",
		Long:  `Convert markdown documents to other formats like HTML, plain text, etc.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			filePath := args[0]

			// Read the document
			doc, err := parser.ParseFile(filePath)
			if err != nil {
				logger.Fatalf("Error reading file: %v", err)
			}

			// Determine output path if not specified
			if outputPath == "" {
				ext := fmt.Sprintf(".%s", strings.ToLower(toFormat))
				basePath := strings.TrimSuffix(filePath, filepath.Ext(filePath))
				outputPath = basePath + ext
			}

			// Convert document
			switch strings.ToLower(toFormat) {
			case "html":
				if err := convertToHTML(doc, outputPath, includeFrontmatter); err != nil {
					logger.Fatalf("Error converting to HTML: %v", err)
				}
				logger.Infof("Converted to HTML: %s", outputPath)
			case "txt", "text":
				if err := convertToText(doc, outputPath, includeFrontmatter); err != nil {
					logger.Fatalf("Error converting to text: %v", err)
				}
				logger.Infof("Converted to text: %s", outputPath)
			default:
				logger.Fatalf("Unsupported output format: %s", toFormat)
			}
		},
	}

	cmd.Flags().StringVar(&toFormat, "to", "html", "Output format (html, txt)")
	cmd.Flags().StringVarP(&outputPath, "output", "o", "", "Output file path")
	cmd.Flags().BoolVar(&includeFrontmatter, "include-frontmatter", false, "Include frontmatter in output")

	return cmd
}

// newFrontmatterCommand creates a command to convert frontmatter
func newFrontmatterCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		toFormat string
		outputPath string
	)

	cmd := &cobra.Command{
		Use:   "frontmatter [file]",
		Short: "Convert document frontmatter",
		Long:  `Extract and convert document frontmatter to other formats like JSON, YAML, etc.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			filePath := args[0]

			// Read the document
			doc, err := parser.ParseFile(filePath)
			if err != nil {
				logger.Fatalf("Error reading file: %v", err)
			}

			// Determine output path if not specified
			if outputPath == "" {
				ext := fmt.Sprintf(".%s", strings.ToLower(toFormat))
				basePath := strings.TrimSuffix(filePath, filepath.Ext(filePath))
				outputPath = basePath + ext
			}

			// Convert frontmatter
			switch strings.ToLower(toFormat) {
			case "json":
				if err := convertFrontmatterToJSON(doc, outputPath); err != nil {
					logger.Fatalf("Error converting to JSON: %v", err)
				}
				logger.Infof("Converted frontmatter to JSON: %s", outputPath)
			case "yaml", "yml":
				if err := convertFrontmatterToYAML(doc, outputPath); err != nil {
					logger.Fatalf("Error converting to YAML: %v", err)
				}
				logger.Infof("Converted frontmatter to YAML: %s", outputPath)
			default:
				logger.Fatalf("Unsupported output format: %s", toFormat)
			}
		},
	}

	cmd.Flags().StringVar(&toFormat, "to", "json", "Output format (json, yaml)")
	cmd.Flags().StringVarP(&outputPath, "output", "o", "", "Output file path")

	return cmd
}

// convertToHTML converts a TTMP document to HTML
func convertToHTML(doc *model.TTMPDocument, outputPath string, includeFrontmatter bool) error {
	// Simple conversion: just wrap content in HTML tags
	var html strings.Builder
	
	html.WriteString("<!DOCTYPE html>\n<html>\n<head>\n")
	html.WriteString(fmt.Sprintf("  <title>%s</title>\n", doc.GetTitle()))
	html.WriteString("  <meta charset=\"utf-8\">\n")
	html.WriteString("  <style>\n")
	html.WriteString("    body { font-family: Arial, sans-serif; line-height: 1.6; max-width: 800px; margin: 0 auto; padding: 20px; }\n")
	html.WriteString("    pre { background-color: #f5f5f5; padding: 10px; border-radius: 5px; overflow-x: auto; }\n")
	html.WriteString("    code { background-color: #f5f5f5; padding: 2px 5px; border-radius: 3px; }\n")
	html.WriteString("    h1, h2, h3, h4, h5, h6 { margin-top: 1.5em; margin-bottom: 0.5em; }\n")
	html.WriteString("    h1 { border-bottom: 2px solid #eee; padding-bottom: 0.3em; }\n")
	html.WriteString("    h2 { border-bottom: 1px solid #eee; padding-bottom: 0.3em; }\n")
	html.WriteString("    blockquote { border-left: 5px solid #eee; margin-left: 0; padding-left: 15px; }\n")
	html.WriteString("    .metadata { background-color: #f8f8f8; border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px; }\n")
	html.WriteString("  </style>\n")
	html.WriteString("</head>\n<body>\n")
	
	// Add metadata section if requested
	if includeFrontmatter {
		html.WriteString("<div class=\"metadata\">\n")
		html.WriteString("  <h2>Metadata</h2>\n")
		html.WriteString("  <ul>\n")
		
		if doc.ID != "" {
			html.WriteString(fmt.Sprintf("    <li><strong>ID:</strong> %s</li>\n", doc.ID))
		}
		
		if doc.Title != "" {
			html.WriteString(fmt.Sprintf("    <li><strong>Title:</strong> %s</li>\n", doc.Title))
		}
		
		if doc.DocumentType != "" {
			html.WriteString(fmt.Sprintf("    <li><strong>Type:</strong> %s</li>\n", doc.DocumentType))
		}
		
		if doc.Status != "" {
			html.WriteString(fmt.Sprintf("    <li><strong>Status:</strong> %s</li>\n", doc.Status))
		}
		
		if len(doc.Tags) > 0 {
			html.WriteString(fmt.Sprintf("    <li><strong>Tags:</strong> %s</li>\n", strings.Join(doc.Tags, ", ")))
		}
		
		if doc.Created != nil {
			html.WriteString(fmt.Sprintf("    <li><strong>Created:</strong> %s</li>\n", doc.Created.Format("2006-01-02")))
		}
		
		if doc.Updated != nil {
			html.WriteString(fmt.Sprintf("    <li><strong>Updated:</strong> %s</li>\n", doc.Updated.Format("2006-01-02")))
		}
		
		html.WriteString("  </ul>\n")
		html.WriteString("</div>\n")
	}
	
	// Very simple markdown-to-HTML conversion
	// Note: in a real implementation, you would use a proper markdown parser
	lines := strings.Split(doc.Content, "\n")
	inCodeBlock := false
	
	for _, line := range lines {
		if strings.HasPrefix(line, "```") {
			if inCodeBlock {
				html.WriteString("</pre>\n")
			} else {
				html.WriteString("<pre><code>")
			}
			inCodeBlock = !inCodeBlock
			continue
		}
		
		if inCodeBlock {
			html.WriteString(line)
			html.WriteString("\n")
			continue
		}
		
		if strings.HasPrefix(line, "# ") {
			html.WriteString(fmt.Sprintf("<h1>%s</h1>\n", strings.TrimPrefix(line, "# ")))
		} else if strings.HasPrefix(line, "## ") {
			html.WriteString(fmt.Sprintf("<h2>%s</h2>\n", strings.TrimPrefix(line, "## ")))
		} else if strings.HasPrefix(line, "### ") {
			html.WriteString(fmt.Sprintf("<h3>%s</h3>\n", strings.TrimPrefix(line, "### ")))
		} else if strings.HasPrefix(line, "#### ") {
			html.WriteString(fmt.Sprintf("<h4>%s</h4>\n", strings.TrimPrefix(line, "#### ")))
		} else if strings.HasPrefix(line, "- ") {
			html.WriteString(fmt.Sprintf("<ul>\n  <li>%s</li>\n</ul>\n", strings.TrimPrefix(line, "- ")))
		} else if line == "" {
			html.WriteString("<p></p>\n")
		} else {
			html.WriteString(fmt.Sprintf("<p>%s</p>\n", line))
		}
	}
	
	html.WriteString("</body>\n</html>")
	
	// Write to file
	return os.WriteFile(outputPath, []byte(html.String()), 0644)
}

// convertToText converts a TTMP document to plain text
func convertToText(doc *model.TTMPDocument, outputPath string, includeFrontmatter bool) error {
	var text strings.Builder
	
	// Add metadata section if requested
	if includeFrontmatter {
		text.WriteString("# METADATA\n\n")
		
		if doc.ID != "" {
			text.WriteString(fmt.Sprintf("ID: %s\n", doc.ID))
		}
		
		if doc.Title != "" {
			text.WriteString(fmt.Sprintf("Title: %s\n", doc.Title))
		}
		
		if doc.DocumentType != "" {
			text.WriteString(fmt.Sprintf("Type: %s\n", doc.DocumentType))
		}
		
		if doc.Status != "" {
			text.WriteString(fmt.Sprintf("Status: %s\n", doc.Status))
		}
		
		if len(doc.Tags) > 0 {
			text.WriteString(fmt.Sprintf("Tags: %s\n", strings.Join(doc.Tags, ", ")))
		}
		
		if doc.Created != nil {
			text.WriteString(fmt.Sprintf("Created: %s\n", doc.Created.Format("2006-01-02")))
		}
		
		if doc.Updated != nil {
			text.WriteString(fmt.Sprintf("Updated: %s\n", doc.Updated.Format("2006-01-02")))
		}
		
		text.WriteString("\n------------------------------\n\n")
	}
	
	// Add content
	text.WriteString(doc.Content)
	
	// Write to file
	return os.WriteFile(outputPath, []byte(text.String()), 0644)
}

// convertFrontmatterToJSON converts document frontmatter to JSON
func convertFrontmatterToJSON(doc *model.TTMPDocument, outputPath string) error {
	// In a real implementation, you would use json.Marshal
	// For now, we'll create a simplified JSON representation
	var json strings.Builder
	
	json.WriteString("{\n")
	
	if doc.ID != "" {
		json.WriteString(fmt.Sprintf("  \"id\": \"%s\",\n", doc.ID))
	}
	
	if doc.Title != "" {
		json.WriteString(fmt.Sprintf("  \"title\": \"%s\",\n", doc.Title))
	}
	
	if doc.DocumentType != "" {
		json.WriteString(fmt.Sprintf("  \"document_type\": \"%s\",\n", doc.DocumentType))
	}
	
	if doc.Status != "" {
		json.WriteString(fmt.Sprintf("  \"status\": \"%s\",\n", doc.Status))
	}
	
	if len(doc.Tags) > 0 {
		json.WriteString("  \"tags\": [\n")
		for i, tag := range doc.Tags {
			json.WriteString(fmt.Sprintf("    \"%s\"", tag))
			if i < len(doc.Tags)-1 {
				json.WriteString(",")
			}
			json.WriteString("\n")
		}
		json.WriteString("  ],\n")
	}
	
	if doc.Created != nil {
		json.WriteString(fmt.Sprintf("  \"created\": \"%s\",\n", doc.Created.Format(time.RFC3339)))
	}
	
	if doc.Updated != nil {
		json.WriteString(fmt.Sprintf("  \"updated\": \"%s\"\n", doc.Updated.Format(time.RFC3339)))
	}
	
	json.WriteString("}")
	
	// Write to file
	return os.WriteFile(outputPath, []byte(json.String()), 0644)
}

// convertFrontmatterToYAML converts document frontmatter to YAML
func convertFrontmatterToYAML(doc *model.TTMPDocument, outputPath string) error {
	// In a real implementation, you would use yaml.Marshal
	// For now, we'll create a simplified YAML representation
	var yaml strings.Builder
	
	if doc.ID != "" {
		yaml.WriteString(fmt.Sprintf("id: %s\n", doc.ID))
	}
	
	if doc.Title != "" {
		yaml.WriteString(fmt.Sprintf("title: \"%s\"\n", doc.Title))
	}
	
	if doc.DocumentType != "" {
		yaml.WriteString(fmt.Sprintf("document_type: %s\n", doc.DocumentType))
	}
	
	if doc.Status != "" {
		yaml.WriteString(fmt.Sprintf("status: %s\n", doc.Status))
	}
	
	if len(doc.Tags) > 0 {
		yaml.WriteString("tags:\n")
		for _, tag := range doc.Tags {
			yaml.WriteString(fmt.Sprintf("  - %s\n", tag))
		}
	}
	
	if doc.Created != nil {
		yaml.WriteString(fmt.Sprintf("created: %s\n", doc.Created.Format(time.RFC3339)))
	}
	
	if doc.Updated != nil {
		yaml.WriteString(fmt.Sprintf("updated: %s\n", doc.Updated.Format(time.RFC3339)))
	}
	
	// Write to file
	return os.WriteFile(outputPath, []byte(yaml.String()), 0644)
}