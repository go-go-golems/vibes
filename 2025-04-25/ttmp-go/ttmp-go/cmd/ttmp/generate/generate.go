package generate

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/model"
	"github.com/scrapybara/ttmp/pkg/parser"
)

// NewGenerateCommand creates a new generate command
func NewGenerateCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "generate [subcommand]",
		Short: "Generate content for TTMP documents",
		Long:  `Generate various types of content for TTMP documents, such as table of contents, metadata, etc.`,
	}

	// Add subcommands
	cmd.AddCommand(newTOCCommand(logger, parser))
	cmd.AddCommand(newMetadataCommand(logger, parser))
	cmd.AddCommand(newTemplateCommand(logger, parser))

	return cmd
}

// newTOCCommand creates a command to generate table of contents
func newTOCCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		maxDepth int
		insertInPlace bool
		startMarker string
		endMarker string
	)

	cmd := &cobra.Command{
		Use:   "toc [file]",
		Short: "Generate table of contents",
		Long:  `Generate a markdown table of contents from headings in the document.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			filePath := args[0]

			// Read the document
			doc, err := parser.ParseFile(filePath)
			if err != nil {
				logger.Fatalf("Error reading file: %v", err)
			}

			// Generate TOC
			toc := generateTOC(doc.Content, maxDepth)

			if insertInPlace {
				// Update the document with the TOC
				if err := insertTOC(filePath, toc, startMarker, endMarker); err != nil {
					logger.Fatalf("Error updating file: %v", err)
				}
				logger.Infof("Table of contents inserted into %s", filePath)
			} else {
				// Just print the TOC
				fmt.Println(toc)
			}
		},
	}

	cmd.Flags().IntVar(&maxDepth, "max-depth", 3, "Maximum heading level to include in TOC")
	cmd.Flags().BoolVar(&insertInPlace, "in-place", false, "Insert TOC directly into the document")
	cmd.Flags().StringVar(&startMarker, "start-marker", "<!-- TOC -->", "Marker to indicate start of TOC section")
	cmd.Flags().StringVar(&endMarker, "end-marker", "<!-- /TOC -->", "Marker to indicate end of TOC section")

	return cmd
}

// generateTOC generates a table of contents from a markdown string
func generateTOC(content string, maxDepth int) string {
	var toc strings.Builder
	toc.WriteString("## Table of Contents\n\n")

	scanner := bufio.NewScanner(strings.NewReader(content))
	headingPattern := regexp.MustCompile(`^(#{1,6})\s+(.+)$`)

	for scanner.Scan() {
		line := scanner.Text()
		matches := headingPattern.FindStringSubmatch(line)
		
		if len(matches) == 3 {
			level := len(matches[1])
			if level <= maxDepth {
				heading := matches[2]
				anchor := makeAnchor(heading)
				indent := strings.Repeat("  ", level-1)
				
				toc.WriteString(fmt.Sprintf("%s- [%s](#%s)\n", indent, heading, anchor))
			}
		}
	}

	return toc.String()
}

// makeAnchor converts a heading to an anchor link
func makeAnchor(heading string) string {
	// Remove special characters
	re := regexp.MustCompile(`[^\w\- ]`)
	anchor := re.ReplaceAllString(heading, "")
	
	// Replace spaces with dashes
	anchor = strings.ReplaceAll(anchor, " ", "-")
	
	// Convert to lowercase
	anchor = strings.ToLower(anchor)
	
	return anchor
}

// insertTOC inserts a table of contents into a markdown file
func insertTOC(filePath, toc, startMarker, endMarker string) error {
	// Read the file
	content, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}
	
	contentStr := string(content)
	
	// Check if markers exist
	startIdx := strings.Index(contentStr, startMarker)
	endIdx := strings.Index(contentStr, endMarker)
	
	var newContent string
	
	if startIdx != -1 && endIdx != -1 && startIdx < endIdx {
		// Replace content between markers
		beforeTOC := contentStr[:startIdx+len(startMarker)]
		afterTOC := contentStr[endIdx:]
		newContent = beforeTOC + "\n\n" + toc + "\n" + afterTOC
	} else {
		// Find the first heading and insert TOC before it
		headingPattern := regexp.MustCompile(`(?m)^#`)
		loc := headingPattern.FindStringIndex(contentStr)
		
		if loc != nil {
			// Insert after the first heading and a blank line
			headingEndIdx := strings.Index(contentStr[loc[0]:], "\n")
			if headingEndIdx == -1 {
				headingEndIdx = len(contentStr) - loc[0]
			} else {
				headingEndIdx += loc[0]
			}
			
			beforeTOC := contentStr[:headingEndIdx+1]
			afterTOC := contentStr[headingEndIdx+1:]
			newContent = beforeTOC + "\n" + startMarker + "\n\n" + toc + "\n" + endMarker + "\n\n" + afterTOC
		} else {
			// Append to the end if no heading found
			newContent = contentStr + "\n\n" + startMarker + "\n\n" + toc + "\n" + endMarker + "\n"
		}
	}
	
	// Write the file
	return os.WriteFile(filePath, []byte(newContent), 0644)
}

// newMetadataCommand creates a command to generate or update metadata
func newMetadataCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		updateTimestamp bool
		addTags string
		addSourceFiles string
		calculateWordCount bool
		generateID bool
	)

	cmd := &cobra.Command{
		Use:   "metadata [file]",
		Short: "Generate or update document metadata",
		Long:  `Generate or update TTMP document metadata fields such as timestamps, tags, etc.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			filePath := args[0]

			// Read the document
			doc, err := parser.ParseFile(filePath)
			if err != nil {
				logger.Fatalf("Error reading file: %v", err)
			}

			modified := false

			// Update timestamp
			if updateTimestamp {
				now := time.Now()
				doc.Updated = &now
				modified = true
				logger.Info("Updated timestamp")
			}

			// Add tags
			if addTags != "" {
				newTags := strings.Split(addTags, ",")
				for _, tag := range newTags {
					tag = strings.TrimSpace(tag)
					if tag != "" {
						// Check if tag already exists
						exists := false
						for _, existingTag := range doc.Tags {
							if existingTag == tag {
								exists = true
								break
							}
						}
						
						if !exists {
							doc.Tags = append(doc.Tags, tag)
							modified = true
						}
					}
				}
				logger.Infof("Added tags: %s", addTags)
			}

			// Add source files
			if addSourceFiles != "" {
				newSources := strings.Split(addSourceFiles, ",")
				for _, source := range newSources {
					source = strings.TrimSpace(source)
					if source != "" {
						// Check if source already exists
						exists := false
						for _, existingSource := range doc.SourceFiles {
							if existingSource == source {
								exists = true
								break
							}
						}
						
						if !exists {
							doc.SourceFiles = append(doc.SourceFiles, source)
							modified = true
						}
					}
				}
				logger.Infof("Added source files: %s", addSourceFiles)
			}

			// Calculate word count
			if calculateWordCount {
				oldCount := doc.WordCount
				doc.UpdateWordCount()
				logger.Infof("Updated word count: %d (was %d)", doc.WordCount, oldCount)
				modified = true
			}

			// Generate ID if missing
			if generateID && doc.ID == "" {
				doc.GenerateID()
				logger.Infof("Generated ID: %s", doc.ID)
				modified = true
			}

			// Write changes if modified
			if modified {
				if err := parser.WriteTTMP(doc, filePath); err != nil {
					logger.Fatalf("Error writing file: %v", err)
				}
				logger.Infof("Updated metadata in %s", filePath)
			} else {
				logger.Info("No changes made")
			}
		},
	}

	cmd.Flags().BoolVar(&updateTimestamp, "update-timestamp", false, "Update the 'updated' timestamp to current time")
	cmd.Flags().StringVar(&addTags, "add-tags", "", "Add comma-separated tags to the document")
	cmd.Flags().StringVar(&addSourceFiles, "add-sources", "", "Add comma-separated source files to the document")
	cmd.Flags().BoolVar(&calculateWordCount, "calc-words", false, "Calculate and update the word count")
	cmd.Flags().BoolVar(&generateID, "generate-id", false, "Generate an ID if missing")

	return cmd
}

// newTemplateCommand creates a command to generate a document from a template
func newTemplateCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		templateType string
		id string
		title string
		tags string
		docType string
		sourceFiles string
	)

	cmd := &cobra.Command{
		Use:   "template [output_file]",
		Short: "Generate a document from a template",
		Long:  `Generate a new TTMP document from a predefined template.`,
		Args:  cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			outputPath := args[0]

			// Check if file already exists
			if _, err := os.Stat(outputPath); err == nil {
				logger.Fatalf("File already exists: %s", outputPath)
			}

			// Create document from template
			doc := createDocumentFromTemplate(templateType, id, title, tags, docType, sourceFiles)

			// Write document to file
			if err := parser.WriteTTMP(doc, outputPath); err != nil {
				logger.Fatalf("Error writing file: %v", err)
			}

			logger.Infof("Created document from template: %s", outputPath)
		},
	}

	cmd.Flags().StringVar(&templateType, "type", "basic", "Template type (basic, guide, spec, tutorial)")
	cmd.Flags().StringVar(&id, "id", "", "Document ID")
	cmd.Flags().StringVar(&title, "title", "", "Document title")
	cmd.Flags().StringVar(&tags, "tags", "", "Comma-separated tags")
	cmd.Flags().StringVar(&docType, "doc-type", "", "Document type")
	cmd.Flags().StringVar(&sourceFiles, "sources", "", "Comma-separated source files")

	return cmd
}

// createDocumentFromTemplate creates a document from a template
func createDocumentFromTemplate(templateType, id, title, tags, docType, sourceFiles string) *model.TTMPDocument {
	doc := model.NewTTMPDocument()
	
	// Set common fields
	doc.ID = id
	doc.Title = title
	
	// Set tags if provided
	if tags != "" {
		doc.Tags = strings.Split(tags, ",")
		for i, tag := range doc.Tags {
			doc.Tags[i] = strings.TrimSpace(tag)
		}
	}
	
	// Set document type
	if docType != "" {
		doc.DocumentType = docType
	}
	
	// Set source files if provided
	if sourceFiles != "" {
		doc.SourceFiles = strings.Split(sourceFiles, ",")
		for i, source := range doc.SourceFiles {
			doc.SourceFiles[i] = strings.TrimSpace(source)
		}
	}
	
	// Set timestamps
	now := time.Now()
	doc.Created = &now
	doc.Updated = &now
	
	// Set content based on template type
	switch templateType {
	case "guide":
		doc.DocumentType = "guide"
		doc.Content = getGuideTemplate(title)
	case "spec":
		doc.DocumentType = "spec"
		doc.Content = getSpecTemplate(title)
	case "tutorial":
		doc.DocumentType = "tutorial"
		doc.Content = getTutorialTemplate(title)
	default:
		doc.Content = getBasicTemplate(title)
	}
	
	// Calculate word count
	doc.UpdateWordCount()
	
	return doc
}

// getBasicTemplate returns a basic document template
func getBasicTemplate(title string) string {
	if title == "" {
		title = "Untitled Document"
	}
	
	return fmt.Sprintf(`# %s

## Introduction

Write your introduction here.

## Content

Main content goes here.

## Conclusion

Summarize your document here.
`, title)
}

// getGuideTemplate returns a guide document template
func getGuideTemplate(title string) string {
	if title == "" {
		title = "Guide"
	}
	
	return fmt.Sprintf(`# %s

## Introduction

This guide provides instructions for...

## Prerequisites

Before starting, ensure you have:

- Prerequisite 1
- Prerequisite 2
- Prerequisite 3

## Step 1: First Step

Instructions for the first step...

## Step 2: Second Step

Instructions for the second step...

## Step 3: Third Step

Instructions for the third step...

## Troubleshooting

Common issues and their solutions:

### Issue 1

Solution...

### Issue 2

Solution...

## Conclusion

In this guide, we covered...
`, title)
}

// getSpecTemplate returns a specification document template
func getSpecTemplate(title string) string {
	if title == "" {
		title = "Specification"
	}
	
	return fmt.Sprintf(`# %s

## 1. Overview

This specification defines...

## 2. Goals

- Goal 1
- Goal 2
- Goal 3

## 3. Non-Goals

- Non-goal 1
- Non-goal 2

## 4. Design

### 4.1 Architecture

Describe the architecture...

### 4.2 Components

Describe the components...

### 4.3 Interfaces

Describe the interfaces...

## 5. Implementation

Describe the implementation details...

## 6. Testing

Describe the testing approach...

## 7. Deployment

Describe the deployment process...

## 8. Open Questions

- Question 1
- Question 2
`, title)
}

// getTutorialTemplate returns a tutorial document template
func getTutorialTemplate(title string) string {
	if title == "" {
		title = "Tutorial"
	}
	
	return fmt.Sprintf(`# %s

## Introduction

In this tutorial, you will learn how to...

## Prerequisites

Before starting, ensure you have:

- Prerequisite 1
- Prerequisite 2
- Prerequisite 3

## Step 1: Setup

Instructions for setting up...

## Step 2: Implementation

Instructions for implementation...

## Step 3: Testing

Instructions for testing...

## Step 4: Deployment

Instructions for deployment...

## Conclusion

In this tutorial, you learned...

## Next Steps

Here are some suggestions for what to learn next:

- Suggestion 1
- Suggestion 2
- Suggestion 3
`, title)
}