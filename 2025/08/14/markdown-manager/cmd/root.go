package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "mdm",
	Short: "Markdown Document Manager",
	Long: `A CLI tool for managing markdown files with YAML metadata frontmatter.
	
Provides commands to list, search, edit, query, and update markdown documents
with rich metadata support for project management and documentation workflows.`,
}

func Execute() error {
	return rootCmd.Execute()
}

func init() {
	rootCmd.AddCommand(listCmd)
	rootCmd.AddCommand(searchCmd)
	rootCmd.AddCommand(infoCmd)
	rootCmd.AddCommand(updateCmd)
	rootCmd.AddCommand(queryCmd)
}

var listCmd = &cobra.Command{
	Use:   "list [directory]",
	Short: "List markdown files with metadata",
	Long:  "List all markdown files in a directory with their metadata information",
	Args:  cobra.MaximumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		directory := "."
		if len(args) > 0 {
			directory = args[0]
		}
		
		recursive, _ := cmd.Flags().GetBool("recursive")
		showPath, _ := cmd.Flags().GetBool("show-path")
		format, _ := cmd.Flags().GetString("format")
		
		if err := listMarkdownFiles(directory, recursive, showPath, format); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	},
}

var searchCmd = &cobra.Command{
	Use:   "search",
	Short: "Search markdown files by metadata and content",
	Long:  "Search for markdown files based on metadata fields and content text",
	Run: func(cmd *cobra.Command, args []string) {
		directory, _ := cmd.Flags().GetString("directory")
		title, _ := cmd.Flags().GetString("title")
		tags, _ := cmd.Flags().GetStringSlice("tags")
		category, _ := cmd.Flags().GetString("category")
		project, _ := cmd.Flags().GetString("project")
		status, _ := cmd.Flags().GetString("status")
		priority, _ := cmd.Flags().GetString("priority")
		author, _ := cmd.Flags().GetString("author")
		content, _ := cmd.Flags().GetString("content")
		showContent, _ := cmd.Flags().GetBool("show-content")
		
		if err := searchMarkdownFiles(directory, title, tags, category, project, status, priority, author, content, showContent); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	},
}

var infoCmd = &cobra.Command{
	Use:   "info <file>",
	Short: "Show detailed information about a markdown file",
	Long:  "Display comprehensive metadata and content information for a specific markdown file",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		filePath := args[0]
		showContent, _ := cmd.Flags().GetBool("show-content")
		touch, _ := cmd.Flags().GetBool("touch")
		
		if err := showFileInfo(filePath, showContent, touch); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	},
}

var updateCmd = &cobra.Command{
	Use:   "update <file>",
	Short: "Update metadata of a markdown file",
	Long:  "Update the YAML frontmatter metadata of a markdown file",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		filePath := args[0]
		
		// Get all the update flags
		title, _ := cmd.Flags().GetString("title")
		description, _ := cmd.Flags().GetString("description")
		tags, _ := cmd.Flags().GetStringSlice("tags")
		addTags, _ := cmd.Flags().GetStringSlice("add-tags")
		removeTags, _ := cmd.Flags().GetStringSlice("remove-tags")
		category, _ := cmd.Flags().GetString("category")
		project, _ := cmd.Flags().GetString("project")
		status, _ := cmd.Flags().GetString("status")
		priority, _ := cmd.Flags().GetString("priority")
		author, _ := cmd.Flags().GetString("author")
		touch, _ := cmd.Flags().GetBool("touch")
		
		if err := updateFileMetadata(filePath, title, description, tags, addTags, removeTags, category, project, status, priority, author, touch); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	},
}

var queryCmd = &cobra.Command{
	Use:   "query",
	Short: "Query and analyze markdown file metadata",
	Long:  "Perform analytical queries on markdown file metadata with aggregation and grouping",
	Run: func(cmd *cobra.Command, args []string) {
		directory, _ := cmd.Flags().GetString("directory")
		queryType, _ := cmd.Flags().GetString("query")
		
		if err := queryMarkdownFiles(directory, queryType); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	},
}

func init() {
	// List command flags
	listCmd.Flags().BoolP("recursive", "r", true, "Recursively scan subdirectories")
	listCmd.Flags().Bool("show-path", false, "Show full file paths")
	listCmd.Flags().StringP("format", "f", "table", "Output format (table, json, yaml, csv)")
	
	// Search command flags
	searchCmd.Flags().StringP("directory", "d", ".", "Directory to search in")
	searchCmd.Flags().String("title", "", "Search by title (partial match)")
	searchCmd.Flags().StringSlice("tags", []string{}, "Search by tags (comma-separated)")
	searchCmd.Flags().String("category", "", "Search by category")
	searchCmd.Flags().String("project", "", "Search by project")
	searchCmd.Flags().String("status", "", "Search by status")
	searchCmd.Flags().String("priority", "", "Search by priority")
	searchCmd.Flags().String("author", "", "Search by author")
	searchCmd.Flags().String("content", "", "Search in content text")
	searchCmd.Flags().Bool("show-content", false, "Include content preview in results")
	
	// Info command flags
	infoCmd.Flags().Bool("show-content", false, "Include full content in output")
	infoCmd.Flags().Bool("touch", false, "Update last_used timestamp")
	
	// Update command flags
	updateCmd.Flags().String("title", "", "Update title")
	updateCmd.Flags().String("description", "", "Update description")
	updateCmd.Flags().StringSlice("tags", []string{}, "Replace all tags (comma-separated)")
	updateCmd.Flags().StringSlice("add-tags", []string{}, "Add tags (comma-separated)")
	updateCmd.Flags().StringSlice("remove-tags", []string{}, "Remove tags (comma-separated)")
	updateCmd.Flags().String("category", "", "Update category")
	updateCmd.Flags().String("project", "", "Update project")
	updateCmd.Flags().String("status", "", "Update status")
	updateCmd.Flags().String("priority", "", "Update priority")
	updateCmd.Flags().String("author", "", "Update author")
	updateCmd.Flags().Bool("touch", false, "Update last_used timestamp")
	
	// Query command flags
	queryCmd.Flags().StringP("directory", "d", ".", "Directory to query")
	queryCmd.Flags().StringP("query", "q", "stats", "Type of query (stats, tags, projects, authors, status, priority, recent, stale)")
}

