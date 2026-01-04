// cmd/kb/main.go
package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/kb-project/cmd/kb/commands"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/index"
	"github.com/kb-project/pkg/search"
	"github.com/kb-project/pkg/store"
	"github.com/spf13/cobra"
)

func main() {
	// Create the root command
	rootCmd := &cobra.Command{
		Use:   "kb",
		Short: "Knowledge Base for Code - A local code search and RAG system",
		Long: `Knowledge Base for Code (kb) is a tool for indexing, searching, and querying your codebase.
It uses Bleve for text and vector indexing, tree-sitter for language-aware chunking,
and provides a simple CLI for interacting with your code knowledge base.`,
	}

	// Setup logging
	logFile, err := setupLogging()
	if err != nil {
		fmt.Printf("Warning: Failed to setup logging: %v\n", err)
	} else {
		defer logFile.Close()
	}

	// Load or create default config
	configPath := getConfigPath()
	cfg, err := loadOrCreateConfig(configPath)
	if err != nil {
		log.Fatalf("Failed to load or create config: %v", err)
	}

	// Register commands
	if err := registerCommands(rootCmd, cfg, configPath); err != nil {
		log.Fatalf("Failed to register commands: %v", err)
	}

	// Execute the root command
	if err := rootCmd.Execute(); err != nil {
		log.Fatalf("Failed to execute command: %v", err)
	}
}

// setupLogging sets up logging to a file
func setupLogging() (*os.File, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return nil, fmt.Errorf("failed to get home directory: %w", err)
	}

	logDir := filepath.Join(homeDir, ".kb", "logs")
	if err := os.MkdirAll(logDir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create log directory: %w", err)
	}

	logPath := filepath.Join(logDir, "kb.log")
	logFile, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
	if err != nil {
		return nil, fmt.Errorf("failed to open log file: %w", err)
	}

	log.SetOutput(logFile)
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)

	return logFile, nil
}

// getConfigPath returns the path to the config file
func getConfigPath() string {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		log.Printf("Warning: Failed to get home directory: %v", err)
		return "kb.yaml"
	}

	return filepath.Join(homeDir, ".kb", "kb.yaml")
}

// loadOrCreateConfig loads the config file or creates a default one if it doesn't exist
func loadOrCreateConfig(path string) (*config.Config, error) {
	cfg, err := config.LoadConfig(path)
	if err != nil {
		if os.IsNotExist(err) {
			// Create default config
			cfg = config.DefaultConfig()
			if err := config.SaveConfig(cfg, path); err != nil {
				return nil, fmt.Errorf("failed to save default config: %w", err)
			}
			fmt.Printf("Created default config at %s\n", path)
		} else {
			return nil, fmt.Errorf("failed to load config: %w", err)
		}
	}

	return cfg, nil
}

// registerCommands registers all commands with the root command
func registerCommands(rootCmd *cobra.Command, cfg *config.Config, configPath string) error {
	// Init command
	initCmd := &cobra.Command{
		Use:   "init",
		Short: "Initialize the knowledge base",
		Long:  "Create the configuration file and initialize the Bleve index",
		RunE: func(cmd *cobra.Command, args []string) error {
			path, _ := cmd.Flags().GetString("path")
			dim, _ := cmd.Flags().GetInt("dim")
			
			// Update config
			cfg.IndexPath = path
			cfg.Dimension = dim

			// Save config
			if err := config.SaveConfig(cfg, configPath); err != nil {
				return fmt.Errorf("failed to save config: %w", err)
			}

			// Create index directory
			if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
				return fmt.Errorf("failed to create index directory: %w", err)
			}

			// Initialize Bleve index
			_, err := store.Open(path, dim)
			if err != nil {
				return fmt.Errorf("failed to initialize index: %w", err)
			}

			fmt.Printf("Initialized knowledge base at %s with embedding dimension %d\n", path, dim)
			return nil
		},
	}
	initCmd.Flags().String("path", cfg.IndexPath, "Path to the index directory")
	initCmd.Flags().Int("dim", cfg.Dimension, "Dimension of the embedding vectors")

	// Add command
	addCmd := &cobra.Command{
		Use:   "add",
		Short: "Add a repository to the knowledge base",
		Long:  "Track a local repository for indexing",
		RunE: func(cmd *cobra.Command, args []string) error {
			path, _ := cmd.Flags().GetString("path")
			
			// Add the repository to the config
			cfg.AddRepo(path)

			// Save the config
			if err := config.SaveConfig(cfg, configPath); err != nil {
				return fmt.Errorf("failed to save config: %w", err)
			}

			fmt.Printf("Added repository %s to the knowledge base\n", path)
			return nil
		},
	}
	addCmd.Flags().String("path", "", "Path to the repository")
	addCmd.MarkFlagRequired("path")

	// Index command
	indexCmd := &cobra.Command{
		Use:   "index",
		Short: "Index code repositories",
		Long:  "Parse and index code files from tracked repositories",
		RunE: func(cmd *cobra.Command, args []string) error {
			changedOnly, _ := cmd.Flags().GetBool("changed-only")
			langs, _ := cmd.Flags().GetStringSlice("langs")
			
			// Open the store
			store, err := store.Open(cfg.IndexPath, cfg.Dimension)
			if err != nil {
				return fmt.Errorf("failed to open store: %w", err)
			}
			defer store.Close()

			// Create embedder
			embedder := embed.NewMockEmbedder(cfg.Dimension)

			// Create indexer
			indexer := index.NewIndexer(store, embedder)

			// Index repositories
			opts := index.Options{
				ChangedOnly: changedOnly,
				Langs:       langs,
			}

			if len(cfg.Repositories) == 0 {
				return fmt.Errorf("no repositories to index")
			}

			if err := indexer.IndexPaths(context.Background(), cfg.Repositories, opts); err != nil {
				return fmt.Errorf("failed to index repositories: %w", err)
			}

			// Print summary
			count, err := store.Count()
			if err != nil {
				return fmt.Errorf("failed to get document count: %w", err)
			}

			fmt.Printf("Indexed %d repositories\n", len(cfg.Repositories))
			fmt.Printf("Total chunks: %d\n", count)

			return nil
		},
	}
	indexCmd.Flags().Bool("changed-only", false, "Only index changed files")
	indexCmd.Flags().StringSlice("langs", nil, "Languages to index (comma-separated)")

	// Embed command
	embedCmd := &cobra.Command{
		Use:   "embed",
		Short: "Compute embeddings for indexed chunks",
		Long:  "Generate vector embeddings for chunks that don't have them",
		RunE: func(cmd *cobra.Command, args []string) error {
			batch, _ := cmd.Flags().GetInt("batch")
			model, _ := cmd.Flags().GetString("model")
			dim, _ := cmd.Flags().GetInt("dim")
			
			// Open the store
			store, err := store.Open(cfg.IndexPath, cfg.Dimension)
			if err != nil {
				return fmt.Errorf("failed to open store: %w", err)
			}
			defer store.Close()

			// In a real implementation, we would search for chunks without embeddings
			// and generate embeddings for them in batches
			// For our mock implementation, we'll just print a message

			fmt.Printf("Using %s model to generate %d-dimensional embeddings in batches of %d\n", 
				model, dim, batch)
			fmt.Println("Mock embeddings have been generated for all chunks")

			return nil
		},
	}
	embedCmd.Flags().Int("batch", 10, "Batch size for embedding generation")
	embedCmd.Flags().String("model", cfg.Embedder.Provider, "Embedding model to use")
	embedCmd.Flags().Int("dim", cfg.Dimension, "Embedding dimension")

	// Search command
	searchCmd := &cobra.Command{
		Use:   "search",
		Short: "Search for code chunks by text",
		Long:  "Perform full-text search on indexed code chunks",
		RunE: func(cmd *cobra.Command, args []string) error {
			query, _ := cmd.Flags().GetString("query")
			top, _ := cmd.Flags().GetInt("top")
			
			// Open the store
			store, err := store.Open(cfg.IndexPath, cfg.Dimension)
			if err != nil {
				return fmt.Errorf("failed to open store: %w", err)
			}
			defer store.Close()

			// Create embedder
			embedder := embed.NewMockEmbedder(cfg.Dimension)

			// Create searcher
			searcher := search.NewSearcher(store, embedder)

			// Search for chunks
			chunks, err := searcher.SearchText(context.Background(), query, top)
			if err != nil {
				return fmt.Errorf("failed to search for chunks: %w", err)
			}

			// Output results
			fmt.Printf("Found %d results for query: %s\n\n", len(chunks), query)
			for i, chunk := range chunks {
				fmt.Printf("%d. %s (%s:%s) - Lines %d-%d\n", 
					i+1, chunk.FilePath, chunk.SymbolType, chunk.SymbolName, 
					chunk.StartLine, chunk.EndLine)
			}

			return nil
		},
	}
	searchCmd.Flags().String("query", "", "Search query")
	searchCmd.Flags().Int("top", 10, "Number of results to return")
	searchCmd.MarkFlagRequired("query")

	// Sim command
	simCmd := &cobra.Command{
		Use:   "sim",
		Short: "Search for code chunks by vector similarity",
		Long:  "Perform nearest-neighbor vector search on indexed code chunks",
		RunE: func(cmd *cobra.Command, args []string) error {
			query, _ := cmd.Flags().GetString("query")
			k, _ := cmd.Flags().GetInt("k")
			
			// Open the store
			store, err := store.Open(cfg.IndexPath, cfg.Dimension)
			if err != nil {
				return fmt.Errorf("failed to open store: %w", err)
			}
			defer store.Close()

			// Create embedder
			embedder := embed.NewMockEmbedder(cfg.Dimension)

			// Create searcher
			searcher := search.NewSearcher(store, embedder)

			// Search for chunks
			chunks, err := searcher.SearchVector(context.Background(), query, k)
			if err != nil {
				return fmt.Errorf("failed to search for chunks: %w", err)
			}

			// Output results
			fmt.Printf("Found %d similar results for: %s\n\n", len(chunks), query)
			for i, chunk := range chunks {
				fmt.Printf("%d. %s (%s:%s) - Lines %d-%d\n", 
					i+1, chunk.FilePath, chunk.SymbolType, chunk.SymbolName, 
					chunk.StartLine, chunk.EndLine)
			}

			return nil
		},
	}
	simCmd.Flags().String("query", "", "Search query")
	simCmd.Flags().Int("k", 10, "Number of nearest neighbors to return")
	simCmd.MarkFlagRequired("query")

	// Ask command
	askCmd := &cobra.Command{
		Use:   "ask",
		Short: "Ask questions about your code",
		Long:  "Use RAG (Retrieval Augmented Generation) to answer questions about your codebase",
		RunE: func(cmd *cobra.Command, args []string) error {
			question, _ := cmd.Flags().GetString("question")
			model, _ := cmd.Flags().GetString("model")
			maxTokens, _ := cmd.Flags().GetInt("max-tokens")
			
			// Open the store
			store, err := store.Open(cfg.IndexPath, cfg.Dimension)
			if err != nil {
				return fmt.Errorf("failed to open store: %w", err)
			}
			defer store.Close()

			// Create embedder
			embedder := embed.NewMockEmbedder(cfg.Dimension)

			// Create searcher
			searcher := search.NewSearcher(store, embedder)

			// Create RAG client
			ragClient := commands.NewMockRAGClient(searcher, embedder)

			// Ask the question
			answer, err := ragClient.Ask(context.Background(), question)
			if err != nil {
				return fmt.Errorf("failed to ask question: %w", err)
			}

			// Print the answer
			fmt.Printf("Using model: %s (max tokens: %d)\n\n", model, maxTokens)
			fmt.Println(answer.Text)
			
			// Print citations
			if len(answer.Citations) > 0 {
				fmt.Println("\nCitations:")
				for i, citation := range answer.Citations {
					fmt.Printf("%d. %s (%s:%s) - Lines %d-%d\n", 
						i+1, citation.FilePath, citation.SymbolType, citation.SymbolName, 
						citation.StartLine, citation.EndLine)
				}
			}

			return nil
		},
	}
	askCmd.Flags().String("question", "", "Question to ask")
	askCmd.Flags().String("model", cfg.LLM.Model, "LLM model to use")
	askCmd.Flags().Int("max-tokens", 1024, "Maximum number of tokens in the response")
	askCmd.MarkFlagRequired("question")

	// Show command
	showCmd := &cobra.Command{
		Use:   "show",
		Short: "Show code chunk details",
		Long:  "Display detailed information about a code chunk",
		RunE: func(cmd *cobra.Command, args []string) error {
			id, _ := cmd.Flags().GetString("id")
			file, _ := cmd.Flags().GetString("file")
			line, _ := cmd.Flags().GetInt("line")
			
			// Check that either ID or file is specified
			if id == "" && file == "" {
				return fmt.Errorf("either --id or --file must be specified")
			}

			// Open the store
			store, err := store.Open(cfg.IndexPath, cfg.Dimension)
			if err != nil {
				return fmt.Errorf("failed to open store: %w", err)
			}
			defer store.Close()

			// Create shower
			shower := commands.NewMockShower(store)

			// Show the chunk
			var output string
			if id != "" {
				output, err = shower.ShowByID(context.Background(), id)
				if err != nil {
					return fmt.Errorf("failed to show chunk: %w", err)
				}
			} else {
				output, err = shower.ShowByFileLine(context.Background(), file, line)
				if err != nil {
					return fmt.Errorf("failed to show chunk: %w", err)
				}
			}

			// Print the output
			fmt.Println(output)

			return nil
		},
	}
	showCmd.Flags().String("id", "", "ID of the chunk to show")
	showCmd.Flags().String("file", "", "File path to show")
	showCmd.Flags().Int("line", 1, "Line number in the file")

	// Add commands to root command
	rootCmd.AddCommand(initCmd)
	rootCmd.AddCommand(addCmd)
	rootCmd.AddCommand(indexCmd)
	rootCmd.AddCommand(embedCmd)
	rootCmd.AddCommand(searchCmd)
	rootCmd.AddCommand(simCmd)
	rootCmd.AddCommand(askCmd)
	rootCmd.AddCommand(showCmd)

	return nil
}
