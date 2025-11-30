package main

import (
	"context"
	"fmt"
	"os"

	"github.com/fact-extraction/go-extractor/pkg/extractor"
	"github.com/fact-extraction/go-extractor/pkg/storage"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var (
	inputDir  string
	outputDB  string
	model     string
	limit     int
	verbose   bool
)

var rootCmd = &cobra.Command{
	Use:   "go-extractor",
	Short: "Extract facts from documents using LLMs and geppetto framework",
	Long: `go-extractor is a high-performance fact extraction tool that uses
the geppetto framework to extract structured RDF triples from text documents.`,
}

var extractCmd = &cobra.Command{
	Use:   "extract",
	Short: "Extract facts from documents",
	RunE:  runExtract,
}

var statsCmd = &cobra.Command{
	Use:   "stats",
	Short: "Show extraction statistics",
	RunE:  runStats,
}

func init() {
	// Extract command flags
	extractCmd.Flags().StringVarP(&inputDir, "input", "i", "", "Input directory with documents (required)")
	extractCmd.Flags().StringVarP(&outputDB, "output", "o", "fact_extraction.db", "Output SQLite database")
	extractCmd.Flags().StringVarP(&model, "model", "m", "gpt-4.1-mini", "LLM model to use")
	extractCmd.Flags().IntVarP(&limit, "limit", "l", 30, "Maximum number of documents to process")
	extractCmd.Flags().BoolVarP(&verbose, "verbose", "v", false, "Verbose logging")
	extractCmd.MarkFlagRequired("input")

	// Stats command flags
	statsCmd.Flags().StringVarP(&outputDB, "db", "d", "fact_extraction.db", "SQLite database path")

	rootCmd.AddCommand(extractCmd)
	rootCmd.AddCommand(statsCmd)
}

func runExtract(cmd *cobra.Command, args []string) error {
	// Setup logging
	setupLogging()

	log.Info().
		Str("input", inputDir).
		Str("output", outputDB).
		Str("model", model).
		Int("limit", limit).
		Msg("Starting fact extraction")

	// Create components
	loader := extractor.NewDocumentLoader(inputDir)
	openaiExtractor, err := extractor.NewOpenAIExtractor(model)
	if err != nil {
		return fmt.Errorf("failed to create extractor: %w", err)
	}

	writer, err := storage.NewSQLiteWriter(outputDB)
	if err != nil {
		return fmt.Errorf("failed to create database writer: %w", err)
	}
	defer writer.Close()

	// Load documents
	log.Info().Msg("Loading documents...")
	documents, err := loader.LoadDocuments(limit)
	if err != nil {
		return fmt.Errorf("failed to load documents: %w", err)
	}

	log.Info().Int("count", len(documents)).Msg("Documents loaded")

	// Process documents
	ctx := context.Background()
	totalCost := 0.0
	totalTriples := 0

	for i, doc := range documents {
		log.Info().
			Int("progress", i+1).
			Int("total", len(documents)).
			Str("doc_id", doc.ID).
			Msg("Processing document")

		result, err := openaiExtractor.Extract(ctx, doc)
		if err != nil {
			log.Error().
				Err(err).
				Str("doc_id", doc.ID).
				Msg("Extraction failed")
			continue
		}

		if err := writer.SaveResult(result); err != nil {
			log.Error().
				Err(err).
				Str("doc_id", doc.ID).
				Msg("Failed to save result")
			continue
		}

		totalCost += result.CostUSD
		totalTriples += len(result.Triples)

		log.Info().
			Str("doc_id", doc.ID).
			Int("triples", len(result.Triples)).
			Float64("cost", result.CostUSD).
			Float64("total_cost", totalCost).
			Msg("Document processed")
	}

	log.Info().
		Int("documents", len(documents)).
		Int("triples", totalTriples).
		Float64("total_cost", totalCost).
		Msg("Extraction completed")

	return nil
}

func runStats(cmd *cobra.Command, args []string) error {
	setupLogging()

	writer, err := storage.NewSQLiteWriter(outputDB)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer writer.Close()

	stats, err := writer.GetStats()
	if err != nil {
		return fmt.Errorf("failed to get stats: %w", err)
	}

	fmt.Println("📊 Extraction Statistics")
	fmt.Println("========================")
	fmt.Printf("Documents:              %d\n", stats["documents"])
	fmt.Printf("Triples:                %d\n", stats["triples"])
	fmt.Printf("Total Cost:             $%.4f\n", stats["total_cost"])
	if avgTriples, ok := stats["avg_triples_per_doc"].(float64); ok {
		fmt.Printf("Avg Triples/Document:   %.1f\n", avgTriples)
	}

	return nil
}

func setupLogging() {
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnix
	if verbose {
		zerolog.SetGlobalLevel(zerolog.DebugLevel)
	} else {
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
	}
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}
