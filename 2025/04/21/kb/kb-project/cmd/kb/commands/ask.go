// cmd/kb/commands/ask.go
package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/rag"
	"github.com/kb-project/pkg/search"
	"github.com/kb-project/pkg/store"
)

// AskSettings contains the settings for the ask command
type AskSettings struct {
	Question  string `glazed.parameter:"question"`
	Model     string `glazed.parameter:"model"`
	MaxTokens int    `glazed.parameter:"max-tokens"`
}

// AskCmd represents the ask command
type AskCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewAskCmd creates a new ask command
func NewAskCmd(cfg *config.Config) *AskCmd {
	return &AskCmd{
		CommandDescription: cmds.NewCommandDescription(
			"ask",
			cmds.WithShort("Ask questions about your code"),
			cmds.WithLong("Use RAG (Retrieval Augmented Generation) to answer questions about your codebase"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"question",
					parameters.ParameterTypeString,
					parameters.WithHelp("Question to ask"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"model",
					parameters.ParameterTypeString,
					parameters.WithHelp("LLM model to use"),
					parameters.WithDefault(cfg.LLM.Model),
				),
				parameters.NewParameterDefinition(
					"max-tokens",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Maximum number of tokens in the response"),
					parameters.WithDefault(1024),
				),
			),
		),
		cfg: cfg,
	}
}

// Run executes the ask command
func (c *AskCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &AskSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return err
	}

	// Open the store
	store, err := store.Open(c.cfg.IndexPath, c.cfg.Dimension)
	if err != nil {
		return fmt.Errorf("failed to open store: %w", err)
	}
	defer store.Close()

	// Create embedder
	embedder := embed.NewMockEmbedder(c.cfg.Dimension)

	// Create searcher
	searcher := search.NewSearcher(store, embedder)

	// Create RAG client
	ragClient := rag.NewRAGClient(searcher, embedder)

	// Ask the question
	answer, err := ragClient.Ask(ctx, s.Question)
	if err != nil {
		return fmt.Errorf("failed to ask question: %w", err)
	}

	// Print the answer
	fmt.Printf("Using model: %s (max tokens: %d)\n\n", s.Model, s.MaxTokens)
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
}
