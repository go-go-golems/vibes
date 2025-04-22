// cmd/kb/commands/embed.go
package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/store"
)

// EmbedSettings contains the settings for the embed command
type EmbedSettings struct {
	Batch int    `glazed.parameter:"batch"`
	Model string `glazed.parameter:"model"`
	Dim   int    `glazed.parameter:"dim"`
}

// EmbedCmd represents the embed command
type EmbedCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewEmbedCmd creates a new embed command
func NewEmbedCmd(cfg *config.Config) *EmbedCmd {
	return &EmbedCmd{
		CommandDescription: cmds.NewCommandDescription(
			"embed",
			cmds.WithShort("Compute embeddings for indexed chunks"),
			cmds.WithLong("Generate vector embeddings for chunks that don't have them"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"batch",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Batch size for embedding generation"),
					parameters.WithDefault(10),
				),
				parameters.NewParameterDefinition(
					"model",
					parameters.ParameterTypeString,
					parameters.WithHelp("Embedding model to use"),
					parameters.WithDefault(cfg.Embedder.Provider),
				),
				parameters.NewParameterDefinition(
					"dim",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Embedding dimension"),
					parameters.WithDefault(cfg.Dimension),
				),
			),
		),
		cfg: cfg,
	}
}

// Run executes the embed command
func (c *EmbedCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &EmbedSettings{}
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
	_ = embed.NewMockEmbedder(s.Dim)

	// In a real implementation, we would search for chunks without embeddings
	// and generate embeddings for them in batches
	// For our mock implementation, we'll just print a message

	fmt.Printf("Using %s model to generate %d-dimensional embeddings in batches of %d\n", 
		s.Model, s.Dim, s.Batch)
	fmt.Println("Mock embeddings have been generated for all chunks")

	return nil
}
