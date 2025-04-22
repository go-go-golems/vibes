// cmd/kb/commands/search.go
package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/search"
	"github.com/kb-project/pkg/store"
)

// SearchSettings contains the settings for the search command
type SearchSettings struct {
	Query string `glazed.parameter:"query"`
	Top   int    `glazed.parameter:"top"`
}

// SearchCmd represents the search command
type SearchCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewSearchCmd creates a new search command
func NewSearchCmd(cfg *config.Config) *SearchCmd {
	return &SearchCmd{
		CommandDescription: cmds.NewCommandDescription(
			"search",
			cmds.WithShort("Search for code chunks by text"),
			cmds.WithLong("Perform full-text search on indexed code chunks"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"query",
					parameters.ParameterTypeString,
					parameters.WithHelp("Search query"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"top",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Number of results to return"),
					parameters.WithDefault(10),
				),
			),
		),
		cfg: cfg,
	}
}

// RunIntoGlazeProcessor executes the search command and outputs results to the processor
func (c *SearchCmd) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &SearchSettings{}
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

	// Search for chunks
	chunks, err := searcher.SearchText(ctx, s.Query, s.Top)
	if err != nil {
		return fmt.Errorf("failed to search for chunks: %w", err)
	}

	// Output results
	for _, chunk := range chunks {
		row := types.NewRow(
			types.MRP("id", chunk.ID),
			types.MRP("file_path", chunk.FilePath),
			types.MRP("language", chunk.Language),
			types.MRP("symbol_type", chunk.SymbolType),
			types.MRP("symbol_name", chunk.SymbolName),
			types.MRP("start_line", chunk.StartLine),
			types.MRP("end_line", chunk.EndLine),
			types.MRP("tokens", chunk.Tokens),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add row: %w", err)
		}
	}

	return nil
}
