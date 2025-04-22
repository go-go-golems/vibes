// cmd/kb/commands/sim.go
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

// SimSettings contains the settings for the sim command
type SimSettings struct {
	Query string `glazed.parameter:"query"`
	K     int    `glazed.parameter:"k"`
}

// SimCmd represents the sim command
type SimCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewSimCmd creates a new sim command
func NewSimCmd(cfg *config.Config) *SimCmd {
	return &SimCmd{
		CommandDescription: cmds.NewCommandDescription(
			"sim",
			cmds.WithShort("Search for code chunks by vector similarity"),
			cmds.WithLong("Perform nearest-neighbor vector search on indexed code chunks"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"query",
					parameters.ParameterTypeString,
					parameters.WithHelp("Search query"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"k",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Number of nearest neighbors to return"),
					parameters.WithDefault(10),
				),
			),
		),
		cfg: cfg,
	}
}

// RunIntoGlazeProcessor executes the sim command and outputs results to the processor
func (c *SimCmd) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &SimSettings{}
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
	chunks, err := searcher.SearchVector(ctx, s.Query, s.K)
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
