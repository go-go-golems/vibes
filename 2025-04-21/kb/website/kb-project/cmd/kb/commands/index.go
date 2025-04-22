// cmd/kb/commands/index.go
package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/kb-project/pkg/config"
	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/index"
	"github.com/kb-project/pkg/store"
)

// IndexSettings contains the settings for the index command
type IndexSettings struct {
	ChangedOnly bool     `glazed.parameter:"changed-only"`
	Langs       []string `glazed.parameter:"langs"`
}

// IndexCmd represents the index command
type IndexCmd struct {
	*cmds.CommandDescription
	cfg *config.Config
}

// NewIndexCmd creates a new index command
func NewIndexCmd(cfg *config.Config) *IndexCmd {
	return &IndexCmd{
		CommandDescription: cmds.NewCommandDescription(
			"index",
			cmds.WithShort("Index code repositories"),
			cmds.WithLong("Parse and index code files from tracked repositories"),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"changed-only",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Only index changed files"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"langs",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("Languages to index (comma-separated)"),
				),
			),
		),
		cfg: cfg,
	}
}

// Run executes the index command
func (c *IndexCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	s := &IndexSettings{}
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

	// Create indexer
	indexer := index.NewIndexer(store, embedder)

	// Index repositories
	opts := index.Options{
		ChangedOnly: s.ChangedOnly,
		Langs:       s.Langs,
	}

	if len(c.cfg.Repositories) == 0 {
		return fmt.Errorf("no repositories to index")
	}

	if err := indexer.IndexPaths(ctx, c.cfg.Repositories, opts); err != nil {
		return fmt.Errorf("failed to index repositories: %w", err)
	}

	// Print summary
	count, err := store.Count()
	if err != nil {
		return fmt.Errorf("failed to get document count: %w", err)
	}

	fmt.Printf("Indexed %d repositories\n", len(c.cfg.Repositories))
	fmt.Printf("Total chunks: %d\n", count)

	return nil
}
