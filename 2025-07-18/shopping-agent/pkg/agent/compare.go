package agent

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"shopping-agent/pkg/search"
)

type CompareCommand struct {
	*cmds.CommandDescription
}

func NewCompareCommand() *cobra.Command {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create glazed parameter layer")
	}

	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	compareCmd := &CompareCommand{
		CommandDescription: cmds.NewCommandDescription(
			"compare",
			cmds.WithShort("Compare products across different sites"),
			cmds.WithLong("Compare products across different e-commerce sites to find the best prices, ratings, and availability."),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"products",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("List of product names or URLs to compare"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"sites",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("List of sites to search (default: all available)"),
					parameters.WithDefault([]string{"all"}),
				),
				parameters.NewParameterDefinition(
					"compare-sort-by",
					parameters.ParameterTypeString,
					parameters.WithHelp("Sort comparison by: price, rating, reviews"),
					parameters.WithDefault("price"),
				),
				parameters.NewParameterDefinition(
					"max-results",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Maximum results per product"),
					parameters.WithDefault(5),
				),
			),
			cmds.WithLayers(glazedLayers),
		),
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(compareCmd)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to build compare command")
	}

	return cobraCmd
}

func (c *CompareCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Define parameter structure
	s := struct {
		Products   []string `glazed.parameter:"products"`
		Sites      []string `glazed.parameter:"sites"`
		SortBy     string   `glazed.parameter:"sort-by"`
		MaxResults int      `glazed.parameter:"max-results"`
	}{}

	// Initialize parameters
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s)
	if err != nil {
		return errors.Wrap(err, "failed to initialize parameters")
	}

	log.Info().
		Strs("products", s.Products).
		Strs("sites", s.Sites).
		Str("sort-by", s.SortBy).
		Int("max-results", s.MaxResults).
		Msg("Starting product comparison")

	searcher := search.NewSearcher()
	
	// Compare each product
	for i, product := range s.Products {
		log.Debug().Str("product", product).Int("index", i+1).Msg("Comparing product")

		// Search for this product across specified sites
		for _, site := range s.Sites {
			searchParams := search.SearchParams{
				Query:      product,
				Site:       site,
				MaxResults: s.MaxResults,
			}

			results, err := searcher.Search(ctx, searchParams)
			if err != nil {
				log.Warn().Err(err).Str("product", product).Str("site", site).Msg("Search failed")
				continue
			}

			// Add comparison metadata and output results
			for j, result := range results {
				row := types.NewRow(
					types.MRP("comparison_id", fmt.Sprintf("comp_%d", i+1)),
					types.MRP("product_query", product),
					types.MRP("result_rank", j+1),
					types.MRP("title", result.Title),
					types.MRP("price", result.Price),
					types.MRP("currency", result.Currency),
					types.MRP("url", result.URL),
					types.MRP("site", result.Site),
					types.MRP("availability", result.Availability),
					types.MRP("rating", result.Rating),
					types.MRP("reviews", result.Reviews),
					types.MRP("image_url", result.ImageURL),
					types.MRP("description", result.Description),
					types.MRP("searched_at", result.SearchedAt.Format("2006-01-02 15:04:05")),
				)

				if err := gp.AddRow(ctx, row); err != nil {
					return errors.Wrap(err, "failed to add comparison row to processor")
				}
			}
		}
	}

	log.Info().
		Int("products", len(s.Products)).
		Strs("sites", s.Sites).
		Msg("Product comparison completed")

	return nil
}

func (c *CompareCommand) Description() *cmds.CommandDescription {
	return c.CommandDescription
}

