package agent

import (
	"context"

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

type SearchCommand struct {
	*cmds.CommandDescription
}

func NewSearchCommand() *cobra.Command {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create glazed parameter layer")
	}

	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	searchCmd := &SearchCommand{
		CommandDescription: cmds.NewCommandDescription(
			"search",
			cmds.WithShort("Search for products across e-commerce sites"),
			cmds.WithLong("Search for products across multiple e-commerce sites and return structured data about products, prices, and availability."),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"query",
					parameters.ParameterTypeString,
					parameters.WithHelp("Product search query"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"site",
					parameters.ParameterTypeString,
					parameters.WithHelp("Specific e-commerce site to search (amazon, ebay, etc.)"),
					parameters.WithDefault("all"),
				),
				parameters.NewParameterDefinition(
					"max-results",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Maximum number of results to return"),
					parameters.WithDefault(10),
				),
				parameters.NewParameterDefinition(
					"min-price",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Minimum price filter"),
					parameters.WithDefault(0.0),
				),
				parameters.NewParameterDefinition(
					"max-price",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Maximum price filter"),
					parameters.WithDefault(0.0),
				),
			),
			cmds.WithLayers(glazedLayers),
		),
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(searchCmd)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to build search command")
	}

	return cobraCmd
}

func (c *SearchCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Define parameter structure
	s := struct {
		Query      string  `glazed.parameter:"query"`
		Site       string  `glazed.parameter:"site"`
		MaxResults int     `glazed.parameter:"max-results"`
		MinPrice   float64 `glazed.parameter:"min-price"`
		MaxPrice   float64 `glazed.parameter:"max-price"`
	}{}

	// Initialize parameters
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s)
	if err != nil {
		return errors.Wrap(err, "failed to initialize parameters")
	}

	log.Info().
		Str("query", s.Query).
		Str("site", s.Site).
		Int("max-results", s.MaxResults).
		Float64("min-price", s.MinPrice).
		Float64("max-price", s.MaxPrice).
		Msg("Starting product search")

	// Create search parameters
	searchParams := search.SearchParams{
		Query:      s.Query,
		Site:       s.Site,
		MaxResults: s.MaxResults,
		MinPrice:   s.MinPrice,
		MaxPrice:   s.MaxPrice,
	}

	// Perform search
	searcher := search.NewSearcher()
	results, err := searcher.Search(ctx, searchParams)
	if err != nil {
		return errors.Wrap(err, "failed to perform search")
	}

	// Output results using glazed
	for _, result := range results {
		row := types.NewRow(
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
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return errors.Wrap(err, "failed to add row to processor")
		}
	}

	log.Info().Int("results", len(results)).Msg("Search completed successfully")
	return nil
}

func (c *SearchCommand) Description() *cmds.CommandDescription {
	return c.CommandDescription
}

