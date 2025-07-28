package agent

import (
	"context"
	"fmt"
	"time"

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

	"shopping-agent/pkg/browser"
)

type MonitorCommand struct {
	*cmds.CommandDescription
}

func NewMonitorCommand() *cobra.Command {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create glazed parameter layer")
	}

	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	monitorCmd := &MonitorCommand{
		CommandDescription: cmds.NewCommandDescription(
			"monitor",
			cmds.WithShort("Monitor product prices and availability"),
			cmds.WithLong("Monitor product prices and availability on specific URLs, useful for tracking price changes and stock status."),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"urls",
					parameters.ParameterTypeStringList,
					parameters.WithHelp("List of product URLs to monitor"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"price-selector",
					parameters.ParameterTypeString,
					parameters.WithHelp("CSS selector for price element"),
					parameters.WithDefault(".price, .a-price-whole, .notranslate"),
				),
				parameters.NewParameterDefinition(
					"title-selector",
					parameters.ParameterTypeString,
					parameters.WithHelp("CSS selector for product title"),
					parameters.WithDefault("h1, .product-title, #productTitle"),
				),
				parameters.NewParameterDefinition(
					"availability-selector",
					parameters.ParameterTypeString,
					parameters.WithHelp("CSS selector for availability status"),
					parameters.WithDefault(".availability, .stock-status, .a-color-success"),
				),
				parameters.NewParameterDefinition(
					"interval",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Monitoring interval in seconds (0 for single check)"),
					parameters.WithDefault(0),
				),
				parameters.NewParameterDefinition(
					"duration",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Total monitoring duration in minutes (0 for indefinite)"),
					parameters.WithDefault(0),
				),
				parameters.NewParameterDefinition(
					"screenshot",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Take screenshot of each monitored page"),
					parameters.WithDefault(false),
				),
			),
			cmds.WithLayers(glazedLayers),
		),
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(monitorCmd)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to build monitor command")
	}

	return cobraCmd
}

func (c *MonitorCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Define parameter structure
	s := struct {
		URLs                 []string `glazed.parameter:"urls"`
		PriceSelector        string   `glazed.parameter:"price-selector"`
		TitleSelector        string   `glazed.parameter:"title-selector"`
		AvailabilitySelector string   `glazed.parameter:"availability-selector"`
		Interval             int      `glazed.parameter:"interval"`
		Duration             int      `glazed.parameter:"duration"`
		TakeScreenshot       bool     `glazed.parameter:"screenshot"`
	}{}

	// Initialize parameters
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s)
	if err != nil {
		return errors.Wrap(err, "failed to initialize parameters")
	}

	log.Info().
		Strs("urls", s.URLs).
		Str("price-selector", s.PriceSelector).
		Str("title-selector", s.TitleSelector).
		Str("availability-selector", s.AvailabilitySelector).
		Int("interval", s.Interval).
		Int("duration", s.Duration).
		Bool("screenshot", s.TakeScreenshot).
		Msg("Starting price monitoring")

	browserClient := browser.NewClient()
	defer browserClient.Close()

	// Calculate monitoring parameters
	var endTime time.Time
	if s.Duration > 0 {
		endTime = time.Now().Add(time.Duration(s.Duration) * time.Minute)
	}

	checkCount := 0
	for {
		checkCount++
		checkTime := time.Now()

		log.Debug().Int("check", checkCount).Msg("Starting monitoring check")

		// Monitor each URL
		for i, url := range s.URLs {
			log.Debug().Str("url", url).Int("index", i+1).Msg("Monitoring URL")

			// Scrape product data
			scrapingParams := browser.ScrapingParams{
				URL: url,
				Selectors: map[string]string{
					"title":        s.TitleSelector,
					"price":        s.PriceSelector,
					"availability": s.AvailabilitySelector,
				},
				Timeout: 30 * time.Second,
			}

			result, err := browserClient.ScrapeData(ctx, scrapingParams)
			if err != nil {
				log.Warn().Err(err).Str("url", url).Msg("Failed to scrape data")
				
				// Output error row
				row := types.NewRow(
					types.MRP("check_number", checkCount),
					types.MRP("url", url),
					types.MRP("timestamp", checkTime.Format("2006-01-02 15:04:05")),
					types.MRP("success", false),
					types.MRP("error", err.Error()),
					types.MRP("title", ""),
					types.MRP("price", ""),
					types.MRP("availability", ""),
					types.MRP("screenshot_path", ""),
				)

				if err := gp.AddRow(ctx, row); err != nil {
					return errors.Wrap(err, "failed to add error row to processor")
				}
				continue
			}

			// Take screenshot if requested
			screenshotPath := ""
			if s.TakeScreenshot {
				timestamp := checkTime.Format("20060102_150405")
				screenshotPath = fmt.Sprintf("monitor_%d_%s.png", i+1, timestamp)
				
				screenshotParams := browser.ScreenshotParams{
					URL:    url,
					Output: screenshotPath,
					Width:  1920,
					Height: 1080,
					Wait:   2 * time.Second,
				}

				_, err := browserClient.TakeScreenshot(ctx, screenshotParams)
				if err != nil {
					log.Warn().Err(err).Str("url", url).Msg("Failed to take screenshot")
					screenshotPath = ""
				}
			}

			// Extract data with safe type conversion
			title := ""
			if titleData, ok := result.Data["title"]; ok && titleData != nil {
				if titleStr, ok := titleData.(string); ok {
					title = titleStr
				}
			}

			price := ""
			if priceData, ok := result.Data["price"]; ok && priceData != nil {
				if priceStr, ok := priceData.(string); ok {
					price = priceStr
				}
			}

			availability := ""
			if availData, ok := result.Data["availability"]; ok && availData != nil {
				if availStr, ok := availData.(string); ok {
					availability = availStr
				}
			}

			// Output monitoring result
			row := types.NewRow(
				types.MRP("check_number", checkCount),
				types.MRP("url", url),
				types.MRP("timestamp", checkTime.Format("2006-01-02 15:04:05")),
				types.MRP("success", result.Success),
				types.MRP("error", result.Error),
				types.MRP("title", title),
				types.MRP("price", price),
				types.MRP("availability", availability),
				types.MRP("screenshot_path", screenshotPath),
				types.MRP("duration_ms", result.Duration.Milliseconds()),
			)

			if err := gp.AddRow(ctx, row); err != nil {
				return errors.Wrap(err, "failed to add monitoring row to processor")
			}
		}

		// Check if we should continue monitoring
		if s.Interval <= 0 {
			// Single check mode
			break
		}

		if s.Duration > 0 && time.Now().After(endTime) {
			// Duration limit reached
			log.Info().Int("checks", checkCount).Msg("Monitoring duration completed")
			break
		}

		// Wait for next check
		log.Debug().Int("interval", s.Interval).Msg("Waiting for next check")
		time.Sleep(time.Duration(s.Interval) * time.Second)
	}

	log.Info().
		Int("urls", len(s.URLs)).
		Int("total_checks", checkCount).
		Msg("Price monitoring completed")

	return nil
}

func (c *MonitorCommand) Description() *cmds.CommandDescription {
	return c.CommandDescription
}

