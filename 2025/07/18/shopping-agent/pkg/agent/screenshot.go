package agent

import (
	"context"
	"fmt"
	"path/filepath"
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

type ScreenshotCommand struct {
	*cmds.CommandDescription
}

func NewScreenshotCommand() *cobra.Command {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create glazed parameter layer")
	}

	glazedLayers := layers.NewParameterLayers()
	glazedLayers.Set(settings.GlazedSlug, glazedParameterLayer)

	screenshotCmd := &ScreenshotCommand{
		CommandDescription: cmds.NewCommandDescription(
			"screenshot",
			cmds.WithShort("Take screenshots of web pages"),
			cmds.WithLong("Take screenshots of web pages, particularly useful for capturing product pages, price comparisons, and shopping cart states."),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"url",
					parameters.ParameterTypeString,
					parameters.WithHelp("URL of the web page to screenshot"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"screenshot-output",
					parameters.ParameterTypeString,
					parameters.WithHelp("Output file path for the screenshot"),
					parameters.WithDefault(""),
				),
				parameters.NewParameterDefinition(
					"width",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Browser viewport width"),
					parameters.WithDefault(1920),
				),
				parameters.NewParameterDefinition(
					"height",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Browser viewport height"),
					parameters.WithDefault(1080),
				),
				parameters.NewParameterDefinition(
					"full-page",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Take full page screenshot"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"wait",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Wait time in seconds before taking screenshot"),
					parameters.WithDefault(2),
				),
				parameters.NewParameterDefinition(
					"selector",
					parameters.ParameterTypeString,
					parameters.WithHelp("CSS selector to wait for before taking screenshot"),
					parameters.WithDefault(""),
				),
			),
			cmds.WithLayers(glazedLayers),
		),
	}

	cobraCmd, err := cli.BuildCobraCommandFromGlazeCommand(screenshotCmd)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to build screenshot command")
	}

	return cobraCmd
}

func (c *ScreenshotCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	// Define parameter structure
	s := struct {
		URL      string `glazed.parameter:"url"`
		Output   string `glazed.parameter:"output"`
		Width    int    `glazed.parameter:"width"`
		Height   int    `glazed.parameter:"height"`
		FullPage bool   `glazed.parameter:"full-page"`
		Wait     int    `glazed.parameter:"wait"`
		Selector string `glazed.parameter:"selector"`
	}{}

	// Initialize parameters
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &s)
	if err != nil {
		return errors.Wrap(err, "failed to initialize parameters")
	}

	log.Info().
		Str("url", s.URL).
		Str("output", s.Output).
		Int("width", s.Width).
		Int("height", s.Height).
		Bool("full-page", s.FullPage).
		Int("wait", s.Wait).
		Str("selector", s.Selector).
		Msg("Starting screenshot capture")

	// Generate output filename if not provided
	outputPath := s.Output
	if outputPath == "" {
		timestamp := time.Now().Format("20060102_150405")
		outputPath = fmt.Sprintf("screenshot_%s.png", timestamp)
	}

	// Ensure output path has .png extension
	if filepath.Ext(outputPath) == "" {
		outputPath += ".png"
	}

	// Create screenshot parameters
	screenshotParams := browser.ScreenshotParams{
		URL:       s.URL,
		Output:    outputPath,
		Width:     s.Width,
		Height:    s.Height,
		FullPage:  s.FullPage,
		Wait:      time.Duration(s.Wait) * time.Second,
		Selector:  s.Selector,
	}

	// Take screenshot
	browserClient := browser.NewClient()
	result, err := browserClient.TakeScreenshot(ctx, screenshotParams)
	if err != nil {
		return errors.Wrap(err, "failed to take screenshot")
	}

	// Output result using glazed
	row := types.NewRow(
		types.MRP("url", result.URL),
		types.MRP("output_path", result.OutputPath),
		types.MRP("file_size", result.FileSize),
		types.MRP("width", result.Width),
		types.MRP("height", result.Height),
		types.MRP("full_page", result.FullPage),
		types.MRP("duration_ms", result.Duration.Milliseconds()),
		types.MRP("timestamp", result.Timestamp.Format(time.RFC3339)),
		types.MRP("success", result.Success),
		types.MRP("error", result.Error),
	)

	if err := gp.AddRow(ctx, row); err != nil {
		return errors.Wrap(err, "failed to add row to processor")
	}

	log.Info().
		Str("output_path", result.OutputPath).
		Int64("file_size", result.FileSize).
		Int64("duration_ms", result.Duration.Milliseconds()).
		Msg("Screenshot captured successfully")

	return nil
}

func (c *ScreenshotCommand) Description() *cmds.CommandDescription {
	return c.CommandDescription
}

