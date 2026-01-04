package commands

import (
	"context"
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/farm/goat-manager/ent"
	"github.com/farm/goat-manager/ent/feedrecord"
	"github.com/farm/goat-manager/internal/database"
)

type FeedSettings struct {
	FeedingDate   string  `glazed.parameter:"feeding-date"`
	FeedType      string  `glazed.parameter:"feed-type"`
	FeedName      string  `glazed.parameter:"feed-name"`
	Quantity      float64 `glazed.parameter:"quantity"`
	Unit          string  `glazed.parameter:"unit"`
	GoatTags      string  `glazed.parameter:"goat-tags"`
	FeedingMethod string  `glazed.parameter:"feeding-method"`
	FedBy         string  `glazed.parameter:"fed-by"`
	CostPerUnit   float64 `glazed.parameter:"cost-per-unit"`
	Notes         string  `glazed.parameter:"notes"`
	Limit         int     `glazed.parameter:"limit"`
}

type FeedCommand struct {
	*cmds.CommandDescription
}

func NewFeedCommand() *FeedCommand {
	return &FeedCommand{
		CommandDescription: cmds.NewCommandDescription(
			"feed",
			cmds.WithShort("Manage feeding records"),
			cmds.WithLong("Record and track feeding activities and feed consumption"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"feed",
					"Feed record parameters",
					parameters.NewParameterDefinition(
						"feeding-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Date and time of feeding (YYYY-MM-DD HH:MM, defaults to now)"),
					),
					parameters.NewParameterDefinition(
						"feed-type",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Type of feed given"),
						parameters.WithChoices("hay", "grain", "pellets", "pasture", "silage", "supplements", "treats", "other"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"feed-name",
						parameters.ParameterTypeString,
						parameters.WithHelp("Specific name or brand of the feed"),
					),
					parameters.NewParameterDefinition(
						"quantity",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Quantity of feed given"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"unit",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Unit of measurement"),
						parameters.WithChoices("kg", "lbs", "cups", "scoops", "bales"),
						parameters.WithDefault("kg"),
					),
					parameters.NewParameterDefinition(
						"goat-tags",
						parameters.ParameterTypeString,
						parameters.WithHelp("Comma-separated list of goat tags (empty for group feeding)"),
					),
					parameters.NewParameterDefinition(
						"feeding-method",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Method of feeding"),
						parameters.WithChoices("individual", "group", "pasture"),
						parameters.WithDefault("group"),
					),
					parameters.NewParameterDefinition(
						"fed-by",
						parameters.ParameterTypeString,
						parameters.WithHelp("Person who performed the feeding"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"cost-per-unit",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Cost per unit of feed"),
					),
					parameters.NewParameterDefinition(
						"notes",
						parameters.ParameterTypeString,
						parameters.WithHelp("Additional notes about feeding"),
					),
					parameters.NewParameterDefinition(
						"limit",
						parameters.ParameterTypeInteger,
						parameters.WithHelp("Limit number of results"),
						parameters.WithDefault(50),
					),
				),
			),
		),
	}
}

func (c *FeedCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &FeedSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	// If quantity is provided, create a feed record
	if settings.Quantity > 0 && settings.FedBy != "" {
		return c.createFeedRecord(ctx, client, settings, gp)
	} else {
		// List feed records
		return c.listFeedRecords(ctx, client, settings, gp)
	}
}

func (c *FeedCommand) createFeedRecord(
	ctx context.Context,
	client *ent.Client,
	settings *FeedSettings,
	gp middlewares.Processor,
) error {
	// Parse feeding date
	var feedingDate time.Time
	var err error
	if settings.FeedingDate != "" {
		feedingDate, err = time.Parse("2006-01-02 15:04", settings.FeedingDate)
		if err != nil {
			// Try date only format
			feedingDate, err = time.Parse("2006-01-02", settings.FeedingDate)
			if err != nil {
				return fmt.Errorf("invalid feeding date format: %w", err)
			}
		}
	} else {
		feedingDate = time.Now()
	}

	// Create feed record
	create := client.FeedRecord.Create().
		SetFeedingDate(feedingDate).
		SetFeedType(settings.FeedType).
		SetQuantity(settings.Quantity).
		SetUnit(settings.Unit).
		SetFeedingMethod(settings.FeedingMethod).
		SetFedBy(settings.FedBy)

	if settings.FeedName != "" {
		create = create.SetFeedName(settings.FeedName)
	}
	if settings.GoatTags != "" {
		create = create.SetGoatTags(settings.GoatTags)
	}
	if settings.CostPerUnit > 0 {
		create = create.SetCostPerUnit(settings.CostPerUnit)
	}
	if settings.Notes != "" {
		create = create.SetNotes(settings.Notes)
	}

	record, err := create.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create feed record: %w", err)
	}

	// Commit changes
	if err := database.CommitChanges(ctx, client, fmt.Sprintf("Added feed record: %s %.2f %s", settings.FeedType, settings.Quantity, settings.Unit)); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	// Output the result
	row := types.NewRowFromStruct(record, true)
	return gp.AddRow(ctx, row)
}

func (c *FeedCommand) listFeedRecords(
	ctx context.Context,
	client *ent.Client,
	settings *FeedSettings,
	gp middlewares.Processor,
) error {
	query := client.FeedRecord.Query()

	// Apply filters
	if settings.FeedType != "" {
		query = query.Where(feedrecord.FeedTypeEQ(settings.FeedType))
	}
	if settings.FedBy != "" {
		query = query.Where(feedrecord.FedByEQ(settings.FedBy))
	}

	// Apply limit and order
	if settings.Limit > 0 {
		query = query.Limit(settings.Limit)
	}
	query = query.Order(ent.Desc(feedrecord.FieldFeedingDate))

	records, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query feed records: %w", err)
	}

	// Output results
	for _, record := range records {
		row := types.NewRowFromStruct(record, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add feed record row: %w", err)
		}
	}

	return nil
}

