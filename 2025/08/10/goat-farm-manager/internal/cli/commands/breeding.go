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
	"github.com/farm/goat-manager/ent/breedingrecord"
	"github.com/farm/goat-manager/internal/database"
)

type BreedingSettings struct {
	DoeTag              string `glazed.parameter:"doe-tag"`
	BuckTag             string `glazed.parameter:"buck-tag"`
	BreedingDate        string `glazed.parameter:"breeding-date"`
	ExpectedKiddingDate string `glazed.parameter:"expected-kidding-date"`
	ActualKiddingDate   string `glazed.parameter:"actual-kidding-date"`
	KidsBorn            int    `glazed.parameter:"kids-born"`
	KidsAlive           int    `glazed.parameter:"kids-alive"`
	BreedingMethod      string `glazed.parameter:"breeding-method"`
	Status              string `glazed.parameter:"status"`
	Complications       string `glazed.parameter:"complications"`
	Notes               string `glazed.parameter:"notes"`
	Limit               int    `glazed.parameter:"limit"`
}

type BreedingCommand struct {
	*cmds.CommandDescription
}

func NewBreedingCommand() *BreedingCommand {
	return &BreedingCommand{
		CommandDescription: cmds.NewCommandDescription(
			"breeding",
			cmds.WithShort("Manage breeding records"),
			cmds.WithLong("Record and track breeding activities and kidding events"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"breeding",
					"Breeding record parameters",
					parameters.NewParameterDefinition(
						"doe-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Tag ID of the female goat (doe)"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"buck-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Tag ID of the male goat (buck)"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"breeding-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Date of breeding (YYYY-MM-DD, defaults to today)"),
					),
					parameters.NewParameterDefinition(
						"expected-kidding-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Expected kidding date (YYYY-MM-DD)"),
					),
					parameters.NewParameterDefinition(
						"actual-kidding-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Actual kidding date (YYYY-MM-DD)"),
					),
					parameters.NewParameterDefinition(
						"kids-born",
						parameters.ParameterTypeInteger,
						parameters.WithHelp("Number of kids born"),
					),
					parameters.NewParameterDefinition(
						"kids-alive",
						parameters.ParameterTypeInteger,
						parameters.WithHelp("Number of kids that survived"),
					),
					parameters.NewParameterDefinition(
						"breeding-method",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Method of breeding"),
						parameters.WithChoices("natural", "artificial_insemination"),
						parameters.WithDefault("natural"),
					),
					parameters.NewParameterDefinition(
						"status",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Status of the breeding"),
						parameters.WithChoices("bred", "confirmed_pregnant", "kidded", "failed", "aborted"),
						parameters.WithDefault("bred"),
					),
					parameters.NewParameterDefinition(
						"complications",
						parameters.ParameterTypeString,
						parameters.WithHelp("Any complications during breeding or kidding"),
					),
					parameters.NewParameterDefinition(
						"notes",
						parameters.ParameterTypeString,
						parameters.WithHelp("Additional notes"),
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

func (c *BreedingCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &BreedingSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	// If both doe and buck tags are provided, create a breeding record
	if settings.DoeTag != "" && settings.BuckTag != "" {
		return c.createBreedingRecord(ctx, client, settings, gp)
	} else {
		// List breeding records
		return c.listBreedingRecords(ctx, client, settings, gp)
	}
}

func (c *BreedingCommand) createBreedingRecord(
	ctx context.Context,
	client *ent.Client,
	settings *BreedingSettings,
	gp middlewares.Processor,
) error {
	// Parse breeding date
	var breedingDate time.Time
	var err error
	if settings.BreedingDate != "" {
		breedingDate, err = time.Parse("2006-01-02", settings.BreedingDate)
		if err != nil {
			return fmt.Errorf("invalid breeding date format: %w", err)
		}
	} else {
		breedingDate = time.Now()
	}

	// Calculate expected kidding date (approximately 150 days after breeding)
	expectedKiddingDate := breedingDate.AddDate(0, 0, 150)
	if settings.ExpectedKiddingDate != "" {
		expectedKiddingDate, err = time.Parse("2006-01-02", settings.ExpectedKiddingDate)
		if err != nil {
			return fmt.Errorf("invalid expected kidding date format: %w", err)
		}
	}

	// Create breeding record
	create := client.BreedingRecord.Create().
		SetDoeTag(settings.DoeTag).
		SetBuckTag(settings.BuckTag).
		SetBreedingDate(breedingDate).
		SetExpectedKiddingDate(expectedKiddingDate).
		SetBreedingMethod(settings.BreedingMethod).
		SetStatus(settings.Status)

	if settings.ActualKiddingDate != "" {
		actualKiddingDate, err := time.Parse("2006-01-02", settings.ActualKiddingDate)
		if err != nil {
			return fmt.Errorf("invalid actual kidding date format: %w", err)
		}
		create = create.SetActualKiddingDate(actualKiddingDate)
	}

	if settings.KidsBorn > 0 {
		create = create.SetKidsBorn(settings.KidsBorn)
	}
	if settings.KidsAlive > 0 {
		create = create.SetKidsAlive(settings.KidsAlive)
	}
	if settings.Complications != "" {
		create = create.SetComplications(settings.Complications)
	}
	if settings.Notes != "" {
		create = create.SetNotes(settings.Notes)
	}

	record, err := create.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create breeding record: %w", err)
	}

	// Commit changes
	if err := database.CommitChanges(ctx, client, fmt.Sprintf("Added breeding record: %s x %s", settings.DoeTag, settings.BuckTag)); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	// Output the result
	row := types.NewRowFromStruct(record, true)
	return gp.AddRow(ctx, row)
}

func (c *BreedingCommand) listBreedingRecords(
	ctx context.Context,
	client *ent.Client,
	settings *BreedingSettings,
	gp middlewares.Processor,
) error {
	query := client.BreedingRecord.Query()

	// Apply filters
	if settings.DoeTag != "" {
		query = query.Where(breedingrecord.DoeTagEQ(settings.DoeTag))
	}
	if settings.BuckTag != "" {
		query = query.Where(breedingrecord.BuckTagEQ(settings.BuckTag))
	}
	if settings.Status != "" {
		query = query.Where(breedingrecord.StatusEQ(settings.Status))
	}

	// Apply limit and order
	if settings.Limit > 0 {
		query = query.Limit(settings.Limit)
	}
	query = query.Order(ent.Desc(breedingrecord.FieldBreedingDate))

	records, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query breeding records: %w", err)
	}

	// Output results
	for _, record := range records {
		row := types.NewRowFromStruct(record, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add breeding record row: %w", err)
		}
	}

	return nil
}

