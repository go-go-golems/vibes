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
	"github.com/farm/goat-manager/ent/milkrecord"
	"github.com/farm/goat-manager/internal/database"
)

type MilkSettings struct {
	GoatTag         string  `glazed.parameter:"goat-tag"`
	MilkingTime     string  `glazed.parameter:"milking-time"`
	Session         string  `glazed.parameter:"session"`
	Volume          float64 `glazed.parameter:"volume"`
	FatContent      float64 `glazed.parameter:"fat-content"`
	ProteinContent  float64 `glazed.parameter:"protein-content"`
	SomaticCellCount float64 `glazed.parameter:"somatic-cell-count"`
	QualityGrade    string  `glazed.parameter:"quality-grade"`
	MilkedBy        string  `glazed.parameter:"milked-by"`
	Notes           string  `glazed.parameter:"notes"`
	Limit           int     `glazed.parameter:"limit"`
	DateFrom        string  `glazed.parameter:"date-from"`
	DateTo          string  `glazed.parameter:"date-to"`
}

type MilkCommand struct {
	*cmds.CommandDescription
}

func NewMilkCommand() *MilkCommand {
	return &MilkCommand{
		CommandDescription: cmds.NewCommandDescription(
			"milk",
			cmds.WithShort("Manage milk production records"),
			cmds.WithLong("Record and track milk production from goats"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"milk",
					"Milk production parameters",
					parameters.NewParameterDefinition(
						"goat-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Tag ID of the goat"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"milking-time",
						parameters.ParameterTypeString,
						parameters.WithHelp("Milking time (YYYY-MM-DD HH:MM format, defaults to now)"),
					),
					parameters.NewParameterDefinition(
						"session",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Milking session"),
						parameters.WithChoices("morning", "afternoon", "evening"),
						parameters.WithDefault("morning"),
					),
					parameters.NewParameterDefinition(
						"volume",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Volume of milk in liters"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"fat-content",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Fat content percentage"),
					),
					parameters.NewParameterDefinition(
						"protein-content",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Protein content percentage"),
					),
					parameters.NewParameterDefinition(
						"somatic-cell-count",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Somatic cell count (cells/ml)"),
					),
					parameters.NewParameterDefinition(
						"quality-grade",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Quality grade of the milk"),
						parameters.WithChoices("A", "B", "C", "reject"),
						parameters.WithDefault("A"),
					),
					parameters.NewParameterDefinition(
						"milked-by",
						parameters.ParameterTypeString,
						parameters.WithHelp("Person who performed the milking"),
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
					parameters.NewParameterDefinition(
						"date-from",
						parameters.ParameterTypeString,
						parameters.WithHelp("Start date for filtering (YYYY-MM-DD)"),
					),
					parameters.NewParameterDefinition(
						"date-to",
						parameters.ParameterTypeString,
						parameters.WithHelp("End date for filtering (YYYY-MM-DD)"),
					),
				),
			),
		),
	}
}

func (c *MilkCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &MilkSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	// If volume is provided, create a milk record
	if settings.Volume > 0 {
		return c.createMilkRecord(ctx, client, settings, gp)
	} else {
		// List milk records
		return c.listMilkRecords(ctx, client, settings, gp)
	}
}

func (c *MilkCommand) createMilkRecord(
	ctx context.Context,
	client *ent.Client,
	settings *MilkSettings,
	gp middlewares.Processor,
) error {
	// Parse milking time
	var milkingTime time.Time
	var err error
	if settings.MilkingTime != "" {
		milkingTime, err = time.Parse("2006-01-02 15:04", settings.MilkingTime)
		if err != nil {
			return fmt.Errorf("invalid milking time format: %w", err)
		}
	} else {
		milkingTime = time.Now()
	}

	// Create milk record
	create := client.MilkRecord.Create().
		SetGoatTag(settings.GoatTag).
		SetMilkingTime(milkingTime).
		SetMilkingSession(settings.Session).
		SetVolumeLiters(settings.Volume).
		SetQualityGrade(settings.QualityGrade)

	if settings.FatContent > 0 {
		create = create.SetFatContent(settings.FatContent)
	}
	if settings.ProteinContent > 0 {
		create = create.SetProteinContent(settings.ProteinContent)
	}
	if settings.SomaticCellCount > 0 {
		create = create.SetSomaticCellCount(settings.SomaticCellCount)
	}
	if settings.MilkedBy != "" {
		create = create.SetMilkedBy(settings.MilkedBy)
	}
	if settings.Notes != "" {
		create = create.SetNotes(settings.Notes)
	}

	record, err := create.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create milk record: %w", err)
	}

	// Commit changes
	if err := database.CommitChanges(ctx, client, fmt.Sprintf("Added milk record for goat %s", settings.GoatTag)); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	// Output the result
	row := types.NewRowFromStruct(record, true)
	return gp.AddRow(ctx, row)
}

func (c *MilkCommand) listMilkRecords(
	ctx context.Context,
	client *ent.Client,
	settings *MilkSettings,
	gp middlewares.Processor,
) error {
	query := client.MilkRecord.Query()

	// Apply filters
	if settings.GoatTag != "" {
		query = query.Where(milkrecord.GoatTagEQ(settings.GoatTag))
	}

	if settings.DateFrom != "" {
		dateFrom, err := time.Parse("2006-01-02", settings.DateFrom)
		if err != nil {
			return fmt.Errorf("invalid date-from format: %w", err)
		}
		query = query.Where(milkrecord.MilkingTimeGTE(dateFrom))
	}

	if settings.DateTo != "" {
		dateTo, err := time.Parse("2006-01-02", settings.DateTo)
		if err != nil {
			return fmt.Errorf("invalid date-to format: %w", err)
		}
		query = query.Where(milkrecord.MilkingTimeLTE(dateTo))
	}

	// Apply limit and order
	if settings.Limit > 0 {
		query = query.Limit(settings.Limit)
	}
	query = query.Order(ent.Desc(milkrecord.FieldMilkingTime))

	records, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query milk records: %w", err)
	}

	// Output results
	for _, record := range records {
		row := types.NewRowFromStruct(record, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add milk record row: %w", err)
		}
	}

	return nil
}

