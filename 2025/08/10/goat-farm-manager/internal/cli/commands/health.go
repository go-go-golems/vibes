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
	"github.com/farm/goat-manager/ent/healthrecord"
	"github.com/farm/goat-manager/internal/database"
)

type HealthSettings struct {
	GoatTag       string  `glazed.parameter:"goat-tag"`
	RecordDate    string  `glazed.parameter:"record-date"`
	RecordType    string  `glazed.parameter:"record-type"`
	Description   string  `glazed.parameter:"description"`
	Veterinarian  string  `glazed.parameter:"veterinarian"`
	Medication    string  `glazed.parameter:"medication"`
	Dosage        string  `glazed.parameter:"dosage"`
	Temperature   float64 `glazed.parameter:"temperature"`
	Weight        float64 `glazed.parameter:"weight"`
	NextDueDate   string  `glazed.parameter:"next-due-date"`
	Cost          float64 `glazed.parameter:"cost"`
	Notes         string  `glazed.parameter:"notes"`
	Limit         int     `glazed.parameter:"limit"`
}

type HealthCommand struct {
	*cmds.CommandDescription
}

func NewHealthCommand() *HealthCommand {
	return &HealthCommand{
		CommandDescription: cmds.NewCommandDescription(
			"health",
			cmds.WithShort("Manage health records"),
			cmds.WithLong("Record and track health events, treatments, and veterinary care"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"health",
					"Health record parameters",
					parameters.NewParameterDefinition(
						"goat-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Tag ID of the goat"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"record-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Date of health record (YYYY-MM-DD, defaults to today)"),
					),
					parameters.NewParameterDefinition(
						"record-type",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Type of health record"),
						parameters.WithChoices("vaccination", "treatment", "checkup", "injury", "illness", "medication", "deworming", "hoof_trim"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"description",
						parameters.ParameterTypeString,
						parameters.WithHelp("Description of the health event"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"veterinarian",
						parameters.ParameterTypeString,
						parameters.WithHelp("Veterinarian who performed the treatment"),
					),
					parameters.NewParameterDefinition(
						"medication",
						parameters.ParameterTypeString,
						parameters.WithHelp("Medication administered"),
					),
					parameters.NewParameterDefinition(
						"dosage",
						parameters.ParameterTypeString,
						parameters.WithHelp("Dosage of medication"),
					),
					parameters.NewParameterDefinition(
						"temperature",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Body temperature in Celsius"),
					),
					parameters.NewParameterDefinition(
						"weight",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Weight at time of record in kg"),
					),
					parameters.NewParameterDefinition(
						"next-due-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Next due date for follow-up (YYYY-MM-DD)"),
					),
					parameters.NewParameterDefinition(
						"cost",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Cost of treatment or medication"),
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

func (c *HealthCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &HealthSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	// If description is provided, create a health record
	if settings.Description != "" {
		return c.createHealthRecord(ctx, client, settings, gp)
	} else {
		// List health records
		return c.listHealthRecords(ctx, client, settings, gp)
	}
}

func (c *HealthCommand) createHealthRecord(
	ctx context.Context,
	client *ent.Client,
	settings *HealthSettings,
	gp middlewares.Processor,
) error {
	// Parse record date
	var recordDate time.Time
	var err error
	if settings.RecordDate != "" {
		recordDate, err = time.Parse("2006-01-02", settings.RecordDate)
		if err != nil {
			return fmt.Errorf("invalid record date format: %w", err)
		}
	} else {
		recordDate = time.Now()
	}

	// Create health record
	create := client.HealthRecord.Create().
		SetGoatTag(settings.GoatTag).
		SetRecordDate(recordDate).
		SetRecordType(settings.RecordType).
		SetDescription(settings.Description)

	if settings.Veterinarian != "" {
		create = create.SetVeterinarian(settings.Veterinarian)
	}
	if settings.Medication != "" {
		create = create.SetMedication(settings.Medication)
	}
	if settings.Dosage != "" {
		create = create.SetDosage(settings.Dosage)
	}
	if settings.Temperature > 0 {
		create = create.SetTemperature(settings.Temperature)
	}
	if settings.Weight > 0 {
		create = create.SetWeight(settings.Weight)
	}
	if settings.Cost > 0 {
		create = create.SetCost(settings.Cost)
	}
	if settings.Notes != "" {
		create = create.SetNotes(settings.Notes)
	}
	if settings.NextDueDate != "" {
		nextDue, err := time.Parse("2006-01-02", settings.NextDueDate)
		if err != nil {
			return fmt.Errorf("invalid next due date format: %w", err)
		}
		create = create.SetNextDueDate(nextDue)
	}

	record, err := create.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create health record: %w", err)
	}

	// Commit changes
	if err := database.CommitChanges(ctx, client, fmt.Sprintf("Added health record for goat %s: %s", settings.GoatTag, settings.RecordType)); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	// Output the result
	row := types.NewRowFromStruct(record, true)
	return gp.AddRow(ctx, row)
}

func (c *HealthCommand) listHealthRecords(
	ctx context.Context,
	client *ent.Client,
	settings *HealthSettings,
	gp middlewares.Processor,
) error {
	query := client.HealthRecord.Query()

	// Apply filters
	if settings.GoatTag != "" {
		query = query.Where(healthrecord.GoatTagEQ(settings.GoatTag))
	}

	if settings.RecordType != "" {
		query = query.Where(healthrecord.RecordTypeEQ(settings.RecordType))
	}

	// Apply limit and order
	if settings.Limit > 0 {
		query = query.Limit(settings.Limit)
	}
	query = query.Order(ent.Desc(healthrecord.FieldRecordDate))

	records, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query health records: %w", err)
	}

	// Output results
	for _, record := range records {
		row := types.NewRowFromStruct(record, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add health record row: %w", err)
		}
	}

	return nil
}

