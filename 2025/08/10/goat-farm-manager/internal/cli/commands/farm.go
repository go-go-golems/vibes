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
	"github.com/farm/goat-manager/ent/farmoperation"
	"github.com/farm/goat-manager/internal/database"
)

type FarmSettings struct {
	OperationDate   string  `glazed.parameter:"operation-date"`
	OperationType   string  `glazed.parameter:"operation-type"`
	Description     string  `glazed.parameter:"description"`
	PerformedBy     string  `glazed.parameter:"performed-by"`
	AffectedGoats   string  `glazed.parameter:"affected-goats"`
	Quantity        float64 `glazed.parameter:"quantity"`
	Unit            string  `glazed.parameter:"unit"`
	Cost            float64 `glazed.parameter:"cost"`
	Revenue         float64 `glazed.parameter:"revenue"`
	SupplierBuyer   string  `glazed.parameter:"supplier-buyer"`
	Notes           string  `glazed.parameter:"notes"`
	Limit           int     `glazed.parameter:"limit"`
}

type FarmCommand struct {
	*cmds.CommandDescription
}

func NewFarmCommand() *FarmCommand {
	return &FarmCommand{
		CommandDescription: cmds.NewCommandDescription(
			"farm",
			cmds.WithShort("Manage farm operations"),
			cmds.WithLong("Record and track general farm operations and activities"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"farm",
					"Farm operation parameters",
					parameters.NewParameterDefinition(
						"operation-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Date of operation (YYYY-MM-DD, defaults to today)"),
					),
					parameters.NewParameterDefinition(
						"operation-type",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Type of farm operation"),
						parameters.WithChoices(
							"feeding", "milking", "cleaning", "maintenance", "vaccination_batch",
							"deworming_batch", "hoof_trimming", "pasture_rotation", "equipment_maintenance",
							"feed_purchase", "supply_purchase", "milk_sale", "goat_sale", "other",
						),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"description",
						parameters.ParameterTypeString,
						parameters.WithHelp("Description of the operation"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"performed-by",
						parameters.ParameterTypeString,
						parameters.WithHelp("Person who performed the operation"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"affected-goats",
						parameters.ParameterTypeString,
						parameters.WithHelp("Comma-separated list of affected goat tags"),
					),
					parameters.NewParameterDefinition(
						"quantity",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Quantity involved in the operation"),
					),
					parameters.NewParameterDefinition(
						"unit",
						parameters.ParameterTypeString,
						parameters.WithHelp("Unit of measurement for quantity"),
					),
					parameters.NewParameterDefinition(
						"cost",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Cost associated with the operation"),
					),
					parameters.NewParameterDefinition(
						"revenue",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Revenue generated from the operation"),
					),
					parameters.NewParameterDefinition(
						"supplier-buyer",
						parameters.ParameterTypeString,
						parameters.WithHelp("Supplier or buyer involved"),
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

func (c *FarmCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &FarmSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	// If description is provided, create a farm operation record
	if settings.Description != "" && settings.PerformedBy != "" {
		return c.createFarmOperation(ctx, client, settings, gp)
	} else {
		// List farm operations
		return c.listFarmOperations(ctx, client, settings, gp)
	}
}

func (c *FarmCommand) createFarmOperation(
	ctx context.Context,
	client *ent.Client,
	settings *FarmSettings,
	gp middlewares.Processor,
) error {
	// Parse operation date
	var operationDate time.Time
	var err error
	if settings.OperationDate != "" {
		operationDate, err = time.Parse("2006-01-02", settings.OperationDate)
		if err != nil {
			return fmt.Errorf("invalid operation date format: %w", err)
		}
	} else {
		operationDate = time.Now()
	}

	// Create farm operation record
	create := client.FarmOperation.Create().
		SetOperationDate(operationDate).
		SetOperationType(settings.OperationType).
		SetDescription(settings.Description).
		SetPerformedBy(settings.PerformedBy)

	if settings.AffectedGoats != "" {
		create = create.SetAffectedGoats(settings.AffectedGoats)
	}
	if settings.Quantity > 0 {
		create = create.SetQuantity(settings.Quantity)
	}
	if settings.Unit != "" {
		create = create.SetUnit(settings.Unit)
	}
	if settings.Cost > 0 {
		create = create.SetCost(settings.Cost)
	}
	if settings.Revenue > 0 {
		create = create.SetRevenue(settings.Revenue)
	}
	if settings.SupplierBuyer != "" {
		create = create.SetSupplierBuyer(settings.SupplierBuyer)
	}
	if settings.Notes != "" {
		create = create.SetNotes(settings.Notes)
	}

	record, err := create.Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create farm operation: %w", err)
	}

	// Commit changes
	if err := database.CommitChanges(ctx, client, fmt.Sprintf("Added farm operation: %s", settings.OperationType)); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	// Output the result
	row := types.NewRowFromStruct(record, true)
	return gp.AddRow(ctx, row)
}

func (c *FarmCommand) listFarmOperations(
	ctx context.Context,
	client *ent.Client,
	settings *FarmSettings,
	gp middlewares.Processor,
) error {
	query := client.FarmOperation.Query()

	// Apply filters
	if settings.OperationType != "" {
		query = query.Where(farmoperation.OperationTypeEQ(settings.OperationType))
	}
	if settings.PerformedBy != "" {
		query = query.Where(farmoperation.PerformedByEQ(settings.PerformedBy))
	}

	// Apply limit and order
	if settings.Limit > 0 {
		query = query.Limit(settings.Limit)
	}
	query = query.Order(ent.Desc(farmoperation.FieldOperationDate))

	records, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query farm operations: %w", err)
	}

	// Output results
	for _, record := range records {
		row := types.NewRowFromStruct(record, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add farm operation row: %w", err)
		}
	}

	return nil
}

