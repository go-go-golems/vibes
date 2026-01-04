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
	"github.com/farm/goat-manager/ent/goat"
	"github.com/farm/goat-manager/internal/database"
)

type GoatSettings struct {
	TagID      string `glazed.parameter:"tag-id"`
	Name       string `glazed.parameter:"name"`
	Breed      string `glazed.parameter:"breed"`
	Gender     string `glazed.parameter:"gender"`
	BirthDate  string `glazed.parameter:"birth-date"`
	Weight     float64 `glazed.parameter:"weight"`
	Status     string `glazed.parameter:"status"`
	SireTag    string `glazed.parameter:"sire-tag"`
	DamTag     string `glazed.parameter:"dam-tag"`
	Notes      string `glazed.parameter:"notes"`
	Limit      int    `glazed.parameter:"limit"`
	Filter     string `glazed.parameter:"filter"`
}

type GoatCommand struct {
	*cmds.CommandDescription
}

func NewGoatCommand() *GoatCommand {
	return &GoatCommand{
		CommandDescription: cmds.NewCommandDescription(
			"goat",
			cmds.WithShort("Manage goats in the farm"),
			cmds.WithLong("Add, update, list, and delete goats in the farm management system"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"goat",
					"Goat management parameters",
					parameters.NewParameterDefinition(
						"tag-id",
						parameters.ParameterTypeString,
						parameters.WithHelp("Unique tag ID for the goat"),
						parameters.WithRequired(false),
					),
					parameters.NewParameterDefinition(
						"name",
						parameters.ParameterTypeString,
						parameters.WithHelp("Name of the goat"),
					),
					parameters.NewParameterDefinition(
						"breed",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Breed of the goat"),
						parameters.WithChoices("nubian", "alpine", "saanen", "toggenburg", "lamancha", "boer", "angus", "other"),
					),
					parameters.NewParameterDefinition(
						"gender",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Gender of the goat"),
						parameters.WithChoices("male", "female"),
					),
					parameters.NewParameterDefinition(
						"birth-date",
						parameters.ParameterTypeString,
						parameters.WithHelp("Birth date (YYYY-MM-DD format)"),
					),
					parameters.NewParameterDefinition(
						"weight",
						parameters.ParameterTypeFloat,
						parameters.WithHelp("Weight in kg"),
					),
					parameters.NewParameterDefinition(
						"status",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Current status of the goat"),
						parameters.WithChoices("active", "pregnant", "lactating", "dry", "sick", "sold", "deceased"),
						parameters.WithDefault("active"),
					),
					parameters.NewParameterDefinition(
						"sire-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Tag ID of the father"),
					),
					parameters.NewParameterDefinition(
						"dam-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Tag ID of the mother"),
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
						"filter",
						parameters.ParameterTypeString,
						parameters.WithHelp("Filter goats by name or tag"),
					),
				),
			),
		),
	}
}

func (c *GoatCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &GoatSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	// Determine action based on provided parameters
	if settings.TagID != "" && settings.Name != "" {
		// Create or update goat
		return c.createOrUpdateGoat(ctx, client, settings, gp)
	} else {
		// List goats
		return c.listGoats(ctx, client, settings, gp)
	}
}

func (c *GoatCommand) createOrUpdateGoat(
	ctx context.Context,
	client *ent.Client,
	settings *GoatSettings,
	gp middlewares.Processor,
) error {
	// Check if goat exists
	existingGoat, err := client.Goat.Get(ctx, settings.TagID)
	if err != nil && !ent.IsNotFound(err) {
		return fmt.Errorf("failed to check existing goat: %w", err)
	}

	var goat *ent.Goat
	if existingGoat != nil {
		// Update existing goat
		update := client.Goat.UpdateOneID(settings.TagID)
		if settings.Name != "" {
			update = update.SetName(settings.Name)
		}
		if settings.Breed != "" {
			update = update.SetBreed(settings.Breed)
		}
		if settings.Gender != "" {
			update = update.SetGender(settings.Gender)
		}
		if settings.Weight > 0 {
			update = update.SetWeight(settings.Weight)
		}
		if settings.Status != "" {
			update = update.SetStatus(settings.Status)
		}
		if settings.SireTag != "" {
			update = update.SetSireTag(settings.SireTag)
		}
		if settings.DamTag != "" {
			update = update.SetDamTag(settings.DamTag)
		}
		if settings.Notes != "" {
			update = update.SetNotes(settings.Notes)
		}
		if settings.BirthDate != "" {
			birthDate, err := time.Parse("2006-01-02", settings.BirthDate)
			if err != nil {
				return fmt.Errorf("invalid birth date format: %w", err)
			}
			update = update.SetBirthDate(birthDate)
		}

		goat, err = update.Save(ctx)
		if err != nil {
			return fmt.Errorf("failed to update goat: %w", err)
		}
	} else {
		// Create new goat
		create := client.Goat.Create().
			SetID(settings.TagID).
			SetName(settings.Name).
			SetBreed(settings.Breed).
			SetGender(settings.Gender).
			SetStatus(settings.Status)

		if settings.Weight > 0 {
			create = create.SetWeight(settings.Weight)
		}
		if settings.SireTag != "" {
			create = create.SetSireTag(settings.SireTag)
		}
		if settings.DamTag != "" {
			create = create.SetDamTag(settings.DamTag)
		}
		if settings.Notes != "" {
			create = create.SetNotes(settings.Notes)
		}
		if settings.BirthDate != "" {
			birthDate, err := time.Parse("2006-01-02", settings.BirthDate)
			if err != nil {
				return fmt.Errorf("invalid birth date format: %w", err)
			}
			create = create.SetBirthDate(birthDate)
		}

		goat, err = create.Save(ctx)
		if err != nil {
			return fmt.Errorf("failed to create goat: %w", err)
		}
	}

	// Commit changes
	if err := database.CommitChanges(ctx, client, fmt.Sprintf("Updated goat %s", settings.TagID)); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	// Output the result
	row := types.NewRowFromStruct(goat, true)
	return gp.AddRow(ctx, row)
}

func (c *GoatCommand) listGoats(
	ctx context.Context,
	client *ent.Client,
	settings *GoatSettings,
	gp middlewares.Processor,
) error {
	query := client.Goat.Query()

	// Apply filters
	if settings.Filter != "" {
		query = query.Where(func(s *ent.GoatQuery) {
			s.Or(
				s.Where(func(q *ent.GoatQuery) { q.Where(goat.NameContains(settings.Filter)) }),
				s.Where(func(q *ent.GoatQuery) { q.Where(goat.IDContains(settings.Filter)) }),
			)
		})
	}

	// Apply limit
	if settings.Limit > 0 {
		query = query.Limit(settings.Limit)
	}

	goats, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query goats: %w", err)
	}

	// Output results
	for _, goat := range goats {
		row := types.NewRowFromStruct(goat, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add goat row: %w", err)
		}
	}

	return nil
}

