package commands

import (
	"context"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/taskplanner/taskplanner/pkg/models"
)

type PlanListCommand struct {
	*cmds.CommandDescription
}

type PlanListSettings struct {
	CreatedBy string `glazed.parameter:"created-by"`
	Status    string `glazed.parameter:"status"`
	Limit     int    `glazed.parameter:"limit"`
	Offset    int    `glazed.parameter:"offset"`
}

var _ cmds.GlazeCommand = (*PlanListCommand)(nil)

func NewPlanListCommand() (*PlanListCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &PlanListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"plan-list",
			cmds.WithShort("List task planning projects"),
			cmds.WithLong(`List all task planning projects with optional filtering.

This command displays all plans that match the specified criteria.
You can filter by creator, status, and limit the number of results.

Plans are displayed with their basic information including ID, name, 
description, status, creator, and creation/update timestamps.

Example usage:
  taskplanner plan-list
  taskplanner plan-list --status active
  taskplanner plan-list --created-by agent-1 --limit 10
  taskplanner plan-list --status draft --limit 5 --offset 10`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"created-by",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by plan creator (agent ID)"),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by plan status (draft, active, completed, archived)"),
				),
				parameters.NewParameterDefinition(
					"limit",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Maximum number of plans to return"),
					parameters.WithDefault(50),
				),
				parameters.NewParameterDefinition(
					"offset",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Number of plans to skip (for pagination)"),
					parameters.WithDefault(0),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *PlanListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &PlanListSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize plan list settings")
		return err
	}

	log.Info().
		Str("created_by", s.CreatedBy).
		Str("status", s.Status).
		Int("limit", s.Limit).
		Int("offset", s.Offset).
		Msg("Starting plan listing")

	// Validate status if provided
	if s.Status != "" {
		if err := validatePlanStatus(s.Status); err != nil {
			log.Error().Err(err).Str("status", s.Status).Msg("Invalid plan status")
			return err
		}
	}

	db, err := getDatabase()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get database connection")
		return err
	}
	defer db.Close()

	// Ensure database is migrated
	if err := db.Migrate(ctx); err != nil {
		log.Error().Err(err).Msg("Failed to migrate database")
		return err
	}

	// Build filters
	filters := models.PlanFilters{
		Limit:  &s.Limit,
		Offset: &s.Offset,
	}

	if s.CreatedBy != "" {
		filters.CreatedBy = &s.CreatedBy
	}

	if s.Status != "" {
		filters.Status = &s.Status
	}

	log.Debug().
		Interface("filters", filters).
		Msg("Listing plans with filters")

	plans, err := db.ListPlans(ctx, filters)
	if err != nil {
		log.Error().Err(err).Msg("Failed to list plans")
		return errors.Wrap(err, "failed to list plans")
	}

	log.Info().
		Int("count", len(plans)).
		Msg("Successfully retrieved plans")

	// Output the results
	for _, plan := range plans {
		row := types.NewRow(
			types.MRP("id", plan.ID),
			types.MRP("name", plan.Name),
			types.MRP("description", plan.Description),
			types.MRP("status", plan.Status),
			types.MRP("created_by", plan.CreatedBy),
			types.MRP("created_at", plan.CreatedAt.Format(time.RFC3339)),
			types.MRP("updated_at", plan.UpdatedAt.Format(time.RFC3339)),
		)

		err = gp.AddRow(ctx, row)
		if err != nil {
			log.Error().Err(err).Str("plan_id", plan.ID).Msg("Failed to add plan row")
			return errors.Wrap(err, "failed to add plan row")
		}
	}

	return nil
}

