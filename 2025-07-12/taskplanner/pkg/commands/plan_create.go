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
	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/taskplanner/taskplanner/pkg/models"
)

type PlanCreateCommand struct {
	*cmds.CommandDescription
}

type PlanCreateSettings struct {
	Name        string `glazed.parameter:"name"`
	Description string `glazed.parameter:"description"`
	Status      string `glazed.parameter:"status"`
}

var _ cmds.GlazeCommand = (*PlanCreateCommand)(nil)

func NewPlanCreateCommand() (*PlanCreateCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &PlanCreateCommand{
		CommandDescription: cmds.NewCommandDescription(
			"plan-create",
			cmds.WithShort("Create a new task planning project"),
			cmds.WithLong(`Create a new task planning project with the specified name and description.

A plan serves as a container for organizing related tasks in a hierarchical structure.
Plans can be in different states: draft, active, completed, or archived.

The plan will be created with the current agent as the creator and can be used
to organize tasks, track progress, and coordinate work across multiple agents.

Example usage:
  taskplanner plan-create --name "Website Redesign" --description "Complete redesign of company website"
  taskplanner plan-create --name "API Development" --description "Build REST API for mobile app" --status active
  taskplanner plan-create --name "Bug Fixes" --description "Critical bug fixes for Q4 release"`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"name",
					parameters.ParameterTypeString,
					parameters.WithHelp("Name of the plan"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"description",
					parameters.ParameterTypeString,
					parameters.WithHelp("Description of the plan"),
					parameters.WithDefault(""),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeString,
					parameters.WithHelp("Initial status of the plan (draft, active, completed, archived)"),
					parameters.WithDefault("draft"),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *PlanCreateCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &PlanCreateSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize plan create settings")
		return err
	}

	log.Info().
		Str("name", s.Name).
		Str("description", s.Description).
		Str("status", s.Status).
		Msg("Starting plan creation")

	// Validate status
	if err := validatePlanStatus(s.Status); err != nil {
		log.Error().Err(err).Str("status", s.Status).Msg("Invalid plan status")
		return err
	}

	agentID, err := getAgentID()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get agent ID")
		return err
	}

	log.Debug().Str("agent_id", agentID).Msg("Retrieved agent ID")

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

	// Create the plan
	now := time.Now()
	plan := &models.Plan{
		ID:          uuid.New().String(),
		Name:        s.Name,
		Description: s.Description,
		Status:      s.Status,
		CreatedBy:   agentID,
		CreatedAt:   now,
		UpdatedAt:   now,
	}

	log.Debug().
		Str("plan_id", plan.ID).
		Str("name", plan.Name).
		Str("created_by", plan.CreatedBy).
		Msg("Creating plan in database")

	err = db.CreatePlan(ctx, plan)
	if err != nil {
		log.Error().Err(err).Str("plan_id", plan.ID).Msg("Failed to create plan")
		return errors.Wrap(err, "failed to create plan")
	}

	log.Info().
		Str("plan_id", plan.ID).
		Str("name", plan.Name).
		Str("status", plan.Status).
		Str("created_by", plan.CreatedBy).
		Msg("Successfully created plan")

	// Publish plan creation to Redis (non-blocking)
	redisClient, err := getRedisClient()
	if err != nil {
		log.Warn().Err(err).Msg("Failed to get Redis client, skipping notification")
	} else {
		defer redisClient.Close()
		
		message := "📋 Created new plan: " + plan.Name
		err = publishPlanUpdate(ctx, redisClient, agentID, plan.ID, "created", message)
		if err != nil {
			log.Warn().Err(err).Str("plan_id", plan.ID).Msg("Failed to publish plan creation")
		}
	}

	// Output the result
	row := types.NewRow(
		types.MRP("id", plan.ID),
		types.MRP("name", plan.Name),
		types.MRP("description", plan.Description),
		types.MRP("status", plan.Status),
		types.MRP("created_by", plan.CreatedBy),
		types.MRP("created_at", plan.CreatedAt.Format(time.RFC3339)),
		types.MRP("updated_at", plan.UpdatedAt.Format(time.RFC3339)),
	)

	return gp.AddRow(ctx, row)
}

