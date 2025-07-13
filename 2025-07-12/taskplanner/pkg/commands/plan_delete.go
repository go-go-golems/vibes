package commands

import (
	"context"

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

type PlanDeleteCommand struct {
	*cmds.CommandDescription
}

type PlanDeleteSettings struct {
	PlanID string `glazed.parameter:"plan-id"`
	Force  bool   `glazed.parameter:"force"`
}

var _ cmds.GlazeCommand = (*PlanDeleteCommand)(nil)

func NewPlanDeleteCommand() (*PlanDeleteCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &PlanDeleteCommand{
		CommandDescription: cmds.NewCommandDescription(
			"plan-delete",
			cmds.WithShort("Delete a task planning project"),
			cmds.WithLong(`Delete a task planning project and all its associated tasks.

WARNING: This operation is irreversible and will delete the plan and all
tasks within it. Use with caution.

By default, the command will fail if the plan contains tasks. Use --force
to delete the plan and all its tasks regardless of their status.

Example usage:
  taskplanner plan-delete --plan-id abc123
  taskplanner plan-delete --plan-id abc123 --force`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"plan-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the plan to delete"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"force",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Force deletion even if plan contains tasks"),
					parameters.WithDefault(false),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *PlanDeleteCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &PlanDeleteSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize plan delete settings")
		return err
	}

	log.Info().
		Str("plan_id", s.PlanID).
		Bool("force", s.Force).
		Msg("Starting plan deletion")

	agentID, err := getAgentID()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get agent ID")
		return err
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

	// Get the plan first to verify it exists and get its name
	log.Debug().Str("plan_id", s.PlanID).Msg("Retrieving plan for deletion")
	plan, err := db.GetPlan(ctx, s.PlanID)
	if err != nil {
		log.Error().Err(err).Str("plan_id", s.PlanID).Msg("Failed to get plan")
		return errors.Wrap(err, "failed to get plan")
	}

	// Check if plan has tasks (unless force is specified)
	if !s.Force {
		log.Debug().Str("plan_id", s.PlanID).Msg("Checking for existing tasks")
		tasks, err := db.ListTasks(ctx, models.TaskFilters{PlanID: &s.PlanID, Limit: &[]int{1}[0]})
		if err != nil {
			log.Error().Err(err).Str("plan_id", s.PlanID).Msg("Failed to check for tasks")
			return errors.Wrap(err, "failed to check for tasks")
		}

		if len(tasks) > 0 {
			log.Warn().
				Str("plan_id", s.PlanID).
				Int("task_count", len(tasks)).
				Msg("Plan contains tasks, use --force to delete")
			return errors.New("plan contains tasks, use --force to delete the plan and all its tasks")
		}
	}

	// Delete the plan (this will cascade delete all tasks due to foreign key constraints)
	log.Debug().Str("plan_id", s.PlanID).Msg("Deleting plan from database")
	err = db.DeletePlan(ctx, s.PlanID)
	if err != nil {
		log.Error().Err(err).Str("plan_id", s.PlanID).Msg("Failed to delete plan")
		return errors.Wrap(err, "failed to delete plan")
	}

	log.Info().
		Str("plan_id", s.PlanID).
		Str("name", plan.Name).
		Bool("forced", s.Force).
		Msg("Successfully deleted plan")

	// Publish plan deletion to Redis (non-blocking)
	redisClient, err := getRedisClient()
	if err != nil {
		log.Warn().Err(err).Msg("Failed to get Redis client, skipping notification")
	} else {
		defer redisClient.Close()
		
		message := "🗑️ Deleted plan: " + plan.Name
		err = publishPlanUpdate(ctx, redisClient, agentID, plan.ID, "deleted", message)
		if err != nil {
			log.Warn().Err(err).Str("plan_id", plan.ID).Msg("Failed to publish plan deletion")
		}
	}

	// Output the result
	row := types.NewRow(
		types.MRP("id", plan.ID),
		types.MRP("name", plan.Name),
		types.MRP("status", "deleted"),
		types.MRP("deleted_by", agentID),
		types.MRP("forced", s.Force),
	)

	return gp.AddRow(ctx, row)
}

