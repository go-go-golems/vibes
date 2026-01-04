package commands

import (
	"context"
	"fmt"
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

type PlanShowCommand struct {
	*cmds.CommandDescription
}

type PlanShowSettings struct {
	PlanID      string `glazed.parameter:"plan-id"`
	ShowTasks   bool   `glazed.parameter:"show-tasks"`
	ShowStats   bool   `glazed.parameter:"show-stats"`
	TasksLimit  int    `glazed.parameter:"tasks-limit"`
}

var _ cmds.GlazeCommand = (*PlanShowCommand)(nil)

func NewPlanShowCommand() (*PlanShowCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &PlanShowCommand{
		CommandDescription: cmds.NewCommandDescription(
			"plan-show",
			cmds.WithShort("Show detailed information about a plan"),
			cmds.WithLong(`Show detailed information about a specific task planning project.

This command displays comprehensive information about a plan including:
- Basic plan information (name, description, status, creator, timestamps)
- Task hierarchy and structure (if --show-tasks is enabled)
- Plan statistics and progress (if --show-stats is enabled)

The task hierarchy shows the parent-child relationships between tasks
and provides a clear view of the project structure.

Example usage:
  taskplanner plan-show --plan-id abc123
  taskplanner plan-show --plan-id abc123 --show-tasks
  taskplanner plan-show --plan-id abc123 --show-tasks --show-stats
  taskplanner plan-show --plan-id abc123 --show-tasks --tasks-limit 20`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"plan-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the plan to show"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"show-tasks",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Include task hierarchy in the output"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"show-stats",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Include plan statistics in the output"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"tasks-limit",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Maximum number of tasks to show"),
					parameters.WithDefault(100),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *PlanShowCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &PlanShowSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize plan show settings")
		return err
	}

	log.Info().
		Str("plan_id", s.PlanID).
		Bool("show_tasks", s.ShowTasks).
		Bool("show_stats", s.ShowStats).
		Int("tasks_limit", s.TasksLimit).
		Msg("Starting plan show")

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

	// Get the plan
	log.Debug().Str("plan_id", s.PlanID).Msg("Retrieving plan")
	plan, err := db.GetPlan(ctx, s.PlanID)
	if err != nil {
		log.Error().Err(err).Str("plan_id", s.PlanID).Msg("Failed to get plan")
		return errors.Wrap(err, "failed to get plan")
	}

	log.Info().
		Str("plan_id", plan.ID).
		Str("name", plan.Name).
		Str("status", plan.Status).
		Msg("Successfully retrieved plan")

	// Output plan information
	planRow := types.NewRow(
		types.MRP("type", "plan"),
		types.MRP("id", plan.ID),
		types.MRP("name", plan.Name),
		types.MRP("description", plan.Description),
		types.MRP("status", plan.Status),
		types.MRP("created_by", plan.CreatedBy),
		types.MRP("created_at", plan.CreatedAt.Format(time.RFC3339)),
		types.MRP("updated_at", plan.UpdatedAt.Format(time.RFC3339)),
	)

	err = gp.AddRow(ctx, planRow)
	if err != nil {
		log.Error().Err(err).Msg("Failed to add plan row")
		return errors.Wrap(err, "failed to add plan row")
	}

	// Show statistics if requested
	if s.ShowStats {
		log.Debug().Str("plan_id", s.PlanID).Msg("Retrieving plan statistics")
		stats, err := db.GetPlanStats(ctx, s.PlanID)
		if err != nil {
			log.Warn().Err(err).Str("plan_id", s.PlanID).Msg("Failed to get plan statistics")
		} else {
			statsRow := types.NewRow(
				types.MRP("type", "stats"),
				types.MRP("plan_id", stats.PlanID),
				types.MRP("total_tasks", stats.TaskStats.Total),
				types.MRP("planned_tasks", stats.TaskStats.Planned),
				types.MRP("active_tasks", stats.TaskStats.Active),
				types.MRP("completed_tasks", stats.TaskStats.Completed),
				types.MRP("blocked_tasks", stats.TaskStats.Blocked),
				types.MRP("cancelled_tasks", stats.TaskStats.Cancelled),
				types.MRP("progress_percent", fmt.Sprintf("%.1f", stats.TaskStats.Progress)),
				types.MRP("total_estimated_hours", stats.TotalEstimated),
				types.MRP("total_actual_hours", stats.TotalActual),
				types.MRP("agents_assigned", stats.AgentsAssigned),
				types.MRP("last_activity", stats.LastActivity.Format(time.RFC3339)),
			)

			err = gp.AddRow(ctx, statsRow)
			if err != nil {
				log.Error().Err(err).Msg("Failed to add stats row")
				return errors.Wrap(err, "failed to add stats row")
			}
		}
	}

	// Show tasks if requested
	if s.ShowTasks {
		log.Debug().Str("plan_id", s.PlanID).Msg("Retrieving task hierarchy")
		hierarchy, err := db.GetTaskHierarchy(ctx, s.PlanID)
		if err != nil {
			log.Warn().Err(err).Str("plan_id", s.PlanID).Msg("Failed to get task hierarchy")
		} else {
			taskCount := 0
			for _, h := range hierarchy {
				if taskCount >= s.TasksLimit {
					break
				}
				err = c.addTaskHierarchyRows(ctx, gp, h, &taskCount, s.TasksLimit)
				if err != nil {
					log.Error().Err(err).Msg("Failed to add task hierarchy rows")
					return errors.Wrap(err, "failed to add task hierarchy rows")
				}
			}
		}
	}

	return nil
}

func (c *PlanShowCommand) addTaskHierarchyRows(ctx context.Context, gp middlewares.Processor, hierarchy *models.TaskHierarchy, count *int, limit int) error {
	if *count >= limit {
		return nil
	}

	task := hierarchy.Task
	indent := ""
	for i := 0; i < hierarchy.Level; i++ {
		indent += "  "
	}

	// Format due date
	dueDateStr := ""
	if task.DueDate != nil {
		dueDateStr = task.DueDate.Format(time.RFC3339)
	}

	// Format hours
	estimatedHoursStr := ""
	if task.EstimatedHours != nil {
		estimatedHoursStr = fmt.Sprintf("%.1f", *task.EstimatedHours)
	}

	actualHoursStr := ""
	if task.ActualHours != nil {
		actualHoursStr = fmt.Sprintf("%.1f", *task.ActualHours)
	}

	agentIDStr := ""
	if task.AgentID != nil {
		agentIDStr = *task.AgentID
	}

	parentIDStr := ""
	if task.ParentID != nil {
		parentIDStr = *task.ParentID
	}

	blockedReasonStr := ""
	if task.BlockedReason != nil {
		blockedReasonStr = *task.BlockedReason
	}

	taskRow := types.NewRow(
		types.MRP("type", "task"),
		types.MRP("level", hierarchy.Level),
		types.MRP("indent", indent),
		types.MRP("id", task.ID),
		types.MRP("title", task.Title),
		types.MRP("description", task.Description),
		types.MRP("status", task.Status),
		types.MRP("priority", task.Priority),
		types.MRP("parent_id", parentIDStr),
		types.MRP("plan_id", task.PlanID),
		types.MRP("agent_id", agentIDStr),
		types.MRP("created_at", task.CreatedAt.Format(time.RFC3339)),
		types.MRP("updated_at", task.UpdatedAt.Format(time.RFC3339)),
		types.MRP("due_date", dueDateStr),
		types.MRP("estimated_hours", estimatedHoursStr),
		types.MRP("actual_hours", actualHoursStr),
		types.MRP("blocked_reason", blockedReasonStr),
	)

	err := gp.AddRow(ctx, taskRow)
	if err != nil {
		return err
	}

	*count++

	// Add children recursively
	for _, child := range hierarchy.Children {
		if *count >= limit {
			break
		}
		err = c.addTaskHierarchyRows(ctx, gp, child, count, limit)
		if err != nil {
			return err
		}
	}

	return nil
}

