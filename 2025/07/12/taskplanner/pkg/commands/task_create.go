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
	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
	"github.com/taskplanner/taskplanner/pkg/models"
)

type TaskCreateCommand struct {
	*cmds.CommandDescription
}

type TaskCreateSettings struct {
	Title          string  `glazed.parameter:"title"`
	Description    string  `glazed.parameter:"description"`
	PlanID         string  `glazed.parameter:"plan-id"`
	ParentID       string  `glazed.parameter:"parent-id"`
	Priority       int     `glazed.parameter:"priority"`
	Status         string  `glazed.parameter:"status"`
	AgentID        string  `glazed.parameter:"agent-id"`
	DueDate        string  `glazed.parameter:"due-date"`
	EstimatedHours float64 `glazed.parameter:"estimated-hours"`
}

var _ cmds.GlazeCommand = (*TaskCreateCommand)(nil)

func NewTaskCreateCommand() (*TaskCreateCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskCreateCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-create",
			cmds.WithShort("Create a new task within a plan"),
			cmds.WithLong(`Create a new task within a task planning project.

Tasks are the fundamental units of work within a plan. They can be organized
hierarchically with parent-child relationships and can have dependencies
on other tasks.

Tasks have various properties including priority (1-10), status, assigned agent,
due date, and estimated effort. Tasks can be created as children of other tasks
to build a hierarchical structure.

Example usage:
  taskplanner task-create --title "Setup Database" --plan-id abc123 --priority 8
  taskplanner task-create --title "Write Tests" --plan-id abc123 --parent-id def456 --priority 5
  taskplanner task-create --title "Deploy to Production" --plan-id abc123 --agent-id agent-1 --due-date "2024-01-15T10:00:00Z"
  taskplanner task-create --title "Code Review" --plan-id abc123 --estimated-hours 2.5 --status active`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"title",
					parameters.ParameterTypeString,
					parameters.WithHelp("Title of the task"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"description",
					parameters.ParameterTypeString,
					parameters.WithHelp("Description of the task"),
					parameters.WithDefault(""),
				),
				parameters.NewParameterDefinition(
					"plan-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the plan this task belongs to"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"parent-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the parent task (for hierarchical organization)"),
				),
				parameters.NewParameterDefinition(
					"priority",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Priority of the task (1-10, higher is more important)"),
					parameters.WithDefault(5),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeString,
					parameters.WithHelp("Initial status of the task (planned, active, completed, blocked, cancelled)"),
					parameters.WithDefault("planned"),
				),
				parameters.NewParameterDefinition(
					"agent-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the agent assigned to this task"),
				),
				parameters.NewParameterDefinition(
					"due-date",
					parameters.ParameterTypeString,
					parameters.WithHelp("Due date for the task (RFC3339 format: 2024-01-15T10:00:00Z)"),
				),
				parameters.NewParameterDefinition(
					"estimated-hours",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Estimated hours to complete the task"),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskCreateCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &TaskCreateSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task create settings")
		return err
	}

	log.Info().
		Str("title", s.Title).
		Str("plan_id", s.PlanID).
		Str("parent_id", s.ParentID).
		Int("priority", s.Priority).
		Str("status", s.Status).
		Msg("Starting task creation")

	// Validate inputs
	if err := validateTaskStatus(s.Status); err != nil {
		log.Error().Err(err).Str("status", s.Status).Msg("Invalid task status")
		return err
	}

	if err := validatePriority(s.Priority); err != nil {
		log.Error().Err(err).Int("priority", s.Priority).Msg("Invalid priority")
		return err
	}

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

	// Verify the plan exists
	log.Debug().Str("plan_id", s.PlanID).Msg("Verifying plan exists")
	_, err = db.GetPlan(ctx, s.PlanID)
	if err != nil {
		log.Error().Err(err).Str("plan_id", s.PlanID).Msg("Plan not found")
		return errors.Wrap(err, "plan not found")
	}

	// Verify parent task exists if specified
	if s.ParentID != "" {
		log.Debug().Str("parent_id", s.ParentID).Msg("Verifying parent task exists")
		parentTask, err := db.GetTask(ctx, s.ParentID)
		if err != nil {
			log.Error().Err(err).Str("parent_id", s.ParentID).Msg("Parent task not found")
			return errors.Wrap(err, "parent task not found")
		}

		// Verify parent task belongs to the same plan
		if parentTask.PlanID != s.PlanID {
			log.Error().
				Str("parent_id", s.ParentID).
				Str("parent_plan_id", parentTask.PlanID).
				Str("task_plan_id", s.PlanID).
				Msg("Parent task belongs to different plan")
			return errors.New("parent task must belong to the same plan")
		}
	}

	// Parse due date if provided
	var dueDate *time.Time
	if s.DueDate != "" {
		parsed, err := time.Parse(time.RFC3339, s.DueDate)
		if err != nil {
			log.Error().Err(err).Str("due_date", s.DueDate).Msg("Invalid due date format")
			return errors.Wrap(err, "invalid due date format, use RFC3339 (2024-01-15T10:00:00Z)")
		}
		dueDate = &parsed
	}

	// Handle estimated hours
	var estimatedHours *float64
	if s.EstimatedHours > 0 {
		estimatedHours = &s.EstimatedHours
	}

	// Handle parent ID
	var parentID *string
	if s.ParentID != "" {
		parentID = &s.ParentID
	}

	// Handle assigned agent ID
	var assignedAgentID *string
	if s.AgentID != "" {
		assignedAgentID = &s.AgentID
	}

	// Create the task
	now := time.Now()
	task := &models.Task{
		ID:             uuid.New().String(),
		Title:          s.Title,
		Description:    s.Description,
		Status:         s.Status,
		Priority:       s.Priority,
		ParentID:       parentID,
		PlanID:         s.PlanID,
		AgentID:        assignedAgentID,
		CreatedAt:      now,
		UpdatedAt:      now,
		DueDate:        dueDate,
		EstimatedHours: estimatedHours,
	}

	log.Debug().
		Str("task_id", task.ID).
		Str("title", task.Title).
		Str("plan_id", task.PlanID).
		Msg("Creating task in database")

	err = db.CreateTask(ctx, task)
	if err != nil {
		log.Error().Err(err).Str("task_id", task.ID).Msg("Failed to create task")
		return errors.Wrap(err, "failed to create task")
	}

	// Add task history entry
	history := &models.TaskHistory{
		ID:        uuid.New().String(),
		TaskID:    task.ID,
		AgentID:   agentID,
		Action:    models.ActionCreated,
		NewValue:  &task.Status,
		Message:   &[]string{"Task created"}[0],
		CreatedAt: now,
	}

	err = db.AddTaskHistory(ctx, history)
	if err != nil {
		log.Warn().Err(err).Str("task_id", task.ID).Msg("Failed to add task history")
	}

	log.Info().
		Str("task_id", task.ID).
		Str("title", task.Title).
		Str("status", task.Status).
		Int("priority", task.Priority).
		Str("plan_id", task.PlanID).
		Msg("Successfully created task")

	// Publish task creation to Redis (non-blocking)
	redisClient, err := getRedisClient()
	if err != nil {
		log.Warn().Err(err).Msg("Failed to get Redis client, skipping notification")
	} else {
		defer redisClient.Close()
		
		message := "📝 Created new task: " + task.Title
		err = publishTaskUpdate(ctx, redisClient, agentID, task.ID, "created", message)
		if err != nil {
			log.Warn().Err(err).Str("task_id", task.ID).Msg("Failed to publish task creation")
		}
	}

	// Output the result
	dueDateStr := ""
	if task.DueDate != nil {
		dueDateStr = task.DueDate.Format(time.RFC3339)
	}

	estimatedHoursStr := ""
	if task.EstimatedHours != nil {
		estimatedHoursStr = fmt.Sprintf("%.1f", *task.EstimatedHours)
	}

	parentIDStr := ""
	if task.ParentID != nil {
		parentIDStr = *task.ParentID
	}

	agentIDStr := ""
	if task.AgentID != nil {
		agentIDStr = *task.AgentID
	}

	row := types.NewRow(
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
	)

	return gp.AddRow(ctx, row)
}

