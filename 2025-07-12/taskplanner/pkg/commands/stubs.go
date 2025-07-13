package commands

import (
	"context"
	"fmt"
	"io"
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

// Placeholder implementations for commands referenced in main.go

// Task List Command
type TaskListCommand struct {
	*cmds.CommandDescription
}

type TaskListSettings struct {
	PlanID   string `glazed.parameter:"plan-id"`
	ParentID string `glazed.parameter:"parent-id"`
	AgentID  string `glazed.parameter:"agent-id"`
	Status   string `glazed.parameter:"status"`
	Priority int    `glazed.parameter:"priority"`
	Limit    int    `glazed.parameter:"limit"`
	Offset   int    `glazed.parameter:"offset"`
}

func NewTaskListCommand() (*TaskListCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-list",
			cmds.WithShort("List tasks with filtering options"),
			cmds.WithLong(`List tasks with various filtering options.

You can filter tasks by plan, parent task, assigned agent, status, or priority.
Results are ordered by priority (descending) and creation time (ascending).

Example usage:
  taskplanner task-list --plan-id abc123
  taskplanner task-list --plan-id abc123 --status active
  taskplanner task-list --agent-id agent-1 --priority 8
  taskplanner task-list --parent-id def456 --limit 10`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"plan-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by plan ID"),
				),
				parameters.NewParameterDefinition(
					"parent-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by parent task ID"),
				),
				parameters.NewParameterDefinition(
					"agent-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by assigned agent ID"),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeString,
					parameters.WithHelp("Filter by task status (planned, active, completed, blocked, cancelled)"),
				),
				parameters.NewParameterDefinition(
					"priority",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Filter by priority level (1-10)"),
				),
				parameters.NewParameterDefinition(
					"limit",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Maximum number of tasks to return"),
					parameters.WithDefault(50),
				),
				parameters.NewParameterDefinition(
					"offset",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Number of tasks to skip"),
					parameters.WithDefault(0),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskListCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &TaskListSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task list settings")
		return err
	}

	log.Info().
		Str("plan_id", s.PlanID).
		Str("parent_id", s.ParentID).
		Str("agent_id", s.AgentID).
		Str("status", s.Status).
		Int("priority", s.Priority).
		Int("limit", s.Limit).
		Int("offset", s.Offset).
		Msg("Starting task listing")

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
	filters := models.TaskFilters{
		Limit:  &s.Limit,
		Offset: &s.Offset,
	}

	if s.PlanID != "" {
		filters.PlanID = &s.PlanID
	}
	if s.ParentID != "" {
		filters.ParentID = &s.ParentID
	}
	if s.AgentID != "" {
		filters.AgentID = &s.AgentID
	}
	if s.Status != "" {
		filters.Status = &s.Status
	}
	if s.Priority > 0 {
		filters.Priority = &s.Priority
	}

	// Get tasks
	tasks, err := db.ListTasks(ctx, filters)
	if err != nil {
		log.Error().Err(err).Msg("Failed to list tasks")
		return errors.Wrap(err, "failed to list tasks")
	}

	log.Info().Int("count", len(tasks)).Msg("Successfully retrieved tasks")

	// Output results
	for _, task := range tasks {
		dueDateStr := ""
		if task.DueDate != nil {
			dueDateStr = task.DueDate.Format(time.RFC3339)
		}

		estimatedHoursStr := ""
		if task.EstimatedHours != nil {
			estimatedHoursStr = fmt.Sprintf("%.1f", *task.EstimatedHours)
		}

		actualHoursStr := ""
		if task.ActualHours != nil {
			actualHoursStr = fmt.Sprintf("%.1f", *task.ActualHours)
		}

		parentIDStr := ""
		if task.ParentID != nil {
			parentIDStr = *task.ParentID
		}

		agentIDStr := ""
		if task.AgentID != nil {
			agentIDStr = *task.AgentID
		}

		blockedReasonStr := ""
		if task.BlockedReason != nil {
			blockedReasonStr = *task.BlockedReason
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
			types.MRP("actual_hours", actualHoursStr),
			types.MRP("blocked_reason", blockedReasonStr),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

// Task Show Command
type TaskShowCommand struct {
	*cmds.CommandDescription
}

type TaskShowSettings struct {
	TaskID string `glazed.parameter:"task-id"`
}

func NewTaskShowCommand() (*TaskShowCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskShowCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-show",
			cmds.WithShort("Show detailed information about a task"),
			cmds.WithLong(`Show detailed information about a specific task including all metadata,
dependencies, and history.

Example usage:
  taskplanner task-show --task-id abc123-def456-789`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"task-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Task ID to show details for"),
					parameters.WithRequired(true),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskShowCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &TaskShowSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task show settings")
		return err
	}

	log.Info().Str("task_id", s.TaskID).Msg("Starting task show")

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

	// Get task
	task, err := db.GetTask(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to get task")
		return errors.Wrap(err, "failed to get task")
	}

	log.Info().Str("task_id", s.TaskID).Str("title", task.Title).Msg("Successfully retrieved task")

	// Format optional fields
	dueDateStr := ""
	if task.DueDate != nil {
		dueDateStr = task.DueDate.Format(time.RFC3339)
	}

	estimatedHoursStr := ""
	if task.EstimatedHours != nil {
		estimatedHoursStr = fmt.Sprintf("%.1f", *task.EstimatedHours)
	}

	actualHoursStr := ""
	if task.ActualHours != nil {
		actualHoursStr = fmt.Sprintf("%.1f", *task.ActualHours)
	}

	parentIDStr := ""
	if task.ParentID != nil {
		parentIDStr = *task.ParentID
	}

	agentIDStr := ""
	if task.AgentID != nil {
		agentIDStr = *task.AgentID
	}

	blockedReasonStr := ""
	if task.BlockedReason != nil {
		blockedReasonStr = *task.BlockedReason
	}

	// Output task details
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
		types.MRP("actual_hours", actualHoursStr),
		types.MRP("blocked_reason", blockedReasonStr),
	)

	return gp.AddRow(ctx, row)
}

// Task Update Command
type TaskUpdateCommand struct {
	*cmds.CommandDescription
}

type TaskUpdateSettings struct {
	TaskID          string   `glazed.parameter:"task-id"`
	Title           string   `glazed.parameter:"title"`
	Description     string   `glazed.parameter:"description"`
	Status          string   `glazed.parameter:"status"`
	Priority        int      `glazed.parameter:"priority"`
	AgentID         string   `glazed.parameter:"agent-id"`
	DueDate         string   `glazed.parameter:"due-date"`
	EstimatedHours  *float64 `glazed.parameter:"estimated-hours"`
	ActualHours     *float64 `glazed.parameter:"actual-hours"`
	BlockedReason   string   `glazed.parameter:"blocked-reason"`
}

func NewTaskUpdateCommand() (*TaskUpdateCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskUpdateCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-update",
			cmds.WithShort("Update an existing task"),
			cmds.WithLong(`Update an existing task with new information.

You can update any field of the task including title, description, status, priority,
assigned agent, due date, estimated hours, actual hours, and blocked reason.

Example usage:
  taskplanner task-update --task-id abc123 --status active
  taskplanner task-update --task-id abc123 --priority 8 --agent-id agent-1
  taskplanner task-update --task-id abc123 --actual-hours 5.5 --status completed`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"task-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Task ID to update"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"title",
					parameters.ParameterTypeString,
					parameters.WithHelp("New task title"),
				),
				parameters.NewParameterDefinition(
					"description",
					parameters.ParameterTypeString,
					parameters.WithHelp("New task description"),
				),
				parameters.NewParameterDefinition(
					"status",
					parameters.ParameterTypeString,
					parameters.WithHelp("New task status (planned, active, completed, blocked, cancelled)"),
				),
				parameters.NewParameterDefinition(
					"priority",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("New priority level (1-10)"),
				),
				parameters.NewParameterDefinition(
					"agent-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Assign task to agent"),
				),
				parameters.NewParameterDefinition(
					"due-date",
					parameters.ParameterTypeString,
					parameters.WithHelp("Due date in RFC3339 format (e.g., 2024-01-15T10:00:00Z)"),
				),
				parameters.NewParameterDefinition(
					"estimated-hours",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Estimated hours to complete the task"),
				),
				parameters.NewParameterDefinition(
					"actual-hours",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Actual hours spent on the task"),
				),
				parameters.NewParameterDefinition(
					"blocked-reason",
					parameters.ParameterTypeString,
					parameters.WithHelp("Reason why task is blocked (only relevant if status is blocked)"),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskUpdateCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &TaskUpdateSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task update settings")
		return err
	}

	log.Info().Str("task_id", s.TaskID).Msg("Starting task update")

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

	// Get existing task to verify it exists
	_, err = db.GetTask(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to get existing task")
		return errors.Wrap(err, "failed to get existing task")
	}

	// Build update object with only changed fields
	updates := models.TaskUpdate{
		ID: s.TaskID,
	}

	if s.Title != "" {
		updates.Title = &s.Title
	}
	if s.Description != "" {
		updates.Description = &s.Description
	}
	if s.Status != "" {
		updates.Status = &s.Status
	}
	if s.Priority > 0 {
		updates.Priority = &s.Priority
	}
	if s.AgentID != "" {
		updates.AgentID = &s.AgentID
	}
	if s.DueDate != "" {
		dueDate, err := time.Parse(time.RFC3339, s.DueDate)
		if err != nil {
			log.Error().Err(err).Str("due_date", s.DueDate).Msg("Failed to parse due date")
			return errors.Wrap(err, "invalid due date format, use RFC3339 (e.g., 2024-01-15T10:00:00Z)")
		}
		updates.DueDate = &dueDate
	}
	if s.EstimatedHours != nil {
		updates.EstimatedHours = s.EstimatedHours
	}
	if s.ActualHours != nil {
		updates.ActualHours = s.ActualHours
	}
	if s.BlockedReason != "" {
		updates.BlockedReason = &s.BlockedReason
	}

	// Update task
	updatedTask, err := db.UpdateTaskFields(ctx, updates)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to update task")
		return errors.Wrap(err, "failed to update task")
	}

	log.Info().Str("task_id", s.TaskID).Str("title", updatedTask.Title).Msg("Successfully updated task")

	// Notify via Redis if available
	if redisClient, err := getRedisClient(); err == nil {
		agentID, _ := getAgentID()
		notification := models.CoordinationEvent{
			ID:        fmt.Sprintf("%s-%d", agentID, time.Now().UnixNano()),
			AgentID:   agentID,
			Type:      "task_update",
			Data:      fmt.Sprintf(`{"task_id":"%s","plan_id":"%s","agent_id":"%s","action":"updated","message":"Task updated by agent %s","timestamp":"%s"}`, updatedTask.ID, updatedTask.PlanID, agentID, agentID, time.Now().Format(time.RFC3339)),
			Timestamp: time.Now(),
		}
		if err := redisClient.PublishCoordinationEvent(ctx, notification); err != nil {
			log.Warn().Err(err).Msg("Failed to publish task update notification")
		}
	}

	// Format optional fields for output
	dueDateStr := ""
	if updatedTask.DueDate != nil {
		dueDateStr = updatedTask.DueDate.Format(time.RFC3339)
	}

	estimatedHoursStr := ""
	if updatedTask.EstimatedHours != nil {
		estimatedHoursStr = fmt.Sprintf("%.1f", *updatedTask.EstimatedHours)
	}

	actualHoursStr := ""
	if updatedTask.ActualHours != nil {
		actualHoursStr = fmt.Sprintf("%.1f", *updatedTask.ActualHours)
	}

	parentIDStr := ""
	if updatedTask.ParentID != nil {
		parentIDStr = *updatedTask.ParentID
	}

	agentIDStr := ""
	if updatedTask.AgentID != nil {
		agentIDStr = *updatedTask.AgentID
	}

	blockedReasonStr := ""
	if updatedTask.BlockedReason != nil {
		blockedReasonStr = *updatedTask.BlockedReason
	}

	// Output updated task
	row := types.NewRow(
		types.MRP("id", updatedTask.ID),
		types.MRP("title", updatedTask.Title),
		types.MRP("description", updatedTask.Description),
		types.MRP("status", updatedTask.Status),
		types.MRP("priority", updatedTask.Priority),
		types.MRP("parent_id", parentIDStr),
		types.MRP("plan_id", updatedTask.PlanID),
		types.MRP("agent_id", agentIDStr),
		types.MRP("created_at", updatedTask.CreatedAt.Format(time.RFC3339)),
		types.MRP("updated_at", updatedTask.UpdatedAt.Format(time.RFC3339)),
		types.MRP("due_date", dueDateStr),
		types.MRP("estimated_hours", estimatedHoursStr),
		types.MRP("actual_hours", actualHoursStr),
		types.MRP("blocked_reason", blockedReasonStr),
	)

	return gp.AddRow(ctx, row)
}

// Task Delete Command
type TaskDeleteCommand struct {
	*cmds.CommandDescription
}

type TaskDeleteSettings struct {
	TaskID string `glazed.parameter:"task-id"`
	Force  bool   `glazed.parameter:"force"`
}

func NewTaskDeleteCommand() (*TaskDeleteCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskDeleteCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-delete",
			cmds.WithShort("Delete a task"),
			cmds.WithLong(`Delete a task from the system.

WARNING: This action cannot be undone. The task and all its subtasks will be permanently deleted.
Use --force to skip confirmation prompts.

Example usage:
  taskplanner task-delete --task-id abc123-def456-789
  taskplanner task-delete --task-id abc123-def456-789 --force`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"task-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Task ID to delete"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"force",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Force deletion without confirmation"),
					parameters.WithDefault(false),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskDeleteCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &TaskDeleteSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task delete settings")
		return err
	}

	log.Info().Str("task_id", s.TaskID).Bool("force", s.Force).Msg("Starting task deletion")

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

	// Get task to verify it exists and get details for logging
	task, err := db.GetTask(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to get task for deletion")
		return errors.Wrap(err, "failed to get task for deletion")
	}

	// Check for subtasks
	filters := models.TaskFilters{
		ParentID: &s.TaskID,
	}
	subtasks, err := db.ListTasks(ctx, filters)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to check for subtasks")
		return errors.Wrap(err, "failed to check for subtasks")
	}

	if len(subtasks) > 0 && !s.Force {
		return errors.New(fmt.Sprintf("task has %d subtasks, use --force to delete anyway", len(subtasks)))
	}

	// Delete task
	err = db.DeleteTaskCascade(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to delete task")
		return errors.Wrap(err, "failed to delete task")
	}

	log.Info().Str("task_id", s.TaskID).Str("title", task.Title).Int("subtasks_deleted", len(subtasks)).Msg("Successfully deleted task")

	// Notify via Redis if available
	if redisClient, err := getRedisClient(); err == nil {
		agentID, _ := getAgentID()
		notification := models.CoordinationEvent{
			ID:        fmt.Sprintf("%s-%d", agentID, time.Now().UnixNano()),
			AgentID:   agentID,
			Type:      "task_update",
			Data:      fmt.Sprintf(`{"task_id":"%s","plan_id":"%s","agent_id":"%s","action":"deleted","message":"Task deleted by agent %s","timestamp":"%s"}`, task.ID, task.PlanID, agentID, agentID, time.Now().Format(time.RFC3339)),
			Timestamp: time.Now(),
		}
		if err := redisClient.PublishCoordinationEvent(ctx, notification); err != nil {
			log.Warn().Err(err).Msg("Failed to publish task deletion notification")
		}
	}

	// Output confirmation
	agentID, _ := getAgentID()
	row := types.NewRow(
		types.MRP("task_id", task.ID),
		types.MRP("title", task.Title),
		types.MRP("status", "deleted"),
		types.MRP("subtasks_deleted", len(subtasks)),
		types.MRP("deleted_by", agentID),
		types.MRP("deleted_at", time.Now().Format(time.RFC3339)),
	)

	return gp.AddRow(ctx, row)
}

// Task Release Command
type TaskReleaseCommand struct {
	*cmds.CommandDescription
}

type TaskReleaseSettings struct {
	TaskID string `glazed.parameter:"task-id"`
}

func NewTaskReleaseCommand() (*TaskReleaseCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskReleaseCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-release",
			cmds.WithShort("Release a claimed task"),
			cmds.WithLong(`Release a task claim, making it available for other agents to claim.

This command removes the Redis-based claim on a task, allowing other agents
to claim and work on it. Only the agent that originally claimed the task
can release it.

Example usage:
  taskplanner task-release --task-id abc123-def456-789`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"task-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Task ID to release claim for"),
					parameters.WithRequired(true),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskReleaseCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &TaskReleaseSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task release settings")
		return err
	}

	log.Info().Str("task_id", s.TaskID).Msg("Starting task release")

	// Get Redis client
	redisClient, err := getRedisClient()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get Redis client")
		return errors.Wrap(err, "Redis is required for task coordination")
	}

	// Get database for task info
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

	// Get task to verify it exists
	task, err := db.GetTask(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to get task")
		return errors.Wrap(err, "failed to get task")
	}

	// Release the claim
	agentID, err := getAgentID()
	if err != nil {
		return errors.Wrap(err, "failed to get agent ID")
	}
	
	released, err := redisClient.ReleaseTaskClaim(ctx, s.TaskID, agentID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to release task claim")
		return errors.Wrap(err, "failed to release task claim")
	}

	if !released {
		log.Warn().Str("task_id", s.TaskID).Str("agent_id", agentID).Msg("Task was not claimed by this agent or claim expired")
		return errors.New("task was not claimed by this agent or claim has already expired")
	}

	log.Info().Str("task_id", s.TaskID).Str("agent_id", agentID).Msg("Successfully released task claim")

	// Notify via Redis
	notification := models.CoordinationEvent{
		ID:        fmt.Sprintf("%s-%d", agentID, time.Now().UnixNano()),
		AgentID:   agentID,
		Type:      "task_update",
		Data:      fmt.Sprintf(`{"task_id":"%s","plan_id":"%s","agent_id":"%s","action":"released","message":"Task claim released by agent %s","timestamp":"%s"}`, task.ID, task.PlanID, agentID, agentID, time.Now().Format(time.RFC3339)),
		Timestamp: time.Now(),
	}
	if err := redisClient.PublishCoordinationEvent(ctx, notification); err != nil {
		log.Warn().Err(err).Msg("Failed to publish task release notification")
	}

	// Output confirmation
	row := types.NewRow(
		types.MRP("task_id", task.ID),
		types.MRP("task_title", task.Title),
		types.MRP("status", "released"),
		types.MRP("released_by", agentID),
		types.MRP("released_at", time.Now().Format(time.RFC3339)),
		types.MRP("message", "Task claim released successfully"),
	)

	return gp.AddRow(ctx, row)
}

// Task Assign Command
type TaskAssignCommand struct {
	*cmds.CommandDescription
}

type TaskAssignSettings struct {
	TaskID  string `glazed.parameter:"task-id"`
	AgentID string `glazed.parameter:"agent-id"`
}

func NewTaskAssignCommand() (*TaskAssignCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskAssignCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-assign",
			cmds.WithShort("Assign a task to an agent"),
			cmds.WithLong(`Assign a task to a specific agent.

This command updates the task's agent_id field in the database, indicating
which agent is responsible for the task. This is different from claiming,
which is a temporary Redis-based lock for coordination.

Example usage:
  taskplanner task-assign --task-id abc123-def456-789 --agent-id agent-1`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"task-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Task ID to assign"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"agent-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("Agent ID to assign the task to"),
					parameters.WithRequired(true),
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskAssignCommand) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
	s := &TaskAssignSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task assign settings")
		return err
	}

	log.Info().Str("task_id", s.TaskID).Str("agent_id", s.AgentID).Msg("Starting task assignment")

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

	// Get existing task to verify it exists
	_, err = db.GetTask(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to get existing task")
		return errors.Wrap(err, "failed to get existing task")
	}

	// Update task with new agent assignment
	updates := models.TaskUpdate{
		ID:      s.TaskID,
		AgentID: &s.AgentID,
	}

	updatedTask, err := db.UpdateTaskFields(ctx, updates)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Str("agent_id", s.AgentID).Msg("Failed to assign task")
		return errors.Wrap(err, "failed to assign task")
	}

	log.Info().Str("task_id", s.TaskID).Str("agent_id", s.AgentID).Str("title", updatedTask.Title).Msg("Successfully assigned task")

	// Notify via Redis if available
	if redisClient, err := getRedisClient(); err == nil {
		agentID, _ := getAgentID()
		notification := models.CoordinationEvent{
			ID:        fmt.Sprintf("%s-%d", agentID, time.Now().UnixNano()),
			AgentID:   agentID,
			Type:      "task_update",
			Data:      fmt.Sprintf(`{"task_id":"%s","plan_id":"%s","agent_id":"%s","action":"assigned","message":"Task assigned to agent %s by %s","timestamp":"%s"}`, updatedTask.ID, updatedTask.PlanID, s.AgentID, s.AgentID, agentID, time.Now().Format(time.RFC3339)),
			Timestamp: time.Now(),
		}
		if err := redisClient.PublishCoordinationEvent(ctx, notification); err != nil {
			log.Warn().Err(err).Msg("Failed to publish task assignment notification")
		}
	}

	// Format optional fields for output
	dueDateStr := ""
	if updatedTask.DueDate != nil {
		dueDateStr = updatedTask.DueDate.Format(time.RFC3339)
	}

	estimatedHoursStr := ""
	if updatedTask.EstimatedHours != nil {
		estimatedHoursStr = fmt.Sprintf("%.1f", *updatedTask.EstimatedHours)
	}

	actualHoursStr := ""
	if updatedTask.ActualHours != nil {
		actualHoursStr = fmt.Sprintf("%.1f", *updatedTask.ActualHours)
	}

	parentIDStr := ""
	if updatedTask.ParentID != nil {
		parentIDStr = *updatedTask.ParentID
	}

	agentIDStr := ""
	if updatedTask.AgentID != nil {
		agentIDStr = *updatedTask.AgentID
	}

	blockedReasonStr := ""
	if updatedTask.BlockedReason != nil {
		blockedReasonStr = *updatedTask.BlockedReason
	}

	// Output assigned task
	agentID, _ := getAgentID()
	row := types.NewRow(
		types.MRP("id", updatedTask.ID),
		types.MRP("title", updatedTask.Title),
		types.MRP("description", updatedTask.Description),
		types.MRP("status", updatedTask.Status),
		types.MRP("priority", updatedTask.Priority),
		types.MRP("parent_id", parentIDStr),
		types.MRP("plan_id", updatedTask.PlanID),
		types.MRP("agent_id", agentIDStr),
		types.MRP("created_at", updatedTask.CreatedAt.Format(time.RFC3339)),
		types.MRP("updated_at", updatedTask.UpdatedAt.Format(time.RFC3339)),
		types.MRP("due_date", dueDateStr),
		types.MRP("estimated_hours", estimatedHoursStr),
		types.MRP("actual_hours", actualHoursStr),
		types.MRP("blocked_reason", blockedReasonStr),
		types.MRP("assigned_by", agentID),
	)

	return gp.AddRow(ctx, row)
}

// Monitor Command (WriterCommand)
type MonitorCommand struct {
	*cmds.CommandDescription
}

func NewMonitorCommand() (*MonitorCommand, error) {
	return &MonitorCommand{
		CommandDescription: cmds.NewCommandDescription(
			"monitor",
			cmds.WithShort("Monitor real-time coordination events"),
			cmds.WithLong(`Monitor real-time coordination events from the Redis stream.

This command connects to the Redis coordination stream and displays
real-time events as they happen, including task creation, updates,
claims, releases, and assignments.

Press Ctrl+C to stop monitoring.

Example usage:
  taskplanner monitor`),
		),
	}, nil
}

func (c *MonitorCommand) RunIntoWriter(ctx context.Context, parsedLayers *layers.ParsedLayers, w io.Writer) error {
	log.Info().Msg("Starting real-time monitoring")

	// Get Redis client
	redisClient, err := getRedisClient()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get Redis client")
		fmt.Fprintf(w, "Error: Redis is required for monitoring: %v\n", err)
		return nil
	}

	fmt.Fprintf(w, "🔍 Monitoring TaskPlanner coordination events...\n")
	fmt.Fprintf(w, "Press Ctrl+C to stop monitoring\n\n")

	// Subscribe to coordination events
	eventChan, err := redisClient.SubscribeToCoordinationEvents(ctx)
	if err != nil {
		log.Error().Err(err).Msg("Failed to subscribe to coordination events")
		fmt.Fprintf(w, "Error: Failed to subscribe to coordination events: %v\n", err)
		return nil
	}

	// Monitor events
	for {
		select {
		case <-ctx.Done():
			fmt.Fprintf(w, "\n📊 Monitoring stopped\n")
			return nil
		case event := <-eventChan:
			timestamp := event.Timestamp.Format("15:04:05")
			fmt.Fprintf(w, "[%s] %s (%s): %s\n", timestamp, event.Type, event.AgentID, event.Data)
		}
	}
}

// Status Command (WriterCommand)
type StatusCommand struct {
	*cmds.CommandDescription
}

func NewStatusCommand() (*StatusCommand, error) {
	return &StatusCommand{
		CommandDescription: cmds.NewCommandDescription(
			"status",
			cmds.WithShort("Show system status and statistics"),
			cmds.WithLong(`Show system status and statistics including database connectivity,
Redis connectivity, task counts, and recent activity.

This command provides an overview of the TaskPlanner system health
and current state.

Example usage:
  taskplanner status`),
		),
	}, nil
}

func (c *StatusCommand) RunIntoWriter(ctx context.Context, parsedLayers *layers.ParsedLayers, w io.Writer) error {
	log.Info().Msg("Checking system status")

	fmt.Fprintf(w, "🚀 TaskPlanner System Status\n")
	fmt.Fprintf(w, "============================\n\n")

	// Check database connectivity
	fmt.Fprintf(w, "📊 Database Status:\n")
	db, err := getDatabase()
	if err != nil {
		fmt.Fprintf(w, "  ❌ Database: FAILED (%v)\n", err)
	} else {
		defer db.Close()
		
		// Test database connection
		if err := db.Migrate(ctx); err != nil {
			fmt.Fprintf(w, "  ❌ Database: MIGRATION FAILED (%v)\n", err)
		} else {
			fmt.Fprintf(w, "  ✅ Database: CONNECTED\n")
			
			// Get statistics
			plans, err := db.ListPlans(ctx, models.PlanFilters{})
			if err != nil {
				fmt.Fprintf(w, "  ⚠️  Plan count: ERROR (%v)\n", err)
			} else {
				fmt.Fprintf(w, "  📋 Total plans: %d\n", len(plans))
			}
			
			tasks, err := db.ListTasks(ctx, models.TaskFilters{})
			if err != nil {
				fmt.Fprintf(w, "  ⚠️  Task count: ERROR (%v)\n", err)
			} else {
				fmt.Fprintf(w, "  📝 Total tasks: %d\n", len(tasks))
				
				// Count by status
				statusCounts := make(map[string]int)
				for _, task := range tasks {
					statusCounts[task.Status]++
				}
				
				for status, count := range statusCounts {
					fmt.Fprintf(w, "    - %s: %d\n", status, count)
				}
			}
		}
	}

	fmt.Fprintf(w, "\n🔗 Redis Status:\n")
	redisClient, err := getRedisClient()
	if err != nil {
		fmt.Fprintf(w, "  ❌ Redis: FAILED (%v)\n", err)
		fmt.Fprintf(w, "  ⚠️  Coordination features disabled\n")
	} else {
		// Test Redis connection
		if err := redisClient.Ping(ctx); err != nil {
			fmt.Fprintf(w, "  ❌ Redis: PING FAILED (%v)\n", err)
		} else {
			fmt.Fprintf(w, "  ✅ Redis: CONNECTED\n")
			
			// Get active claims
			claims, err := redisClient.GetActiveClaims(ctx)
			if err != nil {
				fmt.Fprintf(w, "  ⚠️  Active claims: ERROR (%v)\n", err)
			} else {
				fmt.Fprintf(w, "  🔒 Active claims: %d\n", len(claims))
				for taskID, claimInfo := range claims {
					fmt.Fprintf(w, "    - %s: %s\n", taskID, claimInfo)
				}
			}
			
			// Get recent events count
			eventCount, err := redisClient.GetCoordinationEventCount(ctx)
			if err != nil {
				fmt.Fprintf(w, "  ⚠️  Event count: ERROR (%v)\n", err)
			} else {
				fmt.Fprintf(w, "  📡 Total events: %d\n", eventCount)
			}
		}
	}

	fmt.Fprintf(w, "\n🤖 Agent Information:\n")
	agentID, _ := getAgentID()
	fmt.Fprintf(w, "  🆔 Current agent: %s\n", agentID)
	fmt.Fprintf(w, "  🕐 Current time: %s\n", time.Now().Format(time.RFC3339))

	fmt.Fprintf(w, "\n✅ System status check completed\n")
	
	log.Info().Msg("System status check completed")
	return nil
}

