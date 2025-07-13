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
)

type TaskClaimCommand struct {
	*cmds.CommandDescription
}

type TaskClaimSettings struct {
	TaskID string `glazed.parameter:"task-id"`
	TTL    int    `glazed.parameter:"ttl"`
}

var _ cmds.GlazeCommand = (*TaskClaimCommand)(nil)

func NewTaskClaimCommand() (*TaskClaimCommand, error) {
	glazedParameterLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "could not create glazed parameter layer")
	}

	return &TaskClaimCommand{
		CommandDescription: cmds.NewCommandDescription(
			"task-claim",
			cmds.WithShort("Claim a task for execution"),
			cmds.WithLong(`Claim a task for execution by the current agent.

This creates a temporary claim on the task that prevents other agents
from claiming it simultaneously. The claim has a time-to-live (TTL)
and will automatically expire if not renewed.

Claims are used for coordination in multi-agent environments to ensure
that only one agent works on a task at a time.

Example usage:
  taskplanner task-claim --task-id abc123
  taskplanner task-claim --task-id abc123 --ttl 3600  # Claim for 1 hour`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"task-id",
					parameters.ParameterTypeString,
					parameters.WithHelp("ID of the task to claim"),
					parameters.WithRequired(true),
				),
				parameters.NewParameterDefinition(
					"ttl",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Time-to-live for the claim in seconds"),
					parameters.WithDefault(1800), // 30 minutes
				),
			),
			cmds.WithLayersList(glazedParameterLayer),
		),
	}, nil
}

func (c *TaskClaimCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	s := &TaskClaimSettings{}
	err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)
	if err != nil {
		log.Error().Err(err).Msg("Failed to initialize task claim settings")
		return err
	}

	log.Info().
		Str("task_id", s.TaskID).
		Int("ttl", s.TTL).
		Msg("Starting task claim")

	agentID, err := getAgentID()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get agent ID")
		return err
	}

	// Get database connection to verify task exists
	db, err := getDatabase()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get database connection")
		return err
	}
	defer db.Close()

	// Verify task exists
	log.Debug().Str("task_id", s.TaskID).Msg("Verifying task exists")
	task, err := db.GetTask(ctx, s.TaskID)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Task not found")
		return errors.Wrap(err, "task not found")
	}

	// Get Redis client for claiming
	redisClient, err := getRedisClient()
	if err != nil {
		log.Error().Err(err).Msg("Failed to get Redis client")
		return err
	}
	defer redisClient.Close()

	// Attempt to claim the task
	ttlDuration := time.Duration(s.TTL) * time.Second
	log.Debug().
		Str("task_id", s.TaskID).
		Str("agent_id", agentID).
		Dur("ttl", ttlDuration).
		Msg("Attempting to claim task")

	claimed, err := redisClient.ClaimTask(ctx, s.TaskID, agentID, ttlDuration)
	if err != nil {
		log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to claim task")
		return errors.Wrap(err, "failed to claim task")
	}

	if !claimed {
		// Task is already claimed, get current claim info
		currentClaim, err := redisClient.GetTaskClaim(ctx, s.TaskID)
		if err != nil {
			log.Error().Err(err).Str("task_id", s.TaskID).Msg("Failed to get current claim")
			return errors.Wrap(err, "failed to get current claim")
		}

		log.Warn().
			Str("task_id", s.TaskID).
			Str("current_claim", currentClaim).
			Msg("Task is already claimed")

		return errors.Errorf("task '%s' is already claimed: %s", s.TaskID, currentClaim)
	}

	log.Info().
		Str("task_id", s.TaskID).
		Str("agent_id", agentID).
		Dur("ttl", ttlDuration).
		Msg("Successfully claimed task")

	// Output the result
	row := types.NewRow(
		types.MRP("task_id", task.ID),
		types.MRP("task_title", task.Title),
		types.MRP("claimed_by", agentID),
		types.MRP("claimed_at", time.Now().Format(time.RFC3339)),
		types.MRP("ttl_seconds", s.TTL),
		types.MRP("expires_at", time.Now().Add(ttlDuration).Format(time.RFC3339)),
		types.MRP("status", "claimed"),
	)

	return gp.AddRow(ctx, row)
}

