package commands

import (
	"context"
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/pkg/errors"
	"github.com/spf13/viper"
	"github.com/taskplanner/taskplanner/pkg/database"
	"github.com/taskplanner/taskplanner/pkg/redis"
)

// GlazeCommand interface for glazed commands
type GlazeCommand interface {
	cmds.GlazeCommand
}

// WriterCommand interface for writer commands
type WriterCommand interface {
	cmds.WriterCommand
}

// getAgentID retrieves the agent ID from flag or environment
func getAgentID() (string, error) {
	agentID := viper.GetString("agent")
	if agentID == "" {
		return "", errors.New("AGENT_ID is required (use --agent flag or AGENT_ID env var)")
	}
	return agentID, nil
}

// getDatabase creates a database connection from configuration
func getDatabase() (database.Database, error) {
	dbPath := viper.GetString("database")
	return database.NewSQLiteDatabase(dbPath)
}

// getRedisClient creates a Redis client from configuration
func getRedisClient() (*redis.Client, error) {
	redisURL := viper.GetString("redis-url")
	return redis.NewClient(redisURL)
}

// getOutputFormat returns the output format
func getOutputFormat() string {
	format := viper.GetString("format")
	if format != "json" && format != "text" && format != "table" {
		fmt.Fprintf(os.Stderr, "Warning: invalid format '%s', using 'json'\n", format)
		return "json"
	}
	return format
}

// publishTaskUpdate publishes a task update to the coordination channel
func publishTaskUpdate(ctx context.Context, client *redis.Client, agentID, taskID, action, message string) error {
	return client.PublishTaskUpdate(ctx, agentID, taskID, action, message)
}

// publishPlanUpdate publishes a plan update to the coordination channel
func publishPlanUpdate(ctx context.Context, client *redis.Client, agentID, planID, action, message string) error {
	return client.PublishPlanUpdate(ctx, agentID, planID, action, message)
}

// validateTaskStatus validates if a task status is valid
func validateTaskStatus(status string) error {
	validStatuses := map[string]bool{
		"planned":   true,
		"active":    true,
		"completed": true,
		"blocked":   true,
		"cancelled": true,
	}
	
	if !validStatuses[status] {
		return errors.Errorf("invalid task status '%s'. Valid statuses: planned, active, completed, blocked, cancelled", status)
	}
	return nil
}

// validatePlanStatus validates if a plan status is valid
func validatePlanStatus(status string) error {
	validStatuses := map[string]bool{
		"draft":     true,
		"active":    true,
		"completed": true,
		"archived":  true,
	}
	
	if !validStatuses[status] {
		return errors.Errorf("invalid plan status '%s'. Valid statuses: draft, active, completed, archived", status)
	}
	return nil
}

// validatePriority validates if a priority value is valid
func validatePriority(priority int) error {
	if priority < 1 || priority > 10 {
		return errors.Errorf("invalid priority '%d'. Priority must be between 1 and 10", priority)
	}
	return nil
}

