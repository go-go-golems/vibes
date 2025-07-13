package database

import (
	"context"
	"github.com/taskplanner/taskplanner/pkg/models"
)

// Database defines the interface for all database operations
type Database interface {
	// Connection management
	Close() error
	Ping(ctx context.Context) error
	
	// Migration and setup
	Migrate(ctx context.Context) error
	
	// Plan operations
	CreatePlan(ctx context.Context, plan *models.Plan) error
	GetPlan(ctx context.Context, id string) (*models.Plan, error)
	ListPlans(ctx context.Context, filters models.PlanFilters) ([]*models.Plan, error)
	UpdatePlan(ctx context.Context, plan *models.Plan) error
	DeletePlan(ctx context.Context, id string) error
	GetPlanStats(ctx context.Context, planID string) (*models.PlanStats, error)
	
	// Task operations
	CreateTask(ctx context.Context, task *models.Task) error
	GetTask(ctx context.Context, id string) (*models.Task, error)
	ListTasks(ctx context.Context, filters models.TaskFilters) ([]*models.Task, error)
	UpdateTask(ctx context.Context, task *models.Task) error
	UpdateTaskFields(ctx context.Context, update models.TaskUpdate) (*models.Task, error)
	DeleteTask(ctx context.Context, id string) error
	DeleteTaskCascade(ctx context.Context, taskID string) error
	GetTaskHierarchy(ctx context.Context, planID string) ([]*models.TaskHierarchy, error)
	GetTasksByParent(ctx context.Context, parentID string) ([]*models.Task, error)
	GetRootTasks(ctx context.Context, planID string) ([]*models.Task, error)
	
	// Task dependency operations
	AddDependency(ctx context.Context, dep *models.TaskDependency) error
	RemoveDependency(ctx context.Context, taskID, dependsOnID string) error
	GetDependencies(ctx context.Context, taskID string) ([]*models.TaskDependency, error)
	GetDependents(ctx context.Context, taskID string) ([]*models.TaskDependency, error)
	HasCircularDependency(ctx context.Context, taskID, dependsOnID string) (bool, error)
	
	// Task history operations
	AddTaskHistory(ctx context.Context, history *models.TaskHistory) error
	GetTaskHistory(ctx context.Context, taskID string, limit int) ([]*models.TaskHistory, error)
	
	// Agent operations
	RegisterAgent(ctx context.Context, agent *models.Agent) error
	GetAgent(ctx context.Context, id string) (*models.Agent, error)
	ListAgents(ctx context.Context) ([]*models.Agent, error)
	UpdateAgent(ctx context.Context, agent *models.Agent) error
	UpdateAgentStatus(ctx context.Context, agentID, status string) error
	GetAgentTasks(ctx context.Context, agentID string) ([]*models.Task, error)
	
	// Statistics and reporting
	GetTaskStats(ctx context.Context, planID string) (*models.TaskStats, error)
	GetOverallStats(ctx context.Context) (*models.TaskStats, error)
	
	// Transaction support
	WithTransaction(ctx context.Context, fn func(tx Database) error) error
}

