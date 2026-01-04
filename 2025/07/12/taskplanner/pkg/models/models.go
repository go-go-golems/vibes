package models

import (
	"time"
)

// Plan represents a task planning project
type Plan struct {
	ID          string    `json:"id" db:"id"`
	Name        string    `json:"name" db:"name"`
	Description string    `json:"description" db:"description"`
	Status      string    `json:"status" db:"status"` // draft, active, completed, archived
	CreatedBy   string    `json:"created_by" db:"created_by"`
	CreatedAt   time.Time `json:"created_at" db:"created_at"`
	UpdatedAt   time.Time `json:"updated_at" db:"updated_at"`
}

// Task represents a single task within a plan
type Task struct {
	ID             string     `json:"id" db:"id"`
	Title          string     `json:"title" db:"title"`
	Description    string     `json:"description" db:"description"`
	Status         string     `json:"status" db:"status"` // planned, active, completed, blocked, cancelled
	Priority       int        `json:"priority" db:"priority"`
	ParentID       *string    `json:"parent_id" db:"parent_id"`
	PlanID         string     `json:"plan_id" db:"plan_id"`
	AgentID        *string    `json:"agent_id" db:"agent_id"`
	CreatedAt      time.Time  `json:"created_at" db:"created_at"`
	UpdatedAt      time.Time  `json:"updated_at" db:"updated_at"`
	DueDate        *time.Time `json:"due_date" db:"due_date"`
	EstimatedHours *float64   `json:"estimated_hours" db:"estimated_hours"`
	ActualHours    *float64   `json:"actual_hours" db:"actual_hours"`
	BlockedReason  *string    `json:"blocked_reason" db:"blocked_reason"`
	
	// Computed fields (not stored in DB)
	Children     []*Task            `json:"children,omitempty" db:"-"`
	Dependencies []*TaskDependency  `json:"dependencies,omitempty" db:"-"`
	Plan         *Plan              `json:"plan,omitempty" db:"-"`
}

// TaskDependency represents a dependency relationship between tasks
type TaskDependency struct {
	ID             string `json:"id" db:"id"`
	TaskID         string `json:"task_id" db:"task_id"`
	DependsOnID    string `json:"depends_on_id" db:"depends_on_id"`
	DependencyType string `json:"dependency_type" db:"dependency_type"` // finish_to_start, start_to_start, finish_to_finish, start_to_finish
	CreatedAt      time.Time `json:"created_at" db:"created_at"`
	
	// Computed fields
	Task      *Task `json:"task,omitempty" db:"-"`
	DependsOn *Task `json:"depends_on,omitempty" db:"-"`
}

// TaskHistory represents a history entry for task status changes
type TaskHistory struct {
	ID        string    `json:"id" db:"id"`
	TaskID    string    `json:"task_id" db:"task_id"`
	AgentID   string    `json:"agent_id" db:"agent_id"`
	Action    string    `json:"action" db:"action"` // created, updated, assigned, completed, blocked, etc.
	OldValue  *string   `json:"old_value" db:"old_value"`
	NewValue  *string   `json:"new_value" db:"new_value"`
	Message   *string   `json:"message" db:"message"`
	CreatedAt time.Time `json:"created_at" db:"created_at"`
	
	// Computed fields
	Task *Task `json:"task,omitempty" db:"-"`
}

// Agent represents an agent that can execute tasks
type Agent struct {
	ID           string    `json:"id" db:"id"`
	Name         string    `json:"name" db:"name"`
	Capabilities []string  `json:"capabilities" db:"capabilities"` // JSON array stored as string
	Status       string    `json:"status" db:"status"` // online, offline, busy
	LastSeen     time.Time `json:"last_seen" db:"last_seen"`
	CreatedAt    time.Time `json:"created_at" db:"created_at"`
	UpdatedAt    time.Time `json:"updated_at" db:"updated_at"`
}

// TaskFilters represents filters for querying tasks
type TaskFilters struct {
	PlanID   *string
	ParentID *string
	AgentID  *string
	Status   *string
	Priority *int
	Limit    *int
	Offset   *int
}

// PlanFilters represents filters for querying plans
type PlanFilters struct {
	CreatedBy *string
	Status    *string
	Limit     *int
	Offset    *int
}

// TaskStats represents statistics for a task or plan
type TaskStats struct {
	Total       int     `json:"total"`
	Planned     int     `json:"planned"`
	Active      int     `json:"active"`
	Completed   int     `json:"completed"`
	Blocked     int     `json:"blocked"`
	Cancelled   int     `json:"cancelled"`
	Progress    float64 `json:"progress"` // Percentage completed
}

// PlanStats represents comprehensive statistics for a plan
type PlanStats struct {
	PlanID           string    `json:"plan_id"`
	TaskStats        TaskStats `json:"task_stats"`
	TotalEstimated   float64   `json:"total_estimated_hours"`
	TotalActual      float64   `json:"total_actual_hours"`
	AgentsAssigned   int       `json:"agents_assigned"`
	LastActivity     time.Time `json:"last_activity"`
}

// TaskHierarchy represents a hierarchical view of tasks
type TaskHierarchy struct {
	Task     *Task            `json:"task"`
	Children []*TaskHierarchy `json:"children,omitempty"`
	Level    int              `json:"level"`
}

// Constants for valid values
const (
	// Task statuses
	TaskStatusPlanned   = "planned"
	TaskStatusActive    = "active"
	TaskStatusCompleted = "completed"
	TaskStatusBlocked   = "blocked"
	TaskStatusCancelled = "cancelled"

	// Plan statuses
	PlanStatusDraft     = "draft"
	PlanStatusActive    = "active"
	PlanStatusCompleted = "completed"
	PlanStatusArchived  = "archived"

	// Dependency types
	DependencyFinishToStart  = "finish_to_start"
	DependencyStartToStart   = "start_to_start"
	DependencyFinishToFinish = "finish_to_finish"
	DependencyStartToFinish  = "start_to_finish"

	// Agent statuses
	AgentStatusOnline  = "online"
	AgentStatusOffline = "offline"
	AgentStatusBusy    = "busy"

	// Task actions for history
	ActionCreated   = "created"
	ActionUpdated   = "updated"
	ActionAssigned  = "assigned"
	ActionClaimed   = "claimed"
	ActionReleased  = "released"
	ActionCompleted = "completed"
	ActionBlocked   = "blocked"
	ActionUnblocked = "unblocked"
	ActionCancelled = "cancelled"
)

// IsValidTaskStatus checks if a task status is valid
func IsValidTaskStatus(status string) bool {
	switch status {
	case TaskStatusPlanned, TaskStatusActive, TaskStatusCompleted, TaskStatusBlocked, TaskStatusCancelled:
		return true
	default:
		return false
	}
}

// IsValidPlanStatus checks if a plan status is valid
func IsValidPlanStatus(status string) bool {
	switch status {
	case PlanStatusDraft, PlanStatusActive, PlanStatusCompleted, PlanStatusArchived:
		return true
	default:
		return false
	}
}

// IsValidDependencyType checks if a dependency type is valid
func IsValidDependencyType(depType string) bool {
	switch depType {
	case DependencyFinishToStart, DependencyStartToStart, DependencyFinishToFinish, DependencyStartToFinish:
		return true
	default:
		return false
	}
}

// IsValidAgentStatus checks if an agent status is valid
func IsValidAgentStatus(status string) bool {
	switch status {
	case AgentStatusOnline, AgentStatusOffline, AgentStatusBusy:
		return true
	default:
		return false
	}
}


// TaskUpdate represents fields that can be updated in a task
type TaskUpdate struct {
	ID              string     `json:"id"`
	Title           *string    `json:"title,omitempty"`
	Description     *string    `json:"description,omitempty"`
	Status          *string    `json:"status,omitempty"`
	Priority        *int       `json:"priority,omitempty"`
	AgentID         *string    `json:"agent_id,omitempty"`
	DueDate         *time.Time `json:"due_date,omitempty"`
	EstimatedHours  *float64   `json:"estimated_hours,omitempty"`
	ActualHours     *float64   `json:"actual_hours,omitempty"`
	BlockedReason   *string    `json:"blocked_reason,omitempty"`
}


// CoordinationEvent represents a coordination event in the system
type CoordinationEvent struct {
	ID        string    `json:"id"`
	AgentID   string    `json:"agent_id"`
	Type      string    `json:"type"`
	Data      string    `json:"data"`
	Timestamp time.Time `json:"timestamp"`
}

