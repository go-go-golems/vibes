package database

import (
	"context"
	"database/sql"
	"fmt"
	"strings"
	"time"

	"github.com/pkg/errors"
	_ "github.com/mattn/go-sqlite3"
	"github.com/taskplanner/taskplanner/pkg/models"
)

// SQLiteDatabase implements the Database interface using SQLite
type SQLiteDatabase struct {
	db *sql.DB
}

// NewSQLiteDatabase creates a new SQLite database connection
func NewSQLiteDatabase(dbPath string) (*SQLiteDatabase, error) {
	db, err := sql.Open("sqlite3", dbPath+"?_foreign_keys=on&_journal_mode=WAL")
	if err != nil {
		return nil, errors.Wrap(err, "failed to open database")
	}

	// Configure connection pool
	db.SetMaxOpenConns(25)
	db.SetMaxIdleConns(25)
	db.SetConnMaxLifetime(5 * time.Minute)

	return &SQLiteDatabase{db: db}, nil
}

// Close closes the database connection
func (s *SQLiteDatabase) Close() error {
	return s.db.Close()
}

// Ping tests the database connection
func (s *SQLiteDatabase) Ping(ctx context.Context) error {
	return s.db.PingContext(ctx)
}

// Migrate creates or updates the database schema
func (s *SQLiteDatabase) Migrate(ctx context.Context) error {
	migrations := []string{
		// Plans table
		`CREATE TABLE IF NOT EXISTS plans (
			id TEXT PRIMARY KEY,
			name TEXT NOT NULL,
			description TEXT,
			status TEXT NOT NULL DEFAULT 'draft',
			created_by TEXT NOT NULL,
			created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
		)`,

		// Tasks table
		`CREATE TABLE IF NOT EXISTS tasks (
			id TEXT PRIMARY KEY,
			title TEXT NOT NULL,
			description TEXT,
			status TEXT NOT NULL DEFAULT 'planned',
			priority INTEGER NOT NULL DEFAULT 5,
			parent_id TEXT,
			plan_id TEXT NOT NULL,
			agent_id TEXT,
			created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			due_date DATETIME,
			estimated_hours REAL,
			actual_hours REAL,
			blocked_reason TEXT,
			FOREIGN KEY (parent_id) REFERENCES tasks(id) ON DELETE CASCADE,
			FOREIGN KEY (plan_id) REFERENCES plans(id) ON DELETE CASCADE
		)`,

		// Task dependencies table
		`CREATE TABLE IF NOT EXISTS task_dependencies (
			id TEXT PRIMARY KEY,
			task_id TEXT NOT NULL,
			depends_on_id TEXT NOT NULL,
			dependency_type TEXT NOT NULL DEFAULT 'finish_to_start',
			created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (task_id) REFERENCES tasks(id) ON DELETE CASCADE,
			FOREIGN KEY (depends_on_id) REFERENCES tasks(id) ON DELETE CASCADE,
			UNIQUE(task_id, depends_on_id)
		)`,

		// Task history table
		`CREATE TABLE IF NOT EXISTS task_history (
			id TEXT PRIMARY KEY,
			task_id TEXT NOT NULL,
			agent_id TEXT NOT NULL,
			action TEXT NOT NULL,
			old_value TEXT,
			new_value TEXT,
			message TEXT,
			created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (task_id) REFERENCES tasks(id) ON DELETE CASCADE
		)`,

		// Agents table
		`CREATE TABLE IF NOT EXISTS agents (
			id TEXT PRIMARY KEY,
			name TEXT NOT NULL,
			capabilities TEXT, -- JSON array
			status TEXT NOT NULL DEFAULT 'offline',
			last_seen DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
		)`,

		// Indexes for performance
		`CREATE INDEX IF NOT EXISTS idx_tasks_plan_id ON tasks(plan_id)`,
		`CREATE INDEX IF NOT EXISTS idx_tasks_parent_id ON tasks(parent_id)`,
		`CREATE INDEX IF NOT EXISTS idx_tasks_agent_id ON tasks(agent_id)`,
		`CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)`,
		`CREATE INDEX IF NOT EXISTS idx_task_dependencies_task_id ON task_dependencies(task_id)`,
		`CREATE INDEX IF NOT EXISTS idx_task_dependencies_depends_on_id ON task_dependencies(depends_on_id)`,
		`CREATE INDEX IF NOT EXISTS idx_task_history_task_id ON task_history(task_id)`,
		`CREATE INDEX IF NOT EXISTS idx_agents_status ON agents(status)`,

		// Triggers for updated_at
		`CREATE TRIGGER IF NOT EXISTS update_plans_updated_at 
		 AFTER UPDATE ON plans 
		 BEGIN 
			UPDATE plans SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
		 END`,

		`CREATE TRIGGER IF NOT EXISTS update_tasks_updated_at 
		 AFTER UPDATE ON tasks 
		 BEGIN 
			UPDATE tasks SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
		 END`,

		`CREATE TRIGGER IF NOT EXISTS update_agents_updated_at 
		 AFTER UPDATE ON agents 
		 BEGIN 
			UPDATE agents SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
		 END`,
	}

	for _, migration := range migrations {
		if _, err := s.db.ExecContext(ctx, migration); err != nil {
			return errors.Wrapf(err, "failed to execute migration: %s", migration)
		}
	}

	return nil
}

// Plan operations

func (s *SQLiteDatabase) CreatePlan(ctx context.Context, plan *models.Plan) error {
	query := `INSERT INTO plans (id, name, description, status, created_by, created_at, updated_at)
			  VALUES (?, ?, ?, ?, ?, ?, ?)`
	
	_, err := s.db.ExecContext(ctx, query,
		plan.ID, plan.Name, plan.Description, plan.Status,
		plan.CreatedBy, plan.CreatedAt, plan.UpdatedAt)
	
	return errors.Wrap(err, "failed to create plan")
}

func (s *SQLiteDatabase) GetPlan(ctx context.Context, id string) (*models.Plan, error) {
	query := `SELECT id, name, description, status, created_by, created_at, updated_at
			  FROM plans WHERE id = ?`
	
	plan := &models.Plan{}
	err := s.db.QueryRowContext(ctx, query, id).Scan(
		&plan.ID, &plan.Name, &plan.Description, &plan.Status,
		&plan.CreatedBy, &plan.CreatedAt, &plan.UpdatedAt)
	
	if err == sql.ErrNoRows {
		return nil, errors.Errorf("plan with id '%s' not found", id)
	}
	
	return plan, errors.Wrap(err, "failed to get plan")
}

func (s *SQLiteDatabase) ListPlans(ctx context.Context, filters models.PlanFilters) ([]*models.Plan, error) {
	query := "SELECT id, name, description, status, created_by, created_at, updated_at FROM plans WHERE 1=1"
	args := []interface{}{}

	if filters.CreatedBy != nil {
		query += " AND created_by = ?"
		args = append(args, *filters.CreatedBy)
	}

	if filters.Status != nil {
		query += " AND status = ?"
		args = append(args, *filters.Status)
	}

	query += " ORDER BY created_at DESC"

	if filters.Limit != nil {
		query += " LIMIT ?"
		args = append(args, *filters.Limit)
	}

	if filters.Offset != nil {
		query += " OFFSET ?"
		args = append(args, *filters.Offset)
	}

	rows, err := s.db.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, errors.Wrap(err, "failed to list plans")
	}
	defer rows.Close()

	var plans []*models.Plan
	for rows.Next() {
		plan := &models.Plan{}
		err := rows.Scan(&plan.ID, &plan.Name, &plan.Description, &plan.Status,
			&plan.CreatedBy, &plan.CreatedAt, &plan.UpdatedAt)
		if err != nil {
			return nil, errors.Wrap(err, "failed to scan plan")
		}
		plans = append(plans, plan)
	}

	return plans, nil
}

func (s *SQLiteDatabase) UpdatePlan(ctx context.Context, plan *models.Plan) error {
	query := `UPDATE plans SET name = ?, description = ?, status = ? WHERE id = ?`
	
	result, err := s.db.ExecContext(ctx, query,
		plan.Name, plan.Description, plan.Status, plan.ID)
	if err != nil {
		return errors.Wrap(err, "failed to update plan")
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return errors.Wrap(err, "failed to get rows affected")
	}

	if rowsAffected == 0 {
		return errors.Errorf("plan with id '%s' not found", plan.ID)
	}

	return nil
}

func (s *SQLiteDatabase) DeletePlan(ctx context.Context, id string) error {
	query := `DELETE FROM plans WHERE id = ?`
	
	result, err := s.db.ExecContext(ctx, query, id)
	if err != nil {
		return errors.Wrap(err, "failed to delete plan")
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return errors.Wrap(err, "failed to get rows affected")
	}

	if rowsAffected == 0 {
		return errors.Errorf("plan with id '%s' not found", id)
	}

	return nil
}

// Task operations

func (s *SQLiteDatabase) CreateTask(ctx context.Context, task *models.Task) error {
	query := `INSERT INTO tasks (id, title, description, status, priority, parent_id, plan_id, 
			  agent_id, created_at, updated_at, due_date, estimated_hours, actual_hours, blocked_reason)
			  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`
	
	_, err := s.db.ExecContext(ctx, query,
		task.ID, task.Title, task.Description, task.Status, task.Priority,
		task.ParentID, task.PlanID, task.AgentID, task.CreatedAt, task.UpdatedAt,
		task.DueDate, task.EstimatedHours, task.ActualHours, task.BlockedReason)
	
	return errors.Wrap(err, "failed to create task")
}

func (s *SQLiteDatabase) GetTask(ctx context.Context, id string) (*models.Task, error) {
	query := `SELECT id, title, description, status, priority, parent_id, plan_id, agent_id,
			  created_at, updated_at, due_date, estimated_hours, actual_hours, blocked_reason
			  FROM tasks WHERE id = ?`
	
	task := &models.Task{}
	err := s.db.QueryRowContext(ctx, query, id).Scan(
		&task.ID, &task.Title, &task.Description, &task.Status, &task.Priority,
		&task.ParentID, &task.PlanID, &task.AgentID, &task.CreatedAt, &task.UpdatedAt,
		&task.DueDate, &task.EstimatedHours, &task.ActualHours, &task.BlockedReason)
	
	if err == sql.ErrNoRows {
		return nil, errors.Errorf("task with id '%s' not found", id)
	}
	
	return task, errors.Wrap(err, "failed to get task")
}

func (s *SQLiteDatabase) ListTasks(ctx context.Context, filters models.TaskFilters) ([]*models.Task, error) {
	query := `SELECT id, title, description, status, priority, parent_id, plan_id, agent_id,
			  created_at, updated_at, due_date, estimated_hours, actual_hours, blocked_reason
			  FROM tasks WHERE 1=1`
	args := []interface{}{}

	if filters.PlanID != nil {
		query += " AND plan_id = ?"
		args = append(args, *filters.PlanID)
	}

	if filters.ParentID != nil {
		query += " AND parent_id = ?"
		args = append(args, *filters.ParentID)
	}

	if filters.AgentID != nil {
		query += " AND agent_id = ?"
		args = append(args, *filters.AgentID)
	}

	if filters.Status != nil {
		query += " AND status = ?"
		args = append(args, *filters.Status)
	}

	if filters.Priority != nil {
		query += " AND priority = ?"
		args = append(args, *filters.Priority)
	}

	query += " ORDER BY priority DESC, created_at ASC"

	if filters.Limit != nil {
		query += " LIMIT ?"
		args = append(args, *filters.Limit)
	}

	if filters.Offset != nil {
		query += " OFFSET ?"
		args = append(args, *filters.Offset)
	}

	rows, err := s.db.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, errors.Wrap(err, "failed to list tasks")
	}
	defer rows.Close()

	var tasks []*models.Task
	for rows.Next() {
		task := &models.Task{}
		err := rows.Scan(&task.ID, &task.Title, &task.Description, &task.Status, &task.Priority,
			&task.ParentID, &task.PlanID, &task.AgentID, &task.CreatedAt, &task.UpdatedAt,
			&task.DueDate, &task.EstimatedHours, &task.ActualHours, &task.BlockedReason)
		if err != nil {
			return nil, errors.Wrap(err, "failed to scan task")
		}
		tasks = append(tasks, task)
	}

	return tasks, nil
}

func (s *SQLiteDatabase) UpdateTask(ctx context.Context, task *models.Task) error {
	query := `UPDATE tasks SET title = ?, description = ?, status = ?, priority = ?, 
			  parent_id = ?, agent_id = ?, due_date = ?, estimated_hours = ?, 
			  actual_hours = ?, blocked_reason = ? WHERE id = ?`
	
	result, err := s.db.ExecContext(ctx, query,
		task.Title, task.Description, task.Status, task.Priority,
		task.ParentID, task.AgentID, task.DueDate, task.EstimatedHours,
		task.ActualHours, task.BlockedReason, task.ID)
	if err != nil {
		return errors.Wrap(err, "failed to update task")
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return errors.Wrap(err, "failed to get rows affected")
	}

	if rowsAffected == 0 {
		return errors.Errorf("task with id '%s' not found", task.ID)
	}

	return nil
}

func (s *SQLiteDatabase) DeleteTask(ctx context.Context, id string) error {
	query := `DELETE FROM tasks WHERE id = ?`
	
	result, err := s.db.ExecContext(ctx, query, id)
	if err != nil {
		return errors.Wrap(err, "failed to delete task")
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return errors.Wrap(err, "failed to get rows affected")
	}

	if rowsAffected == 0 {
		return errors.Errorf("task with id '%s' not found", id)
	}

	return nil
}



// Stub implementations for missing interface methods

func (s *SQLiteDatabase) GetTaskHierarchy(ctx context.Context, planID string) ([]*models.TaskHierarchy, error) {
	return nil, errors.New("GetTaskHierarchy not implemented yet")
}

func (s *SQLiteDatabase) GetTasksByParent(ctx context.Context, parentID string) ([]*models.Task, error) {
	return nil, errors.New("GetTasksByParent not implemented yet")
}

func (s *SQLiteDatabase) GetRootTasks(ctx context.Context, planID string) ([]*models.Task, error) {
	return nil, errors.New("GetRootTasks not implemented yet")
}

func (s *SQLiteDatabase) AddDependency(ctx context.Context, dep *models.TaskDependency) error {
	return errors.New("AddDependency not implemented yet")
}

func (s *SQLiteDatabase) RemoveDependency(ctx context.Context, taskID, dependsOnID string) error {
	return errors.New("RemoveDependency not implemented yet")
}

func (s *SQLiteDatabase) GetDependencies(ctx context.Context, taskID string) ([]*models.TaskDependency, error) {
	return nil, errors.New("GetDependencies not implemented yet")
}

func (s *SQLiteDatabase) GetDependents(ctx context.Context, taskID string) ([]*models.TaskDependency, error) {
	return nil, errors.New("GetDependents not implemented yet")
}

func (s *SQLiteDatabase) HasCircularDependency(ctx context.Context, taskID, dependsOnID string) (bool, error) {
	return false, errors.New("HasCircularDependency not implemented yet")
}

func (s *SQLiteDatabase) AddTaskHistory(ctx context.Context, history *models.TaskHistory) error {
	return errors.New("AddTaskHistory not implemented yet")
}

func (s *SQLiteDatabase) GetTaskHistory(ctx context.Context, taskID string, limit int) ([]*models.TaskHistory, error) {
	return nil, errors.New("GetTaskHistory not implemented yet")
}

func (s *SQLiteDatabase) RegisterAgent(ctx context.Context, agent *models.Agent) error {
	return errors.New("RegisterAgent not implemented yet")
}

func (s *SQLiteDatabase) GetAgent(ctx context.Context, id string) (*models.Agent, error) {
	return nil, errors.New("GetAgent not implemented yet")
}

func (s *SQLiteDatabase) ListAgents(ctx context.Context) ([]*models.Agent, error) {
	return nil, errors.New("ListAgents not implemented yet")
}

func (s *SQLiteDatabase) UpdateAgent(ctx context.Context, agent *models.Agent) error {
	return errors.New("UpdateAgent not implemented yet")
}

func (s *SQLiteDatabase) UpdateAgentStatus(ctx context.Context, agentID, status string) error {
	return errors.New("UpdateAgentStatus not implemented yet")
}

func (s *SQLiteDatabase) GetAgentTasks(ctx context.Context, agentID string) ([]*models.Task, error) {
	return nil, errors.New("GetAgentTasks not implemented yet")
}

func (s *SQLiteDatabase) GetTaskStats(ctx context.Context, planID string) (*models.TaskStats, error) {
	return nil, errors.New("GetTaskStats not implemented yet")
}

func (s *SQLiteDatabase) GetOverallStats(ctx context.Context) (*models.TaskStats, error) {
	return nil, errors.New("GetOverallStats not implemented yet")
}

func (s *SQLiteDatabase) GetPlanStats(ctx context.Context, planID string) (*models.PlanStats, error) {
	return nil, errors.New("GetPlanStats not implemented yet")
}

func (s *SQLiteDatabase) WithTransaction(ctx context.Context, fn func(tx Database) error) error {
	return errors.New("WithTransaction not implemented yet")
}


// UpdateTaskFields updates an existing task with the provided changes
func (db *SQLiteDatabase) UpdateTaskFields(ctx context.Context, update models.TaskUpdate) (*models.Task, error) {
	// Build dynamic UPDATE query based on provided fields
	setParts := []string{}
	args := []interface{}{}
	argIndex := 1

	if update.Title != nil {
		setParts = append(setParts, fmt.Sprintf("title = $%d", argIndex))
		args = append(args, *update.Title)
		argIndex++
	}
	if update.Description != nil {
		setParts = append(setParts, fmt.Sprintf("description = $%d", argIndex))
		args = append(args, *update.Description)
		argIndex++
	}
	if update.Status != nil {
		setParts = append(setParts, fmt.Sprintf("status = $%d", argIndex))
		args = append(args, *update.Status)
		argIndex++
	}
	if update.Priority != nil {
		setParts = append(setParts, fmt.Sprintf("priority = $%d", argIndex))
		args = append(args, *update.Priority)
		argIndex++
	}
	if update.AgentID != nil {
		setParts = append(setParts, fmt.Sprintf("agent_id = $%d", argIndex))
		args = append(args, *update.AgentID)
		argIndex++
	}
	if update.DueDate != nil {
		setParts = append(setParts, fmt.Sprintf("due_date = $%d", argIndex))
		args = append(args, *update.DueDate)
		argIndex++
	}
	if update.EstimatedHours != nil {
		setParts = append(setParts, fmt.Sprintf("estimated_hours = $%d", argIndex))
		args = append(args, *update.EstimatedHours)
		argIndex++
	}
	if update.ActualHours != nil {
		setParts = append(setParts, fmt.Sprintf("actual_hours = $%d", argIndex))
		args = append(args, *update.ActualHours)
		argIndex++
	}
	if update.BlockedReason != nil {
		setParts = append(setParts, fmt.Sprintf("blocked_reason = $%d", argIndex))
		args = append(args, *update.BlockedReason)
		argIndex++
	}

	if len(setParts) == 0 {
		return nil, errors.New("no fields to update")
	}

	// Add updated_at
	setParts = append(setParts, fmt.Sprintf("updated_at = $%d", argIndex))
	args = append(args, time.Now())
	argIndex++

	// Add WHERE clause
	args = append(args, update.ID)

	query := fmt.Sprintf(`
		UPDATE tasks 
		SET %s 
		WHERE id = $%d`,
		strings.Join(setParts, ", "),
		argIndex,
	)

	_, err := db.db.ExecContext(ctx, query, args...)
	if err != nil {
		return nil, errors.Wrap(err, "failed to update task")
	}

	// Return updated task
	return db.GetTask(ctx, update.ID)
}

// DeleteTaskCascade deletes a task and all its subtasks
func (db *SQLiteDatabase) DeleteTaskCascade(ctx context.Context, taskID string) error {
	// Start transaction
	tx, err := db.db.BeginTx(ctx, nil)
	if err != nil {
		return errors.Wrap(err, "failed to start transaction")
	}
	defer tx.Rollback()

	// Delete all subtasks recursively
	_, err = tx.ExecContext(ctx, `
		WITH RECURSIVE task_tree AS (
			SELECT id FROM tasks WHERE id = $1
			UNION ALL
			SELECT t.id FROM tasks t
			INNER JOIN task_tree tt ON t.parent_id = tt.id
		)
		DELETE FROM tasks WHERE id IN (SELECT id FROM task_tree)
	`, taskID)
	if err != nil {
		return errors.Wrap(err, "failed to delete task and subtasks")
	}

	// Commit transaction
	if err := tx.Commit(); err != nil {
		return errors.Wrap(err, "failed to commit transaction")
	}

	return nil
}

