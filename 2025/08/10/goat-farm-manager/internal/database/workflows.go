package database

import (
	"context"
	"fmt"
	"time"

	"github.com/farm/goat-manager/ent"
)

// WorkflowManager handles complex version control workflows
type WorkflowManager struct {
	client *ent.Client
}

// NewWorkflowManager creates a new workflow manager
func NewWorkflowManager(client *ent.Client) *WorkflowManager {
	return &WorkflowManager{client: client}
}

// CreateFeatureBranch creates a new feature branch for specific farm operations
func (wm *WorkflowManager) CreateFeatureBranch(ctx context.Context, branchName, description string) error {
	// Create the branch
	if err := CreateBranch(ctx, wm.client, branchName); err != nil {
		return fmt.Errorf("failed to create feature branch: %w", err)
	}

	// Make an initial commit with the branch description
	if err := CommitChanges(ctx, wm.client, fmt.Sprintf("Created feature branch: %s - %s", branchName, description)); err != nil {
		return fmt.Errorf("failed to commit branch creation: %w", err)
	}

	return nil
}

// MergeBranch merges a feature branch back to main
func (wm *WorkflowManager) MergeBranch(ctx context.Context, sourceBranch, targetBranch, mergeMessage string) error {
	// Switch to target branch
	if err := SwitchBranch(ctx, wm.client, targetBranch); err != nil {
		return fmt.Errorf("failed to switch to target branch %s: %w", targetBranch, err)
	}

	// Perform merge (this is a simplified merge - in real Dolt you'd use dolt_merge)
	_, err := wm.client.ExecContext(ctx, "CALL dolt_merge(?)", sourceBranch)
	if err != nil {
		return fmt.Errorf("failed to merge branch %s into %s: %w", sourceBranch, targetBranch, err)
	}

	// Commit the merge
	if err := CommitChanges(ctx, wm.client, mergeMessage); err != nil {
		return fmt.Errorf("failed to commit merge: %w", err)
	}

	return nil
}

// CreateBackup creates a backup branch with timestamp
func (wm *WorkflowManager) CreateBackup(ctx context.Context, reason string) (string, error) {
	timestamp := time.Now().Format("20060102-150405")
	backupBranch := fmt.Sprintf("backup-%s", timestamp)

	if err := CreateBranch(ctx, wm.client, backupBranch); err != nil {
		return "", fmt.Errorf("failed to create backup branch: %w", err)
	}

	if err := CommitChanges(ctx, wm.client, fmt.Sprintf("Backup created: %s", reason)); err != nil {
		return "", fmt.Errorf("failed to commit backup: %w", err)
	}

	return backupBranch, nil
}

// RestoreFromBackup restores data from a backup branch
func (wm *WorkflowManager) RestoreFromBackup(ctx context.Context, backupBranch string) error {
	// Switch to the backup branch
	if err := SwitchBranch(ctx, wm.client, backupBranch); err != nil {
		return fmt.Errorf("failed to switch to backup branch: %w", err)
	}

	// Create a new branch from the backup
	restoreBranch := fmt.Sprintf("restore-%s", time.Now().Format("20060102-150405"))
	if err := CreateBranch(ctx, wm.client, restoreBranch); err != nil {
		return fmt.Errorf("failed to create restore branch: %w", err)
	}

	return nil
}

// GetBranchStatus returns detailed information about a branch
func (wm *WorkflowManager) GetBranchStatus(ctx context.Context, branchName string) (*BranchStatus, error) {
	// Switch to the branch to get its status
	currentBranch, err := wm.getCurrentBranch(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed to get current branch: %w", err)
	}

	if currentBranch != branchName {
		if err := SwitchBranch(ctx, wm.client, branchName); err != nil {
			return nil, fmt.Errorf("failed to switch to branch %s: %w", branchName, err)
		}
		defer SwitchBranch(ctx, wm.client, currentBranch) // Switch back
	}

	// Get latest commit
	commits, err := GetCommitHistory(ctx, wm.client, 1)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit history: %w", err)
	}

	status := &BranchStatus{
		Name:        branchName,
		IsCurrent:   currentBranch == branchName,
		CommitCount: len(commits),
	}

	if len(commits) > 0 {
		status.LastCommit = commits[0]
	}

	// Get record counts for different entities
	status.GoatCount, _ = wm.client.Goat.Query().Count(ctx)
	status.MilkRecordCount, _ = wm.client.MilkRecord.Query().Count(ctx)
	status.HealthRecordCount, _ = wm.client.HealthRecord.Query().Count(ctx)
	status.BreedingRecordCount, _ = wm.client.BreedingRecord.Query().Count(ctx)
	status.FeedRecordCount, _ = wm.client.FeedRecord.Query().Count(ctx)
	status.FarmOperationCount, _ = wm.client.FarmOperation.Query().Count(ctx)

	return status, nil
}

// getCurrentBranch gets the current active branch
func (wm *WorkflowManager) getCurrentBranch(ctx context.Context) (string, error) {
	rows, err := wm.client.QueryContext(ctx, "SELECT name FROM dolt_branches WHERE name = dolt_branch()")
	if err != nil {
		return "", fmt.Errorf("failed to get current branch: %w", err)
	}
	defer rows.Close()

	if rows.Next() {
		var branch string
		if err := rows.Scan(&branch); err != nil {
			return "", fmt.Errorf("failed to scan branch name: %w", err)
		}
		return branch, nil
	}

	return "main", nil // Default to main if no result
}

// BranchStatus represents the status of a branch
type BranchStatus struct {
	Name                string     `json:"name"`
	IsCurrent           bool       `json:"is_current"`
	LastCommit          CommitInfo `json:"last_commit"`
	CommitCount         int        `json:"commit_count"`
	GoatCount           int        `json:"goat_count"`
	MilkRecordCount     int        `json:"milk_record_count"`
	HealthRecordCount   int        `json:"health_record_count"`
	BreedingRecordCount int        `json:"breeding_record_count"`
	FeedRecordCount     int        `json:"feed_record_count"`
	FarmOperationCount  int        `json:"farm_operation_count"`
}

// CompareData compares data between two branches
func (wm *WorkflowManager) CompareData(ctx context.Context, fromBranch, toBranch string) (*DataComparison, error) {
	comparison := &DataComparison{
		FromBranch: fromBranch,
		ToBranch:   toBranch,
		Tables:     make(map[string]*TableComparison),
	}

	// List of tables to compare
	tables := []string{"goats", "milk_records", "health_records", "breeding_records", "feed_records", "farm_operations"}

	for _, table := range tables {
		diff, err := GetTableDiff(ctx, wm.client, fromBranch, toBranch, table)
		if err != nil {
			// If diff fails, just note it and continue
			comparison.Tables[table] = &TableComparison{
				TableName: table,
				Error:     err.Error(),
			}
			continue
		}

		tableComp := &TableComparison{
			TableName:    table,
			RowsChanged:  len(diff),
			Changes:      diff,
		}

		comparison.Tables[table] = tableComp
	}

	return comparison, nil
}

// DataComparison represents a comparison between two branches
type DataComparison struct {
	FromBranch string                       `json:"from_branch"`
	ToBranch   string                       `json:"to_branch"`
	Tables     map[string]*TableComparison  `json:"tables"`
}

// TableComparison represents changes in a specific table
type TableComparison struct {
	TableName   string                       `json:"table_name"`
	RowsChanged int                          `json:"rows_changed"`
	Changes     []map[string]interface{}     `json:"changes"`
	Error       string                       `json:"error,omitempty"`
}

