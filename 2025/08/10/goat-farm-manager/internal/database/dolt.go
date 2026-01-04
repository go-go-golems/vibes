package database

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	_ "github.com/dolthub/driver" // registers driver "dolt"
	"entgo.io/ent/dialect"
	entsql "entgo.io/ent/dialect/sql"

	"github.com/farm/goat-manager/ent"
)

// Config holds the database configuration
type Config struct {
	DatabasePath string
	DatabaseName string
	CommitName   string
	CommitEmail  string
}

// DefaultConfig returns a default database configuration
func DefaultConfig() *Config {
	homeDir, _ := os.UserHomeDir()
	return &Config{
		DatabasePath: filepath.Join(homeDir, "goat-farm-data"),
		DatabaseName: "goat_farm",
		CommitName:   "Farm Manager",
		CommitEmail:  "manager@goatfarm.local",
	}
}

// OpenEnt opens a connection to the embedded Dolt database and returns an Ent client
func OpenEnt(ctx context.Context, config *Config) (*ent.Client, error) {
	// Ensure the database directory exists
	if err := os.MkdirAll(config.DatabasePath, 0755); err != nil {
		return nil, fmt.Errorf("failed to create database directory: %w", err)
	}

	// DSN requires path to the directory that contains dolt DBs and committer info.
	// database=<dbname> selects which DB inside that directory.
	dsn := fmt.Sprintf("file://%s?commitname=%s&commitemail=%s&database=%s",
		config.DatabasePath,
		config.CommitName,
		config.CommitEmail,
		config.DatabaseName,
	)

	db, err := sql.Open("dolt", dsn)
	if err != nil {
		return nil, fmt.Errorf("failed to open dolt database: %w", err)
	}

	// Test the connection
	if err := db.PingContext(ctx); err != nil {
		return nil, fmt.Errorf("failed to ping database: %w", err)
	}

	drv := entsql.OpenDB(dialect.MySQL, db)
	client := ent.NewClient(ent.Driver(drv))

	return client, nil
}

// InitializeDatabase initializes the database and creates tables
func InitializeDatabase(ctx context.Context, client *ent.Client) error {
	// Run the auto migration tool to create the schema resources.
	if err := client.Schema.Create(ctx); err != nil {
		return fmt.Errorf("failed creating schema resources: %w", err)
	}

	// Make an initial commit
	if err := CommitChanges(ctx, client, "Initial schema creation"); err != nil {
		return fmt.Errorf("failed to commit initial schema: %w", err)
	}

	return nil
}

// CommitChanges commits the current changes to the Dolt database
func CommitChanges(ctx context.Context, client *ent.Client, message string) error {
	// Use raw SQL to execute Dolt commit command
	_, err := client.ExecContext(ctx, "CALL dolt_commit('-am', ?)", message)
	if err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}
	return nil
}

// CreateBranch creates a new branch in the Dolt database
func CreateBranch(ctx context.Context, client *ent.Client, branchName string) error {
	_, err := client.ExecContext(ctx, "CALL dolt_checkout('-b', ?)", branchName)
	if err != nil {
		return fmt.Errorf("failed to create branch %s: %w", branchName, err)
	}
	return nil
}

// SwitchBranch switches to an existing branch
func SwitchBranch(ctx context.Context, client *ent.Client, branchName string) error {
	_, err := client.ExecContext(ctx, "CALL dolt_checkout(?)", branchName)
	if err != nil {
		return fmt.Errorf("failed to switch to branch %s: %w", branchName, err)
	}
	return nil
}

// GetBranches returns a list of all branches
func GetBranches(ctx context.Context, client *ent.Client) ([]string, error) {
	rows, err := client.QueryContext(ctx, "SELECT name FROM dolt_branches")
	if err != nil {
		return nil, fmt.Errorf("failed to get branches: %w", err)
	}
	defer rows.Close()

	var branches []string
	for rows.Next() {
		var branch string
		if err := rows.Scan(&branch); err != nil {
			return nil, fmt.Errorf("failed to scan branch name: %w", err)
		}
		branches = append(branches, branch)
	}

	return branches, nil
}

// GetCommitHistory returns the commit history
func GetCommitHistory(ctx context.Context, client *ent.Client, limit int) ([]CommitInfo, error) {
	query := "SELECT commit_hash, committer, date, message FROM dolt_log"
	if limit > 0 {
		query += fmt.Sprintf(" LIMIT %d", limit)
	}

	rows, err := client.QueryContext(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit history: %w", err)
	}
	defer rows.Close()

	var commits []CommitInfo
	for rows.Next() {
		var commit CommitInfo
		if err := rows.Scan(&commit.Hash, &commit.Committer, &commit.Date, &commit.Message); err != nil {
			return nil, fmt.Errorf("failed to scan commit info: %w", err)
		}
		commits = append(commits, commit)
	}

	return commits, nil
}

// CommitInfo represents information about a commit
type CommitInfo struct {
	Hash      string
	Committer string
	Date      string
	Message   string
}

// GetTableDiff returns the diff for a specific table between two commits
func GetTableDiff(ctx context.Context, client *ent.Client, fromCommit, toCommit, tableName string) ([]map[string]interface{}, error) {
	query := fmt.Sprintf("SELECT * FROM dolt_diff('%s','%s','%s')", fromCommit, toCommit, tableName)
	
	rows, err := client.QueryContext(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("failed to get table diff: %w", err)
	}
	defer rows.Close()

	// Get column names
	columns, err := rows.Columns()
	if err != nil {
		return nil, fmt.Errorf("failed to get columns: %w", err)
	}

	var results []map[string]interface{}
	for rows.Next() {
		// Create a slice of interface{} to hold the values
		values := make([]interface{}, len(columns))
		valuePtrs := make([]interface{}, len(columns))
		for i := range values {
			valuePtrs[i] = &values[i]
		}

		if err := rows.Scan(valuePtrs...); err != nil {
			return nil, fmt.Errorf("failed to scan row: %w", err)
		}

		// Create a map for this row
		row := make(map[string]interface{})
		for i, col := range columns {
			row[col] = values[i]
		}
		results = append(results, row)
	}

	return results, nil
}

