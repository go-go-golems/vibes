package database

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	_ "github.com/mattn/go-sqlite3"
)

// DB wraps the SQL database connection
type DB struct {
	*sql.DB
}

// New creates a new database connection
func New(dbPath string) (*DB, error) {
	// Ensure directory exists
	dir := filepath.Dir(dbPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create database directory: %w", err)
	}

	// Open database connection
	sqlDB, err := sql.Open("sqlite3", dbPath+"?_foreign_keys=on")
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	db := &DB{sqlDB}

	// Run migrations
	if err := db.migrate(); err != nil {
		return nil, fmt.Errorf("failed to run migrations: %w", err)
	}

	return db, nil
}

// migrate runs database migrations
func (db *DB) migrate() error {
	migrations := []string{
		`CREATE TABLE IF NOT EXISTS reviews (
			id TEXT PRIMARY KEY,
			title TEXT NOT NULL,
			branch TEXT,
			"commit" TEXT,
			base_commit TEXT,
			reviewer TEXT,
			created DATETIME DEFAULT CURRENT_TIMESTAMP,
			updated DATETIME DEFAULT CURRENT_TIMESTAMP,
			status TEXT DEFAULT 'pending',
			files_changed INTEGER DEFAULT 0,
			lines_added INTEGER DEFAULT 0,
			lines_removed INTEGER DEFAULT 0,
			tags TEXT
		)`,
		`CREATE TABLE IF NOT EXISTS annotations (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			review_id TEXT NOT NULL,
			file TEXT NOT NULL,
			line INTEGER,
			line_start INTEGER,
			line_end INTEGER,
			type TEXT NOT NULL,
			severity TEXT DEFAULT 'minor',
			message TEXT NOT NULL,
			suggestion TEXT,
			status TEXT DEFAULT 'open',
			created DATETIME DEFAULT CURRENT_TIMESTAMP,
			updated DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (review_id) REFERENCES reviews(id) ON DELETE CASCADE
		)`,
		`CREATE TABLE IF NOT EXISTS threads (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			annotation_id INTEGER NOT NULL,
			author TEXT NOT NULL,
			message TEXT NOT NULL,
			timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (annotation_id) REFERENCES annotations(id) ON DELETE CASCADE
		)`,
		`CREATE TABLE IF NOT EXISTS config (
			key TEXT PRIMARY KEY,
			value TEXT NOT NULL,
			updated DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,
		`CREATE INDEX IF NOT EXISTS idx_annotations_review_id ON annotations(review_id)`,
		`CREATE INDEX IF NOT EXISTS idx_annotations_file ON annotations(file)`,
		`CREATE INDEX IF NOT EXISTS idx_threads_annotation_id ON threads(annotation_id)`,
		`CREATE INDEX IF NOT EXISTS idx_reviews_status ON reviews(status)`,
		`CREATE INDEX IF NOT EXISTS idx_reviews_created ON reviews(created)`,
	}

	for _, migration := range migrations {
		if _, err := db.Exec(migration); err != nil {
			return fmt.Errorf("failed to execute migration: %w", err)
		}
	}

	return nil
}

// Close closes the database connection
func (db *DB) Close() error {
	return db.DB.Close()
}

// GetDefaultDBPath returns the default database path
func GetDefaultDBPath() string {
	return filepath.Join(".codereview", "reviews.db")
}
