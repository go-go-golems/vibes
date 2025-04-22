package database

import (
	"database/sql"
	"fmt"
	"log"
	"os"
	"path/filepath"

	_ "github.com/mattn/go-sqlite3"
)

var DB *sql.DB

// InitDB initializes the SQLite database connection
func InitDB(dbPath string) error {
	// Ensure directory exists
	dir := filepath.Dir(dbPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create database directory: %v", err)
	}

	var err error
	DB, err = sql.Open("sqlite3", dbPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %v", err)
	}

	if err = DB.Ping(); err != nil {
		return fmt.Errorf("failed to ping database: %v", err)
	}

	log.Println("Database connection established")
	return createTables()
}

// createTables creates all necessary tables if they don't exist
func createTables() error {
	// Create blackboard table
	_, err := DB.Exec(`
	CREATE TABLE IF NOT EXISTS blackboard (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		level TEXT NOT NULL,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
	)`)
	if err != nil {
		return fmt.Errorf("failed to create blackboard table: %v", err)
	}

	// Create hypothesis table
	_, err = DB.Exec(`
	CREATE TABLE IF NOT EXISTS hypothesis (
		id TEXT PRIMARY KEY,
		blackboard_id INTEGER,
		content TEXT NOT NULL,
		confidence REAL NOT NULL,
		time_range TEXT,
		owner TEXT,
		locked BOOLEAN DEFAULT FALSE,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
		FOREIGN KEY (blackboard_id) REFERENCES blackboard(id)
	)`)
	if err != nil {
		return fmt.Errorf("failed to create hypothesis table: %v", err)
	}

	// Create knowledge source table
	_, err = DB.Exec(`
	CREATE TABLE IF NOT EXISTS knowledge_source (
		id TEXT PRIMARY KEY,
		name TEXT NOT NULL,
		status TEXT NOT NULL,
		last_action TEXT,
		bid_value REAL,
		activation_pattern TEXT,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
	)`)
	if err != nil {
		return fmt.Errorf("failed to create knowledge_source table: %v", err)
	}

	// Create activity log table
	_, err = DB.Exec(`
	CREATE TABLE IF NOT EXISTS activity_log (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		time TEXT NOT NULL,
		knowledge_source_id TEXT,
		action TEXT NOT NULL,
		level TEXT NOT NULL,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
		FOREIGN KEY (knowledge_source_id) REFERENCES knowledge_source(id)
	)`)
	if err != nil {
		return fmt.Errorf("failed to create activity_log table: %v", err)
	}

	// Create focus control table
	_, err = DB.Exec(`
	CREATE TABLE IF NOT EXISTS focus_control (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		current_focus TEXT NOT NULL,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
	)`)
	if err != nil {
		return fmt.Errorf("failed to create focus_control table: %v", err)
	}

	// Create focus priority table
	_, err = DB.Exec(`
	CREATE TABLE IF NOT EXISTS focus_priority (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		level TEXT NOT NULL,
		priority REAL NOT NULL,
		focus_control_id INTEGER,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
		FOREIGN KEY (focus_control_id) REFERENCES focus_control(id)
	)`)
	if err != nil {
		return fmt.Errorf("failed to create focus_priority table: %v", err)
	}

	// Create bid queue table
	_, err = DB.Exec(`
	CREATE TABLE IF NOT EXISTS bid_queue (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		knowledge_source_id TEXT,
		bid_value REAL NOT NULL,
		target TEXT,
		action TEXT,
		created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
		FOREIGN KEY (knowledge_source_id) REFERENCES knowledge_source(id)
	)`)
	if err != nil {
		return fmt.Errorf("failed to create bid_queue table: %v", err)
	}

	log.Println("All tables created successfully")
	return nil
}

// CloseDB closes the database connection
func CloseDB() error {
	if DB != nil {
		return DB.Close()
	}
	return nil
}
