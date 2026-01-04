package main

import (
	"log"
	"path/filepath"

	"github.com/blackboard-system/internal/database"
)

func main() {
	// Set up database
	dbPath := filepath.Join(".", "data", "blackboard.db")
	log.Printf("Initializing database at %s", dbPath)
	
	if err := database.InitDB(dbPath); err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	defer database.CloseDB()
	
	// Initialize default data
	if err := database.InitializeDefaultData(); err != nil {
		log.Fatalf("Failed to initialize default data: %v", err)
	}
	
	log.Println("Database setup completed successfully")
}
