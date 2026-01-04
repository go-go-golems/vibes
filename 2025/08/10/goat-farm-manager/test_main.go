package main

import (
	"context"
	"log"

	"github.com/farm/goat-manager/internal/database"
)

func main() {
	ctx := context.Background()
	
	log.Println("Testing Goat Farm Manager Database Connection...")
	
	// Test database connection
	if err := database.TestDatabaseConnection(); err != nil {
		log.Fatalf("Database test failed: %v", err)
	}
	
	log.Println("All tests passed!")
}

