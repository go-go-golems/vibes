package database

import (
	"context"
	"fmt"
	"log"

	"github.com/farm/goat-manager/ent"
)

// TestDatabaseConnection tests the database connection and basic operations
func TestDatabaseConnection() error {
	ctx := context.Background()
	
	// Use default configuration
	config := DefaultConfig()
	
	// Open database connection
	client, err := OpenEnt(ctx, config)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer client.Close()

	// Initialize database (create tables)
	if err := InitializeDatabase(ctx, client); err != nil {
		return fmt.Errorf("failed to initialize database: %w", err)
	}

	log.Println("Database connection and initialization successful!")

	// Test creating a goat
	goat, err := client.Goat.
		Create().
		SetID("TEST001").
		SetName("Test Goat").
		SetBreed("alpine").
		SetGender("female").
		SetStatus("active").
		Save(ctx)
	if err != nil {
		return fmt.Errorf("failed to create test goat: %w", err)
	}

	log.Printf("Created test goat: %s (%s)", goat.Name, goat.ID)

	// Commit the test data
	if err := CommitChanges(ctx, client, "Added test goat"); err != nil {
		return fmt.Errorf("failed to commit test data: %w", err)
	}

	log.Println("Test data committed successfully!")

	// Test querying
	goats, err := client.Goat.Query().All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query goats: %w", err)
	}

	log.Printf("Found %d goats in database", len(goats))

	return nil
}

