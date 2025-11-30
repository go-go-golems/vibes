package main

import (
	"fmt"
	"log"
	"os"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	_ "github.com/cayleygraph/cayley/graph/kv/bolt"
	"github.com/cayleygraph/quad"
)

func main() {
	dbPath := "/tmp/cayley_exercise_4_1.db"
	
	// Remove existing database for clean test
	os.RemoveAll(dbPath)
	
	fmt.Println("=== BoltDB Persistence Exercise ===\n")
	
	// Initialize the database
	fmt.Println("Initializing BoltDB...")
	err := graph.InitQuadStore("bolt", dbPath, nil)
	if err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	
	// Open the database
	fmt.Println("Opening BoltDB...")
	store, err := cayley.NewGraph("bolt", dbPath, nil)
	if err != nil {
		log.Fatalf("Failed to open database: %v", err)
	}
	
	// Add some data
	fmt.Println("Adding data to persistent store...")
	store.AddQuad(quad.Make("Alice", "likes", "Coffee", nil))
	store.AddQuad(quad.Make("Bob", "likes", "Tea", nil))
	store.AddQuad(quad.Make("Alice", "knows", "Bob", nil))
	
	// Query the data
	fmt.Println("\nQuerying data:")
	p := cayley.StartPath(store, quad.String("Alice")).Out(quad.String("likes"))
	
	err = p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("  Alice likes: %v\n", quad.NativeOf(value))
 	return nil
	})
	if err != nil {
		log.Fatalf("Query failed: %v", err)
	}
	
	// Close the store
	fmt.Println("\nClosing database...")
	store.Close()
	
	// Reopen to verify persistence
	fmt.Println("Reopening database to verify persistence...")
	store2, err := cayley.NewGraph("bolt", dbPath, nil)
	if err != nil {
		log.Fatalf("Failed to reopen database: %v", err)
	}
	defer store2.Close()
	
	// Query again
	fmt.Println("\nQuerying after reopen:")
	p2 := cayley.StartPath(store2, quad.String("Alice")).Out(quad.String("likes"))
	
	err = p2.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("  Alice still likes: %v\n", quad.NativeOf(value))
 	return nil
	})
	if err != nil {
		log.Fatalf("Query failed: %v", err)
	}
	
	fmt.Println("\n=== Data persisted successfully! ===")
	
	// Cleanup
	os.RemoveAll(dbPath)
}
