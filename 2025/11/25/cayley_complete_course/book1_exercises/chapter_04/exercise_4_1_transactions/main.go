package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}

	fmt.Println("=== Transaction Exercise ===\n")

	// Demonstrate atomic updates with transactions
	fmt.Println("Adding user profile atomically...")
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make("user:alice", "rdf:type", "User", nil))
	t.AddQuad(quad.Make("user:alice", "name", "Alice Smith", nil))
	t.AddQuad(quad.Make("user:alice", "email", "alice@example.com", nil))
	t.AddQuad(quad.Make("user:alice", "age", 28, nil))
	
	err = store.ApplyTransaction(t)
	if err != nil {
		log.Fatalf("Transaction failed: %v", err)
	}
	
	fmt.Println("Transaction committed successfully!")
	
	// Query the data
	fmt.Println("\nUser profile:")
	p := cayley.StartPath(store, quad.String("user:alice"))
	
	// Get all properties
	properties := map[string]interface{}{}
	
	// Get name
	namePath := p.Out(quad.String("name"))
	namePath.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		properties["name"] = quad.NativeOf(value)
 	return nil
	})
	
	// Get email
	emailPath := cayley.StartPath(store, quad.String("user:alice")).Out(quad.String("email"))
	emailPath.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		properties["email"] = quad.NativeOf(value)
 	return nil
	})
	
	// Get age
	agePath := cayley.StartPath(store, quad.String("user:alice")).Out(quad.String("age"))
	agePath.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		properties["age"] = quad.NativeOf(value)
 	return nil
	})
	
	for key, value := range properties {
		fmt.Printf("  %s: %v\n", key, value)
	}
	
	// Update using transaction
	fmt.Println("\nUpdating age atomically...")
	t2 := cayley.NewTransaction()
	t2.AddQuad(quad.Make("user:alice", "age", 29, nil))
	
	err = store.ApplyTransaction(t2)
	if err != nil {
		log.Fatalf("Update transaction failed: %v", err)
	}
	
	// Query updated age
	fmt.Println("\nUpdated age:")
	agePath2 := cayley.StartPath(store, quad.String("user:alice")).Out(quad.String("age"))
	agePath2.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("  age: %v\n", quad.NativeOf(value))
 	return nil
	})
	
	fmt.Println("\n=== Exercise Complete ===")
}
