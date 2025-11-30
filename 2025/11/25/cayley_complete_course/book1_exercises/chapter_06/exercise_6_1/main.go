package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func buildOrgChart(store *cayley.Handle) {
	// CEO
	store.AddQuad(quad.Make("John", "title", "CEO", nil))
	
	// VPs report to CEO
	store.AddQuad(quad.Make("Alice", "reports_to", "John", nil))
	store.AddQuad(quad.Make("Alice", "title", "VP Engineering", nil))
	
	store.AddQuad(quad.Make("Bob", "reports_to", "John", nil))
	store.AddQuad(quad.Make("Bob", "title", "VP Sales", nil))
	
	// Managers report to VPs
	store.AddQuad(quad.Make("Charlie", "reports_to", "Alice", nil))
	store.AddQuad(quad.Make("Charlie", "title", "Engineering Manager", nil))
	
	store.AddQuad(quad.Make("Diana", "reports_to", "Alice", nil))
	store.AddQuad(quad.Make("Diana", "title", "Product Manager", nil))
	
	// Engineers report to Charlie
	store.AddQuad(quad.Make("Eve", "reports_to", "Charlie", nil))
	store.AddQuad(quad.Make("Eve", "title", "Senior Engineer", nil))
	
	store.AddQuad(quad.Make("Frank", "reports_to", "Charlie", nil))
	store.AddQuad(quad.Make("Frank", "title", "Junior Engineer", nil))
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}

	buildOrgChart(store)
	
	fmt.Println("=== Morphisms and Advanced Traversal Exercise ===\n")

	// Exercise 1: Create a morphism to find direct reports
	fmt.Println("1. Direct reports of Alice (using morphism):")
	var hasDirectReport = cayley.Morphism().In(quad.String("reports_to"))
	
	p1 := cayley.StartPath(store, quad.String("Alice")).Follow(hasDirectReport)
	
	err = p1.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		name := quad.NativeOf(value)
		// Get their title
		titlePath := cayley.StartPath(store, value).Out(quad.String("title"))
		titlePath.Iterate(nil).EachValue(nil, func(titleVal quad.Value) error {
			fmt.Printf("   - %v (%v)\n", name, quad.NativeOf(titleVal))
		})
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 1 failed: %v", err)
	}

	// Exercise 2: Find all people in Alice's reporting chain (recursive)
	fmt.Println("\n2. Everyone in Alice's reporting chain (recursive):")
	p2 := cayley.StartPath(store, quad.String("Alice")).FollowRecursive(hasDirectReport)
	
	err = p2.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		name := quad.NativeOf(value)
		titlePath := cayley.StartPath(store, value).Out(quad.String("title"))
		titlePath.Iterate(nil).EachValue(nil, func(titleVal quad.Value) error {
			fmt.Printf("   - %v (%v)\n", name, quad.NativeOf(titleVal))
		})
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 2 failed: %v", err)
	}

	// Exercise 3: Find Eve's entire management chain (using Back)
	fmt.Println("\n3. Eve's management chain (using Back and Tags):")
	
	// Start from Eve and go up the chain
	var reportsTo = cayley.Morphism().Out(quad.String("reports_to"))
	p3 := cayley.StartPath(store, quad.String("Eve")).FollowRecursive(reportsTo)
	
	err = p3.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		name := quad.NativeOf(value)
		titlePath := cayley.StartPath(store, value).Out(quad.String("title"))
		titlePath.Iterate(nil).EachValue(nil, func(titleVal quad.Value) error {
			fmt.Printf("   - %v (%v)\n", name, quad.NativeOf(titleVal))
		})
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 3 failed: %v", err)
	}

	fmt.Println("\n=== Exercise Complete ===")
}
