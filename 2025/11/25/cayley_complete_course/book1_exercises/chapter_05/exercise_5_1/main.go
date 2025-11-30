package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func buildFamilyTree(store *cayley.Handle) {
	// Parents
	store.AddQuad(quad.Make("John", "parent_of", "Alice", nil))
	store.AddQuad(quad.Make("Mary", "parent_of", "Alice", nil))
	store.AddQuad(quad.Make("John", "parent_of", "Bob", nil))
	store.AddQuad(quad.Make("Mary", "parent_of", "Bob", nil))
	
	// Grandparents
	store.AddQuad(quad.Make("George", "parent_of", "John", nil))
	store.AddQuad(quad.Make("Helen", "parent_of", "John", nil))
	store.AddQuad(quad.Make("Robert", "parent_of", "Mary", nil))
	store.AddQuad(quad.Make("Susan", "parent_of", "Mary", nil))
	
	// Siblings
	store.AddQuad(quad.Make("Alice", "sibling_of", "Bob", nil))
	store.AddQuad(quad.Make("Bob", "sibling_of", "Alice", nil))
	
	// Ages
	store.AddQuad(quad.Make("Alice", "age", 25, nil))
	store.AddQuad(quad.Make("Bob", "age", 22, nil))
	store.AddQuad(quad.Make("John", "age", 50, nil))
	store.AddQuad(quad.Make("Mary", "age", 48, nil))
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}

	buildFamilyTree(store)
	
	fmt.Println("=== Path API Traversal Exercise ===\n")

	// Exercise 1: Out() - Find children of John
	fmt.Println("1. Children of John (using Out):")
	p1 := cayley.StartPath(store, quad.String("John")).Out(quad.String("parent_of"))
	
	err = p1.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   - %v\n", quad.NativeOf(value))
 	return nil
	return nil
	})
	if err != nil {
		log.Fatalf("Query 1 failed: %v", err)
	}

	// Exercise 2: In() - Find parents of Alice
	fmt.Println("\n2. Parents of Alice (using In):")
	p2 := cayley.StartPath(store, quad.String("Alice")).In(quad.String("parent_of"))
	
	err = p2.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   - %v\n", quad.NativeOf(value))
 	return nil
	return nil
	})
	if err != nil {
		log.Fatalf("Query 2 failed: %v", err)
	}

	// Exercise 3: Both() - Find all sibling relationships
	fmt.Println("\n3. Siblings of Alice (using Both):")
	p3 := cayley.StartPath(store, quad.String("Alice")).Both(quad.String("sibling_of"))
	
	err = p3.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   - %v\n", quad.NativeOf(value))
 	return nil
	return nil
	})
	if err != nil {
		log.Fatalf("Query 3 failed: %v", err)
	}

	// Exercise 4: Multi-hop - Find grandparents of Alice
	fmt.Println("\n4. Grandparents of Alice (multi-hop):")
	p4 := cayley.StartPath(store, quad.String("Alice")).
		In(quad.String("parent_of")).
		In(quad.String("parent_of"))
	
	err = p4.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   - %v\n", quad.NativeOf(value))
 	return nil
	return nil
	})
	if err != nil {
		log.Fatalf("Query 4 failed: %v", err)
	}

	// Exercise 5: Has() - Find people who have an age property
	fmt.Println("\n5. People with age information (using Has):")
	p5 := cayley.StartPath(store).Has(quad.String("age"))
	
	err = p5.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		name := quad.NativeOf(value)
		
		// Get their age
		agePath := cayley.StartPath(store, value).Out(quad.String("age"))
		agePath.Iterate(nil).EachValue(nil, func(ageVal quad.Value) error {
			fmt.Printf("   - %v: %v years old\n", name, quad.NativeOf(ageVal))
		return nil
		})
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 5 failed: %v", err)
	}

	fmt.Println("\n=== Exercise Complete ===")
}
