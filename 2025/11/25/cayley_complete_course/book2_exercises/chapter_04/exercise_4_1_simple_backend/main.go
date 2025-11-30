package main

import (
	"context"
	"fmt"
	"log"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
)

func main() {
	fmt.Println("=== Simple Backend Exercise ===\n")

	// Create our custom backend
	backend := NewSimpleBackend()
	defer backend.Close()

	ctx := context.Background()

	// Test 1: Add some quads
	fmt.Println("1. Adding quads...")
	quads := []quad.Quad{
		quad.Make("alice", "knows", "bob", nil),
		quad.Make("bob", "knows", "charlie", nil),
		quad.Make("alice", "age", 30, nil),
	}

	deltas := make([]graph.Delta, len(quads))
	for i, q := range quads {
		deltas[i] = graph.Delta{Quad: q, Action: graph.Add}
	}

	err := backend.ApplyDeltas(deltas, graph.IgnoreOpts{})
	if err != nil {
		log.Fatalf("Failed to add quads: %v", err)
	}
	fmt.Println("   ✓ Added 3 quads")

	// Test 2: Get stats
	fmt.Println("\n2. Getting statistics...")
	stats, err := backend.Stats(ctx, true)
	if err != nil {
		log.Fatalf("Failed to get stats: %v", err)
	}
	fmt.Printf("   Nodes: %d, Quads: %d\n", stats.Nodes.Value, stats.Quads.Value)

	// Test 3: Query by subject
	fmt.Println("\n3. Querying for all quads with subject 'alice'...")
	aliceRef, err := backend.ValueOf(quad.String("alice"))
	if err != nil {
		log.Fatalf("Failed to get ref for alice: %v", err)
	}

	shape := backend.QuadIterator(quad.Subject, aliceRef)
	it := shape.Iterate()
	defer it.Close()

	count := 0
	for it.Next(ctx) {
		quadRef := it.Result()
		q, err := backend.Quad(quadRef)
		if err != nil {
			log.Printf("Error getting quad: %v", err)
			continue
		}
		fmt.Printf("   Found: %s %s %s\n", q.Subject, q.Predicate, q.Object)
		count++
	}
	fmt.Printf("   ✓ Found %d quads\n", count)

	// Test 4: Iterate all quads
	fmt.Println("\n4. Iterating all quads...")
	allShape := backend.QuadsAllIterator()
	allIt := allShape.Iterate()
	defer allIt.Close()

	count = 0
	for allIt.Next(ctx) {
		count++
	}
	fmt.Printf("   ✓ Iterated %d quads\n", count)

	// Test 5: Delete a quad
	fmt.Println("\n5. Deleting a quad...")
	err = backend.ApplyDeltas([]graph.Delta{
		{Quad: quads[0], Action: graph.Delete},
	}, graph.IgnoreOpts{})
	if err != nil {
		log.Fatalf("Failed to delete quad: %v", err)
	}

	stats, _ = backend.Stats(ctx, true)
	fmt.Printf("   ✓ Quads after delete: %d\n", stats.Quads.Value)

	fmt.Println("\n=== Exercise Complete ===")
	fmt.Println("\nYou've successfully implemented a basic QuadStore backend!")
	fmt.Println("Key concepts demonstrated:")
	fmt.Println("  - ValueOf/NameOf for Ref ↔ Value conversion")
	fmt.Println("  - QuadIterator for directional queries")
	fmt.Println("  - ApplyDeltas for transactional updates")
	fmt.Println("  - Custom iterator implementation")
}
