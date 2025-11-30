package main

import (
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func buildSocialGraph(store *cayley.Handle) {
	// Alice knows Bob
	store.AddQuad(quad.Make("Alice", "knows", "Bob", nil))
	
	// Alice is 25 years old
	store.AddQuad(quad.Make("Alice", "age", 25, nil))
	
	// Bob knows Charlie
	store.AddQuad(quad.Make("Bob", "knows", "Charlie", nil))
	
	// Bob is a developer
	store.AddQuad(quad.Make("Bob", "occupation", "developer", nil))
	
	// Charlie is 30 years old
	store.AddQuad(quad.Make("Charlie", "age", 30, nil))
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	buildSocialGraph(store)
	log.Println("Social graph built successfully!")
}
