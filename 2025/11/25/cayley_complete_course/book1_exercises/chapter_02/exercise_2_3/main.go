package main

import (
	"fmt"
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

func querySocialGraph(store *cayley.Handle) {
	// Query 1: Who does Alice know?
	fmt.Println("1. Who does Alice know?")
	p1 := cayley.StartPath(store, quad.String("Alice")).Out(quad.String("knows"))
	err := p1.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   Alice knows: %v\n", quad.NativeOf(value))
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 1 failed: %v", err)
	}
	
	// Query 2: What is Bob's occupation?
	fmt.Println("\n2. What is Bob's occupation?")
	p2 := cayley.StartPath(store, quad.String("Bob")).Out(quad.String("occupation"))
	err = p2.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   Bob's occupation: %v\n", quad.NativeOf(value))
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 2 failed: %v", err)
	}
	
	// Query 3: Who are the friends of Alice's friends?
	fmt.Println("\n3. Who are the friends of Alice's friends?")
	p3 := cayley.StartPath(store, quad.String("Alice")).
		Out(quad.String("knows")).
		Out(quad.String("knows"))
	err = p3.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		fmt.Printf("   Friend of Alice's friend: %v\n", quad.NativeOf(value))
 	return nil
	})
	if err != nil {
		log.Fatalf("Query 3 failed: %v", err)
	}
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	buildSocialGraph(store)
	querySocialGraph(store)
}
