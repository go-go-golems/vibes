package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func main() {
	// Create a brand new in-memory graph
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}

	// Add the quad {"hello", "says", "world", nil}
	store.AddQuad(quad.Make("hello", "says", "world", nil))

	// Query for the object where subject is "hello" and predicate is "says"
	p := cayley.StartPath(store, quad.String("hello")).Out(quad.String("says"))

	// Iterate over results and print them
	err = p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		nativeValue := quad.NativeOf(value)
		fmt.Println(nativeValue)
 	return nil
	})
	if err != nil {
		log.Fatalf("Failed to iterate results: %v", err)
	}
}
