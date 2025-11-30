package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc"
)

func init() {
	// Register custom namespaces for our movie domain
	voc.RegisterPrefix("movie:", "http://example.org/movies/")
	voc.RegisterPrefix("person:", "http://example.org/people/")
	voc.RegisterPrefix("vocab:", "http://example.org/vocab/")
}

func main() {
	// Create in-memory graph
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}

	fmt.Println("=== Movie Database with IRIs ===\n")

	// Add movies with proper IRIs
	store.AddQuad(quad.Make(
		quad.IRI("movie:inception"),
		quad.IRI("rdf:type"),
		quad.IRI("vocab:Movie"),
		nil,
	))
	
	store.AddQuad(quad.Make(
		quad.IRI("movie:inception"),
		quad.IRI("vocab:title"),
		quad.String("Inception"),
		nil,
	))
	
	store.AddQuad(quad.Make(
		quad.IRI("movie:inception"),
		quad.IRI("vocab:year"),
		quad.Int(2010),
		nil,
	))
	
	store.AddQuad(quad.Make(
		quad.IRI("movie:inception"),
		quad.IRI("vocab:director"),
		quad.IRI("person:nolan"),
		nil,
	))

	// Add director information
	store.AddQuad(quad.Make(
		quad.IRI("person:nolan"),
		quad.IRI("rdf:type"),
		quad.IRI("vocab:Person"),
		nil,
	))
	
	store.AddQuad(quad.Make(
		quad.IRI("person:nolan"),
		quad.IRI("vocab:name"),
		quad.String("Christopher Nolan"),
		nil,
	))

	fmt.Println("Movies added to graph:")
	
	// Query for all movies
	p := cayley.StartPath(store).
		Has(quad.IRI("rdf:type"), quad.IRI("vocab:Movie"))
	
	err = p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		movieID := quad.NativeOf(value)
		fmt.Printf("  Movie IRI: %v\n", movieID)
		
		// Get title
		titlePath := cayley.StartPath(store, value).
			Out(quad.IRI("vocab:title"))
		
		titlePath.Iterate(nil).EachValue(nil, func(titleVal quad.Value) error {
			fmt.Printf("    Title: %s\n", quad.NativeOf(titleVal))
		return nil
		})
		
		// Get year
		yearPath := cayley.StartPath(store, value).
			Out(quad.IRI("vocab:year"))
		
		yearPath.Iterate(nil).EachValue(nil, func(yearVal quad.Value) error {
			fmt.Printf("    Year: %v\n", quad.NativeOf(yearVal))
		return nil
		})
		
		// Get director
		dirPath := cayley.StartPath(store, value).
			Out(quad.IRI("vocab:director")).
			Out(quad.IRI("vocab:name"))
		
		dirPath.Iterate(nil).EachValue(nil, func(dirVal quad.Value) error {
			fmt.Printf("    Director: %s\n", quad.NativeOf(dirVal))
		return nil
		})
		
		fmt.Println()
 	return nil
	})
	
	if err != nil {
		log.Fatalf("Query failed: %v", err)
	}

	fmt.Println("=== Exercise Complete ===")
}
