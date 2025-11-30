package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	
	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	_ "github.com/cayleygraph/cayley/graph/kv/bolt"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/nquads"
)

func main() {
	dbPath := flag.String("db", "cayley_facts.db", "Path to Cayley database")
	nquadsPath := flag.String("nq", "facts.nq", "Path to N-Quads file")
	flag.Parse()
	
	_ = context.Background() // For future use
	
	// Initialize database
	fmt.Printf("Initializing database: %s\n", *dbPath)
	err := graph.InitQuadStore("bolt", *dbPath, nil)
	if err != nil && err != graph.ErrDatabaseExists {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	
	// Open database
	store, err := cayley.NewGraph("bolt", *dbPath, nil)
	if err != nil {
		log.Fatalf("Failed to open database: %v", err)
	}
	defer store.Close()
	
	// Load N-Quads
	fmt.Printf("Loading N-Quads from: %s\n", *nquadsPath)
	file, err := os.Open(*nquadsPath)
	if err != nil {
		log.Fatalf("Failed to open N-Quads file: %v", err)
	}
	defer file.Close()
	
	scanner := bufio.NewScanner(file)
	count := 0
	batch := make([]quad.Quad, 0, 1000)
	
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" || line[0] == '#' {
			continue
		}
		
		q, err := nquads.Parse(line)
		if err != nil {
			log.Printf("Warning: failed to parse line: %s (error: %v)", line, err)
			continue
		}
		
		batch = append(batch, q)
		count++
		
		// Write in batches of 1000
		if len(batch) >= 1000 {
			if err := store.AddQuadSet(batch); err != nil {
				log.Fatalf("Failed to add quads: %v", err)
			}
			batch = batch[:0]
			fmt.Printf("Loaded %d quads...\n", count)
		}
	}
	
	// Write remaining quads
	if len(batch) > 0 {
		if err := store.AddQuadSet(batch); err != nil {
			log.Fatalf("Failed to add quads: %v", err)
		}
	}
	
	if err := scanner.Err(); err != nil {
		log.Fatalf("Error reading file: %v", err)
	}
	
	fmt.Printf("\nSuccessfully loaded %d quads into %s\n", count, *dbPath)
}
