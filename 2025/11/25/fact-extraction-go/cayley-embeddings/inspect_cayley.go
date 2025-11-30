package main

import (
	"context"
	"fmt"
	"log"
	
	"github.com/cayleygraph/cayley"
	_ "github.com/cayleygraph/cayley/graph/kv/bolt"
)

func main() {
	ctx := context.Background()
	
	// Open database
	store, err := cayley.NewGraph("bolt", "cayley_facts.db", nil)
	if err != nil {
		log.Fatalf("Failed to open database: %v", err)
	}
	defer store.Close()
	
	// Get all quads
	fmt.Println("=== All Quads in Database ===\n")
	
	it := store.QuadsAllIterator()
	scanner := it.Iterate()
	defer scanner.Close()
	
	count := 0
	subjects := make(map[string]int)
	
	for scanner.Next(ctx) && count < 50 {
		q, _ := store.Quad(scanner.Result())
		fmt.Printf("%d. %v\n", count+1, q)
		
		// Track unique subjects
		subj := fmt.Sprintf("%v", q.Subject)
		subjects[subj]++
		
		count++
	}
	
	fmt.Printf("\n=== Total quads shown: %d ===\n", count)
	fmt.Printf("\n=== Unique Subjects (first 20) ===\n")
	
	i := 0
	for subj, cnt := range subjects {
		if i >= 20 {
			break
		}
		fmt.Printf("%d. %q (count: %d)\n", i+1, subj, cnt)
		i++
	}
}
