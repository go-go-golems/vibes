package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"strings"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	_ "github.com/cayleygraph/cayley/graph/kv/bolt"
	"github.com/cayleygraph/quad"
	_ "github.com/mattn/go-sqlite3"
	"github.com/spf13/cobra"
)

var (
	dbPath    string
	graphPath string
	store     *cayley.Handle
)

// RDFTriple represents a relationship from the database
type RDFTriple struct {
	ID             int
	DocID          string
	Timestamp      sql.NullString
	Actor          string
	Action         string
	Target         string
	Location       sql.NullString
	ActorType      sql.NullString
	Tags           string
	ExplicitTopic  string
	ImplicitTopic  string
	SequenceOrder  int
}

func main() {
	rootCmd := &cobra.Command{
		Use:   "graph-query",
		Short: "Query fact extraction graph database",
		Long:  "A CLI tool to query the Cayley graph database built from extracted RDF triples",
	}

	rootCmd.PersistentFlags().StringVarP(&dbPath, "db", "d", "../fact_extraction.db", "Path to SQLite database")
	rootCmd.PersistentFlags().StringVarP(&graphPath, "graph", "g", "facts.db", "Path to Cayley graph database")

	rootCmd.AddCommand(
		loadCmd(),
		queryCmd(),
		pathsCmd(),
		neighborsCmd(),
		statsCmd(),
	)

	if err := rootCmd.Execute(); err != nil {
		log.Fatal(err)
	}
}

func loadCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "load",
		Short: "Load data from SQLite into Cayley graph",
		Run: func(cmd *cobra.Command, args []string) {
			if err := loadDataIntoGraph(); err != nil {
				log.Fatalf("Failed to load data: %v", err)
			}
			fmt.Println("✓ Data loaded successfully into graph database")
		},
	}
}

func queryCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "query [actor]",
		Short: "Query relationships for a specific actor",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			actor := strings.Join(args, " ")
			if err := initGraph(); err != nil {
				log.Fatalf("Failed to open graph: %v", err)
			}
			defer store.Close()

			queryActorRelationships(actor)
		},
	}
	return cmd
}

func pathsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "paths [from] [to]",
		Short: "Find paths between two people",
		Args:  cobra.MinimumNArgs(2),
		Run: func(cmd *cobra.Command, args []string) {
			from := args[0]
			to := args[1]
			if err := initGraph(); err != nil {
				log.Fatalf("Failed to open graph: %v", err)
			}
			defer store.Close()

			findPaths(from, to)
		},
	}
	return cmd
}

func neighborsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "neighbors [person]",
		Short: "Find all direct connections to a person",
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			person := strings.Join(args, " ")
			if err := initGraph(); err != nil {
				log.Fatalf("Failed to open graph: %v", err)
			}
			defer store.Close()

			findNeighbors(person)
		},
	}
	return cmd
}

func statsCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "stats",
		Short: "Show graph statistics",
		Run: func(cmd *cobra.Command, args []string) {
			if err := initGraph(); err != nil {
				log.Fatalf("Failed to open graph: %v", err)
			}
			defer store.Close()

			showStats()
		},
	}
}

func loadDataIntoGraph() error {
	// Open SQLite database
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return fmt.Errorf("failed to open SQLite: %w", err)
	}
	defer db.Close()

	// Initialize Cayley graph
	if err := graph.InitQuadStore("bolt", graphPath, nil); err != nil && err != graph.ErrDatabaseExists {
		return fmt.Errorf("failed to init graph: %w", err)
	}

	store, err := cayley.NewGraph("bolt", graphPath, nil)
	if err != nil {
		return fmt.Errorf("failed to open graph: %w", err)
	}
	defer store.Close()

	// Query all triples
	rows, err := db.Query(`
		SELECT id, doc_id, timestamp, actor, action, target, location, 
		       actor_likely_type, triple_tags, explicit_topic, implicit_topic, sequence_order
		FROM rdf_triples
	`)
	if err != nil {
		return fmt.Errorf("failed to query triples: %w", err)
	}
	defer rows.Close()

	// Load into graph
	count := 0
	var quads []quad.Quad

	for rows.Next() {
		var t RDFTriple
		if err := rows.Scan(&t.ID, &t.DocID, &t.Timestamp, &t.Actor, &t.Action,
			&t.Target, &t.Location, &t.ActorType, &t.Tags, &t.ExplicitTopic,
			&t.ImplicitTopic, &t.SequenceOrder); err != nil {
			return fmt.Errorf("failed to scan row: %w", err)
		}

		// Create main relationship quad
		quads = append(quads, quad.Make(
			t.Actor,
			t.Action,
			t.Target,
			fmt.Sprintf("triple:%d", t.ID),
		))

		// Add metadata as additional quads
		tripleNode := quad.IRI(fmt.Sprintf("triple:%d", t.ID))

		quads = append(quads, quad.Make(tripleNode, "doc_id", t.DocID, nil))
		quads = append(quads, quad.Make(tripleNode, "explicit_topic", t.ExplicitTopic, nil))
		quads = append(quads, quad.Make(tripleNode, "implicit_topic", t.ImplicitTopic, nil))

		if t.Timestamp.Valid {
			quads = append(quads, quad.Make(tripleNode, "timestamp", t.Timestamp.String, nil))
		}

		if t.Location.Valid {
			quads = append(quads, quad.Make(tripleNode, "location", t.Location.String, nil))
		}

		// Parse and add tags
		if t.Tags != "" {
			var tags []string
			if err := json.Unmarshal([]byte(t.Tags), &tags); err == nil {
				for _, tag := range tags {
					quads = append(quads, quad.Make(tripleNode, "tag", tag, nil))
				}
			}
		}

		count++
		if count%100 == 0 {
			if err := store.AddQuadSet(quads); err != nil {
				return fmt.Errorf("failed to add quads: %w", err)
			}
			quads = quads[:0]
			fmt.Printf("Loaded %d triples...\n", count)
		}
	}

	// Add remaining quads
	if len(quads) > 0 {
		if err := store.AddQuadSet(quads); err != nil {
			return fmt.Errorf("failed to add final quads: %w", err)
		}
	}

	fmt.Printf("✓ Loaded %d triples into graph\n", count)
	return nil
}

func initGraph() error {
	var err error
	store, err = cayley.NewGraph("bolt", graphPath, nil)
	return err
}

func queryActorRelationships(actor string) {
	ctx := context.Background()
	p := cayley.StartPath(store, quad.String(actor)).Out()

	fmt.Printf("\n🔍 Relationships for: %s\n", actor)
	fmt.Println(strings.Repeat("=", 80))

	count := 0
	err := p.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		// Get the predicate (action) and object (target)
		it := store.QuadIterator(quad.Subject, store.ValueOf(quad.String(actor)))
		defer it.Close()

		for it.Next(ctx) {
			q := store.Quad(it.Result())
			if q.Object.String() == value.String() {
				count++
				fmt.Printf("%d. %s → [%s] → %s\n", count, actor, q.Predicate, q.Object)

				// Get metadata
				label := q.Label
				if label != nil {
					printTripleMetadata(ctx, label)
				}
				fmt.Println()
			}
		}
	})

	if err != nil {
		log.Printf("Error iterating: %v", err)
	}

	if count == 0 {
		fmt.Println("No relationships found")
	}
}

func findNeighbors(person string) {
	ctx := context.Background()

	fmt.Printf("\n👥 Direct connections to: %s\n", person)
	fmt.Println(strings.Repeat("=", 80))

	// Find outgoing relationships
	fmt.Println("\n📤 Outgoing relationships:")
	outPath := cayley.StartPath(store, quad.String(person)).Out()
	count := 0
	outPath.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		count++
		fmt.Printf("  %d. %s\n", count, value)
	})

	// Find incoming relationships
	fmt.Println("\n📥 Incoming relationships:")
	inPath := cayley.StartPath(store, quad.String(person)).In()
	count = 0
	inPath.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		count++
		fmt.Printf("  %d. %s\n", count, value)
	})
}

func findPaths(from, to string) {
	ctx := context.Background()

	fmt.Printf("\n🛤️  Finding paths from '%s' to '%s'\n", from, to)
	fmt.Println(strings.Repeat("=", 80))

	// Try to find paths up to 3 hops
	for hops := 1; hops <= 3; hops++ {
		fmt.Printf("\nSearching %d-hop paths...\n", hops)

		p := cayley.StartPath(store, quad.String(from))
		for i := 0; i < hops; i++ {
			p = p.Out()
		}
		p = p.Has(quad.IRI("id"), quad.String(to))

		found := false
			p.Iterate(ctx).EachValue(nil, func(value quad.Value) {
				found = true
				fmt.Printf("  Found: %s\n", value)
			})

		if found {
			break
		}
	}
}

func printTripleMetadata(ctx context.Context, label quad.Value) {
	// Query metadata about this triple
	metaPath := cayley.StartPath(store, label).Out()
	metaPath.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		it := store.QuadIterator(quad.Subject, store.ValueOf(label))
		defer it.Close()

		for it.Next(ctx) {
			q := store.Quad(it.Result())
			pred := q.Predicate.String()
			if pred != "id" {
				fmt.Printf("     %s: %s\n", pred, q.Object)
			}
		}
	})
}

func showStats() {
	ctx := context.Background()

	fmt.Println("\n📊 Graph Database Statistics")
	fmt.Println(strings.Repeat("=", 80))

	// Count total quads
	it := store.QuadsAllIterator()
	defer it.Close()

	quadCount := 0
	for it.Next(ctx) {
		quadCount++
	}

	fmt.Printf("Total quads: %d\n", quadCount)

	// Count unique subjects (actors)
	subjects := make(map[string]bool)
	it = store.QuadsAllIterator()
	for it.Next(ctx) {
		q := store.Quad(it.Result())
		subjects[q.Subject.String()] = true
	}
	it.Close()

	fmt.Printf("Unique subjects (actors): %d\n", len(subjects))

	// Count unique predicates (actions)
	predicates := make(map[string]int)
	it = store.QuadsAllIterator()
	for it.Next(ctx) {
		q := store.Quad(it.Result())
		predicates[q.Predicate.String()]++
	}
	it.Close()

	fmt.Printf("Unique predicates (actions): %d\n", len(predicates))

	fmt.Println("\nTop 10 predicates:")
	// Sort and display top predicates
	type kv struct {
		Key   string
		Value int
	}
	var sorted []kv
	for k, v := range predicates {
		sorted = append(sorted, kv{k, v})
	}

	// Simple bubble sort
	for i := 0; i < len(sorted); i++ {
		for j := i + 1; j < len(sorted); j++ {
			if sorted[j].Value > sorted[i].Value {
				sorted[i], sorted[j] = sorted[j], sorted[i]
			}
		}
	}

	for i := 0; i < 10 && i < len(sorted); i++ {
		fmt.Printf("  %d. %s: %d\n", i+1, sorted[i].Key, sorted[i].Value)
	}
}
