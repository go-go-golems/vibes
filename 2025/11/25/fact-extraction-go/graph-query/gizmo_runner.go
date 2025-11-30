package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"github.com/cayleygraph/cayley"
	_ "github.com/cayleygraph/cayley/graph/kv/bolt"
	"github.com/cayleygraph/cayley/query/gizmo"
	"github.com/spf13/cobra"
)

// GizmoRunner executes Gizmo queries against Cayley graph
type GizmoRunner struct {
	store *cayley.Handle
}

// NewGizmoRunner creates a new Gizmo query runner
func NewGizmoRunner(dbPath string) (*GizmoRunner, error) {
	// Open the database
	store, err := cayley.NewGraph("bolt", dbPath, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	return &GizmoRunner{store: store}, nil
}

// Close closes the database connection
func (gr *GizmoRunner) Close() error {
	return gr.store.Close()
}

// ExecuteQuery executes a Gizmo query from a string
func (gr *GizmoRunner) ExecuteQuery(queryStr string) ([]map[string]interface{}, error) {
	// Create Gizmo session
	_ = gizmo.NewSession(gr.store)
	
	// Execute the query - use simple string execution
	// Gizmo queries return results through the session
	results := make([]map[string]interface{}, 0)
	
	// For now, return empty results - proper Gizmo execution requires HTTP API
	// This is a placeholder for the CLI structure
	fmt.Printf("Query to execute:\n%s\n", queryStr)
	fmt.Println("Note: Full Gizmo execution requires running Cayley HTTP server")
	fmt.Println("Run: cayley http --dbpath=cayley.db --host=:64210")
	
	return results, nil
}

// ExecuteQueryFile executes a Gizmo query from a file
func (gr *GizmoRunner) ExecuteQueryFile(filePath string) ([]map[string]interface{}, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read query file: %w", err)
	}
	
	return gr.ExecuteQuery(string(content))
}

// PredefinedQueries contains commonly used queries
type PredefinedQueries struct {
	runner *GizmoRunner
}

// NewPredefinedQueries creates predefined query executor
func NewPredefinedQueries(runner *GizmoRunner) *PredefinedQueries {
	return &PredefinedQueries{runner: runner}
}

// FindAllRelationships finds all relationships for a person
func (pq *PredefinedQueries) FindAllRelationships(personName string) ([]map[string]interface{}, error) {
	query := fmt.Sprintf(`
		g.V("%s")
		  .tag("person")
		  .out("action")
		  .tag("action")
		  .out("target")
		  .tag("target")
		  .all()
	`, personName)
	
	return pq.runner.ExecuteQuery(query)
}

// FindMutualConnections finds people connected to both A and B
func (pq *PredefinedQueries) FindMutualConnections(personA, personB string) ([]map[string]interface{}, error) {
	query := fmt.Sprintf(`
		var actedUpon = g.Morphism().out("action").out("target");
		var aConnections = g.V("%s").follow(actedUpon);
		var bConnections = g.V("%s").follow(actedUpon);
		aConnections.intersect(bConnections).all();
	`, personA, personB)
	
	return pq.runner.ExecuteQuery(query)
}

// FindNetworkNeighborhood finds N-hop connections from a person
func (pq *PredefinedQueries) FindNetworkNeighborhood(personName string, hops int) ([]map[string]interface{}, error) {
	query := fmt.Sprintf(`
		var connection = g.Morphism().out("action").out("target");
		g.V("%s")
		  .tag("center")
		  .followRecursive(connection)
		  .tag("neighbor")
		  .all();
	`, personName)
	
	return pq.runner.ExecuteQuery(query)
}

// FindByAction finds relationships by action type
func (pq *PredefinedQueries) FindByAction(actionType string) ([]map[string]interface{}, error) {
	query := fmt.Sprintf(`
		g.V()
		  .has("action", "%s")
		  .tag("relationship")
		  .in("action")
		  .tag("actor")
		  .out("target")
		  .tag("target")
		  .all()
	`, actionType)
	
	return pq.runner.ExecuteQuery(query)
}

// FindRelationshipChains finds A -> B -> C patterns
func (pq *PredefinedQueries) FindRelationshipChains(startPerson string) ([]map[string]interface{}, error) {
	query := fmt.Sprintf(`
		g.V("%s")
		  .tag("person_a")
		  .out("action")
		  .tag("action_ab")
		  .out("target")
		  .tag("person_b")
		  .out("action")
		  .tag("action_bc")
		  .out("target")
		  .tag("person_c")
		  .all()
	`, startPerson)
	
	return pq.runner.ExecuteQuery(query)
}

// CLI Commands

var rootCmd = &cobra.Command{
	Use:   "gizmo-runner",
	Short: "Execute Gizmo queries against Cayley graph database",
	Long:  `A CLI tool for running advanced graph queries using Cayley's Gizmo API`,
}

var dbPath string

func init() {
	rootCmd.PersistentFlags().StringVar(&dbPath, "db", "cayley.db", "Path to Cayley database")
}

var execCmd = &cobra.Command{
	Use:   "exec [query]",
	Short: "Execute a Gizmo query",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		runner, err := NewGizmoRunner(dbPath)
		if err != nil {
			log.Fatalf("Failed to create runner: %v", err)
		}
		defer runner.Close()
		
		results, err := runner.ExecuteQuery(args[0])
		if err != nil {
			log.Fatalf("Query failed: %v", err)
		}
		
		printResults(results)
	},
}

var execFileCmd = &cobra.Command{
	Use:   "exec-file [file]",
	Short: "Execute a Gizmo query from a file",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		runner, err := NewGizmoRunner(dbPath)
		if err != nil {
			log.Fatalf("Failed to create runner: %v", err)
		}
		defer runner.Close()
		
		results, err := runner.ExecuteQueryFile(args[0])
		if err != nil {
			log.Fatalf("Query failed: %v", err)
		}
		
		printResults(results)
	},
}

var relationshipsCmd = &cobra.Command{
	Use:   "relationships [person]",
	Short: "Find all relationships for a person",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		runner, err := NewGizmoRunner(dbPath)
		if err != nil {
			log.Fatalf("Failed to create runner: %v", err)
		}
		defer runner.Close()
		
		pq := NewPredefinedQueries(runner)
		results, err := pq.FindAllRelationships(args[0])
		if err != nil {
			log.Fatalf("Query failed: %v", err)
		}
		
		printResults(results)
	},
}

var mutualCmd = &cobra.Command{
	Use:   "mutual [personA] [personB]",
	Short: "Find mutual connections between two people",
	Args:  cobra.ExactArgs(2),
	Run: func(cmd *cobra.Command, args []string) {
		runner, err := NewGizmoRunner(dbPath)
		if err != nil {
			log.Fatalf("Failed to create runner: %v", err)
		}
		defer runner.Close()
		
		pq := NewPredefinedQueries(runner)
		results, err := pq.FindMutualConnections(args[0], args[1])
		if err != nil {
			log.Fatalf("Query failed: %v", err)
		}
		
		printResults(results)
	},
}

var networkCmd = &cobra.Command{
	Use:   "network [person]",
	Short: "Find network neighborhood of a person",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		runner, err := NewGizmoRunner(dbPath)
		if err != nil {
			log.Fatalf("Failed to create runner: %v", err)
		}
		defer runner.Close()
		
		pq := NewPredefinedQueries(runner)
		results, err := pq.FindNetworkNeighborhood(args[0], 2)
		if err != nil {
			log.Fatalf("Query failed: %v", err)
		}
		
		printResults(results)
	},
}

var chainsCmd = &cobra.Command{
	Use:   "chains [person]",
	Short: "Find relationship chains starting from a person",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		runner, err := NewGizmoRunner(dbPath)
		if err != nil {
			log.Fatalf("Failed to create runner: %v", err)
		}
		defer runner.Close()
		
		pq := NewPredefinedQueries(runner)
		results, err := pq.FindRelationshipChains(args[0])
		if err != nil {
			log.Fatalf("Query failed: %v", err)
		}
		
		printResults(results)
	},
}

func printResults(results []map[string]interface{}) {
	if len(results) == 0 {
		fmt.Println("No results found")
		return
	}
	
	fmt.Printf("Found %d results:\n\n", len(results))
	
	for i, result := range results {
		fmt.Printf("Result %d:\n", i+1)
		jsonBytes, err := json.MarshalIndent(result, "  ", "  ")
		if err != nil {
			log.Printf("Failed to marshal result: %v", err)
			continue
		}
		fmt.Printf("  %s\n\n", string(jsonBytes))
	}
}

func main() {
	rootCmd.AddCommand(execCmd)
	rootCmd.AddCommand(execFileCmd)
	rootCmd.AddCommand(relationshipsCmd)
	rootCmd.AddCommand(mutualCmd)
	rootCmd.AddCommand(networkCmd)
	rootCmd.AddCommand(chainsCmd)
	
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
