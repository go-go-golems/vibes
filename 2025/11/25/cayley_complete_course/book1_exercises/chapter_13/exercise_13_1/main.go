package main

import (
	"fmt"
	"log"
	"time"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc"
)

func init() {
	// Register our custom blackboard namespace
	voc.RegisterPrefix("bb:", "http://blackboard.example.org/")
}

// registerAgent adds an agent to the blackboard
func registerAgent(store *cayley.Handle, agentID, name string, expertise []string) {
	timestamp := time.Now().Format(time.RFC3339)
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(agentID, "rdf:type", "bb:Agent", timestamp))
	t.AddQuad(quad.Make(agentID, "bb:name", name, timestamp))
	t.AddQuad(quad.Make(agentID, "bb:status", "active", timestamp))
	
	for _, exp := range expertise {
		t.AddQuad(quad.Make(agentID, "bb:expertise", exp, timestamp))
	}
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to register agent: %v", err)
	}
	
	fmt.Printf("Agent registered: %s (%s)\n", name, agentID)
}

// postTask adds a task to the blackboard
func postTask(store *cayley.Handle, taskID, description string, requiredExpertise string, priority int) {
	timestamp := time.Now().Format(time.RFC3339)
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(taskID, "rdf:type", "bb:Task", timestamp))
	t.AddQuad(quad.Make(taskID, "bb:description", description, timestamp))
	t.AddQuad(quad.Make(taskID, "bb:status", "pending", timestamp))
	t.AddQuad(quad.Make(taskID, "bb:requiredExpertise", requiredExpertise, timestamp))
	t.AddQuad(quad.Make(taskID, "bb:priority", priority, timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to post task: %v", err)
	}
	
	fmt.Printf("Task posted: %s (requires: %s)\n", taskID, requiredExpertise)
}

// findMatchingAgent finds an agent with the required expertise
func findMatchingAgent(store *cayley.Handle, requiredExpertise string) string {
	p := cayley.StartPath(store).
		Has(quad.IRI("rdf:type"), quad.IRI("bb:Agent")).
		Has(quad.IRI("bb:expertise"), quad.String(requiredExpertise)).
		Has(quad.IRI("bb:status"), quad.String("active"))
	
	var agentID string
	p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		if agentID == "" {
			agentID = value.Native().(string)
		}
 	return nil
	return nil
	})
	
	return agentID
}

// assignTask assigns a task to an agent
func assignTask(store *cayley.Handle, taskID, agentID string) {
	timestamp := time.Now().Format(time.RFC3339)
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(taskID, "bb:status", "assigned", timestamp))
	t.AddQuad(quad.Make(taskID, "bb:assignedTo", agentID, timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to assign task: %v", err)
	}
	
	fmt.Printf("Task %s assigned to agent %s\n", taskID, agentID)
}

// processTask simulates an agent processing a task
func processTask(store *cayley.Handle, taskID string) {
	timestamp := time.Now().Format(time.RFC3339)
	
	// Update status to in_progress
	t1 := cayley.NewTransaction()
	t1.AddQuad(quad.Make(taskID, "bb:status", "in_progress", timestamp))
	
	if err := store.ApplyTransaction(t1); err != nil {
		log.Fatalf("Failed to update task status: %v", err)
	}
	
	fmt.Printf("Processing task: %s\n", taskID)
	
	// Simulate work
	time.Sleep(100 * time.Millisecond)
	
	// Complete the task
	timestamp = time.Now().Format(time.RFC3339)
	t2 := cayley.NewTransaction()
	t2.AddQuad(quad.Make(taskID, "bb:status", "completed", timestamp))
	t2.AddQuad(quad.Make(taskID, "bb:completedAt", timestamp, timestamp))
	
	if err := store.ApplyTransaction(t2); err != nil {
		log.Fatalf("Failed to complete task: %v", err)
	}
	
	fmt.Printf("Task completed: %s\n", taskID)
}

// coordinator manages task assignment
func coordinator(store *cayley.Handle) {
	fmt.Println("\n=== Coordinator starting task assignment ===")
	
	// Find all pending tasks
	p := cayley.StartPath(store).
		Has(quad.IRI("rdf:type"), quad.IRI("bb:Task")).
		Has(quad.IRI("bb:status"), quad.String("pending"))
	
	var tasks []struct {
		id        string
		expertise string
	}
	
	// Collect tasks
	p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		taskID := value.Native().(string)
		
		// Get required expertise for this task
		expPath := cayley.StartPath(store, quad.String(taskID)).
			Out(quad.IRI("bb:requiredExpertise"))
		
		expPath.Iterate(nil).EachValue(nil, func(expValue quad.Value) error {
			tasks = append(tasks, struct {
				id        string
				expertise string
			}{taskID, expValue.Native().(string)})
		return nil
		})
 	return nil
	})
	
	// Assign tasks to matching agents
	for _, task := range tasks {
		agentID := findMatchingAgent(store, task.expertise)
		if agentID != "" {
			assignTask(store, task.id, agentID)
			processTask(store, task.id)
		} else {
			fmt.Printf("No agent found for task %s (requires: %s)\n", task.id, task.expertise)
		}
	}
}

func main() {
	// Create the blackboard (graph store)
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	
	fmt.Println("=== Blackboard System Initialized ===\n")
	
	// Register agents with different expertise
	registerAgent(store, "agent:nlp1", "NLP Agent", []string{"nlp", "text_processing"})
	registerAgent(store, "agent:vision1", "Vision Agent", []string{"image_processing", "object_detection"})
	registerAgent(store, "agent:data1", "Data Agent", []string{"data_analysis", "statistics"})
	
	fmt.Println()
	
	// Post tasks requiring different expertise
	postTask(store, "task:001", "Extract entities from document", "nlp", 1)
	postTask(store, "task:002", "Detect objects in image", "image_processing", 2)
	postTask(store, "task:003", "Analyze sales data", "data_analysis", 1)
	
	// Run the coordinator to assign and process tasks
	coordinator(store)
	
	fmt.Println("\n=== Blackboard System Complete ===")
}
