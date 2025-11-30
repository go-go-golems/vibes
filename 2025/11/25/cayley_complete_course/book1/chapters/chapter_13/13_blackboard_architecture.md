# Chapter 13: Blackboard Architecture Fundamentals

After building a solid foundation in Cayley and knowledge bases, we now turn our attention to one of the most powerful architectural patterns for multi-agent systems: the **blackboard pattern**. This pattern, which originated in artificial intelligence research in the 1970s, provides an elegant solution for coordinating multiple specialized agents to solve complex, ill-defined problems collaboratively. In this chapter, we will explore the fundamentals of blackboard systems and begin implementing one using Cayley as our shared knowledge store.

## What is a Blackboard System?

A blackboard system is an architectural pattern where multiple independent agents, called **knowledge sources**, collaborate by reading from and writing to a shared workspace, the **blackboard**. The metaphor comes from a group of human experts gathered around a physical blackboard, each contributing their specialized knowledge to solve a problem incrementally.

The blackboard pattern is particularly well-suited for problems that are:

*   **Complex and ill-defined**: No single algorithm can solve the entire problem.
*   **Decomposable**: The problem can be broken into subtasks that different specialists can address.
*   **Opportunistic**: Solutions emerge incrementally, with each contribution potentially enabling others.
*   **Collaborative**: Multiple perspectives and expertise areas are needed.

## The Three Core Components

A blackboard system consists of three essential components that work together to enable collaborative problem-solving.

### 1. Knowledge Sources (KSs)

Knowledge sources are independent, specialized modules that encapsulate specific expertise. Each knowledge source is autonomous and knows when it can contribute based on the current state of the blackboard. In a speech recognition system, for example, you might have knowledge sources for phoneme detection, word recognition, grammar checking, and semantic interpretation.

Key characteristics of knowledge sources include:

*   **Autonomy**: Each KS operates independently and makes its own decisions about when to act.
*   **Specialization**: Each KS has expertise in a specific domain or task.
*   **Opportunistic activation**: KSs monitor the blackboard and activate when their preconditions are met.
*   **Partial contribution**: Each KS adds a piece to the solution, not the complete answer.

### 2. The Blackboard

The blackboard is a shared, structured repository that holds the current problem state, partial solutions, hypotheses, and any other information relevant to solving the problem. It serves as the central communication medium for all knowledge sources. The blackboard is typically organized hierarchically or by levels of abstraction, allowing knowledge sources to work at different granularities.

In our Cayley-based implementation, the blackboard will be represented as a quad store, where:

*   **Facts** are represented as quads.
*   **Hypotheses** and **partial solutions** are nodes with associated metadata.
*   **Temporal information** is captured using quad labels.
*   **Relationships** between entities are explicit edges in the graph.

### 3. The Control Shell

The control shell is the orchestrator of the blackboard system. It manages the problem-solving process by deciding which knowledge source should be activated next based on the current state of the blackboard. The control strategy can range from simple (e.g., first-come, first-served) to sophisticated (e.g., opportunistic planning with meta-level reasoning).

The control shell's responsibilities include:

*   **Monitoring the blackboard** for changes.
*   **Selecting knowledge sources** to activate based on their applicability.
*   **Managing conflicts** when multiple KSs want to contribute simultaneously.
*   **Detecting solution completion** or deadlock conditions.

## Event-Driven Updates and Opportunistic Reasoning

One of the defining features of blackboard systems is their **opportunistic** nature. Unlike traditional top-down or bottom-up problem-solving approaches, blackboard systems allow the solution to emerge from wherever progress can be made. This is achieved through an event-driven architecture where knowledge sources react to changes on the blackboard.

When a knowledge source writes new information to the blackboard, it may trigger other knowledge sources whose preconditions are now satisfied. This creates a dynamic, self-organizing problem-solving process that adapts to the evolving state of the solution.

## Designing a Blackboard Schema for Cayley

To implement a blackboard system with Cayley, we need to design a schema that captures the essential elements of our problem domain. Let's define a general-purpose schema that can be adapted to various applications.

### Core Entity Types

We will use RDF types to distinguish different kinds of entities on our blackboard:

```go
type KnowledgeSource struct {
	ID         quad.IRI `json:"@id"`
	Type       string   `quad:"@type > bb:KnowledgeSource"`
	Name       string   `json:"bb:name"`
	Expertise  []string `json:"bb:expertise"`
	Status     string   `json:"bb:status"` // "active", "idle", "disabled"
}

type Hypothesis struct {
	ID         quad.IRI  `json:"@id"`
	Type       string    `quad:"@type > bb:Hypothesis"`
	ProposedBy quad.IRI  `json:"bb:proposedBy"`
	Content    string    `json:"bb:content"`
	Confidence float64   `json:"bb:confidence"`
	Timestamp  time.Time `json:"bb:timestamp"`
}

type Task struct {
	ID          quad.IRI `json:"@id"`
	Type        string   `quad:"@type > bb:Task"`
	Description string   `json:"bb:description"`
	Status      string   `json:"bb:status"` // "pending", "in_progress", "completed"
	AssignedTo  quad.IRI `json:"bb:assignedTo"`
	Priority    int      `json:"bb:priority"`
}

type Fact struct {
	ID        quad.IRI `json:"@id"`
	Type      string   `quad:"@type > bb:Fact"`
	Subject   string   `json:"bb:subject"`
	Predicate string   `json:"bb:predicate"`
	Object    string   `json:"bb:object"`
	Source    quad.IRI `json:"bb:source"` // Which KS asserted this fact
}
```

### Relationships

Beyond the entity types, we need predicates to express relationships:

*   `bb:dependsOn`: A task or hypothesis depends on another.
*   `bb:supports`: A fact or hypothesis supports another hypothesis.
*   `bb:conflicts`: Two hypotheses are in conflict.
*   `bb:supersedes`: A newer fact replaces an older one.

## Implementing Basic Agent Communication

Let's build a simple example where two knowledge sources communicate via the blackboard. We'll create a system where one agent posts a task, and another agent claims and processes it.

```go
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
	// Register our custom namespace
	voc.RegisterPrefix("bb:", "http://blackboard.example.org/")
}

func postTask(store *cayley.Handle, taskID, description string, priority int) {
	timestamp := time.Now().Format(time.RFC3339)
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(taskID, "rdf:type", "bb:Task", timestamp))
	t.AddQuad(quad.Make(taskID, "bb:description", description, timestamp))
	t.AddQuad(quad.Make(taskID, "bb:status", "pending", timestamp))
	t.AddQuad(quad.Make(taskID, "bb:priority", priority, timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to post task: %v", err)
	}
	
	fmt.Printf("Task posted: %s\n", taskID)
}

func claimTask(store *cayley.Handle, agentID string) string {
	// Find pending tasks
	p := cayley.StartPath(store).
		Has(quad.IRI("rdf:type"), quad.IRI("bb:Task")).
		Has(quad.IRI("bb:status"), quad.String("pending"))
	
	var taskID string
	p.Iterate(nil).EachValue(nil, func(value quad.Value) {
		if taskID == "" {
			taskID = value.Native().(string)
		}
	})
	
	if taskID == "" {
		return ""
	}
	
	// Claim the task
	timestamp := time.Now().Format(time.RFC3339)
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(taskID, "bb:status", "in_progress", timestamp))
	t.AddQuad(quad.Make(taskID, "bb:assignedTo", agentID, timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to claim task: %v", err)
	}
	
	fmt.Printf("Agent %s claimed task: %s\n", agentID, taskID)
	return taskID
}

func completeTask(store *cayley.Handle, taskID string) {
	timestamp := time.Now().Format(time.RFC3339)
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(taskID, "bb:status", "completed", timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to complete task: %v", err)
	}
	
	fmt.Printf("Task completed: %s\n", taskID)
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	
	// Agent 1 posts a task
	postTask(store, "task:001", "Analyze data from sensor A", 1)
	
	// Agent 2 claims the task
	taskID := claimTask(store, "agent:worker1")
	
	// Simulate work
	time.Sleep(100 * time.Millisecond)
	
	// Agent 2 completes the task
	if taskID != "" {
		completeTask(store, taskID)
	}
}
```

This example demonstrates the fundamental pattern of blackboard communication: agents post information (tasks), other agents monitor the blackboard for opportunities (pending tasks), and they update the shared state as they make progress.

In the exercises for this chapter, you will expand on this foundation to build a more sophisticated blackboard system with multiple agents, conflict resolution, and coordination mechanisms.

---

### References

[1] Erman, L. D., et al. "The Hearsay-II Speech-Understanding System: Integrating Knowledge to Resolve Uncertainty." ACM Computing Surveys, 1980. https://dl.acm.org/doi/10.1145/356810.356816

[2] Corkill, Daniel D. "Blackboard Systems." AI Expert, 1991. http://mas.cs.umass.edu/Documents/Corkill/ai-expert.pdf

[3] Hayes-Roth, B. "A blackboard architecture for control." Artificial Intelligence, 1985. https://www.sciencedirect.com/science/article/pii/0004370285900633
