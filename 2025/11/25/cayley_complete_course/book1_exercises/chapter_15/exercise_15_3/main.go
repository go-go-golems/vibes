package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
	"github.com/cayleygraph/quad/voc"
	"github.com/openai/openai-go"
	"github.com/openai/openai-go/option"
)

func init() {
	voc.RegisterPrefix("bb:", "http://blackboard.example.org/")
	voc.RegisterPrefix("kb:", "http://knowledge.example.org/")
}

// Agent represents an AI agent in the blackboard system
type Agent struct {
	ID        string
	Name      string
	Expertise string
	Store     *cayley.Handle
	LLMClient *openai.Client
}

// registerAgent adds an agent to the blackboard
func registerAgent(store *cayley.Handle, agentID, name, expertise string) {
	timestamp := time.Now().Format(time.RFC3339)
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(agentID, "rdf:type", "bb:Agent", timestamp))
	t.AddQuad(quad.Make(agentID, "bb:name", name, timestamp))
	t.AddQuad(quad.Make(agentID, "bb:expertise", expertise, timestamp))
	t.AddQuad(quad.Make(agentID, "bb:status", "active", timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to register agent: %v", err)
	}
	
	fmt.Printf("[SYSTEM] Agent registered: %s (expertise: %s)\n", name, expertise)
}

// addKnowledgeToBlackboard adds a fact to the blackboard
func addKnowledgeToBlackboard(store *cayley.Handle, agentID, subject, predicate, object string) {
	timestamp := time.Now().Format(time.RFC3339)
	factID := fmt.Sprintf("fact:%s:%d", agentID, time.Now().UnixNano())
	
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(factID, "rdf:type", "bb:Fact", timestamp))
	t.AddQuad(quad.Make(factID, "bb:subject", subject, timestamp))
	t.AddQuad(quad.Make(factID, "bb:predicate", predicate, timestamp))
	t.AddQuad(quad.Make(factID, "bb:object", object, timestamp))
	t.AddQuad(quad.Make(factID, "bb:source", agentID, timestamp))
	t.AddQuad(quad.Make(factID, "bb:timestamp", timestamp, timestamp))
	
	// Also add the actual triple to the knowledge base
	t.AddQuad(quad.Make(subject, predicate, object, timestamp))
	
	if err := store.ApplyTransaction(t); err != nil {
		log.Fatalf("Failed to add knowledge: %v", err)
	}
}

// queryBlackboard retrieves facts from the blackboard
func queryBlackboard(store *cayley.Handle, subject string) []string {
	var facts []string
	
	// Find all facts about the subject
	p := cayley.StartPath(store).
		Has(quad.IRI("rdf:type"), quad.IRI("bb:Fact")).
		Has(quad.IRI("bb:subject"), quad.String(subject))
	
	p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		factID := value.Native().(string)
		
		// Get predicate and object for this fact
		predPath := cayley.StartPath(store, quad.String(factID)).
			Out(quad.IRI("bb:predicate"))
		
		var predicate string
		predPath.Iterate(nil).EachValue(nil, func(v quad.Value) error {
			predicate = v.Native().(string)
		return nil
		})
		
		objPath := cayley.StartPath(store, quad.String(factID)).
			Out(quad.IRI("bb:object"))
		
		var object string
		objPath.Iterate(nil).EachValue(nil, func(v quad.Value) error {
			object = v.Native().(string)
		return nil
		})
		
		if predicate != "" && object != "" {
			facts = append(facts, fmt.Sprintf("%s %s %s", subject, predicate, object))
		}
 	return nil
	})
	
	return facts
}

// processWithLLM uses an LLM to extract knowledge from text
func (a *Agent) processWithLLM(ctx context.Context, text string) []string {
	prompt := fmt.Sprintf(`You are a knowledge extraction agent with expertise in %s.
Extract factual statements from the following text as simple subject-predicate-object triples.
Format each triple as: subject | predicate | object
Only extract facts related to %s.

Text: %s

Triples:`, a.Expertise, a.Expertise, text)
	
	if a.LLMClient == nil {
		// Fallback if no LLM available
		return []string{
			fmt.Sprintf("document | analyzed_by | %s", a.Name),
			fmt.Sprintf("document | expertise_area | %s", a.Expertise),
		}
	}
	
	chat, err := a.LLMClient.Chat.Completions.New(ctx, openai.ChatCompletionNewParams{
		Messages: []openai.ChatCompletionMessageParamUnion{
			openai.UserMessage(prompt),
		},
		Model: openai.ChatModelGPT4_1Mini,
	})
	
	if err != nil {
		log.Printf("[ERROR] LLM call failed: %v", err)
		return []string{fmt.Sprintf("document | analyzed_by | %s", a.Name)}
	}
	
	response := chat.Choices[0].Message.Content
	
	// Parse the response into triples
	var triples []string
	lines := strings.Split(response, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		
		parts := strings.Split(line, "|")
		if len(parts) == 3 {
			subject := strings.TrimSpace(parts[0])
			predicate := strings.TrimSpace(parts[1])
			object := strings.TrimSpace(parts[2])
			triples = append(triples, fmt.Sprintf("%s | %s | %s", subject, predicate, object))
		}
	}
	
	return triples
}

// analyzeDocument is the main agent task
func (a *Agent) analyzeDocument(ctx context.Context, documentText string) {
	fmt.Printf("\n[%s] Starting analysis...\n", a.Name)
	
	// Extract knowledge using LLM
	triples := a.processWithLLM(ctx, documentText)
	
	// Add extracted knowledge to blackboard
	for _, triple := range triples {
		parts := strings.Split(triple, "|")
		if len(parts) == 3 {
			subject := strings.TrimSpace(parts[0])
			predicate := strings.TrimSpace(parts[1])
			object := strings.TrimSpace(parts[2])
			
			addKnowledgeToBlackboard(a.Store, a.ID, subject, predicate, object)
			fmt.Printf("[%s] Added: %s %s %s\n", a.Name, subject, predicate, object)
		}
	}
	
	fmt.Printf("[%s] Analysis complete\n", a.Name)
}

// synthesizeKnowledge combines insights from multiple agents
func synthesizeKnowledge(store *cayley.Handle) {
	fmt.Println("\n=== Knowledge Synthesis ===")
	
	// Find all unique subjects in the knowledge base
	subjects := make(map[string]bool)
	
	p := cayley.StartPath(store).
		Has(quad.IRI("rdf:type"), quad.IRI("bb:Fact"))
	
	p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
		factID := value.Native().(string)
		
		subjPath := cayley.StartPath(store, quad.String(factID)).
			Out(quad.IRI("bb:subject"))
		
		subjPath.Iterate(nil).EachValue(nil, func(v quad.Value) error {
			subjects[v.Native().(string)] = true
		return nil
		})
 	return nil
	})
	
	// Display knowledge about each subject
	for subject := range subjects {
		facts := queryBlackboard(store, subject)
		if len(facts) > 0 {
			fmt.Printf("\nKnowledge about '%s':\n", subject)
			for _, fact := range facts {
				fmt.Printf("  - %s\n", fact)
			}
		}
	}
}

func main() {
	// Create the blackboard
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	
	fmt.Println("=== AI Agent Blackboard System ===\n")
	
	// Initialize LLM client (optional)
	var llmClient *openai.Client
	if apiKey := os.Getenv("OPENAI_API_KEY"); apiKey != "" {
		client := openai.NewClient(option.WithAPIKey(apiKey))
		llmClient = &client
		fmt.Println("[SYSTEM] LLM integration enabled")
	} else {
		fmt.Println("[SYSTEM] LLM integration disabled (no API key)")
	}
	
	// Register specialized agents
	registerAgent(store, "agent:nlp1", "NLP Specialist", "natural_language_processing")
	registerAgent(store, "agent:entity1", "Entity Extractor", "entity_recognition")
	registerAgent(store, "agent:relation1", "Relation Analyzer", "relationship_extraction")
	
	// Create agent instances
	nlpAgent := &Agent{
		ID:        "agent:nlp1",
		Name:      "NLP Specialist",
		Expertise: "natural_language_processing",
		Store:     store,
		LLMClient: llmClient,
	}
	
	entityAgent := &Agent{
		ID:        "agent:entity1",
		Name:      "Entity Extractor",
		Expertise: "entity_recognition",
		Store:     store,
		LLMClient: llmClient,
	}
	
	relationAgent := &Agent{
		ID:        "agent:relation1",
		Name:      "Relation Analyzer",
		Expertise: "relationship_extraction",
		Store:     store,
		LLMClient: llmClient,
	}
	
	// Sample document to analyze
	document := `
	Cayley is an open-source graph database written in Go. It was inspired by the 
	graph database behind Google's Knowledge Graph. Cayley supports multiple query 
	languages including Gizmo, GraphQL, and MQL. The database can use various storage 
	backends such as BoltDB, PostgreSQL, and MongoDB.
	`
	
	ctx := context.Background()
	
	// Each agent analyzes the document from their perspective
	nlpAgent.analyzeDocument(ctx, document)
	entityAgent.analyzeDocument(ctx, document)
	relationAgent.analyzeDocument(ctx, document)
	
	// Synthesize knowledge from all agents
	synthesizeKnowledge(store)
	
	fmt.Println("\n=== Blackboard System Complete ===")
}
