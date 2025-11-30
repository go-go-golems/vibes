package embedding

import (
	"context"
	"fmt"
	
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
	"github.com/cayleygraph/quad"
)

// RerankResult represents a reranked search result
type RerankResult struct {
	NodeID          string
	EmbeddingScore  float32
	GraphScore      float32
	FinalScore      float32
	Features        map[string]float32
}

// MockReranker combines embedding scores with graph features
type MockReranker struct {
	QS graph.QuadStore
	
	// Weights for score combination
	EmbeddingWeight float32
	GraphWeight     float32
	TypeWeight      float32
}

// NewMockReranker creates a new mock reranker with default weights
func NewMockReranker(qs graph.QuadStore) *MockReranker {
	return &MockReranker{
		QS:              qs,
		EmbeddingWeight: 0.6,
		GraphWeight:     0.2,
		TypeWeight:      0.2,
	}
}

// Rerank reranks candidates by combining embedding scores with graph features
func (r *MockReranker) Rerank(ctx context.Context, candidates []Candidate, targetType string) ([]RerankResult, error) {
	fmt.Printf("\n[DEBUG] Reranking %d candidates\n", len(candidates))
	results := make([]RerankResult, 0, len(candidates))
	
	for _, cand := range candidates {
		// Extract graph features
		fmt.Printf("[DEBUG] Processing candidate: %q\n", cand.NodeID)
		features := r.extractGraphFeatures(ctx, cand.NodeID)
		fmt.Printf("[DEBUG] Features: %+v\n", features)
		
		// Compute graph score (mock: based on degree centrality)
		graphScore := r.computeGraphScore(features)
		
		// Type matching bonus
		typeScore := r.computeTypeScore(ctx, cand.NodeID, targetType)
		
		// Combine scores
		finalScore := (r.EmbeddingWeight * cand.Score) +
			(r.GraphWeight * graphScore) +
			(r.TypeWeight * typeScore)
		
		results = append(results, RerankResult{
			NodeID:         cand.NodeID,
			EmbeddingScore: cand.Score,
			GraphScore:     graphScore,
			FinalScore:     finalScore,
			Features:       features,
		})
	}
	
	// Sort by final score descending
	// (Would use sort.Slice in real implementation)
	
	return results, nil
}

// extractGraphFeatures extracts graph-based features for a node
func (r *MockReranker) extractGraphFeatures(ctx context.Context, nodeID string) map[string]float32 {
	features := make(map[string]float32)
	
	// Create node reference
	fmt.Printf("[DEBUG] Looking up node in graph: %q\n", nodeID)
	nodeValue := quad.IRI(nodeID)
	fmt.Printf("[DEBUG] Created node value: %v\n", nodeValue)
	
	// Get ValueOf from QuadStore
	ref, err := r.QS.ValueOf(nodeValue)
	if err != nil || ref == nil {
		fmt.Printf("[DEBUG] Node not found in graph (err: %v)!\n", err)
		return features
	}
	fmt.Printf("[DEBUG] Got ref from store: %v\n", ref)
	
	// Count outgoing edges (degree centrality)
	fmt.Printf("[DEBUG] Counting outgoing edges...\n")
	outIt := r.QS.QuadIterator(quad.Subject, ref)
	outCount := r.countIterator(outIt)
	fmt.Printf("[DEBUG] Out degree: %d\n", outCount)
	features["out_degree"] = float32(outCount)
	
	// Count incoming edges
	fmt.Printf("[DEBUG] Counting incoming edges...\n")
	inIt := r.QS.QuadIterator(quad.Object, ref)
	inCount := r.countIterator(inIt)
	fmt.Printf("[DEBUG] In degree: %d\n", inCount)
	features["in_degree"] = float32(inCount)
	
	// Total degree
	features["total_degree"] = float32(outCount + inCount)
	
	// Normalize by max degree (mock: assume max=100)
	features["degree_normalized"] = features["total_degree"] / 100.0
	
	return features
}

// countIterator counts the number of results in an iterator
func (r *MockReranker) countIterator(itShape iterator.Shape) int {
	count := 0
	ctx := context.Background()
	
	// Get scanner from shape
	it := itShape.Iterate()
	defer it.Close()
	
	for it.Next(ctx) {
		count++
		if count > 1000 { // Safety limit
			break
		}
	}
	
	return count
}

// computeGraphScore computes a graph-based score from features
func (r *MockReranker) computeGraphScore(features map[string]float32) float32 {
	// Mock: use normalized degree as graph score
	degreeScore := features["degree_normalized"]
	
	// Cap at 1.0
	if degreeScore > 1.0 {
		degreeScore = 1.0
	}
	
	return degreeScore
}

// computeTypeScore checks if node matches target type
func (r *MockReranker) computeTypeScore(ctx context.Context, nodeID string, targetType string) float32 {
	if targetType == "" {
		return 0.5 // neutral if no type specified
	}
	
	// Check if node has the target type
	nodeRef := refs.PreFetched(quad.IRI(nodeID))
	typeRef := refs.PreFetched(quad.IRI("rdf:type"))
	targetRef := refs.PreFetched(quad.IRI(targetType))
	
	// Build iterator: (nodeID, rdf:type, targetType)
	it := r.QS.QuadsAllIterator()
	
	// This is simplified - in production, use proper quad filtering
	// For now, return mock score
	_ = it
	_ = nodeRef
	_ = typeRef
	_ = targetRef
	
	// Mock: 50% chance of type match
	return 0.5
}

// RerankWithExplanation returns results with detailed explanations
func (r *MockReranker) RerankWithExplanation(ctx context.Context, candidates []Candidate, targetType string) ([]RerankResult, error) {
	results, err := r.Rerank(ctx, candidates, targetType)
	if err != nil {
		return nil, err
	}
	
	// Add explanations (mock)
	for i := range results {
		results[i].Features["explanation"] = float32(i) // placeholder
	}
	
	return results, nil
}

// PrintResults prints reranked results in human-readable format
func PrintResults(results []RerankResult) {
	fmt.Println("=== Reranked Results ===")
	for i, res := range results {
		fmt.Printf("\n%d. %s\n", i+1, res.NodeID)
		fmt.Printf("   Embedding Score: %.3f\n", res.EmbeddingScore)
		fmt.Printf("   Graph Score:     %.3f\n", res.GraphScore)
		fmt.Printf("   Final Score:     %.3f\n", res.FinalScore)
		fmt.Printf("   Features:\n")
		for k, v := range res.Features {
			fmt.Printf("     - %s: %.3f\n", k, v)
		}
	}
}
