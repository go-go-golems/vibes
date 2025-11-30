package embedding

import (
	"context"
	"fmt"
	
	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
)

// SearchService provides embedding-based search on top of Cayley
type SearchService struct {
	Store graph.QuadStore
	Index EmbeddingIndex
}

// NewSearchService creates a new search service
func NewSearchService(store graph.QuadStore, index EmbeddingIndex) *SearchService {
	return &SearchService{
		Store: store,
		Index: index,
	}
}

// SearchSimilar finds nodes similar to a query vector
func (s *SearchService) SearchSimilar(ctx context.Context, query Vector, k int) ([]Candidate, error) {
	return s.Index.Search(query, k)
}

// SearchSimilarToNode finds nodes similar to a given node
func (s *SearchService) SearchSimilarToNode(ctx context.Context, nodeID string, k int) ([]Candidate, error) {
	return s.Index.SearchByID(nodeID, k)
}

// SearchWithConstraints finds similar nodes that match graph constraints
func (s *SearchService) SearchWithConstraints(ctx context.Context, query Vector, k int, constraints map[string]string) ([]Candidate, error) {
	// Get initial candidates from embedding search
	candidates, err := s.Index.Search(query, k*2) // Get 2x to allow for filtering
	if err != nil {
		return nil, err
	}
	
	// Filter candidates based on graph constraints
	filtered := make([]Candidate, 0, k)
	for _, cand := range candidates {
		if s.matchesConstraints(ctx, cand.NodeID, constraints) {
			filtered = append(filtered, cand)
			if len(filtered) >= k {
				break
			}
		}
	}
	
	return filtered, nil
}

// matchesConstraints checks if a node matches the given constraints
func (s *SearchService) matchesConstraints(ctx context.Context, nodeID string, constraints map[string]string) bool {
	if len(constraints) == 0 {
		return true
	}
	
	// Check type constraint
	if targetType, ok := constraints["type"]; ok {
		if !s.hasType(ctx, nodeID, targetType) {
			return false
		}
	}
	
	// Add more constraint checks as needed
	
	return true
}

// hasType checks if a node has a specific type
func (s *SearchService) hasType(ctx context.Context, nodeID string, targetType string) bool {
	// Query: (nodeID, rdf:type, targetType)
	p := cayley.StartPath(s.Store, quad.IRI(nodeID)).
		Out(quad.IRI("rdf:type")).
		Is(quad.IRI(targetType))
	
	itShape := p.BuildIterator(ctx)
	it := itShape.Iterate()
	defer it.Close()
	
	return it.Next(ctx)
}

// HybridSearch combines embedding search with graph traversal
func (s *SearchService) HybridSearch(ctx context.Context, query Vector, k int, opts HybridSearchOptions) ([]RerankResult, error) {
	// Step 1: Get embedding candidates
	candidates, err := s.Index.Search(query, opts.CandidateK)
	if err != nil {
		return nil, err
	}
	
	// Step 2: Filter by graph constraints
	if len(opts.Constraints) > 0 {
		filtered := make([]Candidate, 0, len(candidates))
		for _, cand := range candidates {
			if s.matchesConstraints(ctx, cand.NodeID, opts.Constraints) {
				filtered = append(filtered, cand)
			}
		}
		candidates = filtered
	}
	
	// Step 3: Rerank using graph features
	reranker := NewMockReranker(s.Store)
	results, err := reranker.Rerank(ctx, candidates, opts.TargetType)
	if err != nil {
		return nil, err
	}
	
	// Step 4: Return top-K
	if k > len(results) {
		k = len(results)
	}
	return results[:k], nil
}

// HybridSearchOptions configures hybrid search
type HybridSearchOptions struct {
	CandidateK  int               // Number of embedding candidates to retrieve
	TargetType  string            // Target node type (e.g., "Person")
	Constraints map[string]string // Additional graph constraints
}

// DefaultHybridSearchOptions returns default options
func DefaultHybridSearchOptions() HybridSearchOptions {
	return HybridSearchOptions{
		CandidateK:  50,
		TargetType:  "",
		Constraints: make(map[string]string),
	}
}

// ExplainSearch provides detailed explanation of search results
func (s *SearchService) ExplainSearch(ctx context.Context, query Vector, k int) (string, error) {
	candidates, err := s.Index.Search(query, k)
	if err != nil {
		return "", err
	}
	
	explanation := fmt.Sprintf("Embedding Search Results (top %d):\n\n", k)
	for i, cand := range candidates {
		explanation += fmt.Sprintf("%d. %s (score: %.3f)\n", i+1, cand.NodeID, cand.Score)
		
		// Get node metadata from graph
		metadata := s.getNodeMetadata(ctx, cand.NodeID)
		for k, v := range metadata {
			explanation += fmt.Sprintf("   %s: %s\n", k, v)
		}
		explanation += "\n"
	}
	
	return explanation, nil
}

// getNodeMetadata retrieves metadata for a node
func (s *SearchService) getNodeMetadata(ctx context.Context, nodeID string) map[string]string {
	metadata := make(map[string]string)
	
	// Get all outgoing properties
	p := cayley.StartPath(s.Store, quad.IRI(nodeID)).Out()
	
	itShape := p.BuildIterator(ctx)
	it := itShape.Iterate()
	defer it.Close()
	
	count := 0
	for it.Next(ctx) && count < 10 { // Limit to 10 properties
		val := it.Result()
		if qv, ok := val.(quad.Value); ok {
			metadata[fmt.Sprintf("property_%d", count)] = qv.String()
			count++
		}
	}
	
	return metadata
}
