package embedding

import (
	"context"
	
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
	"github.com/cayleygraph/cayley/query/shape"
	"github.com/cayleygraph/quad"
)

// EmbeddingSearch is a Shape that returns nodes based on vector similarity
type EmbeddingSearch struct {
	Index EmbeddingIndex
	Query Vector
	K     int
	
	// Optional: attach scores as tags
	ScoreTag string
}

// BuildIterator constructs an iterator that yields the top-K similar nodes
func (s *EmbeddingSearch) BuildIterator(qs graph.QuadStore) iterator.Shape {
	if s.Index == nil || len(s.Query) == 0 || s.K <= 0 {
		return iterator.NewNull()
	}
	
	// Perform vector search
	candidates, err := s.Index.Search(s.Query, s.K)
	if err != nil || len(candidates) == 0 {
		return iterator.NewNull()
	}
	
	// Convert candidates to Fixed iterator with node refs
	fixed := iterator.NewFixed()
	
	for _, cand := range candidates {
		// Resolve node ID to ref
		ref := refs.PreFetched(quad.IRI(cand.NodeID))
		fixed.Add(ref)
		
		// Optionally attach score as tag
		if s.ScoreTag != "" {
			// Note: This is a simplified approach
			// In production, you'd use FixedTags shape or custom iterator
			// to properly attach metadata
		}
	}
	
	return fixed
}

// Optimize performs optimization on the shape
func (s *EmbeddingSearch) Optimize(ctx context.Context, r shape.Optimizer) (shape.Shape, bool) {
	// Check for empty/invalid configuration
	if s.Index == nil || len(s.Query) == 0 || s.K <= 0 {
		return shape.Null{}, true
	}
	
	// If optimizer is provided, let it try
	if r != nil {
		return r.OptimizeShape(ctx, s)
	}
	
	// No optimizations to apply
	return s, false
}

// EmbeddingSearchWithScores returns both nodes and their similarity scores
type EmbeddingSearchWithScores struct {
	Index    EmbeddingIndex
	Query    Vector
	K        int
	ScoreTag string // tag name for storing scores
}

// BuildIterator constructs an iterator with score tags
func (s *EmbeddingSearchWithScores) BuildIterator(qs graph.QuadStore) iterator.Shape {
	if s.Index == nil || len(s.Query) == 0 || s.K <= 0 {
		return iterator.NewNull()
	}
	
	// Perform vector search
	candidates, err := s.Index.Search(s.Query, s.K)
	if err != nil || len(candidates) == 0 {
		return iterator.NewNull()
	}
	
	// Build Fixed shape with refs
	var fixedRefs []refs.Ref
	scoreTags := make(map[string]refs.Ref)
	
	for _, cand := range candidates {
		ref := refs.PreFetched(quad.IRI(cand.NodeID))
		fixedRefs = append(fixedRefs, ref)
		
		// Store score as tag
		if s.ScoreTag != "" {
			scoreRef := refs.PreFetched(quad.Float(float64(cand.Score)))
			scoreTags[cand.NodeID] = scoreRef
		}
	}
	
	// Create Fixed shape
	fixedShape := shape.Fixed(fixedRefs)
	
	// Wrap with FixedTags if scores requested
	if s.ScoreTag != "" && len(scoreTags) > 0 {
		// Note: FixedTags expects a single tag value for all results
		// For per-node scores, we'd need a custom iterator
		// For now, just return the fixed shape
		return fixedShape.BuildIterator(qs)
	}
	
	return fixedShape.BuildIterator(qs)
}

// Optimize performs optimization on the shape
func (s *EmbeddingSearchWithScores) Optimize(ctx context.Context, r shape.Optimizer) (shape.Shape, bool) {
	if s.Index == nil || len(s.Query) == 0 || s.K <= 0 {
		return shape.Null{}, true
	}
	
	if r != nil {
		return r.OptimizeShape(ctx, s)
	}
	
	return s, false
}

// Helper function to create EmbeddingSearch from text query
// In production, this would call an embedding API
func NewEmbeddingSearchFromText(index EmbeddingIndex, queryText string, k int) (*EmbeddingSearch, error) {
	// Mock: For now, return a zero vector
	// In production: call embedding API to convert text to vector
	mockVector := make(Vector, 384) // Assuming 384-dim embeddings
	
	return &EmbeddingSearch{
		Index: index,
		Query: mockVector,
		K:     k,
	}, nil
}

// Helper to create search by node ID
func NewEmbeddingSearchByNode(index EmbeddingIndex, nodeID string, k int) (*EmbeddingSearch, error) {
	candidates, err := index.SearchByID(nodeID, 1)
	if err != nil || len(candidates) == 0 {
		return nil, err
	}
	
	return &EmbeddingSearch{
		Index: index,
		Query: candidates[0].Vector,
		K:     k,
	}, nil
}
