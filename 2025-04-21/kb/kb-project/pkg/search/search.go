// pkg/search/search.go
package search

import (
	"context"
	"fmt"

	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/model"
	"github.com/kb-project/pkg/store"
)

// Searcher handles searching for code chunks
type Searcher struct {
	Store    *store.Store
	Embedder embed.Embedder
}

// NewSearcher creates a new searcher
func NewSearcher(store *store.Store, embedder embed.Embedder) *Searcher {
	return &Searcher{
		Store:    store,
		Embedder: embedder,
	}
}

// SearchText searches for chunks by text query
func (s *Searcher) SearchText(ctx context.Context, query string, k int) ([]model.Chunk, error) {
	return s.Store.SearchText(ctx, query, k)
}

// SearchVector searches for chunks by vector similarity
func (s *Searcher) SearchVector(ctx context.Context, query string, k int) ([]model.Chunk, error) {
	// Generate embedding for the query
	embeddings, err := s.Embedder.Encode(ctx, []string{query})
	if err != nil {
		return nil, fmt.Errorf("failed to generate embedding for query: %w", err)
	}

	// Search by vector similarity
	return s.Store.SearchVector(ctx, embeddings[0], k)
}

// SearchHybrid performs a hybrid search using both text and vector similarity
func (s *Searcher) SearchHybrid(ctx context.Context, query string, k int) ([]model.Chunk, error) {
	// In a real implementation, we would combine text and vector search results
	// For simplicity, we'll just use vector search
	return s.SearchVector(ctx, query, k)
}
