package memory

import (
	"context"
	"fmt"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
	"math"
	"sort"
	"sync"
)

// SimpleVectorMemory implements a simple vector memory system
type SimpleVectorMemory struct {
	mu      sync.RWMutex
	entries map[string]types.MemoryEntry
	llm     interface {
		GenerateEmbedding(ctx context.Context, text string) ([]float32, error)
	}
}

// NewSimpleVectorMemory creates a new SimpleVectorMemory
func NewSimpleVectorMemory(llm interface {
	GenerateEmbedding(ctx context.Context, text string) ([]float32, error)
}) (*SimpleVectorMemory, error) {
	return &SimpleVectorMemory{
		entries: make(map[string]types.MemoryEntry),
		llm:     llm,
	}, nil
}

// Add adds a memory to the system
func (m *SimpleVectorMemory) Add(ctx context.Context, memory types.MemoryEntry) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	if memory.ID == "" {
		memory.ID = uuid.New().String()
	}

	var embedding []float32
	var err error

	if len(memory.Embedding) > 0 {
		embedding = memory.Embedding
	} else {
		embedding, err = m.llm.GenerateEmbedding(ctx, memory.Content)
		if err != nil {
			return fmt.Errorf("failed to generate embedding: %w", err)
		}
		memory.Embedding = embedding
	}

	m.entries[memory.ID] = memory
	return nil
}

// Get retrieves memories by ID
func (m *SimpleVectorMemory) Get(ctx context.Context, ids []string) ([]types.MemoryEntry, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	var entries []types.MemoryEntry
	for _, id := range ids {
		if entry, ok := m.entries[id]; ok {
			entries = append(entries, entry)
		}
	}
	return entries, nil
}

// cosineSimilarity calculates the cosine similarity between two vectors
func cosineSimilarity(a, b []float32) float32 {
	var dotProduct float32
	var normA float32
	var normB float32

	for i := 0; i < len(a) && i < len(b); i++ {
		dotProduct += a[i] * b[i]
		normA += a[i] * a[i]
		normB += b[i] * b[i]
	}

	if normA == 0 || normB == 0 {
		return 0
	}

	return dotProduct / (float32(math.Sqrt(float64(normA))) * float32(math.Sqrt(float64(normB))))
}

// Search searches for similar memories
func (m *SimpleVectorMemory) Search(ctx context.Context, query string, limit int) ([]types.MemoryEntry, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	queryEmbedding, err := m.llm.GenerateEmbedding(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("failed to generate embedding: %w", err)
	}

	type similarityEntry struct {
		entry      types.MemoryEntry
		similarity float32
	}

	var similarities []similarityEntry
	for _, entry := range m.entries {
		similarity := cosineSimilarity(queryEmbedding, entry.Embedding)
		similarities = append(similarities, similarityEntry{
			entry:      entry,
			similarity: similarity,
		})
	}

	// Sort by similarity (highest first)
	sort.Slice(similarities, func(i, j int) bool {
		return similarities[i].similarity > similarities[j].similarity
	})

	// Limit results
	if len(similarities) > limit {
		similarities = similarities[:limit]
	}

	// Extract entries
	var results []types.MemoryEntry
	for _, sim := range similarities {
		results = append(results, sim.entry)
	}

	return results, nil
}

// Clear clears all memories
func (m *SimpleVectorMemory) Clear(ctx context.Context) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	m.entries = make(map[string]types.MemoryEntry)
	return nil
}
