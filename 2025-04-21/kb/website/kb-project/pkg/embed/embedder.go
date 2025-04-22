// pkg/embed/embedder.go
package embed

import (
	"context"
	"crypto/sha256"
	"encoding/binary"
	"math"
	"math/rand"
)

// Embedder interface defines the contract for text embedding services
type Embedder interface {
	Encode(ctx context.Context, texts []string) ([][]float32, error)
}

// MockEmbedder implements the Embedder interface with deterministic mock embeddings
type MockEmbedder struct {
	Dimension int
}

// NewMockEmbedder creates a new mock embedder with the specified dimension
func NewMockEmbedder(dimension int) *MockEmbedder {
	return &MockEmbedder{
		Dimension: dimension,
	}
}

// Encode generates mock embeddings for the given texts
// The embeddings are deterministic based on the text content hash
func (m *MockEmbedder) Encode(ctx context.Context, texts []string) ([][]float32, error) {
	embeddings := make([][]float32, len(texts))
	
	for i, text := range texts {
		// Create a deterministic embedding based on text hash
		embeddings[i] = m.generateEmbedding(text)
	}
	
	return embeddings, nil
}

// generateEmbedding creates a deterministic embedding vector from text
func (m *MockEmbedder) generateEmbedding(text string) []float32 {
	// Create a hash of the text
	hash := sha256.Sum256([]byte(text))
	
	// Use the hash to seed a random number generator for deterministic output
	seed := int64(binary.BigEndian.Uint64(hash[:8]))
	rng := rand.New(rand.NewSource(seed))
	
	// Generate embedding vector with values between -1 and 1
	embedding := make([]float32, m.Dimension)
	for i := 0; i < m.Dimension; i++ {
		embedding[i] = float32(rng.Float64()*2 - 1)
	}
	
	// Normalize the vector to unit length
	normalize(embedding)
	
	return embedding
}

// normalize converts a vector to unit length
func normalize(vector []float32) {
	var sum float64
	for _, v := range vector {
		sum += float64(v * v)
	}
	
	magnitude := float32(math.Sqrt(sum))
	if magnitude > 0 {
		for i := range vector {
			vector[i] /= magnitude
		}
	}
}
