package embedding

import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"sort"
)

// Vector represents an embedding vector
type Vector []float32

// Candidate represents a search result with score
type Candidate struct {
	NodeID string
	Score  float32
	Vector Vector
}

// EmbeddingIndex provides vector similarity search
type EmbeddingIndex interface {
	// Search finds the top-K most similar nodes to the query vector
	Search(query Vector, k int) ([]Candidate, error)
	
	// SearchByID finds similar nodes to a given node
	SearchByID(nodeID string, k int) ([]Candidate, error)
	
	// Add adds a node embedding to the index
	Add(nodeID string, vector Vector) error
	
	// Size returns the number of indexed nodes
	Size() int
}

// InMemoryIndex is a simple in-memory vector index using brute-force search
// For production, use FAISS or similar
type InMemoryIndex struct {
	vectors map[string]Vector
}

// NewInMemoryIndex creates a new in-memory index
func NewInMemoryIndex() *InMemoryIndex {
	return &InMemoryIndex{
		vectors: make(map[string]Vector),
	}
}

// Add adds a node embedding to the index
func (idx *InMemoryIndex) Add(nodeID string, vector Vector) error {
	if len(vector) == 0 {
		return fmt.Errorf("empty vector for node %s", nodeID)
	}
	idx.vectors[nodeID] = vector
	return nil
}

// Size returns the number of indexed nodes
func (idx *InMemoryIndex) Size() int {
	return len(idx.vectors)
}

// Search finds the top-K most similar nodes to the query vector
func (idx *InMemoryIndex) Search(query Vector, k int) ([]Candidate, error) {
	if len(query) == 0 {
		return nil, fmt.Errorf("empty query vector")
	}
	
	if len(idx.vectors) == 0 {
		return []Candidate{}, nil
	}
	
	// Compute cosine similarity with all vectors
	candidates := make([]Candidate, 0, len(idx.vectors))
	for nodeID, vec := range idx.vectors {
		score := cosineSimilarity(query, vec)
		candidates = append(candidates, Candidate{
			NodeID: nodeID,
			Score:  score,
			Vector: vec,
		})
	}
	
	// Sort by score descending
	sort.Slice(candidates, func(i, j int) bool {
		return candidates[i].Score > candidates[j].Score
	})
	
	// Return top-K
	if k > len(candidates) {
		k = len(candidates)
	}
	return candidates[:k], nil
}

// SearchByID finds similar nodes to a given node
func (idx *InMemoryIndex) SearchByID(nodeID string, k int) ([]Candidate, error) {
	vec, ok := idx.vectors[nodeID]
	if !ok {
		return nil, fmt.Errorf("node %s not found in index", nodeID)
	}
	return idx.Search(vec, k+1) // +1 to exclude self
}

// cosineSimilarity computes cosine similarity between two vectors
func cosineSimilarity(a, b Vector) float32 {
	if len(a) != len(b) {
		return 0.0
	}
	
	var dotProduct, normA, normB float32
	for i := range a {
		dotProduct += a[i] * b[i]
		normA += a[i] * a[i]
		normB += b[i] * b[i]
	}
	
	if normA == 0 || normB == 0 {
		return 0.0
	}
	
	return dotProduct / (float32(math.Sqrt(float64(normA))) * float32(math.Sqrt(float64(normB))))
}

// LoadFromJSON loads embeddings from a JSON file
// Expected format: {"node_id": [0.1, 0.2, ...], ...}
func (idx *InMemoryIndex) LoadFromJSON(path string) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}
	
	var embeddings map[string][]float32
	if err := json.Unmarshal(data, &embeddings); err != nil {
		return fmt.Errorf("failed to parse JSON: %w", err)
	}
	
	for nodeID, vec := range embeddings {
		if err := idx.Add(nodeID, vec); err != nil {
			return fmt.Errorf("failed to add node %s: %w", nodeID, err)
		}
	}
	
	return nil
}

// SaveToJSON saves embeddings to a JSON file
func (idx *InMemoryIndex) SaveToJSON(path string) error {
	data, err := json.MarshalIndent(idx.vectors, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}
	
	if err := os.WriteFile(path, data, 0644); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}
	
	return nil
}
