package memory

import (
	"context"
	"github.com/goagent/framework/goagent/types"
	"github.com/google/uuid"
	"sync"
)

// MockMemory implements the Memory interface for testing
type MockMemory struct {
	mu       sync.RWMutex
	entries  map[string]types.MemoryEntry
	searches map[string][]types.MemoryEntry
}

// NewMockMemory creates a new MockMemory
func NewMockMemory() *MockMemory {
	return &MockMemory{
		entries:  make(map[string]types.MemoryEntry),
		searches: make(map[string][]types.MemoryEntry),
	}
}

// Add adds a memory to the system
func (m *MockMemory) Add(ctx context.Context, memory types.MemoryEntry) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	if memory.ID == "" {
		memory.ID = uuid.New().String()
	}

	m.entries[memory.ID] = memory
	return nil
}

// Get retrieves memories by ID
func (m *MockMemory) Get(ctx context.Context, ids []string) ([]types.MemoryEntry, error) {
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

// Search searches for similar memories
func (m *MockMemory) Search(ctx context.Context, query string, limit int) ([]types.MemoryEntry, error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	if entries, ok := m.searches[query]; ok {
		if len(entries) > limit {
			return entries[:limit], nil
		}
		return entries, nil
	}
	return []types.MemoryEntry{}, nil
}

// AddSearchResult adds a search result for a given query
func (m *MockMemory) AddSearchResult(query string, entries []types.MemoryEntry) {
	m.mu.Lock()
	defer m.mu.Unlock()

	m.searches[query] = entries
}

// Clear clears all memories
func (m *MockMemory) Clear(ctx context.Context) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	m.entries = make(map[string]types.MemoryEntry)
	m.searches = make(map[string][]types.MemoryEntry)
	return nil
}
