// Package memory defines interfaces and implementations for agent memory systems
package memory

import (
	"context"
	"github.com/goagent/framework/goagent/types"
)

// Memory interface defines the functionality of a memory system
type Memory interface {
	// Add adds a memory to the system
	Add(ctx context.Context, memory types.MemoryEntry) error

	// Get retrieves memories by ID
	Get(ctx context.Context, ids []string) ([]types.MemoryEntry, error)

	// Search searches for similar memories
	Search(ctx context.Context, query string, limit int) ([]types.MemoryEntry, error)

	// Clear clears all memories
	Clear(ctx context.Context) error
}
