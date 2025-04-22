// pkg/show/show.go
package show

import (
	"context"
	"fmt"
	"strings"

	"github.com/kb-project/pkg/model"
	"github.com/kb-project/pkg/store"
)

// Shower handles displaying code chunks
type Shower struct {
	Store *store.Store
}

// NewShower creates a new shower
func NewShower(store *store.Store) *Shower {
	return &Shower{
		Store: store,
	}
}

// ShowByID displays a chunk by ID
func (s *Shower) ShowByID(ctx context.Context, id string) (string, error) {
	// Get the chunk
	chunk, err := s.Store.Get(ctx, id)
	if err != nil {
		return "", fmt.Errorf("failed to get chunk: %w", err)
	}

	return s.formatChunk(chunk), nil
}

// ShowByFileLine displays a chunk by file path and line number
func (s *Shower) ShowByFileLine(ctx context.Context, filePath string, line int) (string, error) {
	// In a real implementation, we would search for chunks that contain the specified line
	// For simplicity, we'll just search for chunks with the specified file path
	query := fmt.Sprintf("file_path:%s", filePath)
	chunks, err := s.Store.SearchText(ctx, query, 10)
	if err != nil {
		return "", fmt.Errorf("failed to search for chunks: %w", err)
	}

	// Find the chunk that contains the specified line
	var targetChunk *model.Chunk
	for i, chunk := range chunks {
		if line >= chunk.StartLine && line <= chunk.EndLine {
			targetChunk = &chunks[i]
			break
		}
	}

	if targetChunk == nil {
		return "", fmt.Errorf("no chunk found for file %s at line %d", filePath, line)
	}

	return s.formatChunk(targetChunk), nil
}

// formatChunk formats a chunk for display
func (s *Shower) formatChunk(chunk *model.Chunk) string {
	var sb strings.Builder

	// Header
	sb.WriteString(fmt.Sprintf("ID: %s\n", chunk.ID))
	sb.WriteString(fmt.Sprintf("File: %s\n", chunk.FilePath))
	sb.WriteString(fmt.Sprintf("Language: %s\n", chunk.Language))
	sb.WriteString(fmt.Sprintf("Symbol: %s (%s)\n", chunk.SymbolName, chunk.SymbolType))
	sb.WriteString(fmt.Sprintf("Lines: %d-%d\n", chunk.StartLine, chunk.EndLine))
	sb.WriteString(fmt.Sprintf("Tokens: %d\n", chunk.Tokens))
	sb.WriteString(fmt.Sprintf("Hash: %s\n", chunk.Hash))
	sb.WriteString("\n")

	// Documentation
	if chunk.Doc != "" {
		sb.WriteString("Documentation:\n")
		sb.WriteString(chunk.Doc)
		sb.WriteString("\n\n")
	}

	// Code
	sb.WriteString("Code:\n")
	sb.WriteString(chunk.Code)
	sb.WriteString("\n")

	return sb.String()
}
