package extractor

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/fact-extraction/go-extractor/pkg/types"
)

// DocumentLoader loads documents from a directory
type DocumentLoader struct {
	basePath string
}

// NewDocumentLoader creates a new document loader
func NewDocumentLoader(basePath string) *DocumentLoader {
	return &DocumentLoader{basePath: basePath}
}

// LoadDocuments loads up to limit documents from the base path
func (dl *DocumentLoader) LoadDocuments(limit int) ([]types.Document, error) {
	var documents []types.Document

	entries, err := os.ReadDir(dl.basePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read directory: %w", err)
	}

	count := 0
	for _, entry := range entries {
		if count >= limit {
			break
		}

		if entry.IsDir() {
			continue
		}

		// Only process .txt files
		if !strings.HasSuffix(entry.Name(), ".txt") {
			continue
		}

		filePath := filepath.Join(dl.basePath, entry.Name())
		content, err := os.ReadFile(filePath)
		if err != nil {
			return nil, fmt.Errorf("failed to read file %s: %w", filePath, err)
		}

		// Extract document ID from filename (remove .txt extension)
		docID := strings.TrimSuffix(entry.Name(), ".txt")

		documents = append(documents, types.Document{
			ID:       docID,
			FilePath: filePath,
			Content:  string(content),
		})

		count++
	}

	return documents, nil
}
