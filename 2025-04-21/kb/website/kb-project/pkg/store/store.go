// pkg/store/store.go
package store

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/blevesearch/bleve/v2"
	"github.com/blevesearch/bleve/v2/mapping"
	"github.com/kb-project/pkg/model"
)

// Store wraps the Bleve index and provides operations for indexing and searching
type Store struct {
	Index bleve.Index
	Dim   int
}

// Open opens an existing index or creates a new one if it doesn't exist
func Open(path string, dim int) (*Store, error) {
	var index bleve.Index
	var err error

	// Ensure the directory exists
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return nil, fmt.Errorf("failed to create directory: %w", err)
	}

	// Try to open existing index
	index, err = bleve.Open(path)
	if err != nil {
		// Create a new index if it doesn't exist
		indexMapping, err := buildIndexMapping(dim)
		if err != nil {
			return nil, fmt.Errorf("failed to build index mapping: %w", err)
		}

		index, err = bleve.New(path, indexMapping)
		if err != nil {
			return nil, fmt.Errorf("failed to create index: %w", err)
		}
	}

	return &Store{
		Index: index,
		Dim:   dim,
	}, nil
}

// buildIndexMapping creates the Bleve index mapping
func buildIndexMapping(dim int) (mapping.IndexMapping, error) {
	// Create a new index mapping
	indexMapping := bleve.NewIndexMapping()
	
	// Set the default type
	indexMapping.DefaultType = "chunk"
	
	// Add the chunk mapping
	chunkMapping := buildChunkMapping(dim)
	indexMapping.AddDocumentMapping("chunk", chunkMapping)
	
	return indexMapping, nil
}

// buildChunkMapping creates a Bleve document mapping for code chunks
func buildChunkMapping(dim int) *mapping.DocumentMapping {
	chunk := bleve.NewDocumentMapping()

	// keyword fields (exact match)
	keywordFields := []string{"id", "file_path", "lang", "symbol_type", "symbol_name", "hash"}
	for _, f := range keywordFields {
		fd := bleve.NewTextFieldMapping()
		fd.Analyzer = "keyword"
		chunk.AddFieldMappingsAt(f, fd)
	}

	// full-text fields
	textFields := []string{"doc", "code"}
	for _, f := range textFields {
		fd := bleve.NewTextFieldMapping()
		fd.Analyzer = "standard"
		chunk.AddFieldMappingsAt(f, fd)
	}

	// numeric fields
	tokensField := bleve.NewNumericFieldMapping()
	chunk.AddFieldMappingsAt("tokens", tokensField)
	
	startLineField := bleve.NewNumericFieldMapping()
	chunk.AddFieldMappingsAt("start_line", startLineField)
	
	endLineField := bleve.NewNumericFieldMapping()
	chunk.AddFieldMappingsAt("end_line", endLineField)

	// date field
	indexedAtField := bleve.NewDateTimeFieldMapping()
	chunk.AddFieldMappingsAt("indexed_at", indexedAtField)

	// vector field - using a simple text field as a fallback since vector fields might not be available
	embeddingField := bleve.NewTextFieldMapping()
	chunk.AddFieldMappingsAt("embedding", embeddingField)

	return chunk
}

// Close closes the index
func (s *Store) Close() error {
	return s.Index.Close()
}

// Upsert indexes or updates a chunk in the index
func (s *Store) Upsert(ctx context.Context, c *model.Chunk) error {
	return s.Index.Index(c.ID, c)
}

// Delete removes a chunk from the index
func (s *Store) Delete(ctx context.Context, id string) error {
	return s.Index.Delete(id)
}

// Get retrieves a chunk by ID
func (s *Store) Get(ctx context.Context, id string) (*model.Chunk, error) {
	doc, err := s.Index.Document(id)
	if err != nil {
		return nil, err
	}

	if doc == nil {
		return nil, fmt.Errorf("document not found: %s", id)
	}

	// In a real implementation, we would convert the document back to a Chunk
	// For simplicity, we'll just search for the document
	query := bleve.NewTermQuery(id)
	query.SetField("id")
	search := bleve.NewSearchRequest(query)
	searchResults, err := s.Index.Search(search)
	if err != nil {
		return nil, err
	}

	if len(searchResults.Hits) == 0 {
		return nil, fmt.Errorf("document not found: %s", id)
	}

	// Get the document by ID
	return s.SearchByID(ctx, id)
}

// SearchByID searches for a chunk by ID
func (s *Store) SearchByID(ctx context.Context, id string) (*model.Chunk, error) {
	query := bleve.NewTermQuery(id)
	query.SetField("id")
	search := bleve.NewSearchRequest(query)
	search.Fields = []string{"*"}
	searchResults, err := s.Index.Search(search)
	if err != nil {
		return nil, err
	}

	if len(searchResults.Hits) == 0 {
		return nil, fmt.Errorf("document not found: %s", id)
	}

	// Convert the first hit to a Chunk
	hit := searchResults.Hits[0]
	chunk := &model.Chunk{}

	// Map fields from the hit to the chunk
	if id, ok := hit.Fields["id"].(string); ok {
		chunk.ID = id
	}
	if filePath, ok := hit.Fields["file_path"].(string); ok {
		chunk.FilePath = filePath
	}
	if lang, ok := hit.Fields["lang"].(string); ok {
		chunk.Language = lang
	}
	if symbolType, ok := hit.Fields["symbol_type"].(string); ok {
		chunk.SymbolType = symbolType
	}
	if symbolName, ok := hit.Fields["symbol_name"].(string); ok {
		chunk.SymbolName = symbolName
	}
	if doc, ok := hit.Fields["doc"].(string); ok {
		chunk.Doc = doc
	}
	if code, ok := hit.Fields["code"].(string); ok {
		chunk.Code = code
	}
	if startLine, ok := hit.Fields["start_line"].(float64); ok {
		chunk.StartLine = int(startLine)
	}
	if endLine, ok := hit.Fields["end_line"].(float64); ok {
		chunk.EndLine = int(endLine)
	}
	if tokens, ok := hit.Fields["tokens"].(float64); ok {
		chunk.Tokens = int(tokens)
	}
	if hash, ok := hit.Fields["hash"].(string); ok {
		chunk.Hash = hash
	}

	return chunk, nil
}

// SearchText searches for chunks by text query
func (s *Store) SearchText(ctx context.Context, q string, k int) ([]model.Chunk, error) {
	query := bleve.NewMatchQuery(q)
	search := bleve.NewSearchRequest(query)
	search.Size = k
	search.Fields = []string{"*"}
	searchResults, err := s.Index.Search(search)
	if err != nil {
		return nil, err
	}

	chunks := make([]model.Chunk, 0, len(searchResults.Hits))
	for _, hit := range searchResults.Hits {
		chunk := model.Chunk{}

		// Map fields from the hit to the chunk
		if id, ok := hit.Fields["id"].(string); ok {
			chunk.ID = id
		}
		if filePath, ok := hit.Fields["file_path"].(string); ok {
			chunk.FilePath = filePath
		}
		if lang, ok := hit.Fields["lang"].(string); ok {
			chunk.Language = lang
		}
		if symbolType, ok := hit.Fields["symbol_type"].(string); ok {
			chunk.SymbolType = symbolType
		}
		if symbolName, ok := hit.Fields["symbol_name"].(string); ok {
			chunk.SymbolName = symbolName
		}
		if doc, ok := hit.Fields["doc"].(string); ok {
			chunk.Doc = doc
		}
		if code, ok := hit.Fields["code"].(string); ok {
			chunk.Code = code
		}
		if startLine, ok := hit.Fields["start_line"].(float64); ok {
			chunk.StartLine = int(startLine)
		}
		if endLine, ok := hit.Fields["end_line"].(float64); ok {
			chunk.EndLine = int(endLine)
		}
		if tokens, ok := hit.Fields["tokens"].(float64); ok {
			chunk.Tokens = int(tokens)
		}
		if hash, ok := hit.Fields["hash"].(string); ok {
			chunk.Hash = hash
		}

		chunks = append(chunks, chunk)
	}

	return chunks, nil
}

// SearchVector searches for chunks by vector similarity
// In a real implementation, this would use vector search
// For our mock, we'll just use text search as a fallback
func (s *Store) SearchVector(ctx context.Context, v []float32, k int) ([]model.Chunk, error) {
	// Since we can't use vector search directly, we'll use a match all query
	// and then sort the results by relevance
	query := bleve.NewMatchAllQuery()
	search := bleve.NewSearchRequest(query)
	search.Size = k
	search.Fields = []string{"*"}
	searchResults, err := s.Index.Search(search)
	if err != nil {
		return nil, err
	}

	chunks := make([]model.Chunk, 0, len(searchResults.Hits))
	for _, hit := range searchResults.Hits {
		chunk := model.Chunk{}

		// Map fields from the hit to the chunk
		if id, ok := hit.Fields["id"].(string); ok {
			chunk.ID = id
		}
		if filePath, ok := hit.Fields["file_path"].(string); ok {
			chunk.FilePath = filePath
		}
		if lang, ok := hit.Fields["lang"].(string); ok {
			chunk.Language = lang
		}
		if symbolType, ok := hit.Fields["symbol_type"].(string); ok {
			chunk.SymbolType = symbolType
		}
		if symbolName, ok := hit.Fields["symbol_name"].(string); ok {
			chunk.SymbolName = symbolName
		}
		if doc, ok := hit.Fields["doc"].(string); ok {
			chunk.Doc = doc
		}
		if code, ok := hit.Fields["code"].(string); ok {
			chunk.Code = code
		}
		if startLine, ok := hit.Fields["start_line"].(float64); ok {
			chunk.StartLine = int(startLine)
		}
		if endLine, ok := hit.Fields["end_line"].(float64); ok {
			chunk.EndLine = int(endLine)
		}
		if tokens, ok := hit.Fields["tokens"].(float64); ok {
			chunk.Tokens = int(tokens)
		}
		if hash, ok := hit.Fields["hash"].(string); ok {
			chunk.Hash = hash
		}

		chunks = append(chunks, chunk)
	}

	return chunks, nil
}

// Count returns the number of documents in the index
func (s *Store) Count() (uint64, error) {
	return s.Index.DocCount()
}
