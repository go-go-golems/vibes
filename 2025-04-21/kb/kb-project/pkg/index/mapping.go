// pkg/index/mapping.go
package index

import (
	"github.com/blevesearch/bleve/v2"
	"github.com/blevesearch/bleve/v2/analysis/analyzer/keyword"
	"github.com/blevesearch/bleve/v2/analysis/analyzer/standard"
	"github.com/blevesearch/bleve/v2/mapping"
)

// BuildChunkMapping creates a Bleve document mapping for code chunks
// with support for both text and vector search
func BuildChunkMapping(dim int) *mapping.DocumentMapping {
	chunk := bleve.NewDocumentMapping()

	// keyword fields (exact match)
	for _, f := range []string{"id", "file_path", "lang", "symbol_type", "symbol_name", "hash"} {
		fd := bleve.NewTextFieldMapping()
		fd.Analyzer = keyword.Name
		chunk.AddFieldMappingsAt(f, fd)
	}

	// full-text fields
	for _, f := range []string{"doc", "code"} {
		fd := bleve.NewTextFieldMapping()
		fd.Analyzer = standard.Name // ok for code comments too
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

	// vector field - using a text field as a fallback
	embeddingField := bleve.NewTextFieldMapping()
	chunk.AddFieldMappingsAt("embedding", embeddingField)

	return chunk
}

// BuildIndexMapping creates the overall index mapping
func BuildIndexMapping(dim int) (mapping.IndexMapping, error) {
	// Create a new index mapping
	indexMapping := bleve.NewIndexMapping()
	
	// Set the default type
	indexMapping.DefaultType = "chunk"
	
	// Add the chunk mapping
	chunkMapping := BuildChunkMapping(dim)
	indexMapping.AddDocumentMapping("chunk", chunkMapping)
	
	return indexMapping, nil
}
