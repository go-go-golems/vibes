// pkg/model/chunk.go
package model

import (
	"time"
)

// Chunk is the atomic unit indexed in Bleve.
type Chunk struct {
	ID           string    `json:"id"`            // ULID = file‑hash + byte‑range
	FilePath     string    `json:"file_path"`     // absolute or repo‑relative
	Language     string    `json:"lang"`          // "go", "py", …
	SymbolType   string    `json:"symbol_type"`   // "function", "method", "class", …
	SymbolName   string    `json:"symbol_name"`   // "NewServer"
	Doc          string    `json:"doc"`           // docstring / comment header
	Code         string    `json:"code"`          // source slice
	StartLine    int       `json:"start_line"`
	EndLine      int       `json:"end_line"`
	Tokens       int       `json:"tokens"`        // for RAG budgeting
	Embedding    []float32 `json:"embedding"`     // N‑dim vector
	IndexedAtUTC time.Time `json:"indexed_at"`
	Hash         string    `json:"hash"`          // SHA‑256 of code slice
}
