// pkg/index/indexer.go
package index

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/kb-project/pkg/embed"
	"github.com/kb-project/pkg/store"
	"github.com/kb-project/pkg/ts"
)

// Options represents indexing options
type Options struct {
	ChangedOnly bool
	Langs       []string
}

// Indexer handles the indexing of code files
type Indexer struct {
	Store    *store.Store
	Embedder embed.Embedder
}

// NewIndexer creates a new indexer
func NewIndexer(store *store.Store, embedder embed.Embedder) *Indexer {
	return &Indexer{
		Store:    store,
		Embedder: embedder,
	}
}

// IndexPaths indexes all files in the specified paths
func (i *Indexer) IndexPaths(ctx context.Context, roots []string, opts Options) error {
	// Process each root path
	for _, root := range roots {
		if err := i.indexPath(ctx, root, opts); err != nil {
			return fmt.Errorf("failed to index path %s: %w", root, err)
		}
	}

	return nil
}

// indexPath indexes all files in the specified path
func (i *Indexer) indexPath(ctx context.Context, root string, opts Options) error {
	// Walk the directory tree
	return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip directories
		if info.IsDir() {
			return nil
		}

		// Check if the file has a supported language extension
		lang := detectLanguage(path)
		if lang == "" {
			return nil
		}

		// If languages are specified, check if this file's language is included
		if len(opts.Langs) > 0 {
			included := false
			for _, l := range opts.Langs {
				if l == lang {
					included = true
					break
				}
			}
			if !included {
				return nil
			}
		}

		// Read the file content
		content, err := ioutil.ReadFile(path)
		if err != nil {
			return fmt.Errorf("failed to read file %s: %w", path, err)
		}

		// Calculate file hash
		fileHash := sha256.Sum256(content)
		_ = hex.EncodeToString(fileHash[:]) // Use the variable to avoid unused variable error

		// If ChangedOnly is true, check if the file has changed
		if opts.ChangedOnly {
			// In a real implementation, we would check if the file hash has changed
			// For simplicity, we'll just proceed with indexing
		}

		// Parse the file using the appropriate parser
		parser := ts.NewParser(lang)
		chunks, err := parser.ParseFile(ctx, path, string(content))
		if err != nil {
			return fmt.Errorf("failed to parse file %s: %w", path, err)
		}

		// Index each chunk
		for _, chunk := range chunks {
			// In a real implementation, we would check if the chunk has changed
			// For simplicity, we'll just index all chunks

			// Generate embeddings for the chunk
			texts := []string{chunk.Code}
			if chunk.Doc != "" {
				texts[0] = chunk.Doc + "\n" + chunk.Code
			}

			embeddings, err := i.Embedder.Encode(ctx, texts)
			if err != nil {
				return fmt.Errorf("failed to generate embeddings for chunk %s: %w", chunk.ID, err)
			}

			// Set the embedding
			chunk.Embedding = embeddings[0]

			// Index the chunk
			if err := i.Store.Upsert(ctx, chunk); err != nil {
				return fmt.Errorf("failed to index chunk %s: %w", chunk.ID, err)
			}
		}

		return nil
	})
}

// detectLanguage detects the language of a file based on its extension
func detectLanguage(path string) string {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".go":
		return "go"
	case ".py":
		return "python"
	case ".ts":
		return "typescript"
	case ".js":
		return "javascript"
	case ".cpp", ".cc", ".cxx", ".c++":
		return "cpp"
	case ".c":
		return "c"
	case ".java":
		return "java"
	case ".rb":
		return "ruby"
	case ".php":
		return "php"
	case ".rs":
		return "rust"
	case ".swift":
		return "swift"
	case ".kt", ".kts":
		return "kotlin"
	default:
		return ""
	}
}
