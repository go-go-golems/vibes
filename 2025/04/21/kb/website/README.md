# KB - Knowledge Base for Code

This is a local code knowledge base system that implements:

1. Bleve v2.4+ for text and vector indexing/search
2. Mock tree-sitter for language-aware chunking
3. Mock embeddings for vector similarity search
4. A clean CLI interface using Cobra

## Project Structure

```
kb-project/
├── cmd/
│   └── kb/            # Command-line interface
│       ├── commands/  # Command implementations
│       └── main.go    # Entry point
├── pkg/
│   ├── config/        # Configuration management
│   ├── embed/         # Mock embeddings implementation
│   ├── index/         # Indexing functionality
│   ├── model/         # Domain model
│   ├── rag/           # Retrieval Augmented Generation
│   ├── search/        # Search functionality
│   ├── show/          # Display functionality
│   ├── store/         # Bleve store implementation
│   └── ts/            # Mock tree-sitter implementation
├── run_demo.sh        # Demo script
└── kb_demo_logs.txt   # Demo logs
```

## Features

- Initialize a knowledge base with configurable embedding dimensions
- Add repositories to track for indexing
- Index code repositories with language-aware chunking
- Generate mock embeddings for indexed chunks
- Search for code by text or vector similarity
- Ask questions about your codebase using RAG
- Show detailed information about code chunks

## Commands

- `kb init`: Initialize the knowledge base
- `kb add`: Add a repository to track
- `kb index`: Index code repositories
- `kb embed`: Generate embeddings for chunks
- `kb search`: Search for code by text
- `kb sim`: Search for code by vector similarity
- `kb ask`: Ask questions about your code
- `kb show`: Show code chunk details

## Building and Running

```bash
# Build the project
go build -o kb ./cmd/kb

# Initialize the knowledge base
./kb init --path ~/.kb/index --dim 768

# Add a repository to track
./kb add --path /path/to/your/repo

# Index the repository
./kb index

# Generate embeddings
./kb embed

# Search for code
./kb search --query "database connection"

# Ask a question
./kb ask --question "How does the database connection work?"
```

## Implementation Notes

- The system uses mock embeddings instead of a real machine learning model
- Tree-sitter queries are simulated for language-aware chunking
- Bleve is used for both text and vector indexing/search
- The CLI is implemented using Cobra for a clean interface

## Website

A comprehensive website is included that showcases the implementation with:
- Architecture overview
- Component details
- Command documentation
- Usage examples

To view the website, open `website/index.html` in your browser.
