# Academic Search CLI

A command-line interface tool for searching academic papers from various sources including arXiv, Crossref, and Library Genesis.

## Features

- Search for academic papers across multiple sources
- Get detailed information about papers including authors, publication dates, and download links
- Filter and sort search results
- Concurrent searching across multiple sources

## Installation

### Requirements

- Go 1.18 or higher

### Building from source

```bash
# Clone the repository
git clone https://github.com/yourusername/academic-search-cli.git
cd academic-search-cli

# Build the application
go build -o academic-search

# Install the application (optional)
go install
```

## Usage

```bash
# Basic search across all sources
./academic-search search "artificial intelligence"

# Search only arXiv
./academic-search arxiv "machine learning"

# Search only Crossref
./academic-search crossref "deep learning"

# Search only Libgen
./academic-search libgen "neural networks"

# Search with maximum results
./academic-search search "reinforcement learning" --max 10

# Specify source for search
./academic-search search "computer vision" --source arxiv
```

### Command-line options

#### Global options

- `--help` - Show help for any command

#### arXiv search options

- `--max` - Maximum number of results to return (default 5)
- `--start` - Result offset (default 0)
- `--sort-by` - Sort by (relevance, lastUpdatedDate, submittedDate)
- `--sort-order` - Sort order (ascending, descending)

#### Crossref search options

- `--rows` - Number of results to return (default 5)
- `--offset` - Result offset (default 0)
- `--sort` - Sort by (score, relevance, updated, deposited, indexed, published)
- `--order` - Sort order (asc, desc)
- `--filter` - Filter results (e.g., 'type:journal-article')
- `--email` - Your email address to provide to Crossref as per their API etiquette

#### Libgen search options

- `--limit` - Number of results to return (default 5)
- `--page` - Page number (default 1)
- `--fields` - Fields to search (comma-separated, default: title,author,year,publisher)

#### Combined search options

- `--max` - Maximum number of results to return per source (default 5)
- `--source` - Source to search (all, arxiv, crossref, libgen)

## Examples

```bash
# Search for recent papers on transformers in arXiv
./academic-search arxiv "transformer architecture" --sort-by lastUpdatedDate

# Search for journal articles in Crossref
./academic-search crossref "climate change" --filter "type:journal-article"

# Search for books on quantum computing in Libgen
./academic-search libgen "quantum computing" --fields "title"

# Search across all sources for AI ethics
./academic-search search "ai ethics" --max 3
```

## License

MIT