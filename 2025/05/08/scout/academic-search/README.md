# Academic Search CLI

A command-line interface tool for searching academic papers from various sources including arXiv, Crossref, OpenAlex, Internet Archive, and Library Genesis.

## Features

- Search for academic papers across multiple sources
- Get detailed information about papers including authors, publication dates, and download links
- Filter and sort search results
- Concurrent searching across multiple sources
- Advanced verbs for scholarly research workflows
- Fetch complete metadata for papers
- Find full-text PDFs for academic papers
- Get citation graphs and metrics

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

## Basic Usage

```bash
# Basic search across all sources
./academic-search search "artificial intelligence"

# Search specific sources
./academic-search arxiv "machine learning"
./academic-search crossref "deep learning"
./academic-search openalex "neural networks"
./academic-search archive "reinforcement learning"
./academic-search libgen "data science"

# Search with maximum results
./academic-search search "reinforcement learning" --max 10

# Specify source for search
./academic-search search "computer vision" --source arxiv
```

## Advanced Commands

### Search Works

Search scholarly records across sources with lightweight metadata:

```bash
./academic-search search-works "transformer architecture" --source openalex --limit 10
./academic-search search-works "climate change" --source crossref --limit 20 --filter "has_oa:true"
./academic-search search-works "protein folding" --output json
```

### Resolve DOI

Fetch complete metadata for a DOI, merging data from Crossref and OpenAlex:

```bash
./academic-search resolve-doi 10.1038/nphys1170
./academic-search resolve-doi 10.1038/nature14539 --output json
```

### Suggest Keywords

Get controlled-vocabulary concepts for a title or text:

```bash
./academic-search suggest-keywords "large language models for protein design"
./academic-search suggest-keywords "quantum computing applications" --max 15 --output json
```

### Get Metrics

Pull headline quantitative metrics for a work:

```bash
./academic-search get-metrics W2741809809
./academic-search get-metrics 10.1038/nature14539 --output json
```

### Get Citations

Retrieve citation graph - either works citing a paper or works cited by a paper:

```bash
./academic-search get-citations W2741809809 --direction cited_by --limit 50
./academic-search get-citations W2741809809 --direction refs --limit 20
./academic-search get-citations W2741809809 --cursor AoJ0... --output json
```

### Find Full Text

Find the best PDF URL for a work, trying legal open access sources first:

```bash
./academic-search find-full-text --doi 10.1038/nature14539 --email user@example.com
./academic-search find-full-text --title "Attention is all you need" --prefer-version accepted
./academic-search find-full-text --doi 10.1038/nphys1170 --output json
```

## Command-line options

### Source-specific search options

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

#### OpenAlex search options

- `--per-page` - Number of results per page (default 5)
- `--page` - Page number (default 1)
- `--sort` - Sort order (e.g., cited_by_count:desc, publication_date:desc)
- `--filter` - Filter results (e.g., 'is_oa:true,publication_year:2020')
- `--email` - Your email address to use OpenAlex's polite pool

#### Internet Archive search options

- `--rows` - Number of results (default 5)
- `--page` - Page number (default 1)
- `--sort` - Sort order (e.g., downloads desc, date desc)
- `--type` - Media type (e.g., texts, audio, movies)

#### Libgen search options

- `--limit` - Number of results to return (default 5)
- `--page` - Page number (default 1)
- `--fields` - Fields to search (comma-separated, default: title,author,year,publisher)

#### Combined search options

- `--max` - Maximum number of results to return per source (default 5)
- `--source` - Source to search (all, arxiv, crossref, openalex, archive, libgen)

## License

MIT