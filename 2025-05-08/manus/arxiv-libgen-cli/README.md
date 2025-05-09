# Arxiv, LibGen, Crossref, and OpenAlex CLI Searcher

`arxiv-libgen-cli` is a command-line interface (CLI) tool written in Go to search for scientific papers on Arxiv, Library Genesis (LibGen), Crossref, and OpenAlex.

## Features

- Search Arxiv using its official API.
- Search LibGen mirrors for scientific articles (via web scraping).
- Search Crossref for metadata of scholarly works.
- Search OpenAlex for scholarly works and entities.
- Uses Cobra for a structured CLI experience.
- Customizable number of results for each platform.
- Option to specify LibGen mirror for searching.
- Global `--debug` flag to enable detailed zerolog logging for troubleshooting.

## Prerequisites

- Go (version 1.18 or later recommended) installed on your system. You can download it from [golang.org/dl/](https://golang.org/dl/).

## Installation / Building

1.  Clone this repository or place the source code in a directory.
2.  Navigate to the project directory in your terminal:
    ```bash
    cd /path/to/arxiv-libgen-cli
    ```
3.  Build the executable:
    ```bash
    go build -o arxiv-libgen-searcher .
    ```
    This will create an executable file named `arxiv-libgen-searcher` (or `arxiv-libgen-searcher.exe` on Windows) in the current directory.

## Usage

Once built, you can run the tool from your terminal.

### General Help

```bash
./arxiv-libgen-searcher --help
```

To enable debug logging for any command, add the `--debug` or `-d` flag:
```bash
./arxiv-libgen-searcher [command] [flags] --debug
```

### Arxiv Search

The `arxiv` command searches for papers on Arxiv. This command has been tested and is working correctly.

**Command:**
```bash
./arxiv-libgen-searcher arxiv [flags]
```

**Flags:**
- `-q, --query string`: Search query for Arxiv (e.g., "all:electron", "ti:\"quantum computing\" AND au:\"John Preskill\"") (required)
- `-n, --max_results int`: Maximum number of results to return (default 10)

**Examples:**

1.  Search for papers with "large language models" in any field, show top 3 results, with debug logging:
    ```bash
    ./arxiv-libgen-searcher arxiv -q "all:large language models" -n 3 --debug
    ```

**Arxiv API Acknowledgment:**
This tool uses the arXiv API. Please acknowledge arXiv data usage with the statement: "Thank you to arXiv for use of its open access interoperability."

### LibGen Search

The `libgen` command searches for scientific articles on Library Genesis mirrors by scraping search results. 
**Important Note on Reliability:** This functionality is highly dependent on the structure of the LibGen mirror websites, which change frequently. During the latest round of testing (May 2025), the scraper **did not yield results** even when trying different mirrors (e.g., `libgen.is`, `libgen.st`). The HTML structure of these sites appears to have changed significantly, breaking the current scraping logic. This command may require frequent updates to its web scraping selectors to remain functional.

**Command:**
```bash
./arxiv-libgen-searcher libgen [flags]
```

**Flags:**
- `-q, --query string`: Search query for LibGen (e.g., "artificial intelligence", "ISBN:9783319994912") (required)
- `-n, --max_results int`: Maximum number of results to display (default 10)
- `-m, --mirror string`: LibGen mirror URL (e.g., https://libgen.is, http://libgen.st) (default "https://libgen.rs")

**Examples:**

1.  Search for "deep learning" on `libgen.is`, show top 2 results with debug logs (Note: likely to fail with current scraper):
    ```bash
    ./arxiv-libgen-searcher libgen -q "deep learning" -n 2 --mirror "https://libgen.is" --debug
    ```

### Crossref Search

The `crossref` command searches for scholarly metadata on Crossref. This command has been tested and is working correctly.

**Command:**
```bash
./arxiv-libgen-searcher crossref [flags]
```

**Flags:**
- `-q, --query string`: Search query for Crossref (e.g., "explainable AI") (required)
- `-n, --rows int`: Number of results to return (default 10)
- `-f, --filter string`: Filter parameters for Crossref (e.g., "type:journal-article,from-pub-date:2020-01-01")
- `-m, --mailto string`: Your email address for the Crossref polite pool (recommended)

**Examples:**

1.  Search for "climate change mitigation", show top 3 results, providing an email:
    ```bash
    ./arxiv-libgen-searcher crossref -q "climate change mitigation" -n 3 --mailto "user@example.com"
    ```

### OpenAlex Search

The `openalex` command searches for scholarly works on OpenAlex. This command has been tested and is working correctly after several fixes to its Go struct definitions and API parameter handling.

**Command:**
```bash
./arxiv-libgen-searcher openalex [flags]
```

**Flags:**
- `-q, --query string`: Search query for OpenAlex (searches title, abstract, fulltext)
- `-n, --per_page int`: Number of results per page (default 10)
- `-m, --mailto string`: Email address for OpenAlex polite pool (highly recommended)
- `-f, --filter string`: Filter parameters for OpenAlex (e.g., "publication_year:2022,type:journal-article")
- `-s, --sort string`: Sort order (e.g., "cited_by_count:desc", "publication_date:asc") (default "relevance_score:desc")

**Examples:**

1.  Search for "generative adversarial networks", show 2 results, with email and debug:
    ```bash
    ./arxiv-libgen-searcher openalex -q "generative adversarial networks" -n 2 --mailto "user@example.com" --debug
    ```

## Important Notes & Limitations

- **LibGen Scraping Fragility**: As noted above, the LibGen search functionality is currently **not reliable** due to frequent changes in mirror website HTML structures. The scraper requires updates to function correctly.
- **API Rate Limiting & Politeness**:
    - **Arxiv**: The Arxiv API has rate limits. The documentation suggests waiting 3 seconds between automated requests. The current tool makes requests sequentially per command.
    - **Crossref & OpenAlex**: Both APIs have a "polite pool" which you can join by providing your email address via the `--mailto` flag. This is highly recommended for better service and to avoid being rate-limited.
    - **LibGen**: Web scraping can also trigger rate limiting or IP blocks on LibGen mirrors if too many requests are made too quickly. This tool includes a small delay, but use responsibly (though currently non-functional).
- **OpenAlex `select` Field & Structs**: The OpenAlex command uses a predefined set of `select` fields and Go structs to parse the API response. These were updated to match the API documentation and ensure correct parsing during recent testing. If the API changes, these may need further updates.
- **Error Handling**: Basic error handling is implemented. If a search fails, an error message will be displayed. Debug logs provide more details.
- **Output Format**: Results are printed to the console in a human-readable format.

## Project Structure

- `main.go`: Entry point for the Cobra CLI application.
- `cmd/`: Contains the Cobra command definitions.
    - `root.go`: Defines the root command and global flags (like `--debug`).
    - `arxiv.go`: Implements the `arxiv` command.
    - `libgen.go`: Implements the `libgen` command (including web scraping - currently needs update).
    - `crossref.go`: Implements the `crossref` command.
    - `openalex.go`: Implements the `openalex` command.
- `go.mod`, `go.sum`: Go module files.
- `*.md` research files: Contain notes on API research for each platform.

## Dependencies

- [github.com/spf13/cobra](https://github.com/spf13/cobra): For CLI structure.
- [github.com/PuerkitoBio/goquery](https://github.com/PuerkitoBio/goquery): For HTML parsing (used in LibGen scraping).
- [github.com/rs/zerolog](https://github.com/rs/zerolog): For structured, leveled logging.

## Disclaimer

This tool is provided for educational and research purposes. Please respect the terms of service of Arxiv, LibGen, Crossref, and OpenAlex. The developers of this tool are not responsible for how it is used.

