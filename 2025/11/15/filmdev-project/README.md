# Film Development Database & Query Tool

A comprehensive tool for scraping and querying film development times from [The Massive Dev Chart](https://www.digitaltruth.com/devchart.php). This project consists of a Python web scraper that populates a SQLite database and a Go CLI tool built with the [glazed framework](https://github.com/go-go-golems/glazed) for querying the data.

## Overview

The Massive Dev Chart is an invaluable resource for film photographers, containing thousands of development time combinations for various films, developers, dilutions, and ISO ratings. This project makes that data easily accessible through a local database and powerful command-line interface.

## Components

### 1. Python Web Scraper (`scraper.py`)

The scraper fetches film development data from digitaltruth.com and stores it in a SQLite database.

**Features:**
- Scrapes film and developer combinations with development times
- Handles multiple dilutions, ISO ratings, and temperatures
- Stores data in normalized SQLite database with proper indexing
- Polite scraping with rate limiting (0.5s delay between requests)
- Progress tracking and error handling

**Database Schema:**
- `films` - Film names
- `developers` - Developer names  
- `development_times` - Development time data with foreign keys to films and developers

### 2. Go CLI Query Tool (`filmdev`)

A powerful command-line tool built with the glazed framework for querying the database.

**Features:**
- Multiple output formats: table, JSON, CSV, YAML
- Flexible filtering by film name, developer name, and ISO rating
- Partial string matching for film and developer names
- List all available films and developers
- Field selection and sorting capabilities (via glazed)

## Installation

### Prerequisites

- Python 3.11+
- Go 1.24+ (with CGO support)
- gcc/build-essential (for SQLite CGO bindings)

### Setup

1. **Clone or download the project files**

2. **Install Python dependencies:**
   ```bash
   pip3 install requests beautifulsoup4
   ```

3. **Run the scraper to populate the database:**
   ```bash
   cd filmdev-project
   python3 scraper.py common_films.json common_developers.json
   ```
   
   This will scrape 30 common films × 22 common developers = 660 combinations.
   Takes approximately 5-6 minutes with rate limiting.

4. **Build the Go CLI tool:**
   ```bash
   cd filmdev-cli
   CGO_ENABLED=1 go build -o filmdev
   ```

5. **Optional: Install globally:**
   ```bash
   sudo cp filmdev /usr/local/bin/
   ```

## Usage

### Query Development Times

**Basic query:**
```bash
./filmdev query --film "Tri-X" --developer "D-76"
```

**Filter by ISO:**
```bash
./filmdev query --film "HP5" --developer "ID-11" --iso 400
```

**JSON output:**
```bash
./filmdev query --film "TMax 400" --output json
```

**CSV output:**
```bash
./filmdev query --film "Delta 400" --developer "Rodinal" --output csv
```

**Select specific fields:**
```bash
./filmdev query --film "Tri-X" --fields film,developer,iso,time_35mm
```

### List Available Films

```bash
./filmdev list-films
```

Output as JSON:
```bash
./filmdev list-films --output json
```

### List Available Developers

```bash
./filmdev list-developers
```

### Help

```bash
./filmdev --help
./filmdev query --help
```

## Examples

### Example 1: Find all Kodak Tri-X 400 development times with D-76 at ISO 400

```bash
$ ./filmdev query --film "Tri-X" --developer "D-76" --iso 400

+-----------------+-----------+----------+-----+-----------+----------+------------+--------+-----------+
| film            | developer | dilution | iso | time_35mm | time_120 | time_sheet | temp_c | notes     |
+-----------------+-----------+----------+-----+-----------+----------+------------+--------+-----------+
| Kodak Tri-X 400 | D-76      | 1+1      | 400 | 9.75      | 9.75     |            | 20C    | Has notes |
| Kodak Tri-X 400 | D-76      | 1+2      | 400 | 13        | 13       |            | 20C    | Has notes |
| Kodak Tri-X 400 | D-76      | 1+3      | 400 | 20        | 25       |            | 20C    | Has notes |
| Kodak Tri-X 400 | D-76      | stock    | 400 | 6.75      | 6.75     |            | 20C    | Has notes |
+-----------------+-----------+----------+-----+-----------+----------+------------+--------+-----------+
```

### Example 2: Export all Ilford HP5+ data to CSV

```bash
$ ./filmdev query --film "HP5" --output csv > hp5_times.csv
```

### Example 3: Find all development options for a specific film

```bash
$ ./filmdev query --film "Delta 400" --iso 400
```

## Database Statistics

After scraping with the common films/developers lists:

- **Films:** ~30 unique film types
- **Developers:** ~60+ unique developers (including variants)
- **Development times:** ~3000+ individual data points

## Customization

### Scraping Different Films/Developers

1. Edit `common_films.json` and `common_developers.json` to include your desired films/developers
2. Use wildcards (%) for pattern matching (e.g., "Ilford HP5%" matches all HP5 variants)
3. Run the scraper again - it will add new data without duplicating existing entries

### Scraping All Films/Developers

To scrape the entire database:

```bash
python3 scraper.py films.json developers.json
```

**Warning:** This will scrape 156 films × 167 developers = 26,052 combinations and take several hours.

## Technical Details

### Glazed Framework Integration

The CLI tool uses the [glazed framework](https://github.com/go-go-golems/glazed) which provides:

- **Automatic output formatting** - JSON, CSV, YAML, tables without custom code
- **Field selection** - `--fields` flag to choose specific columns
- **Sorting** - `--sort-columns` for custom ordering
- **Type-safe parameters** - Struct tags for parameter mapping
- **Help system** - Automatic help generation from command metadata

### Key Implementation Patterns

**Command Structure:**
```go
type QueryCommand struct {
    *cmds.CommandDescription
}

type QuerySettings struct {
    Film      string `glazed.parameter:"film"`
    Developer string `glazed.parameter:"developer"`
    ISO       int    `glazed.parameter:"iso"`
}
```

**Structured Output:**
```go
row := types.NewRow(
    types.MRP("film", film),
    types.MRP("developer", developer),
    types.MRP("iso", iso),
    // ...
)
gp.AddRow(ctx, row)
```

The glazed processor (`gp`) automatically handles formatting based on the `--output` flag.

## Project Structure

```
filmdev-project/
├── scraper.py              # Python web scraper
├── extract_lists.py        # Extract film/developer lists from website
├── films.json              # All available films (156)
├── developers.json         # All available developers (167)
├── common_films.json       # Curated list of 30 common films
├── common_developers.json  # Curated list of 22 common developers
├── filmdev.db             # SQLite database (created by scraper)
├── filmdev-cli/           # Go CLI application
│   ├── main.go            # CLI implementation
│   ├── go.mod             # Go module definition
│   └── filmdev            # Compiled binary
└── README.md              # This file
```

## Future Enhancements

Potential improvements:
- Add temperature conversion (Celsius ↔ Fahrenheit)
- Push/pull processing calculations
- Reciprocity failure compensation
- Web interface
- Mobile app
- Automatic database updates
- Notes extraction and display

## Credits

- Data source: [The Massive Dev Chart](https://www.digitaltruth.com/devchart.php) by Digitaltruth Photo Ltd
- CLI framework: [glazed](https://github.com/go-go-golems/glazed) by go-go-golems
- Built with: Python, Go, SQLite, BeautifulSoup, Cobra

## License

This tool is for personal use. Please respect the terms of service of digitaltruth.com when scraping data. The Massive Dev Chart data is copyright © Digitaltruth Photo Ltd, 2025.

## Support

For issues with:
- **Scraper:** Check your internet connection and verify the website structure hasn't changed
- **CLI tool:** Ensure CGO is enabled and gcc is installed for SQLite support
- **Database:** Delete `filmdev.db` and re-run the scraper to rebuild

## Contributing

Contributions welcome! Areas for improvement:
- Additional scrapers for other development chart sources
- Enhanced query capabilities
- Data validation and cleaning
- Performance optimizations
- Additional output formats
