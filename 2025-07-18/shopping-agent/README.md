# Shopping Agent

A powerful CLI tool built with Go and the go-go-golems/glazed library that provides automated shopping capabilities including product search, price comparison, screenshot capture, and monitoring.

## Features

- **Product Search**: Search for products across multiple e-commerce sites with structured output
- **Price Comparison**: Compare products across different sites to find the best deals
- **Screenshot Capture**: Take screenshots of web pages with customizable options
- **Price Monitoring**: Monitor product prices and availability over time
- **Structured Output**: Beautiful table output powered by the glazed framework
- **Multiple Output Formats**: Support for JSON, YAML, CSV, and more via glazed

## Installation

### Prerequisites

- Go 1.21 or later
- Internet connection for browser automation

### Build from Source

```bash
git clone <repository-url>
cd shopping-agent
go mod tidy
go build ./cmd/shop
```

## Usage

### Basic Commands

#### Product Search
```bash
./shop search --query "laptop" --max-results 5
./shop search --query "smartphone" --site "amazon" --min-price 500 --max-price 1000
```

#### Product Comparison
```bash
./shop compare --products "MacBook Air,Dell XPS 13" --compare-sort-by price --max-results 3
./shop compare --products "iPhone 15,Samsung Galaxy S24" --sites "amazon,ebay"
```

#### Screenshot Capture
```bash
./shop screenshot --url "https://amazon.com" --wait 3
./shop screenshot --url "https://example.com" --screenshot-output "custom_name.png" --full-page true
./shop screenshot --url "https://ebay.com" --width 1920 --height 1080 --selector ".main-content"
```

#### Price Monitoring
```bash
./shop monitor --urls "https://amazon.com/product/123" --screenshot true
./shop monitor --urls "https://ebay.com/item/456,https://amazon.com/product/789" --interval 300 --duration 60
```

### Advanced Usage

#### Custom Output Formats
The glazed framework provides multiple output formats:

```bash
# JSON output
./shop search --query "laptop" --output json

# CSV output
./shop search --query "laptop" --output csv

# YAML output
./shop search --query "laptop" --output yaml
```

#### Filtering and Sorting
```bash
# Filter by price range
./shop search --query "laptop" --min-price 800 --max-price 1500

# Sort comparison results
./shop compare --products "laptop1,laptop2" --compare-sort-by rating
```

## Architecture

### Project Structure
```
shopping-agent/
├── cmd/shop/           # Main CLI application
├── pkg/agent/          # Glazed command implementations
├── pkg/browser/        # Browser automation using Rod
├── pkg/search/         # Search engine implementations
├── examples/           # Example configurations and usage
└── docs/              # Additional documentation
```

### Key Components

#### Glazed Integration
The shopping agent leverages the go-go-golems/glazed framework for:
- Command-line interface generation
- Parameter validation and parsing
- Structured output formatting
- Multiple output format support

#### Browser Automation
Uses the Rod library for:
- Headless Chrome automation
- Screenshot capture
- Web scraping
- Element interaction

#### Search Engines
Modular search engine architecture supporting:
- Demo search engine (for testing)
- Extensible interface for real e-commerce APIs
- Configurable search parameters

## Configuration

### Environment Variables
- `CHROME_BIN`: Path to Chrome binary (optional)
- `HEADLESS`: Set to "false" to run browser in visible mode

### Configuration Files
The agent supports YAML configuration files for:
- Default search parameters
- Site-specific selectors
- Output preferences

Example configuration:
```yaml
search:
  default_max_results: 10
  default_sites: ["amazon", "ebay"]
  
screenshot:
  default_width: 1920
  default_height: 1080
  default_wait: 2

monitoring:
  default_interval: 300
  screenshot_enabled: true
```

## Development

### Adding New Search Engines
1. Implement the `SearchEngine` interface in `pkg/search/`
2. Register the engine in the search factory
3. Add configuration options as needed

### Extending Commands
1. Create new command in `pkg/agent/`
2. Implement the `GlazeCommand` interface
3. Add to the main CLI in `cmd/shop/main.go`

### Testing
```bash
# Run unit tests
go test ./...

# Run integration tests
go test -tags=integration ./...

# Run demo script
./demo_shopping_agent.sh
```

## Examples

### Demo Script
A comprehensive demonstration script is included:
```bash
./demo_shopping_agent.sh
```

This script demonstrates:
- Product search functionality
- Product comparison features
- Screenshot capture of major e-commerce sites
- Full-page screenshot capabilities

### Sample Outputs

#### Product Search Output
```
+------------------------------+---------+----------+-----------------------------------------+------+--------------+--------+---------+
| title                        | price   | currency | url                                     | site | availability | rating | reviews |
+------------------------------+---------+----------+-----------------------------------------+------+--------------+--------+---------+
| MacBook Air M3 13-inch 256GB | 1119.18 | USD      | https://demo-store.com/macbook-air-m3   | demo | In Stock     | 4.9    | 567     |
| Dell XPS 13 Plus Laptop      | 1311.66 | USD      | https://demo-store.com/dell-xps-13-plus | demo | In Stock     | 4.5    | 678     |
+------------------------------+---------+----------+-----------------------------------------+------+--------------+--------+---------+
```

#### Screenshot Output
```
+--------------------+--------------------------------+-----------+-------+--------+-----------+-------------+---------------------------+---------+-------+
| url                | output_path                    | file_size | width | height | full_page | duration_ms | timestamp                 | success | error |
+--------------------+--------------------------------+-----------+-------+--------+-----------+-------------+---------------------------+---------+-------+
| https://amazon.com | screenshot_20250718_164035.png | 1095479   | 1920  | 1080   | false     | 3681        | 2025-07-18T16:40:36-04:00 | true    |       |
+--------------------+--------------------------------+-----------+-------+--------+-----------+-------------+---------------------------+---------+-------+
```

## Troubleshooting

### Common Issues

#### Chrome Download Issues
If Chrome fails to download automatically:
```bash
# Set custom Chrome path
export CHROME_BIN=/path/to/chrome
```

#### Permission Issues
Ensure the binary has execute permissions:
```bash
chmod +x shop
```

#### Network Issues
For proxy environments, configure Go's HTTP proxy:
```bash
export HTTP_PROXY=http://proxy.example.com:8080
export HTTPS_PROXY=http://proxy.example.com:8080
```

### Debug Mode
Enable debug logging:
```bash
./shop --log-level debug search --query "laptop"
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- [go-go-golems/glazed](https://github.com/go-go-golems/glazed) - CLI framework and structured output
- [go-rod/rod](https://github.com/go-rod/rod) - Browser automation
- [spf13/cobra](https://github.com/spf13/cobra) - CLI library
- [rs/zerolog](https://github.com/rs/zerolog) - Structured logging

