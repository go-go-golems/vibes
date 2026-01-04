# Goat Farm Management System

A comprehensive goat farm management system built with Go, Ent ORM, embedded Dolt for version control, and Glazed CLI framework.

## Features

### Core Farm Management
- **Goat Management**: Track individual goats with detailed records including breed, health status, and lineage
- **Milk Production Tracking**: Record and analyze milk production with quality metrics
- **Health Records**: Maintain comprehensive veterinary and health records
- **Breeding Management**: Track breeding activities and kidding events
- **Feed Management**: Monitor feed consumption and costs
- **Farm Operations**: Log general farm activities and operations

### Advanced Analytics
- **Production Analytics**: Analyze milk production trends and performance
- **Health Insights**: Track health patterns and vaccination schedules
- **Financial Reporting**: Monitor costs, revenue, and profitability
- **Performance Metrics**: Individual goat performance analysis

### Version Control
- **Data Versioning**: Built-in version control using embedded Dolt
- **Branch Management**: Create feature branches for different farm operations
- **Backup & Restore**: Automated backup and restore capabilities
- **Change Tracking**: Track all changes with detailed commit history
- **Data Comparison**: Compare data between different time periods or branches

## Installation

### Prerequisites
- Go 1.24.5 or later
- Build tools (gcc, make)

### Quick Start

1. **Clone and build the project:**
```bash
git clone <repository-url>
cd goat-farm-manager
make deps
make build
```

2. **Initialize the database:**
```bash
make init-db
```

3. **Install the application:**
```bash
make install
```

## Usage

### Basic Commands

#### Goat Management
```bash
# Add a new goat
goat-manager goat --tag-id "G001" --name "Bella" --breed "alpine" --gender "female" --status "active"

# List all goats
goat-manager goat

# Filter goats
goat-manager goat --filter "alpine" --limit 10
```

#### Milk Production
```bash
# Record milk production
goat-manager milk --goat-tag "G001" --volume 2.5 --session "morning" --quality-grade "A"

# View milk records
goat-manager milk --goat-tag "G001" --date-from "2024-01-01"
```

#### Health Records
```bash
# Add health record
goat-manager health --goat-tag "G001" --record-type "vaccination" --description "Annual vaccination" --veterinarian "Dr. Smith"

# View health history
goat-manager health --goat-tag "G001"
```

#### Breeding Management
```bash
# Record breeding
goat-manager breeding --doe-tag "G001" --buck-tag "G002" --breeding-date "2024-01-15"

# View breeding records
goat-manager breeding --status "pregnant"
```

#### Feed Management
```bash
# Record feeding
goat-manager feed --feed-type "hay" --quantity 50 --unit "kg" --fed-by "John" --feeding-method "group"

# View feed records
goat-manager feed --feed-type "grain" --limit 20
```

#### Farm Operations
```bash
# Log farm operation
goat-manager farm --operation-type "cleaning" --description "Barn cleaning" --performed-by "Jane"

# View operations
goat-manager farm --operation-type "maintenance"
```

### Analytics and Reporting

```bash
# Farm summary
goat-manager analytics --report-type "farm-summary"

# Milk production analysis
goat-manager analytics --report-type "milk-production" --date-from "2024-01-01" --date-to "2024-01-31"

# Individual goat performance
goat-manager analytics --report-type "goat-performance" --goat-tag "G001"
```

### Version Control

```bash
# Commit changes
goat-manager version --action "commit" --message "Updated goat records"

# Create a branch
goat-manager version --action "branch" --branch "breeding-season-2024"

# Switch branches
goat-manager version --action "switch" --branch "breeding-season-2024"

# View commit history
goat-manager version --action "log" --limit 10

# Create backup
goat-manager version --action "backup" --message "Before major updates"

# Compare branches
goat-manager version --action "compare" --branch "breeding-season-2024" --from-commit "main"

# View branch status
goat-manager version --action "status" --branch "main"
```

## Output Formats

The system supports multiple output formats through the Glazed framework:

```bash
# JSON output
goat-manager goat --output json

# CSV output
goat-manager milk --output csv

# Table output (default)
goat-manager analytics --report-type "farm-summary" --output table

# YAML output
goat-manager version --action "log" --output yaml
```

## Configuration

The system uses sensible defaults but can be configured through environment variables:

- `GOAT_FARM_DB_PATH`: Database directory path (default: `~/goat-farm-data`)
- `GOAT_FARM_DB_NAME`: Database name (default: `goat_farm`)
- `GOAT_FARM_COMMIT_NAME`: Default commit author name
- `GOAT_FARM_COMMIT_EMAIL`: Default commit author email

## Architecture

### Technology Stack
- **Language**: Go 1.24.5
- **ORM**: Ent (Entity Framework for Go)
- **Database**: Embedded Dolt (MySQL-compatible with Git-like version control)
- **CLI Framework**: Glazed (structured data processing)
- **Command Framework**: Cobra

### Project Structure
```
goat-farm-manager/
├── cmd/goat-manager/          # Main application entry point
├── ent/                       # Generated Ent ORM code
│   └── schema/               # Entity schema definitions
├── internal/
│   ├── cli/commands/         # CLI command implementations
│   └── database/             # Database connection and utilities
├── pkg/models/               # Shared data models
├── docs/                     # Documentation
├── Makefile                  # Build automation
└── README.md                 # This file
```

### Database Schema

The system manages the following entities:

- **Goats**: Individual animal records with breed, status, and lineage
- **Milk Records**: Production data with quality metrics
- **Health Records**: Veterinary care and health events
- **Breeding Records**: Mating and kidding information
- **Feed Records**: Feeding activities and consumption
- **Farm Operations**: General farm activities and maintenance

All entities are versioned and tracked through Dolt's built-in version control system.

## Development

### Building from Source

```bash
# Install dependencies
make deps

# Generate Ent code
make generate

# Build the application
make build

# Run tests
make test

# Clean build artifacts
make clean
```

### Testing

```bash
# Run all tests
make test

# Run database connectivity test
make run-test

# Initialize database for development
make init-db
```

## Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `make test`
6. Commit your changes: `git commit -am 'Add feature'`
7. Push to the branch: `git push origin feature-name`
8. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Support

For support and questions:
- Create an issue in the GitHub repository
- Check the documentation in the `docs/` directory
- Use the built-in help system: `goat-manager help`

## Roadmap

- [ ] Web dashboard interface
- [ ] Mobile app integration
- [ ] IoT sensor integration
- [ ] Advanced machine learning analytics
- [ ] Multi-farm management
- [ ] Cloud synchronization
- [ ] Automated reporting and alerts

