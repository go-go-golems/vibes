# Document Management System with Cayley and Glazed Commands

This package contains a complete implementation of a document management system using Cayley graph database and Glazed CLI framework.

## Package Contents

- `source/` - Complete source code for the CLI application and Cayley setup
- `database/` - Database files with sample data and N-Quads export
- `documentation/` - Comprehensive technical report and project documentation
- `examples/` - Example queries and usage patterns
- `screenshots/` - Screenshots of the system in operation

## Quick Start

1. Install Go 1.24.5 or later from https://golang.org/
2. Install build essentials: `sudo apt install build-essential sqlite3`
3. Navigate to the `source/` directory
4. Build the CLI: `go build -o docmgmt`
5. Run `./docmgmt --help` to see available commands

## Key Features

- Document lifecycle management with status tracking
- Relationship modeling between documents and people
- SQL-based analytical queries
- Graph database integration with Cayley
- Multiple output formats (table, JSON, YAML)
- Comprehensive CLI with parameter validation

## Documentation

See `documentation/REPORT.md` for the complete technical documentation including:
- System architecture
- Database schema design
- Installation instructions
- Usage examples
- Validation results

## Sample Data

The package includes a complete sample dataset with:
- 6 documents of various types (plans, reports, howtos, etc.)
- 3 people with ownership relationships
- Realistic document metadata and relationships

## Support

This system was implemented by Manus AI as a demonstration of modern document management capabilities using open-source technologies.
