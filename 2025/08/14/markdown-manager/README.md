# Markdown Document Manager (MDM)

A powerful CLI tool for managing markdown files with YAML metadata frontmatter. Built with Go and designed for development teams, documentation workflows, and project management.

## Features

- **Rich Metadata Support**: Comprehensive YAML frontmatter schema for project management
- **Advanced Search**: Search by tags, project, author, content, and more
- **Analytics & Queries**: Analyze your documentation with built-in statistics
- **Multiple Output Formats**: Table, JSON, and more
- **File Management**: Update metadata, track usage, and maintain relationships
- **Fast & Efficient**: Built with Go for performance and reliability

## Installation

### Prerequisites

- Go 1.21 or later
- Git (for cloning repositories)

### Build from Source

```bash
git clone <repository-url>
cd markdown-manager
go build -o mdm
```

### Install Binary

Copy the `mdm` binary to your PATH:

```bash
sudo cp mdm /usr/local/bin/
```

## Quick Start

### 1. List Markdown Files

```bash
# List all markdown files in current directory
mdm list

# List files in specific directory with full paths
mdm list /path/to/docs --show-path

# Output as JSON
mdm list --format json
```

### 2. Search Files

```bash
# Search by tags
mdm search --tags api,documentation

# Search by project
mdm search --project web-dev

# Search in content
mdm search --content "transformer architecture"

# Combined search with content preview
mdm search --tags ai --project research --show-content
```

### 3. View File Information

```bash
# Show detailed file metadata
mdm info path/to/file.md

# Include full content
mdm info path/to/file.md --show-content

# Update last_used timestamp
mdm info path/to/file.md --touch
```

### 4. Update Metadata

```bash
# Update status and add tags
mdm update file.md --status published --add-tags v2.0,stable

# Update multiple fields
mdm update file.md --title "New Title" --project "new-project" --priority high

# Remove specific tags
mdm update file.md --remove-tags draft,wip
```

### 5. Query and Analytics

```bash
# Get overview statistics
mdm query --query stats

# Analyze tag usage
mdm query --query tags

# Project statistics
mdm query --query projects

# Recent files
mdm query --query recent

# Find stale files (not modified in 30 days)
mdm query --query stale
```

## YAML Metadata Schema

The tool supports a comprehensive metadata schema in YAML frontmatter:

```yaml
---
# Basic Information
title: "Document Title"
description: "Brief description of the document"
tags: ["tag1", "tag2", "tag3"]
category: "documentation"

# Timestamps
created: 2024-08-01T10:00:00Z
modified: 2024-08-14T15:30:00Z
last_used: 2024-08-14T09:15:00Z

# Project Information
project: "project-name"
repository: "https://github.com/user/repo"
branch: "main"
status: "draft"          # draft, review, final, published, archived
priority: "medium"       # low, medium, high, critical
version: "1.0"

# People
author: "Author Name"
contributors: ["Person 1", "Person 2"]

# Technical Details
language: "markdown"
format: "technical-guide"
template: "standard"

# Relationships
related_files: ["file1.md", "file2.md"]
dependencies: ["config.yaml", "script.py"]
references: ["https://example.com", "https://docs.example.com"]

# Custom Fields (optional)
custom:
  conference: "TechConf 2024"
  deadline: "2024-09-15"
  word_count: 5000
---

# Your markdown content here...
```

## Command Reference

### Global Flags

- `-h, --help`: Show help information
- `--version`: Show version information

### `mdm list [directory]`

List markdown files with metadata.

**Flags:**
- `-r, --recursive`: Recursively scan subdirectories (default: true)
- `--show-path`: Show full file paths instead of just filenames
- `-f, --format string`: Output format (table, json) (default: "table")

**Examples:**
```bash
mdm list
mdm list docs/ --show-path
mdm list --format json
```

### `mdm search`

Search markdown files by metadata and content.

**Flags:**
- `-d, --directory string`: Directory to search in (default: ".")
- `--title string`: Search by title (partial match)
- `--tags strings`: Search by tags (comma-separated)
- `--category string`: Search by category
- `--project string`: Search by project
- `--status string`: Search by status
- `--priority string`: Search by priority
- `--author string`: Search by author
- `--content string`: Search in content text
- `--show-content`: Include content preview in results

**Examples:**
```bash
mdm search --tags api,documentation
mdm search --project web-dev --status final
mdm search --content "kubernetes" --show-content
```

### `mdm info <file>`

Show detailed information about a markdown file.

**Flags:**
- `--show-content`: Include full content in output
- `--touch`: Update last_used timestamp

**Examples:**
```bash
mdm info docs/api.md
mdm info docs/api.md --show-content --touch
```

### `mdm update <file>`

Update metadata of a markdown file.

**Flags:**
- `--title string`: Update title
- `--description string`: Update description
- `--tags strings`: Replace all tags (comma-separated)
- `--add-tags strings`: Add tags (comma-separated)
- `--remove-tags strings`: Remove tags (comma-separated)
- `--category string`: Update category
- `--project string`: Update project
- `--status string`: Update status
- `--priority string`: Update priority
- `--author string`: Update author
- `--touch`: Update last_used timestamp

**Examples:**
```bash
mdm update file.md --status published
mdm update file.md --add-tags v2.0,stable --priority high
mdm update file.md --remove-tags draft --touch
```

### `mdm query`

Query and analyze markdown file metadata.

**Flags:**
- `-d, --directory string`: Directory to query (default: ".")
- `-q, --query string`: Type of query (default: "stats")

**Query Types:**
- `stats`: Overview statistics
- `tags`: Tag usage analysis
- `projects`: Project statistics
- `authors`: Author statistics
- `status`: Status distribution
- `priority`: Priority distribution
- `recent`: Most recently modified files
- `stale`: Files not modified in 30+ days

**Examples:**
```bash
mdm query --query stats
mdm query --query tags
mdm query --directory docs/ --query projects
```

## Use Cases

### 1. Documentation Management

```bash
# Find all API documentation
mdm search --tags api --category documentation

# Check documentation status
mdm query --query status

# Update documentation after review
mdm update api-guide.md --status published --add-tags reviewed
```

### 2. Project Tracking

```bash
# List all files in a project
mdm search --project web-app

# Find high-priority items
mdm search --priority critical

# Get project statistics
mdm query --query projects
```

### 3. Content Maintenance

```bash
# Find stale documentation
mdm query --query stale

# Update last_used for accessed files
mdm info important-doc.md --touch

# Find files without proper metadata
mdm search --status ""
```

### 4. Team Collaboration

```bash
# Find files by author
mdm search --author "John Doe"

# Track recent activity
mdm query --query recent

# Analyze team contributions
mdm query --query authors
```

## Examples

The `examples/` directory contains sample markdown files demonstrating various use cases:

- **API Documentation**: Technical documentation with version tracking
- **Research Papers**: Academic documents with citations and contributors
- **Component Libraries**: Development guides with technical specifications
- **Project Roadmaps**: Strategic planning with timelines and metrics
- **Troubleshooting Guides**: Operations documentation with procedures
- **Meeting Notes**: Team coordination with action items

## Best Practices

### 1. Consistent Metadata

- Always include `title`, `description`, and `tags`
- Use consistent tag naming (lowercase, hyphen-separated)
- Set appropriate `status` and `priority` levels
- Include `project` for organization

### 2. File Organization

- Use descriptive filenames
- Organize files in logical directory structures
- Maintain `related_files` links for connected documents
- Track `dependencies` for technical documentation

### 3. Regular Maintenance

- Use `mdm query --query stale` to find outdated files
- Update `last_used` timestamps when accessing files
- Review and update `status` fields regularly
- Clean up unused tags periodically

### 4. Team Workflows

- Establish standard `status` values (draft → review → final → published)
- Use `priority` for task management
- Track `contributors` for collaboration
- Maintain `references` for external links

## Development

### Project Structure

```
markdown-manager/
├── cmd/                    # Command implementations
│   ├── root.go            # Root command and CLI setup
│   └── commands.go        # Command logic
├── pkg/                   # Core packages
│   ├── metadata/          # Metadata schema definitions
│   │   └── schema.go
│   └── parser/            # File parsing and processing
│       └── frontmatter.go
├── examples/              # Example markdown files
├── screenshots/           # Command output examples
├── main.go               # Application entry point
├── go.mod                # Go module definition
└── README.md             # This file
```

### Building

```bash
# Build for current platform
go build -o mdm

# Build for multiple platforms
GOOS=linux GOARCH=amd64 go build -o mdm-linux-amd64
GOOS=darwin GOARCH=amd64 go build -o mdm-darwin-amd64
GOOS=windows GOARCH=amd64 go build -o mdm-windows-amd64.exe
```

### Testing

```bash
# Run tests
go test ./...

# Test with example files
./mdm list examples/
./mdm search --tags ai
./mdm query --query stats
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Changelog

### v1.0.0 (2024-08-14)

- Initial release
- Core CLI commands: list, search, info, update, query
- Comprehensive YAML metadata schema
- Multiple output formats (table, JSON)
- Advanced search and filtering capabilities
- Analytics and statistics features
- Example files and documentation

## Support

For questions, issues, or feature requests, please:

1. Check the documentation above
2. Search existing issues
3. Create a new issue with detailed information
4. Include example files and command output when reporting bugs

---

**Happy documenting! 📝**

