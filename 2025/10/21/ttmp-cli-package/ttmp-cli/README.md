# TTMP CLI Tool

A command-line tool for managing structured LLM workflow documentation using the TTMP (Ticket-Targeted Markdown Pages) system.

## Quick Start

```bash
# Initialize a new ticket
ttmp init MEN-3475 --title "Chat backend refactoring" --topics chat,backend --owners alice

# Add documentation
cd ttmp/MEN-3475-chat-backend-refactoring
ttmp add working-note "initial context"
ttmp add design-doc "api-architecture"

# Link related files
ttmp relate --ticket MEN-3475 --files go/pkg/chat/handler.go,web/src/api/chat.ts

# List and inspect
ttmp list tickets
ttmp list docs --ticket MEN-3475
ttmp doctor --ticket MEN-3475
```

## Installation

```bash
# Build from source
go build -o ttmp .

# Install to PATH
sudo cp ttmp /usr/local/bin/
chmod +x /usr/local/bin/ttmp

# Verify installation
ttmp --version
```

## Commands

### Core Commands

- `ttmp init` - Initialize a new ticket directory
- `ttmp add` - Create a new document from template
- `ttmp relate` - Update RelatedFiles metadata
- `ttmp meta update` - Edit metadata fields

### Query Commands

- `ttmp list tickets` - List all tickets
- `ttmp list docs` - List documents in a ticket
- `ttmp vocab list` - List vocabulary entries
- `ttmp vocab add` - Add a vocabulary entry
- `ttmp doctor` - Run health checks

### Help System

- `ttmp help introduction` - Overview and quick start
- `ttmp help tutorial-basic-workflow` - Step-by-step tutorial
- `ttmp help commands-reference` - Complete command reference
- `ttmp help metadata-schema` - Metadata field reference
- `ttmp help vocabulary-guide` - Vocabulary management guide

## Features

- **Structured Documentation**: Consistent ticket-based organization
- **Controlled Vocabulary**: Enforce consistent metadata tagging
- **Multiple Output Formats**: Table, JSON, YAML, CSV for all queries
- **Template System**: Generate documents from built-in templates
- **Health Validation**: Detect missing metadata and stale documents
- **LLM-Friendly**: Designed for easy parsing by language models

## Requirements

- Go 1.23.4 or later
- Glazed framework v0.8.9

## Documentation

See `TTMP-CLI-REPORT.md` for comprehensive documentation including:
- Architecture overview
- Complete command reference
- Implementation details
- Test results
- Deployment guide

## License

MIT License - See LICENSE file for details

## Contributing

Contributions welcome! Please see CONTRIBUTING.md for guidelines.

## Support

For issues and questions, please open an issue on GitHub.

