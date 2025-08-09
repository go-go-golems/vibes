# Changelog

All notable changes to the Turn Inspector CLI project will be documented in this file.

## [1.0.0] - 2025-08-09

### Added
- Initial release of Turn Inspector CLI
- Complete conversation turn management system
- Ent ORM-based data model with SQLite backend
- Comprehensive CLI interface using Cobra framework

#### Core Features
- **Turn Management**
  - Create conversation turns with ordered blocks
  - List all turns with summary information
  - Show detailed turn information with metadata
  - Delete individual turns or all data

- **Block System**
  - Support for multiple block types: `user`, `llm_text`, `tool_call`, `tool_use`, `system`, `other`
  - Ordered blocks within turns to maintain conversation flow
  - Flexible JSON payload system for diverse content types
  - Optional role assignment for blocks

- **Metadata System**
  - Turn-level metadata with source-key-value structure
  - Block-level metadata for granular information
  - Unique constraints to prevent duplication
  - Rich querying capabilities

#### CLI Commands
- `create turn` - Create new conversation turns
- `list turns` - List all turns with pagination support
- `show turn` - Display detailed turn information
- `show blocks` - Show blocks for specific turns
- `query turns` - Search turns by metadata, content, and block types
- `stats` - Database statistics and analytics
- `delete turn` - Remove specific turns
- `delete all` - Remove all data with confirmation

#### Query Capabilities
- Search by metadata key-value pairs
- Full-text search in block payloads
- Filter by block types
- Combined query conditions
- Case-insensitive text search

#### Output Formats
- Human-readable table format (default)
- JSON output for programmatic use
- Detailed statistics with breakdowns
- Structured data for integration

#### Database Features
- SQLite3 backend for reliability and portability
- Automatic schema creation and migration
- Foreign key constraints for data integrity
- Optimized indexes for query performance
- Configurable database location

#### Documentation
- Comprehensive README with installation and usage
- Detailed examples for all use cases
- Test scenarios covering diverse conversation types
- Demo script showcasing all features

#### Test Scenarios
- Simple user-assistant conversations
- Tool-calling workflows (weather, calculator, database)
- Error handling and system messages
- Complex multi-tool interactions
- Customer support scenarios
- Code assistance conversations

### Technical Details
- **Language**: Go 1.24+
- **ORM**: Ent v0.14.5
- **CLI Framework**: Cobra v1.9.1
- **Database**: SQLite3 with CGO support
- **Architecture**: Clean separation of concerns with modular command structure

### Performance
- Efficient indexed queries for metadata and block searches
- Optimized foreign key relationships
- Batch operations support
- Minimal memory footprint with SQLite

### Security
- Input validation for all JSON payloads
- SQL injection protection through Ent ORM
- Safe deletion operations with confirmation prompts
- No external network dependencies

### Compatibility
- Cross-platform support (Linux, macOS, Windows)
- Self-contained binary with embedded SQLite
- No external runtime dependencies
- Environment variable configuration support

## Development Notes

### Architecture Decisions
- **Ent ORM**: Chosen for type-safe database operations and automatic migration support
- **SQLite**: Selected for simplicity, portability, and zero-configuration deployment
- **Cobra CLI**: Provides robust command-line interface with help system and flag parsing
- **JSON Payloads**: Enable flexible content storage without rigid schema constraints
- **Ordered Blocks**: Ensure conversation flow integrity with unique ordering constraints

### Schema Design
- **Normalized structure** with separate tables for turns, blocks, and metadata
- **Flexible metadata** system supporting arbitrary key-value pairs with source categorization
- **Unique constraints** preventing duplicate metadata entries
- **Cascading deletes** ensuring data consistency when removing turns

### Future Considerations
- Potential for additional output formats (CSV, XML)
- Possible integration with external databases (PostgreSQL, MySQL)
- Extension points for custom block types
- API server mode for remote access
- Import/export functionality for data migration

## Known Limitations

### Current Version
- SQLite only (no multi-user concurrent access)
- Local file storage only
- Limited to text-based payloads (binary data requires encoding)
- No built-in backup/restore functionality
- Single database per instance

### Workarounds
- Use file-based backups for data protection
- Encode binary data as base64 in JSON payloads
- Use environment variables for different database instances
- Implement external backup scripts as needed

## Acknowledgments

Built using excellent open-source libraries:
- [Ent](https://entgo.io/) - Entity framework for Go
- [Cobra](https://github.com/spf13/cobra) - CLI library for Go
- [SQLite](https://sqlite.org/) - Embedded database engine

## License

This project is provided as-is for demonstration and educational purposes.

