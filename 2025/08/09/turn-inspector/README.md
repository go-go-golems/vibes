# Turn Inspector CLI

A comprehensive CLI tool for inspecting and managing conversation turns with blocks and metadata, built using Go, Ent ORM, and Cobra CLI framework.

## Overview

Turn Inspector provides a powerful interface for managing conversation data with support for:
- **Structured conversation turns** with ordered blocks
- **Rich metadata** at both turn and block levels
- **Multiple block types**: user messages, LLM responses, tool calls, tool usage, system messages
- **Flexible querying** by metadata, content, and block types
- **Database statistics** and analytics

## Features

### Core Commands

- **`create turn`** - Create new conversation turns with blocks and metadata
- **`list turns`** - List all turns with summary information
- **`show turn`** - Display detailed turn information including all blocks and metadata
- **`show blocks`** - Show blocks for a specific turn
- **`query turns`** - Search turns by metadata, content, or block types
- **`stats`** - Show database statistics and analytics
- **`delete turn`** - Remove specific turns
- **`delete all`** - Remove all turns (with confirmation)

### Data Model

The tool uses a sophisticated data model based on Ent ORM:

#### Turn
- Unique ID and timestamps
- Collection of ordered blocks
- Turn-level metadata

#### Block
- Ordered position within a turn
- Block kind: `user`, `llm_text`, `tool_call`, `tool_use`, `system`, `other`
- Optional role (e.g., "assistant", "user", "tool")
- JSON payload for flexible content storage
- Block-level metadata

#### Metadata
- Source-key-value triplets
- Attached to both turns and blocks
- Enables rich querying and categorization

## Installation

### Prerequisites

- Go 1.24+ (latest version recommended)
- SQLite3 support (included)
- Build tools for CGO

### Build from Source

```bash
# Clone or extract the project
cd turn-inspector

# Install dependencies
go mod tidy

# Build the binary
go build -o turn-inspector .
```

## Quick Start

### 1. Create Your First Turn

```bash
# Simple user-assistant conversation
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Hello, how are you?"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Hello! I am doing well, thank you for asking."}
  }
]' --metadata '{"source":"session","key":"id","value":"demo-001"}'
```

### 2. List All Turns

```bash
./turn-inspector list turns
```

### 3. Show Turn Details

```bash
./turn-inspector show turn --id 1
```

### 4. Query Turns

```bash
# Find turns by metadata
./turn-inspector query turns --metadata-key session

# Find turns containing specific text
./turn-inspector query turns --text "weather"

# Find turns with tool calls
./turn-inspector query turns --block-kind tool_call
```

## Usage Examples

### Creating Complex Conversations

#### Tool-Calling Conversation
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "What is the weather in New York?"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "get_weather",
      "args": {"city": "New York", "units": "fahrenheit"}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "72°F, partly cloudy"
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "The weather in New York is 72°F and partly cloudy."}
  }
]' --metadata '{"source":"session","key":"type","value":"weather_query"}'
```

#### Error Handling Conversation
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Calculate square root of -1"}
  },
  {
    "order": 1,
    "kind": "system",
    "role": "system",
    "payload": {
      "error": "Invalid operation: cannot calculate square root of negative number",
      "error_code": "MATH_ERROR_001"
    }
  },
  {
    "order": 2,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I cannot calculate the square root of -1 using real numbers."}
  }
]' --metadata '{"source":"debug","key":"error_handled","value":"true"}'
```

### Querying and Analysis

#### Find All Tool-Related Conversations
```bash
./turn-inspector query turns --block-kind tool_call
```

#### Find Conversations by User
```bash
./turn-inspector query turns --metadata-key user --metadata-value alice
```

#### Search by Content
```bash
./turn-inspector query turns --text "error"
```

#### Get Database Statistics
```bash
# Basic stats
./turn-inspector stats

# Detailed stats with breakdowns
./turn-inspector stats --detailed
```

### Output Formats

#### JSON Output
```bash
./turn-inspector show turn --id 1 --json
```

#### Table Output (default)
```bash
./turn-inspector list turns
./turn-inspector show turn --id 1
```

## Configuration

### Database Location

By default, the tool creates a SQLite database file named `turns.db` in the current directory. You can specify a different location using the `TURN_INSPECTOR_DB` environment variable:

```bash
export TURN_INSPECTOR_DB="/path/to/my/database.db"
./turn-inspector stats
```

## Block Types

The tool supports the following block types:

- **`user`** - User messages and input
- **`llm_text`** - LLM-generated text responses
- **`tool_call`** - Tool invocation requests
- **`tool_use`** - Tool execution results
- **`system`** - System messages and notifications
- **`other`** - Custom or miscellaneous block types

## Metadata Structure

Metadata follows a source-key-value pattern:

- **Source**: Category or origin of the metadata (e.g., "session", "user", "runtime")
- **Key**: Specific metadata field name
- **Value**: String value for the metadata

Examples:
```json
{"source": "session", "key": "id", "value": "abc123"}
{"source": "user", "key": "name", "value": "Alice"}
{"source": "runtime", "key": "latency_ms", "value": "245"}
```

## Demo and Testing

The project includes comprehensive test scenarios and a demo script:

### Run Test Scenarios
```bash
./test-scenarios.sh
```

This creates 6 diverse conversation scenarios:
1. Simple user-assistant conversation
2. Tool-calling conversation (weather)
3. Complex multi-tool conversation (travel)
4. Error handling conversation
5. Long customer support conversation
6. Code assistance conversation

### Run Full Demo
```bash
./demo-script.sh
```

This demonstrates all CLI features with the test data.

## Architecture

### Technology Stack
- **Go 1.24+** - Core language
- **Ent ORM** - Database schema and queries
- **Cobra CLI** - Command-line interface framework
- **SQLite3** - Embedded database
- **JSON** - Flexible payload storage

### Database Schema
- **turns** - Main conversation turn records
- **turn_metadata** - Turn-level metadata
- **blocks** - Individual conversation blocks
- **block_metadata** - Block-level metadata

### Key Design Decisions
- **Ordered blocks** ensure conversation flow integrity
- **JSON payloads** provide flexibility for different content types
- **Rich metadata** enables powerful querying and categorization
- **Unique constraints** prevent data duplication
- **Foreign key relationships** maintain data consistency

## Performance Considerations

- **Indexed queries** for efficient metadata and block searches
- **Batch operations** for creating complex turns
- **Optimized joins** for loading related data
- **SQLite** provides excellent performance for local use

## Troubleshooting

### Common Issues

1. **Database locked errors**
   - Ensure no other instances are running
   - Check file permissions

2. **JSON parsing errors**
   - Validate JSON syntax in blocks and metadata
   - Use proper escaping in shell commands

3. **Schema errors**
   - Delete the database file to recreate schema
   - Rebuild the binary after schema changes

### Debug Mode

Set environment variable for verbose logging:
```bash
export TURN_INSPECTOR_DEBUG=1
./turn-inspector [command]
```

## Contributing

The codebase is well-structured for extensions:

- **Add new block types** in `ent/schema/block.go`
- **Add new commands** in `cmd/` directory
- **Extend queries** using Ent's query builder
- **Add output formats** using Cobra's flag system

## License

This project is provided as-is for demonstration and educational purposes.

## Support

For issues or questions:
1. Check the troubleshooting section
2. Review the demo script for usage examples
3. Examine the test scenarios for data structure examples

