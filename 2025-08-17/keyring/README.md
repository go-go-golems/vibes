# Keyring CLI System

A Go CLI keyring system using the glazed framework with SQLite backend, organized with one file per verb. This implementation provides hierarchical paths, profiles with fallback, and pluggable backends with auditing and key state management.

## Features

- **Hierarchical Paths**: Organize secrets using path segments (e.g., `openai/api_key`, `aws/ses/smtp_password`)
- **Profile Support**: Multiple profiles with fallback order (e.g., `work` → `default`)
- **SQLite Backend**: Robust local storage with full CRUD operations
- **Glazed Framework**: Multiple output formats (table, JSON, YAML, CSV)
- **Auditing System**: Complete audit trail of all operations
- **Key State Management**: Support for active, deprecated, and invalidated states
- **One File Per Verb**: Clean command organization following best practices

## Architecture

### Core Components

1. **Keyring Core Library** (`pkg/keyring/`)
   - `path.go` - Hierarchical path handling
   - `secret.go` - Secret structure with metadata and expiration
   - `backend.go` - Backend interfaces
   - `ring.go` - Core aggregator with profile fallback
   - `state.go` - Key state management
   - `audit.go` - Auditing system
   - `admin.go` - Administrative operations

2. **SQLite Backend** (`pkg/stores/sqlite/`)
   - `sqlite.go` - Main SQLite implementation
   - `adapter.go` - StateStore interface adapter

3. **CLI Commands** (`cmd/`)
   - `get.go` - Retrieve secrets
   - `put.go` - Store secrets
   - `simple_commands.go` - List and delete operations

## Installation

### Prerequisites

- Go 1.24.2 or later
- Build essentials (for CGO support)

### Build from Source

```bash
git clone <repository>
cd keyring
go mod tidy
go build -o keyring .
```

## Usage

### Basic Commands

#### Store a Secret
```bash
keyring put --path openai/api_key --value sk-xxx
```

#### Retrieve a Secret
```bash
keyring get --path openai/api_key
```

#### List Secrets
```bash
# List top-level paths
keyring list

# List under a prefix
keyring list --prefix openai/
```

#### Delete a Secret
```bash
keyring delete --path openai/api_key
```

### Output Formats

The keyring supports multiple output formats through the glazed framework:

```bash
# Table format (default)
keyring get --path openai/api_key

# JSON format
keyring get --path openai/api_key --output json

# YAML format
keyring get --path openai/api_key --output yaml

# CSV format
keyring list --output csv
```

### Advanced Features

#### Profiles
```bash
# Use specific profile
keyring get --path openai/api_key --profile work

# Set profile search order
keyring --profile work,default get --path openai/api_key
```

#### Metadata and Expiration
```bash
# Store secret with metadata
keyring put --path api/key --value secret --metadata '{"source":"manual","version":"1"}'

# Store secret with expiration
keyring put --path temp/token --value abc123 --expires-at 2024-12-31T23:59:59Z
```

#### Database Configuration
```bash
# Use custom database path
keyring --db-path /custom/path/keyring.db get --path openai/api_key

# Set actor for audit logging
keyring --actor john.doe put --path openai/api_key --value sk-xxx
```

## Database Schema

The SQLite backend uses three main tables:

### Secrets Table
```sql
CREATE TABLE secrets (
  profile TEXT NOT NULL,
  path    TEXT NOT NULL,
  value   TEXT NOT NULL,
  metadata TEXT,           -- JSON
  expires_at TEXT,         -- RFC3339 format
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  PRIMARY KEY (profile, path)
);
```

### Key States Table
```sql
CREATE TABLE key_states (
  profile TEXT NOT NULL,
  path    TEXT NOT NULL,
  status  INTEGER NOT NULL, -- 0 active, 1 deprecated, 2 invalidated
  since   TEXT NOT NULL,
  message TEXT,
  replace_with TEXT,
  reason  TEXT,
  delete_at_source INTEGER NOT NULL DEFAULT 0,
  extra   TEXT,             -- JSON
  updated_at TEXT NOT NULL,
  PRIMARY KEY (profile, path)
);
```

### Audit Events Table
```sql
CREATE TABLE audit_events (
  id      INTEGER PRIMARY KEY AUTOINCREMENT,
  at      TEXT NOT NULL,
  type    TEXT NOT NULL,
  profile TEXT,
  path    TEXT,
  backend TEXT,
  actor   TEXT,
  success INTEGER NOT NULL,
  err     TEXT,
  meta    TEXT         -- JSON
);
```

## Configuration

### Default Locations
- Database: `~/.config/keyring/keyring.db`
- Config: `~/.config/keyring/config.yaml` (future)

### Global Flags
- `--db-path`: SQLite database path
- `--profile`: Profile search order (comma-separated)
- `--actor`: Actor name for audit logging

## Demo Output

Here's a complete demonstration of the keyring functionality:

```
=== Keyring CLI Demo ===

1. Show help:
Usage:
  keyring [command]

Available Commands:
  completion  Generate the autocompletion script for the specified shell
  delete      Delete a secret from the keyring
  get         Retrieve a secret from the keyring
  help        Help about any command
  list        List secrets and paths in the keyring
  put         Store a secret in the keyring

2. Store some secrets:
+----------------+--------+---------+
| path           | status | profile |
+----------------+--------+---------+
| openai/api_key | stored | default |
+----------------+--------+---------+

3. List all top-level paths:
+--------+------+
| path   | type |
+--------+------+
| aws    | path |
| openai | path |
+--------+------+

4. Retrieve a secret:
+----------------+------------------+---------+---------+
| path           | value            | profile | backend |
+----------------+------------------+---------+---------+
| openai/api_key | sk-demo123456789 | default | sqlite  |
+----------------+------------------+---------+---------+

5. Get secret in JSON format:
[
{
  "backend": "sqlite",
  "path": "aws/access_key",
  "profile": "default",
  "value": "AKIADEMO123"
}
]

6. Delete a secret:
+----------------+---------+
| path           | status  |
+----------------+---------+
| aws/secret_key | deleted |
+----------------+---------+
```

## Implementation Details

### Glazed Framework Integration

The keyring uses the glazed framework for CLI command structure and output formatting. Each command follows the pattern:

1. **Command Struct**: Embeds `*cmds.CommandDescription`
2. **Settings Struct**: Maps command-line flags using `glazed.parameter` tags
3. **RunIntoGlazeProcessor**: Implements the core command logic
4. **Constructor Function**: Sets up parameters and layers

### Backend Architecture

The system uses a pluggable backend architecture:

- **Backend Interface**: Defines Get, Put, Delete, List operations
- **StateStore Interface**: Manages key lifecycle states
- **AuditSink Interface**: Records all operations for audit trails

### Error Handling

The system provides comprehensive error handling:

- `ErrNotFound`: Key not found in backend
- `ErrReadOnly`: Backend doesn't support writes
- `ErrInvalidated`: Key is invalidated and cannot be accessed

## Future Enhancements

The current implementation provides core functionality. Future enhancements could include:

1. **Additional Commands**: Complete implementation of deprecate, invalidate, reinstate, and audit commands
2. **Environment Backend**: Read secrets from environment variables
3. **File Backend**: YAML-based file storage
4. **Vault Integration**: HashiCorp Vault backend
5. **Configuration File**: YAML-based configuration
6. **Key Rotation**: Automatic key rotation capabilities
7. **Encryption**: At-rest encryption for SQLite storage

## Development

### Project Structure
```
keyring/
├── main.go                 # Main CLI entry point
├── go.mod
├── go.sum
├── pkg/
│   ├── keyring/           # Core keyring library
│   │   ├── path.go
│   │   ├── secret.go
│   │   ├── backend.go
│   │   ├── ring.go
│   │   ├── state.go
│   │   ├── audit.go
│   │   └── admin.go
│   └── stores/
│       └── sqlite/
│           ├── sqlite.go   # SQLite backend implementation
│           └── adapter.go  # StateStore adapter
├── cmd/                   # CLI commands (one file per verb)
│   ├── get.go
│   ├── put.go
│   └── simple_commands.go
└── README.md
```

### Testing

The implementation has been tested with:
- Basic CRUD operations (Create, Read, Update, Delete)
- Multiple output formats (table, JSON)
- Hierarchical path navigation
- Profile-based organization
- Error handling for missing keys

### Dependencies

- `github.com/go-go-golems/glazed` - CLI framework and output formatting
- `github.com/spf13/cobra` - Command-line interface
- `github.com/mattn/go-sqlite3` - SQLite driver

## License

This project is provided as-is for demonstration purposes. Please ensure compliance with all dependency licenses when using in production.

