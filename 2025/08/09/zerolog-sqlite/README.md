# Zerolog SQLite Backend

A high-performance SQLite backend for the [zerolog](https://github.com/rs/zerolog) logging library in Go. This project provides structured logging with efficient querying capabilities using a key-value storage approach for log fields.

## Features

- **High Performance**: Optimized SQLite schema with key-value field storage
- **Structured Logging**: Full support for zerolog's structured logging features
- **Efficient Querying**: Fast filtering by level, timestamp, message content, and custom fields
- **CLI Tool**: Powerful command-line interface for log introspection and analysis
- **Multiple Output Formats**: JSON and table output formats
- **Database Management**: Built-in cleanup and maintenance tools

## Architecture

The backend uses a normalized database schema with separate tables for logs and fields:

- **logs**: Core log entries (id, timestamp, level, message, caller, stack, created_at)
- **log_fields**: Key-value pairs for structured data with proper typing
- **Indexes**: Optimized for common query patterns

This approach provides better queryability and performance compared to storing everything as JSON.

## Installation

```bash
# Clone the repository
git clone <repository-url>
cd zerolog-sqlite

# Build the library and CLI tool
go build .
cd cmd/logctl && go build -o logctl .
```

## Usage

### Basic Logging

```go
package main

import (
    "os"
    "github.com/rs/zerolog"
    "github.com/rs/zerolog/log"
)

func main() {
    // Create SQLite writer
    writer, err := NewSQLiteWriter("./logs.db")
    if err != nil {
        panic(err)
    }
    defer writer.Close()

    // Configure zerolog
    log.Logger = zerolog.New(writer).With().Timestamp().Logger()

    // Log with structured data
    log.Info().
        Str("user_id", "12345").
        Int("age", 30).
        Bool("active", true).
        Msg("User login")

    log.Error().
        Str("service", "payment").
        Str("error", "connection timeout").
        Msg("Payment processing failed")
}
```

### Advanced Configuration

```go
// Custom configuration
config := SQLiteConfig{
    DatabasePath: "./custom-logs.db",
    MaxOpenConns: 10,
    MaxIdleConns: 5,
    BatchSize:    100,
    FlushInterval: 5 * time.Second,
}

writer, err := NewSQLiteWriterWithConfig(config)
if err != nil {
    panic(err)
}
```

### Querying Logs

```go
// Create database connection
db, err := NewDatabase("./logs.db")
if err != nil {
    panic(err)
}
defer db.Close()

// Create querier
querier := NewLogQuerier(db)

// Query with filters
options := QueryOptions{
    Levels:    []string{"error", "warn"},
    StartTime: &startTime,
    EndTime:   &endTime,
    Fields:    map[string]string{"user_id": "12345"},
    Limit:     100,
}

logs, err := querier.QueryLogs(options)
if err != nil {
    panic(err)
}

// Get statistics
stats, err := querier.GetLogStats(options)
if err != nil {
    panic(err)
}
```

## CLI Tool (logctl)

The included CLI tool provides powerful log introspection capabilities:

### Installation

```bash
cd cmd/logctl
go build -o logctl .
# Optionally install to PATH
sudo cp logctl /usr/local/bin/
```

### Commands

#### List Logs
```bash
# List recent logs
logctl list

# Filter by level
logctl list --level error,warn

# Filter by time
logctl list --since 1h
logctl list --since 2023-12-01T10:00:00Z

# Filter by message content
logctl list --message "database"

# Limit results
logctl list --limit 50

# JSON output
logctl list --output json
```

#### Statistics
```bash
# Show overall statistics
logctl stats

# Statistics for specific time period
logctl stats --since 24h

# JSON output
logctl stats --output json
```

#### Search
```bash
# Search log messages
logctl search "connection timeout"

# Search with level filter
logctl search "error" --level error

# Limit search results
logctl search "database" --limit 20
```

#### Field Analysis
```bash
# Show all fields and their usage
logctl fields

# JSON output for processing
logctl fields --output json
```

#### Database Cleanup
```bash
# Dry run - show what would be deleted
logctl cleanup --dry-run 30d

# Actually delete logs older than 30 days
logctl cleanup 30d

# Delete logs older than 1 week
logctl cleanup 1w
```

### Command Options

All commands support:
- `--database`: Path to SQLite database file (default: ./logs.db)
- `--output`: Output format (table, json)

## Performance

The SQLite backend is optimized for high-throughput logging:

- **Write Performance**: ~3000 logs/second on typical hardware
- **Query Performance**: Sub-millisecond queries for most operations
- **Storage Efficiency**: Normalized schema reduces storage overhead
- **Concurrent Access**: WAL mode for better concurrent read/write performance

### Benchmarks

Based on the included performance test:

```
Write performance: 1000 logs in 342ms (2916 logs/sec)
Query performance: Retrieved 100 logs in 3.2ms
Count performance: Counted 1000 logs in 0.5ms
```

## Database Schema

### logs table
```sql
CREATE TABLE logs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    level TEXT NOT NULL,
    message TEXT NOT NULL,
    caller TEXT,
    stack TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### log_fields table
```sql
CREATE TABLE log_fields (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    log_id INTEGER NOT NULL,
    field_name TEXT NOT NULL,
    field_value TEXT NOT NULL,
    field_type TEXT NOT NULL,
    FOREIGN KEY (log_id) REFERENCES logs(id) ON DELETE CASCADE
);
```

### Indexes
- `idx_logs_timestamp`: Fast time-based queries
- `idx_logs_level`: Fast level filtering
- `idx_logs_message`: Fast message search
- `idx_log_fields_log_id`: Fast field lookups
- `idx_log_fields_name_value`: Fast field filtering

## Configuration

### SQLiteConfig Options

```go
type SQLiteConfig struct {
    DatabasePath  string        // Path to SQLite database file
    MaxOpenConns  int          // Maximum open connections
    MaxIdleConns  int          // Maximum idle connections
    BatchSize     int          // Batch size for bulk operations
    FlushInterval time.Duration // Flush interval for batched writes
}
```

### Default Configuration

```go
DefaultConfig = SQLiteConfig{
    DatabasePath:  "./logs.db",
    MaxOpenConns:  10,
    MaxIdleConns:  5,
    BatchSize:     100,
    FlushInterval: 5 * time.Second,
}
```

## Examples

See the `main.go` file for comprehensive examples including:

1. Basic logging setup
2. Structured logging with various data types
3. Complex nested objects
4. Error logging with context
5. Performance testing
6. Query examples

## Best Practices

1. **Database Location**: Use a dedicated directory for log databases
2. **Rotation**: Implement log rotation using the cleanup functionality
3. **Indexing**: The provided indexes cover most use cases, but consider custom indexes for specific query patterns
4. **Monitoring**: Use the stats functionality to monitor log volume and patterns
5. **Backup**: Regular database backups for important logs

## Troubleshooting

### Common Issues

1. **Database Locked**: Ensure proper connection management and close connections
2. **Performance**: Check database size and consider cleanup for old logs
3. **Disk Space**: Monitor disk usage, especially for high-volume logging

### Debug Mode

Enable debug logging to troubleshoot issues:

```go
log.Logger = log.Logger.Level(zerolog.DebugLevel)
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- [zerolog](https://github.com/rs/zerolog) - Fast and simple logger for Go
- [SQLite](https://sqlite.org/) - Self-contained, high-reliability SQL database engine
- [go-sqlite3](https://github.com/mattn/go-sqlite3) - SQLite driver for Go

