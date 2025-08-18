# Cayley Installation Notes

## Installation Options

### Ubuntu
```bash
snap install --edge --devmode cayley
```

### macOS
First install Homebrew (if not already installed), then:
```bash
brew install cayley
```

### Docker
```bash
docker run -p 64210:64210 cayleygraph/cayley
```

### Build from Source
This option is available but requires Go toolchain and building from the GitHub repository.

## Basic Usage
After installation, start Cayley with:
```bash
cayley http
```

This will start Cayley with:
- Default backend: memstore (in-memory)
- Web interface at: http://127.0.0.1:64210
- Version: 0.7.7 (as of documentation)

## Configuration
- Cayley can be configured via YAML/JSON configuration files
- Command line flags are also supported
- For SQL backends, additional configuration is needed

## Next Steps
- Need to research SQL backend configuration
- Need to understand how to set up with SQLite
- Need to understand the quad format for data ingestion



## Configuration Details

### SQL Backends Supported
- `postgres`: PostgreSQL database
- `cockroach`: CockroachDB cluster  
- `mysql`: MySQL or MariaDB
- `sqlite`: SQLite database (perfect for our needs)

### SQLite Configuration
For SQLite backend:
- `store.backend`: "sqlite"
- `store.address`: filepath of the SQLite database
- More options available on the SQLite driver page

### Sample Configuration File (cayley.yml)
```yaml
store:
  backend: sqlite
  address: /path/to/database.db
  read_only: false
```

### Configuration File Locations
Cayley looks for `cayley.yml` or `cayley.json` in:
1. Command line flag location
2. `$CAYLEY_CFG` environment variable
3. Current directory
4. `$HOME/.cayley/`
5. `/etc/`

### Key Points
- Command line flags take precedence over configuration file
- For large datasets, `bolt` backend is recommended over `memstore`
- SQLite backend stores graph data and indices in SQLite database
- Read-only mode can be enabled for testing/read-only instances

