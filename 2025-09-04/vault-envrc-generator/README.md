# Vault .envrc Generator

A comprehensive Go utility for generating .envrc files from HashiCorp Vault secrets with advanced features and audit logging.

## Features

- 🔐 **Multi-Engine Support**: Works with KV v1 and KV v2 secret engines
- 🎯 **Multiple Interfaces**: CLI, Interactive, and Batch processing modes
- 🎨 **Flexible Output**: .envrc, JSON, and YAML formats
- 🔧 **Customization**: Prefix addition, key transformation, and filtering
- 📝 **Templates**: Custom Go templates for advanced formatting
- 📊 **Audit Integration**: Complete audit trail of all operations
- ⚡ **Performance**: Efficient processing with parallel batch operations

## Quick Start

### Prerequisites

- HashiCorp Vault server running and accessible
- Valid Vault token with appropriate permissions

### Installation

1. Download the binary from the releases or build from source:
```bash
go build -o vault-envrc-generator .
```

2. Set environment variables:
```bash
export VAULT_ADDR="http://127.0.0.1:8200"
export VAULT_TOKEN="your-vault-token"
```

### Basic Usage

```bash
# Test connectivity
./vault-envrc-generator test

# Generate .envrc from secrets
./vault-envrc-generator generate --path secret/myapp

# Interactive mode
./vault-envrc-generator interactive

# Batch processing
./vault-envrc-generator batch --config batch-config.yaml
```

## Commands

### `generate`
Generate .envrc files from Vault secrets with extensive customization options.

```bash
vault-envrc-generator generate [flags]

Flags:
  -p, --path string          Vault secret path (required)
  -o, --output string        Output file path (default ".envrc")
      --prefix string        Prefix for environment variable names
      --exclude strings      Keys to exclude (comma-separated)
      --include strings      Keys to include (comma-separated)
      --transform-keys       Transform keys to uppercase and replace - with _
      --template string      Custom template file
  -f, --format string        Output format: envrc, json, yaml (default "envrc")
      --dry-run             Show output without writing file
```

**Examples:**
```bash
# Basic generation
vault-envrc-generator generate --path secret/myapp

# With prefix and transformation
vault-envrc-generator generate --path secret/myapp --prefix MYAPP_ --transform-keys

# Exclude sensitive keys
vault-envrc-generator generate --path secret/myapp --exclude password,secret_key

# JSON output
vault-envrc-generator generate --path secret/myapp --format json

# Dry run
vault-envrc-generator generate --path secret/myapp --dry-run
```

### `interactive`
Interactive mode for guided secret selection and configuration.

```bash
vault-envrc-generator interactive
```

Features:
- Browse available Vault paths
- Select specific secrets to include
- Configure generation options interactively
- Preview output before saving

### `batch`
Process multiple Vault paths using a YAML configuration file.

```bash
vault-envrc-generator batch --config batch-config.yaml [flags]

Flags:
  -c, --config string       Batch configuration file (required)
      --parallel            Run jobs in parallel
      --continue-on-error   Continue processing if a job fails
```

**Batch Configuration Example:**
```yaml
jobs:
  - name: "Frontend App"
    path: "secret/frontend"
    output: "frontend/.envrc"
    prefix: "FRONTEND_"
    transform_keys: true
    exclude_keys: ["internal_key"]
    
  - name: "Backend API"
    path: "secret/backend"
    output: "backend/.envrc"
    format: "json"
```

### `test`
Test Vault connectivity and authentication.

```bash
vault-envrc-generator test [flags]

Flags:
  -v, --verbose   Enable verbose output
```

## Configuration

### Environment Variables
- `VAULT_ADDR`: Vault server address
- `VAULT_TOKEN`: Vault authentication token
- `VAULT_ENVRC_*`: Application-specific configuration

### Configuration File
Create `~/.vault-envrc-generator.yaml`:
```yaml
vault:
  addr: "https://vault.example.com:8200"
  token: "your-token"
output: ".envrc"
verbose: false
```

## Templates

Create custom templates for advanced formatting:

```go
# Custom .envrc template
{{range $key, $value := .}}
export {{$key}}="{{$value}}"
{{end}}

# Additional configuration
export GENERATED_AT="$(date)"
```

Use with:
```bash
vault-envrc-generator generate --path secret/app --template custom.tmpl
```

## Output Formats

### .envrc Format (Default)
```bash
export DATABASE_URL=postgresql://localhost:5432/myapp
export API_KEY=secret-key-123
export DEBUG_MODE=true
```

### JSON Format
```json
{
  "DATABASE_URL": "postgresql://localhost:5432/myapp",
  "API_KEY": "secret-key-123",
  "DEBUG_MODE": "true"
}
```

### YAML Format
```yaml
DATABASE_URL: postgresql://localhost:5432/myapp
API_KEY: secret-key-123
DEBUG_MODE: "true"
```

## Security Features

- **Audit Logging**: All Vault operations are logged for compliance
- **Secure Value Handling**: Proper escaping prevents injection attacks
- **Token Validation**: Comprehensive authentication checking
- **Permission Boundaries**: Respects Vault access policies

## Troubleshooting

### Common Issues

1. **Connection Failed**
   ```bash
   vault-envrc-generator test --verbose
   ```

2. **Permission Denied**
   - Verify token has read access to the secret path
   - Check Vault policies and permissions

3. **Secret Not Found**
   - Verify the secret path exists
   - Check if using correct KV engine version

4. **Template Errors**
   - Validate template syntax
   - Ensure template file is accessible

### Debug Mode
Enable verbose output for detailed information:
```bash
vault-envrc-generator generate --path secret/app --verbose
```

## Development

### Building from Source
```bash
git clone <repository>
cd vault-envrc-generator
go mod download
go build -o vault-envrc-generator .
```

### Running Tests
```bash
go test ./...
```

### Project Structure
```
vault-envrc-generator/
├── main.go                 # Application entry point
├── cmd/                    # CLI commands
│   ├── root.go            # Root command
│   ├── generate.go        # Generate command
│   ├── interactive.go     # Interactive mode
│   ├── batch.go           # Batch processing
│   └── test.go            # Test command
└── pkg/                   # Core packages
    ├── vault/             # Vault client
    └── envrc/             # Generation engine
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## Support

For issues and questions:
- Check the troubleshooting section
- Review the project documentation
- Open an issue on the repository

