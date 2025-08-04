# Installation Instructions

## Quick Install (Binary)

1. Download the `diary` binary from the release
2. Make it executable: `chmod +x diary`
3. Move to your PATH: `sudo mv diary /usr/local/bin/`
4. Initialize your diary: `diary init`

## Build from Source

### Prerequisites

- Go 1.24.5 or later
- Git

### Steps

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd diary-cli
   ```

2. Install dependencies:
   ```bash
   go mod tidy
   ```

3. Build the binary:
   ```bash
   go build -o diary
   ```

4. (Optional) Install globally:
   ```bash
   sudo cp diary /usr/local/bin/
   ```

5. Initialize your diary:
   ```bash
   diary init
   ```

## First Time Setup

After installation, initialize your diary in your preferred location:

```bash
# In your Obsidian vault
cd /path/to/obsidian/vault
diary init

# Or in a new directory
mkdir ~/my-diary
cd ~/my-diary
diary init
```

This creates:
- Configuration file (`~/.diary-config.yaml`)
- Logs directory structure
- Today's diary file
- Sample README

## Verification

Test your installation:

```bash
# Check version and help
diary --help

# Add your first entry
diary add til "Successfully installed diary CLI"

# List entries
diary list

# Check configuration
diary config
```

## Troubleshooting

### Permission Issues
```bash
# Make binary executable
chmod +x diary

# Check PATH
echo $PATH
which diary
```

### Configuration Issues
```bash
# Check configuration
diary config

# Reset configuration
rm ~/.diary-config.yaml
diary init
```

### Build Issues
```bash
# Update Go modules
go mod tidy
go mod download

# Clean build
go clean
go build -o diary
```

## Next Steps

- Read the [Getting Started Guide](README.md#quick-start)
- Explore [Entry Types](pkg/doc/entry-types.md)
- Set up [Obsidian Integration](pkg/doc/obsidian-integration.md)
- Learn [Advanced Usage](pkg/doc/advanced-usage.md)

