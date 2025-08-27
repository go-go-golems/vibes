# Git Pre-commit Guard

A powerful and configurable git pre-commit hook utility written in Go that detects and prevents committing undesired files such as:

- **ELF binaries and executables** - Prevents accidental commits of compiled binaries
- **Large files** - Configurable size limits with directory-specific overrides  
- **Blocked MIME types** - Flexible MIME type filtering with allow/block lists

## Features

- 🔍 **Multi-layered Detection**: ELF magic numbers, file command patterns, and MIME type analysis
- ⚙️ **Highly Configurable**: YAML-based configuration with directory-specific overrides
- 🎨 **Rich Output**: Colorized console output and structured JSON reporting
- 🚀 **Fast & Reliable**: Built in Go for performance and cross-platform compatibility
- 🔧 **Easy Integration**: Simple installation as git pre-commit hook

## Installation

### Option 1: Download Binary (Recommended)

1. Download the latest binary for your platform from the releases page
2. Make it executable: `chmod +x git-precommit-guard`
3. Move to your PATH: `sudo mv git-precommit-guard /usr/local/bin/`

### Option 2: Build from Source

```bash
# Clone the repository
git clone <repository-url>
cd git-precommit-guard

# Build the binary
go build -o git-precommit-guard .

# Install to PATH
sudo mv git-precommit-guard /usr/local/bin/
```

## Quick Start

1. **Create a configuration file** in your repository root:

```bash
# Copy the sample configuration
cp .precommit-guard.yml.example .precommit-guard.yml
```

2. **Install the git hook**:

```bash
cd your-git-repository
git-precommit-guard install
```

3. **Test the configuration**:

```bash
git-precommit-guard validate-config
```

That's it! The hook will now run automatically before each commit.

## Configuration

Create a `.precommit-guard.yml` file in your repository root:

```yaml
version: "1.0"

settings:
  fail_fast: true
  timeout: "10s"

global_excludes:
  - ".git/*"
  - "node_modules/*"
  - "vendor/*"
  - "*.md"

rules:
  elf_detection:
    enabled: true
    severity: "error"
    config:
      elf_magic: "7f454c46"
      file_patterns:
        - "ELF.*executable"
        - "ELF.*shared object"
      mime_types:
        - "application/x-executable"
        - "application/x-sharedlib"
      message: "ELF binary detected: {{.File}}. Use Git LFS or add to excludes"
      directory_overrides:
        "bin/*":
          enabled: false  # Allow binaries in bin/ directory

  file_size:
    enabled: true
    severity: "error"
    config:
      max_size_mb: 10
      warn_size_mb: 5
      message: "File {{.File}} ({{.SizeMB}}MB) exceeds size limit ({{.MaxSizeMB}}MB)"
      directory_overrides:
        "assets/*":
          max_size_mb: 50  # Allow larger files in assets/

  mime_detection:
    enabled: true
    severity: "warning"
    config:
      blocked_types:
        - "application/octet-stream"
        - "application/x-msdownload"
      allowed_types:
        - "text/*"
        - "application/json"
      message: "Binary file type detected: {{.File}} ({{.MimeType}})"

reporting:
  format: "console"
  colors: true
  summary: true
```

## Usage

### As Pre-commit Hook

Once installed, the hook runs automatically:

```bash
git add some-file.txt
git commit -m "Add file"  # Hook runs automatically
```

To bypass the hook for a specific commit:

```bash
git commit --no-verify -m "Skip hook"
```

### Manual Checking

Check staged files:

```bash
git-precommit-guard check
```

Check specific files:

```bash
git-precommit-guard check --all file1.txt file2.bin
```

Get JSON output:

```bash
git-precommit-guard check --json
```

Verbose output with details:

```bash
git-precommit-guard check --verbose
```

### Configuration Management

Validate configuration:

```bash
git-precommit-guard validate-config
```

Use custom config file:

```bash
git-precommit-guard check --config /path/to/config.yml
```

## Directory Overrides

Configure different rules for different directories:

```yaml
rules:
  elf_detection:
    config:
      directory_overrides:
        "bin/*":
          enabled: false        # Disable ELF detection in bin/
        "build/*":
          enabled: false        # Disable ELF detection in build/
        
  file_size:
    config:
      directory_overrides:
        "assets/*":
          max_size_mb: 100      # Allow 100MB files in assets/
        "docs/*":
          max_size_mb: 20       # Allow 20MB files in docs/
        "test/fixtures/*":
          enabled: false        # No size limits in test fixtures
```

## Exit Codes

- `0`: All checks passed
- `1`: One or more checks failed (blocks commit)

## Examples

### Detecting ELF Binaries

```bash
$ git-precommit-guard check --all /bin/ls
❌ FAIL [elf_detection] ELF binary detected: /bin/ls. Use Git LFS or add to excludes
```

### File Size Limits

```bash
$ git-precommit-guard check --all large-file.bin
❌ FAIL [file_size] File large-file.bin (15.00MB) exceeds size limit (10.00MB)
```

### MIME Type Detection

```bash
$ git-precommit-guard check --all binary-file
⚠️ WARN [mime_detection] Binary file type detected: binary-file (application/octet-stream)
```

## Troubleshooting

### Hook Not Running

1. Check if hook is installed: `ls -la .git/hooks/pre-commit`
2. Ensure hook is executable: `chmod +x .git/hooks/pre-commit`
3. Verify git-precommit-guard is in PATH: `which git-precommit-guard`

### Configuration Issues

1. Validate syntax: `git-precommit-guard validate-config`
2. Check file location: `.precommit-guard.yml` in repository root
3. Verify YAML syntax with online validator

### Performance Issues

1. Reduce timeout in configuration
2. Add more patterns to `global_excludes`
3. Use `fail_fast: true` to stop on first failure

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Support

- Create an issue for bug reports
- Check existing issues for known problems
- Contribute improvements via pull requests

