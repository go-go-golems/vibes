# TTMP CLI - Quick Start Guide

## Installation

### Option 1: Use Pre-built Binary

```bash
# Extract the package
unzip ttmp-cli-package.zip
cd ttmp-cli-package/ttmp-cli

# Copy binary to PATH
sudo cp ttmp /usr/local/bin/
chmod +x /usr/local/bin/ttmp

# Verify installation
ttmp --version
```

### Option 2: Build from Source

```bash
cd ttmp-cli-package/ttmp-cli
go build -o ttmp .
sudo cp ttmp /usr/local/bin/
```

## Initial Setup

### 1. Create Vocabulary File

```bash
# Create doc directory
mkdir -p doc

# Copy example vocabulary
cp examples/doc/vocabulary.yaml doc/

# Or create your own
ttmp vocab list --category topics  # See available topics
```

### 2. Initialize Your First Ticket

```bash
# Basic initialization
ttmp init MEN-0001 --title "Setup documentation system" --topics documentation

# With owners and intent
ttmp init MEN-0002 --title "Feature X" --topics backend,api --owners alice,bob --intent long-term
```

### 3. Explore the Structure

```bash
# View created structure
ls -la ttmp/MEN-0001-setup-documentation-system/

# Check the index
cat ttmp/MEN-0001-setup-documentation-system/index.md
```

## Basic Workflow

### Step 1: Add Working Notes

```bash
cd ttmp/MEN-0001-setup-documentation-system
ttmp add working-note "initial exploration"

# Edit the generated file
vim various/01-initial-exploration.md
```

### Step 2: Link Related Files

```bash
ttmp relate --ticket MEN-0001 --files src/main.go,src/config.go
```

### Step 3: Create Design Documentation

```bash
ttmp add design-doc "system-architecture"

# Edit the design doc
vim design/01-system-architecture.md
```

### Step 4: List and Inspect

```bash
# List all tickets
ttmp list tickets

# List documents in a ticket
ttmp list docs --ticket MEN-0001

# Get JSON output
ttmp list tickets --output json
```

### Step 5: Update Metadata

```bash
# Mark ticket as active
ttmp meta update --doc index.md --field Status --value active

# Update intent
ttmp meta update --doc design/01-system-architecture.md --field Intent --value long-term
```

### Step 6: Run Health Checks

```bash
# Check all tickets
ttmp doctor

# Check specific ticket
ttmp doctor --ticket MEN-0001

# Get JSON output for CI/CD
ttmp doctor --output json
```

## Common Commands

### Vocabulary Management

```bash
# List topics
ttmp vocab list --category topics

# Add a new topic
ttmp vocab add topics --slug performance --description "Performance optimization work"

# List doc types
ttmp vocab list --category docTypes
```

### Querying

```bash
# List all tickets with specific fields
ttmp list tickets --fields ticket,status,topics

# Export to CSV
ttmp list tickets --output csv > tickets.csv

# Filter with jq
ttmp list tickets --output json | jq '.[] | select(.status == "active")'
```

### Document Creation

```bash
# Create different document types
ttmp add working-note "meeting notes"
ttmp add design-doc "api-design"
ttmp add reference "api-contract"
ttmp add playbook "deployment-steps"
```

## Help System

```bash
# View introduction
ttmp help introduction

# Step-by-step tutorial
ttmp help tutorial-basic-workflow

# Complete command reference
ttmp help commands-reference

# Metadata schema
ttmp help metadata-schema

# Vocabulary guide
ttmp help vocabulary-guide
```

## Examples

The `examples/` directory contains a complete test environment with:
- 5 sample tickets
- 15 documents of various types
- Complete vocabulary file
- Test scripts

Explore these examples to see the tool in action:

```bash
cd examples
ttmp list tickets
ttmp list docs --output json
ttmp doctor
```

## Next Steps

1. **Read the Full Report**: See `TTMP-CLI-REPORT.md` for comprehensive documentation
2. **Customize Vocabulary**: Edit `doc/vocabulary.yaml` for your project
3. **Integrate with Git**: Add ttmp/ directory to version control
4. **Set Up CI/CD**: Use `ttmp doctor --output json` in your pipeline
5. **Train Your Team**: Share `ttmp help introduction` with team members

## Tips

- **Use JSON output** for scripting: `--output json`
- **Filter fields** for cleaner output: `--fields ticket,status`
- **Run doctor regularly** to maintain documentation quality
- **Mark long-term docs** with `--intent long-term` for promotion to permanent docs
- **Link files early** with `ttmp relate` to maintain context

## Troubleshooting

### Command not found

```bash
# Ensure ttmp is in PATH
which ttmp

# If not, add to PATH or use full path
export PATH=$PATH:/path/to/ttmp
```

### Missing vocabulary.yaml

```bash
# Create from template
mkdir -p doc
cp examples/doc/vocabulary.yaml doc/
```

### Permission denied

```bash
# Make binary executable
chmod +x /usr/local/bin/ttmp
```

## Support

For detailed documentation, see:
- `README.md` - Project overview
- `TTMP-CLI-REPORT.md` - Complete implementation report
- `ttmp help <topic>` - Built-in help system

For issues and questions, please open an issue on GitHub.

