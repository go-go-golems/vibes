---
Title: Getting Started — Vault Envrc Generator
Slug: vault-envrc-getting-started
Short: Build, configure, and run core workflows with Glazed
Topics:
- tutorial
- quick-start
- glazed
- vault
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: Tutorial
---

# Getting Started — Vault Envrc Generator

This comprehensive guide walks through building the CLI, configuring Vault settings and logging, and running common workflows using Glazed layers and structured outputs. By the end of this guide, you'll understand how to effectively use all five commands and integrate the tool into your development workflow.

## Prerequisites

Before getting started, ensure you have:
- **Go 1.21+** installed for building the application
- **HashiCorp Vault** instance accessible (local or remote)
- **Valid Vault token** with appropriate permissions
- **Basic understanding** of environment variables and shell scripting

## Quick Start Summary

For experienced users, here's the essential workflow:
```bash
# Build
GOCACHE=$(pwd)/.gocache go build -o vault-envrc-generator .

# Configure Vault access (choose one method)
export VAULT_ADDR=https://vault.example.com
export VAULT_TOKEN=your_token_here
# OR use token file: echo "your_token" > ~/.vault-token

# Generate environment file
./vault-envrc-generator batch -c your-config.yaml --output .envrc
source .envrc
```

## 1) Build the application

The application uses Go modules and can be built with a simple go build command:

```bash
cd vibes/2025-09-04/vault-envrc-generator/go-utility

# Build with custom cache directory (recommended)
GOCACHE=$(pwd)/.gocache go build -o vault-envrc-generator .

# Verify the build
./vault-envrc-generator --help
./vault-envrc-generator --version
```

**Build Options:**
- **Custom Cache**: `GOCACHE=$(pwd)/.gocache` uses local cache for faster rebuilds
- **Static Binary**: Add `-ldflags "-w -s"` for smaller binary size
- **Cross Compilation**: Use `GOOS=linux GOARCH=amd64` for different platforms

**Troubleshooting Build Issues:**
- Ensure Go 1.21+ is installed: `go version`
- Clear module cache if needed: `go clean -modcache`
- Update dependencies: `go mod tidy && go mod download`

## 2) Configure Vault Connection & Authentication

The application provides multiple ways to configure Vault connectivity, allowing flexibility across different environments and security requirements.

### **Vault Connection Parameters**

#### **Core Connection Settings**
- `--vault-addr`: Vault server address (default: `http://127.0.0.1:8200`)
- `--vault-token`: Explicit token (overrides other sources)
- `--vault-token-source`: Token resolution strategy (`auto|env|file|lookup`)
- `--vault-token-file`: Custom token file path

#### **Token Resolution Strategies**

**1. Auto Mode (Default)**
```bash
# Auto-discovers token from multiple sources in order:
# 1. --vault-token flag
# 2. VAULT_TOKEN environment variable  
# 3. ~/.vault-token file
# 4. Vault agent socket (if available)
./vault-envrc-generator list --path secrets/ --vault-token-source auto
```

**2. Environment Variable**
```bash
export VAULT_TOKEN=hvs.your_token_here
./vault-envrc-generator list --path secrets/ --vault-token-source env
```

**3. Token File**
```bash
echo "hvs.your_token_here" > ~/.vault-token
# OR custom file:
echo "hvs.your_token_here" > /path/to/custom-token
./vault-envrc-generator list --path secrets/ \
  --vault-token-source file --vault-token-file /path/to/custom-token
```

**4. Explicit Token**
```bash
./vault-envrc-generator list --path secrets/ \
  --vault-token hvs.your_token_here
```

### **Environment Variables**
The application respects standard Vault environment variables:
```bash
export VAULT_ADDR=https://vault.company.com:8200
export VAULT_TOKEN=hvs.your_token_here
export VAULT_CACERT=/path/to/ca.pem        # For TLS verification
export VAULT_CLIENT_CERT=/path/to/cert.pem # For mTLS
export VAULT_CLIENT_KEY=/path/to/key.pem   # For mTLS
```

### **Connection Verification**
Test your connection before proceeding:
```bash
# Basic connectivity test
./vault-envrc-generator list --path secrets/ --depth 0

# Detailed connection info with debug logging
./vault-envrc-generator --log-level debug \
  list --path secrets/ --depth 0 --vault-addr https://vault.example.com
```

## 3) Configure Logging & Observability

The application uses **zerolog** for structured, high-performance logging with multiple configuration options.

### **Logging Configuration**

#### **Log Levels**
- `trace`: Extremely detailed execution flow (use sparingly)
- `debug`: Detailed internal operations and API calls
- `info`: High-level operations and progress (default)
- `warn`: Recoverable issues and fallback operations
- `error`: Failed operations requiring attention
- `fatal`: Unrecoverable errors that terminate execution

#### **Log Formats**
- `text`: Human-readable format (default)
- `json`: Machine-readable structured format

#### **Log Destinations**
- Default: stderr (doesn't interfere with stdout output)
- File: `--log-file /path/to/logfile.log`

### **Logging Examples**

**Development/Debugging:**
```bash
# Detailed debugging with human-readable format
./vault-envrc-generator --log-level debug --log-format text \
  batch -c config.yaml --dry-run

# Trace-level logging to file
./vault-envrc-generator --log-level trace --log-file debug.log \
  seed -c seed.yaml --dry-run
```

**Production/Monitoring:**
```bash
# Structured JSON logs for log aggregation
./vault-envrc-generator --log-level info --log-format json \
  batch -c prod-config.yaml --output /etc/app/env
```

**CI/CD Pipelines:**
```bash
# Minimal logging for clean CI output
./vault-envrc-generator --log-level warn \
  generate --path secrets/ci/app --output ci.envrc
```

## 4) Single Path Generation (`generate` command)

The `generate` command is perfect for quick secret extraction and simple use cases. It focuses on a single Vault path and provides flexible output options.

### **Basic Usage Patterns**

#### **Preview Without Writing Files**
```bash
# Preview envrc format to stdout
./vault-envrc-generator generate \
  --path secrets/environments/development/shared/database \
  --format envrc --dry-run

# Preview with key transformation
./vault-envrc-generator generate \
  --path secrets/app/api-keys \
  --format envrc --transform-keys --prefix "MYAPP_" --dry-run
```

#### **Generate Configuration Files**
```bash
# Generate JSON configuration
./vault-envrc-generator generate \
  --path secrets/providers/openai \
  --format json --sort-keys \
  --output config/openai.json

# Generate YAML for Kubernetes secrets
./vault-envrc-generator generate \
  --path secrets/k8s/app-secrets \
  --format yaml --sort-keys \
  --output k8s/app-secrets.yaml
```

#### **Key Filtering and Transformation**
```bash
# Include only specific keys
./vault-envrc-generator generate \
  --path secrets/database \
  --include-keys "host,port,database,username,password" \
  --format envrc --output db.envrc

# Exclude sensitive keys for development
./vault-envrc-generator generate \
  --path secrets/app/config \
  --exclude-keys "*_prod,*_production" \
  --format envrc --transform-keys

# Add service prefix to avoid conflicts
./vault-envrc-generator generate \
  --path secrets/shared/redis \
  --prefix "REDIS_" --transform-keys \
  --format envrc --output redis.envrc
```

### **Real-World Examples**

Based on the successful run we performed earlier, here are practical examples:

#### **Database Configuration**
```bash
# Generate database environment variables
./vault-envrc-generator generate \
  --path secrets/environments/development/personal/$(vault token lookup -format=json | jq -r .data.meta.oidc_user_id)/local/db \
  --format envrc --output db.envrc

# Result: exports for DATABASE_URL, DB_HOST, DB_PASSWORD, etc.
```

#### **API Keys and Tokens**
```bash
# OpenAI API configuration
./vault-envrc-generator generate \
  --path secrets/providers/openai \
  --prefix "OPENAI_" --transform-keys \
  --format envrc --output openai.envrc

# Multiple provider APIs as JSON
./vault-envrc-generator generate \
  --path secrets/providers/anthropic \
  --format json --sort-keys \
  --output providers/anthropic.json
```

## 5) Batch Processing (`batch` command)

The `batch` command is the most powerful feature, allowing you to process multiple Vault paths with complex configurations. Based on our successful test run, here's how to use it effectively.

### **Understanding Batch Configuration**

A batch configuration file defines jobs with multiple sections. Here's the structure we successfully tested:

```yaml
# batch-personal.yaml (simplified)
base_path: secrets/environments/development/personal/{{ .Token.OIDCUserID }}/local

jobs:
  - name: personal-seed-envrc
    description: "Environment variables from personal Vault namespace"
    output: out/personal/seed.envrc
    format: envrc
    sections:
      - name: google-service-account
        description: "Google Service Account"
        path: google
        env_map:
          GOOGLE_EMAIL: client_email
          GOOGLE_PRIVATE_KEY: private_key
      
      - name: oauth-google
        description: "Google OAuth client"
        path: oauth/google
        include_keys: [client_id, client_secret]
        prefix: GOOGLE_
        transform_keys: true
```

### **Basic Batch Operations**

#### **Preview Mode (Dry Run)**
```bash
# Preview what would be generated (stdout)
./vault-envrc-generator batch -c batch-personal.yaml \
  --dry-run --format envrc

# Preview as JSON for inspection
./vault-envrc-generator batch -c batch-personal.yaml \
  --dry-run --format json --output -
```

#### **Generate Environment Files**
```bash
# Generate complete environment file
./vault-envrc-generator batch -c batch-personal.yaml

# Override output location
./vault-envrc-generator batch -c batch-personal.yaml \
  --output .envrc

# Generate in different format
./vault-envrc-generator batch -c batch-personal.yaml \
  --format json --output config.json
```

### **Output Semantics**

- Envrc: sections are appended to the target file with headers; when writing to an existing file, the global header is suppressed by default to avoid duplication.
- JSON/YAML: shallow merge of top-level keys into the target file; use `--sort-keys` for deterministic key ordering.

### **Advanced Batch Features**

#### **Section Types and Configuration**

From our successful test run, here are the key section configuration patterns:

**1. Environment Mapping (`env_map`)**
```yaml
sections:
  - name: custom-mapping
    path: slack
    env_map:
      MANUEL_SLACK_APP_ID: app_id  # Custom variable name
      SLACK_BOT_TOKEN: bot_token   # Direct mapping
```

**2. Key Filtering and Transformation**
```yaml
sections:
  - name: filtered-keys
    path: oauth/google
    include_keys: [client_id, client_secret]  # Only these keys
    prefix: GOOGLE_                           # Add prefix
    transform_keys: true                      # Uppercase & underscore
```

**3. Mixed Configuration**
```yaml
sections:
  - name: redis-base
    path: redis
    include_keys: [url]
    prefix: REDIS_
    transform_keys: true
    
  - name: redis-mento
    path: redis  # Same path, different processing
    include_keys: [server_addr, database]
    prefix: MENTO_SERVICE_REDIS_
    transform_keys: true
```

### **Real-World Batch Examples**

Based on our successful test with 19 sections processing various services:

#### **Complete Development Environment**
```bash
# Generate complete development environment
./vault-envrc-generator batch -c batch-personal.yaml \
  --output .envrc

# Source the generated environment
source .envrc

# Verify variables are loaded
echo $GOOGLE_CLIENT_ID
echo $OPENAI_API_KEY
```

#### **Service-Specific Configurations**
```bash
# Generate only database configuration
./vault-envrc-generator batch -c batch-database.yaml \
  --format json --output db-config.json

# Generate Kubernetes secrets
./vault-envrc-generator batch -c batch-k8s.yaml \
  --format yaml --output k8s-secrets.yaml
```

#### **Multi-Environment Processing**
```bash
# Development environment
./vault-envrc-generator batch -c batch-config.yaml \
  --base-path secrets/environments/development \
  --output dev.envrc

# Production environment  
./vault-envrc-generator batch -c batch-config.yaml \
  --base-path secrets/environments/production \
  --output prod.envrc
```

##  b6) Vault Exploration (`list` command)

The `list` command provides powerful Vault exploration and auditing capabilities with Glazed's structured output formats.

### **Basic Listing Operations**

#### **Directory Structure Exploration**
```bash
# List top-level directories
./vault-envrc-generator list --path secrets/ --depth 1 --output table

# Explore development environment structure  
./vault-envrc-generator list \
  --path secrets/environments/development/ \
  --depth 2 --output json

# Deep exploration with depth limit
./vault-envrc-generator list \
  --path secrets/environments/ \
  --depth 3 --output yaml
```

#### **Different Output Formats**
```bash
# Human-readable table format
./vault-envrc-generator list --path secrets/providers/ \
  --depth 1 --output table

# JSON for programmatic processing
./vault-envrc-generator list --path secrets/apps/ \
  --depth 2 --output json > vault-inventory.json

# CSV for spreadsheet import
./vault-envrc-generator list --path secrets/ \
  --depth 1 --output csv > vault-structure.csv

# YAML for documentation
./vault-envrc-generator list --path secrets/environments/ \
  --depth 2 --output yaml > environments.yaml
```

### **Value Inspection and Security**

#### **Include Values with Censoring**
```bash
# Show censored values for structure understanding
./vault-envrc-generator list --path secrets/database/ \
  --include-values --censor "***REDACTED***" \
  --output yaml

# Different censoring patterns
./vault-envrc-generator list --path secrets/api-keys/ \
  --include-values --censor "sk-...HIDDEN" \
  --output table

# JSON with censored values for documentation
./vault-envrc-generator list --path secrets/oauth/ \
  --include-values --censor "XXXXX" \
  --output json > oauth-structure.json
```

### **Audit and Documentation Use Cases**

#### **Security Audit**
```bash
# Generate complete Vault inventory
./vault-envrc-generator list --path secrets/ \
  --depth 5 --output json > vault-audit-$(date +%Y%m%d).json

# Check specific environment access
./vault-envrc-generator list \
  --path secrets/environments/production/ \
  --depth 3 --include-values --censor "***" \
  --output yaml
```

#### **Access Verification**
```bash
# Test read access to all paths
./vault-envrc-generator list --path secrets/ \
  --depth 2 --output table

# Verify specific service access
./vault-envrc-generator list --path secrets/services/myapp/ \
  --include-values --censor "..." --output json
```

## 7) Vault Population (`seed` command)

The `seed` command populates Vault from local sources. Based on our successful test with 17 different paths, here's how to use it effectively.

### **Understanding Seed Configuration**

From our successful test, the seed configuration supports multiple data sources:

```yaml
# seed-personal.yaml (simplified)
base_path: secrets/environments/development/personal/{{ .Token.OIDCUserID }}/local

sets:
  - path: core
    env:
      OP_ACCOUNT: OP_ACCOUNT          # From environment variable
      VAULT_ADDR: VAULT_ADDR
      
  - path: google
    env:
      client_email: GOOGLE_EMAIL       # Map env var to vault key
      private_key: GOOGLE_PRIVATE_KEY
      
  - path: db
    data:                              # Static data
      type: postgres
      username: postgres
      password: your_password_here
      server: localhost
      port: "5432"
```

### **Seed Operations**

#### **Preview Mode (Dry Run)**
```bash
# See what would be written to Vault
./vault-envrc-generator seed -c seed-personal.yaml --dry-run

# With debug logging to see detailed operations
./vault-envrc-generator --log-level debug \
  seed -c seed-personal.yaml --dry-run
```

#### **Actual Seeding**
```bash
# Populate Vault with secrets
./vault-envrc-generator seed -c seed-personal.yaml

# Seed with custom base path
./vault-envrc-generator seed -c seed.yaml \
  --base-path secrets/environments/staging
```

### **Data Source Types**

From our successful test, here are the three data source types:

#### **Environment Variables (`env`)**
```yaml
sets:
  - path: api-keys
    env:
      openai_key: OPENAI_API_KEY      # Vault key: env var
      anthropic_key: ANTHROPIC_API_KEY
```

#### **Static Data (`data`)**
```yaml
sets:
  - path: database
    data:
      host: localhost
      port: "5432"
      ssl_mode: disable
```

#### **File Contents (`files`)**
```yaml
sets:
  - path: certificates
    files:
      ca_cert: /path/to/ca.pem        # File content as value
      client_cert: /path/to/client.pem
```

### **Real-World Seeding Examples**

#### **Development Environment Bootstrap**
```bash
# Set up complete development environment
./vault-envrc-generator seed -c dev-bootstrap.yaml --dry-run
./vault-envrc-generator seed -c dev-bootstrap.yaml

# Verify seeding worked
./vault-envrc-generator list --path secrets/environments/development/ \
  --depth 2 --output table
```

#### **Service Migration**
```bash
# Migrate secrets from old system
./vault-envrc-generator seed -c migration.yaml --dry-run
./vault-envrc-generator seed -c migration.yaml

# Generate new environment files
./vault-envrc-generator batch -c batch-migrated.yaml --output .envrc
```

## 8) Quick Exploration (`interactive` command)

Perfect for learning and ad-hoc exploration:

```bash
# Interactive exploration and generation
./vault-envrc-generator interactive

# Follow the prompts to:
# 1. Choose Vault path
# 2. Select include/exclude patterns
# 3. Configure prefix and transformation
# 4. Choose output format
# 5. Preview and save results
```

## 9) Integration Patterns & Workflows

### **Development Workflow**
```bash
# 1. Bootstrap development secrets
./vault-envrc-generator seed -c dev-seed.yaml --dry-run
./vault-envrc-generator seed -c dev-seed.yaml

# 2. Generate development environment
./vault-envrc-generator batch -c dev-batch.yaml --output .envrc

# 3. Load environment and start development
source .envrc
npm run dev
```

### **CI/CD Integration**
```bash
# Generate environment for CI
./vault-envrc-generator batch -c ci-config.yaml \
  --format json --output ci-secrets.json

# Generate Kubernetes secrets
./vault-envrc-generator batch -c k8s-config.yaml \
  --format yaml --output k8s-secrets.yaml
kubectl apply -f k8s-secrets.yaml
```

### **Multi-Environment Management**
```bash
# Development
./vault-envrc-generator batch -c app-config.yaml \
  --base-path secrets/environments/development \
  --output dev.envrc

# Staging  
./vault-envrc-generator batch -c app-config.yaml \
  --base-path secrets/environments/staging \
  --output staging.envrc

# Production
./vault-envrc-generator batch -c app-config.yaml \
  --base-path secrets/environments/production \
  --output prod.envrc
```

## Best Practices & Tips

### **Configuration Management**
- **Version Control**: Keep seed and batch configs in git
- **Environment Separation**: Use different configs for dev/staging/prod
- **Template Paths**: Use Go templates for dynamic path construction
- **Documentation**: Add descriptions to all jobs and sections

### **Security Considerations**
- **Token Security**: Use token files or environment variables, not command-line flags
- **Dry Run First**: Always test with `--dry-run` before actual operations
- **Audit Logs**: Use debug logging for audit trails
- **Least Privilege**: Use Vault tokens with minimal required permissions

### **Operational Excellence**
- **Deterministic Output**: Use `--sort-keys` for consistent file generation
- **Error Handling**: Use `--continue-on-error` for resilient batch processing
- **Logging**: Configure appropriate log levels for different environments
- **Monitoring**: Parse structured JSON logs for operational insights

### **Performance Optimization**
- **Batch Over Generate**: Use batch command for multiple paths
- **Connection Reuse**: Single command execution reuses Vault connections
- **Depth Limits**: Use appropriate depth limits for list operations
- **Output Formats**: Choose appropriate formats for downstream consumption

## Troubleshooting

### **Common Issues**
- **Connection Refused**: Check `VAULT_ADDR` and network connectivity
- **Authentication Failed**: Verify token validity with `vault token lookup`
- **Permission Denied**: Ensure token has read/write permissions for target paths
- **Path Not Found**: Use `list` command to verify path existence

### **Debug Techniques**
```bash
# Enable debug logging
./vault-envrc-generator --log-level debug [command]

# Test connection
./vault-envrc-generator list --path secrets/ --depth 0

# Verify token
vault token lookup

# Check Vault status
vault status
```

This comprehensive guide provides everything needed to effectively use the Vault Envrc Generator in real-world scenarios, from simple secret extraction to complex multi-environment deployments.
