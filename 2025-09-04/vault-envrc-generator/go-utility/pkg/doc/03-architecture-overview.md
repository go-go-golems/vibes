---
Title: Architecture Overview — Vault Envrc Generator
Slug: vault-envrc-architecture
Short: Deep dive into the system architecture, design patterns, and component interactions
Topics:
- architecture
- design-patterns
- glazed
- vault-client
- layers
- commands
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: Tutorial
---

# Architecture Overview — Vault Envrc Generator

The Vault Envrc Generator is built using modern Go patterns and leverages the **Glazed** CLI framework to provide a robust, extensible system for managing HashiCorp Vault secrets and generating environment configuration files.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           CLI Layer                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
│  │   batch     │  │  generate   │  │    seed     │  │    list     │ │
│  │   command   │  │   command   │  │   command   │  │   command   │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Glazed Framework                                │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │                    Parameter Layers                             │ │
│  │  • Vault Layer (connection settings)                           │ │
│  │  • Logging Layer (structured logging)                          │ │
│  │  • Command Settings Layer (per-command options)                │ │
│  └─────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Business Logic Layer                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
│  │   Batch     │  │    Envrc    │  │    Seed     │  │   Output    │ │
│  │ Processor   │  │  Generator  │  │   Runner    │  │   Writer    │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                       Vault Integration Layer                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │                    Vault Client Wrapper                        │ │
│  │  • KV v1/v2 Auto-detection                                     │ │
│  │  • Token Resolution (env, file, lookup)                       │ │
│  │  │  • Connection Health Checking                               │ │
│  │  • Path Templating Support                                     │ │
│  └─────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. CLI Commands (`cmds/`)

The application provides five main commands, each implementing the `gcmds.BareCommand` interface:

#### **BatchCommand** (`cmds/batch.go`)
- **Purpose**: Process multiple Vault paths from YAML configuration files
- **Key Features**: 
  - Multi-job processing with sections
  - Output aggregation (merge-only semantics)
  - Flexible format support (envrc, JSON, YAML)
  - Continue-on-error handling
- **Dependencies**: `batch.Processor`, `vault.Client`

#### **SeedCommand** (`cmds/seed.go`)
- **Purpose**: Populate Vault with secrets from local environment and files
- **Key Features**:
  - YAML-based seed specifications
  - Environment variable and file sourcing
  - Template-based path resolution
  - Dry-run capabilities
- **Dependencies**: `seed.Runner`, `vault.Client`

#### **GenerateCommand** (`cmds/generate.go`)
- **Purpose**: Generate configuration files from a single Vault path
- **Key Features**:
  - Single-path focused generation
  - Format transformation (envrc, JSON, YAML)
  - Key filtering and transformation
  - Template support

#### **ListCommand** (`cmds/list.go`)
- **Purpose**: Structured listing of Vault paths and secrets
- **Key Features**:
  - Recursive directory traversal
  - Glazed output formats (table, JSON, YAML, CSV)
  - Optional value censoring
  - Depth control

#### **InteractiveCommand** (`cmds/interactive.go`)
- **Purpose**: Interactive CLI for quick exploration and generation

### 2. Glazed Integration

The application leverages **Glazed** extensively for:

#### **Parameter Layers System**
- **Vault Layer** (`pkg/vaultlayer/`): Reusable Vault connection parameters
- **Logging Layer**: Structured logging with zerolog
- **Command Settings**: Per-command parameter definitions

#### **Structured Output**
- Consistent output formatting across all commands
- Support for JSON, YAML, CSV, and table formats
- Configurable field selection and transformation

### 3. Vault Integration (`pkg/vault/`)

#### **Client Wrapper** (`pkg/vault/client.go`)
The `vault.Client` provides a high-level abstraction over HashiCorp's Vault API:

```go
type Client struct {
    client *api.Client
}

// Key methods:
func (c *Client) GetSecrets(path string) (map[string]interface{}, error)
func (c *Client) PutSecrets(path string, data map[string]interface{}) error
func (c *Client) ListSecrets(path string) ([]string, error)
```

**Features**:
- **KV Version Auto-detection**: Automatically handles both KV v1 and v2 engines
- **Connection Health Checking**: Validates connectivity and authentication
- **Path Parsing**: Intelligent mount path and secret path separation

#### **Token Resolution** (`pkg/vault/token_loader.go`)
Sophisticated token resolution supporting:
- Environment variables (`VAULT_TOKEN`)
- Token files (`~/.vault-token`)
- Auto-discovery from multiple sources
- Explicit token specification

#### **Template Support** (`pkg/vault/templates.go`)
Go template rendering for dynamic path construction:
```go
func RenderTemplateString(s string, tctx TemplateContext) (string, error)
```

### 4. Business Logic Components

#### **Batch Processor** (`pkg/batch/`)
The batch processing system handles complex multi-section jobs:

```go
type Processor struct {
    Client *vault.Client
}

type Config struct {
    BasePath string `yaml:"base_path"`
    Jobs     []Job  `yaml:"jobs"`
}

type Job struct {
    Name        string    `yaml:"name"`
    Description string    `yaml:"description,omitempty"`
    Output      string    `yaml:"output"`
    Sections    []Section `yaml:"sections,omitempty"`
    // ... additional configuration
}
```

**Key Features**:
- **Section-based Processing**: Each job can have multiple sections with different configurations
- **Merge-Only Output**: JSON/YAML shallow merge; envrc appends with headers
- **Flexible Filtering**: Per-section include/exclude key patterns
- **Environment Mapping**: Direct environment variable to Vault key mapping via `env_map`

#### **Envrc Generator** (`pkg/envrc/`)
Handles format-specific output generation:

```go
type Generator struct {
    options *Options
}

type Options struct {
    Prefix         string
    ExcludeKeys    []string
    IncludeKeys    []string
    TransformKeys  bool
    Format         string
    SortKeys       bool
    // ... additional options
}
```

**Features**:
- **Multi-format Support**: envrc, JSON, YAML
- **Key Transformation**: Uppercase conversion, hyphen to underscore
- **Deterministic Output**: Sorted keys for consistent results
- **Value Escaping**: Proper shell escaping for envrc format
- **Template Support**: Custom template file support

#### **Seed Runner** (`pkg/seed/`)
Manages the seeding of Vault from local sources:

```go
type Spec struct {
    BasePath string `yaml:"base_path"`
    Sets     []Set  `yaml:"sets"`
}

type Set struct {
    Path  string            `yaml:"path"`
    Data  map[string]string `yaml:"data"`      // Static data
    Env   map[string]string `yaml:"env"`       // From environment
    Files map[string]string `yaml:"files"`     // From files
}
```

## Design Patterns

### 1. **Command Pattern with Glazed**
Each command implements the `gcmds.BareCommand` interface, providing consistent parameter handling and execution patterns.

### 2. **Strategy Pattern for Output Formats**
The `envrc.Generator` uses strategy pattern for different output formats (envrc, JSON, YAML).

### 3. **Template Method Pattern**
The batch processor follows template method pattern with customizable steps for each section.

### 4. **Adapter Pattern**
The `vault.Client` acts as an adapter, providing a simplified interface over the complex Vault API.

### 5. **Builder Pattern**
Parameter layers use builder pattern for constructing command configurations.

## Data Flow

### Typical Batch Processing Flow:
1. **Configuration Loading**: YAML config parsed into `batch.Config`
2. **Vault Connection**: Client initialized with resolved token
3. **Job Processing**: Each job processed sequentially
4. **Section Processing**: Within each job, sections processed in order
5. **Data Fetching**: Secrets retrieved from Vault paths
6. **Transformation**: Keys filtered, transformed, prefixed as configured
7. **Output Generation**: Content generated in specified format
8. **Aggregation**: Multiple sections aggregated based on output mode
9. **File Writing**: Final content written to specified outputs

### Template Context Resolution:
Templates receive context including:
- Token information (user ID, etc.)
- Environment variables
- Job/section metadata

## Error Handling Strategy

### 1. **Graceful Degradation**
- KV v2 fallback to v1 on failure
- Continue-on-error support in batch processing

### 2. **Contextual Errors**
- Detailed error messages with path and operation context
- Wrapped errors maintain error chains

### 3. **Validation**
- Early validation of configurations
- Connection testing before processing

## Performance Considerations

### 1. **Connection Reuse**
- Single Vault client instance per command execution
- Connection health checking to avoid unnecessary retries

### 2. **Batching**
- Multiple sections processed in single command execution
- Efficient secret retrieval with minimal API calls

### 3. **Memory Management**
- Streaming output for large configurations
- Minimal in-memory buffering

## Security Features

### 1. **Token Security**
- Multiple token resolution methods
- No token logging or exposure in debug output
- Secure token file handling

### 2. **Value Protection**
- Proper shell escaping prevents injection
- Censoring support for sensitive value display

### 3. **Path Validation**
- Template validation prevents path injection
- Secure path construction and validation

This architecture provides a robust, extensible foundation for Vault secret management with excellent separation of concerns and clear component boundaries.
