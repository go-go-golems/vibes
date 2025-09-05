---
Title: Vault Envrc Generator — Concepts & System
Slug: vault-envrc-topics
Short: Overview of commands, layers, output, and behaviors
Topics:
- overview
- vault
- layers
- output
- batch
- seed
- envrc
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
SectionType: Tutorial
---

# Vault Envrc Generator — Concepts & System

This CLI generates `.envrc`, JSON, and YAML files from HashiCorp Vault and can also seed Vault from local environment and files. It is built with the **Glazed** CLI framework to provide consistent parameters, structured outputs, and a rich help system with automatic KV version detection and sophisticated token resolution.

## System Overview

The Vault Envrc Generator is designed around the principle of **configuration as code** for environment management. It bridges the gap between HashiCorp Vault's secure secret storage and the practical needs of development and deployment environments.

### Core Philosophy
- **Declarative Configuration**: Define what you want, not how to get it
- **Environment Parity**: Same tooling for dev, staging, and production
- **Secret Normalization**: Consistent format regardless of source structure
- **Audit Trail**: All operations are logged and trackable

## Commands Deep Dive

### **batch** - Multi-Path Processing Engine
The most powerful command for complex environment generation scenarios.

**Core Capabilities:**
- **Multi-job Processing**: Execute multiple related tasks in sequence
- **Section-based Organization**: Each job can have multiple logical sections
- **Merge-only Output**: Envrc appends with headers; JSON/YAML shallow-merge keys
- **Format Flexibility**: Generate envrc, JSON, or YAML from the same configuration
- **Error Resilience**: Continue processing even if individual sections fail
- **Template Support**: Dynamic path construction with Go templates

**Use Cases:**
- Generating complete application environment files
- Creating deployment-specific configurations
- Consolidating secrets from multiple Vault paths
- Building CI/CD environment matrices

### **generate** - Single Path Generator
Focused command for simple, single-path secret extraction.

**Core Capabilities:**
- **Single Path Focus**: Extract secrets from one Vault path
- **Format Transformation**: Convert to envrc, JSON, or YAML
- **Key Processing**: Filter, transform, and prefix keys
- **Template Support**: Custom output templates
- **Dry-run Mode**: Preview without file creation

**Use Cases:**
- Quick secret extraction for debugging
- Single-service environment generation
- Testing Vault connectivity and permissions

### **list** - Vault Explorer & Auditing Tool
Comprehensive tool for exploring and documenting Vault contents.

**Core Capabilities:**
- **Recursive Traversal**: Explore directory structures with depth control
- **Structured Output**: Glazed-powered formatting (table, JSON, YAML, CSV)
- **Value Inspection**: Optional value display with censoring
- **Path Documentation**: Understand Vault organization

**Use Cases:**
- Vault content auditing and documentation
- Understanding secret organization
- Debugging access permissions

### **seed** - Vault Population Engine
Reverse operation that populates Vault from local sources.

**Core Capabilities:**
- **YAML-driven Configuration**: Declarative seed specifications
- **Multiple Sources**: Environment variables, files, and static data
- **Template Paths**: Dynamic path construction
- **Dry-run Support**: Preview changes before execution

**Use Cases:**
- Initial Vault setup and population
- Migrating secrets from other systems
- Development environment bootstrapping

### **interactive** - Quick Exploration Tool
Lightweight command-line interface for rapid exploration and testing.

**Use Cases:**
- Learning the tool's capabilities
- Quick one-off secret extraction
- Testing Vault connectivity

## Vault Layer (Parameters)

This app defines a reusable Glazed layer `vault`:
- vault-addr: Vault address (default: http://127.0.0.1:8200)
- vault-token: Optional token (empty by default)
- vault-token-source: auto|env|file|lookup (default: auto)
- vault-token-file: Path to token file (default: empty; lookup can fallback to ~/.vault-token)

Commands use InitializeStruct with the `vault` layer to unify configuration.

## Output Semantics

- envrc: Emits export statements. In batch, sections are appended with per-section headers. When writing to an existing file, the global header is suppressed.
- json/yaml: Shallow merge of top-level keys. Use `--sort-keys` for deterministic key ordering.

## Aggregation Rules (batch)

- Stdout: Aggregates per job and prints once.
  - JSON/YAML: merged object
  - envrc: concatenated sections with headers
- Files: Each section writes directly to its target. For JSON/YAML, writes merge into existing content; for envrc, content is appended.

## Key Transform & Prefix

- transform-keys: Uppercases keys and converts `-` to `_` consistently across formats.
- prefix: Adds a prefix to emitted keys (applied before transform).
- env_map (batch): Explicit mapping `ENV_VAR -> source key` disables transform/prefix/include/exclude for that section.

## Zerolog Logging

Logging is configured via Glazed’s logging layer on the root command (stderr output). Use `--log-level debug` for detailed diagnostics. The application logs internal steps with structured debug logs and prints high-level progress lines to stdout where appropriate.

## Typical Workflows

- Preview envrc for a path: `generate --path ... --format envrc --dry-run`
- Produce merged JSON/YAML for multiple sections: `batch --format json|yaml --output -`
- Merged JSON/YAML to stdout: `batch --format json|yaml --output -`
- Seed Vault from env/files: `seed --config seed.yaml --dry-run`
- Structured listing: `list --path ... --output json|yaml|csv|table`
