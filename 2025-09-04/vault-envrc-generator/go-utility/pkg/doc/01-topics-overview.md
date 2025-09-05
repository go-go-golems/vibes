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

This CLI generates `.envrc`, JSON, and YAML files from HashiCorp Vault and can also seed Vault from local environment and files. It is built with Glazed to provide consistent parameters, structured outputs, and a rich help system.

## Commands

- batch: Run jobs defined in a YAML file to fetch and emit env data. Supports envrc/json/yaml formats, append/merge/overwrite modes, per-section headers (envrc), and aggregated JSON/YAML for stdout and files.
- generate: Fetch a single Vault path and emit envrc/json/yaml with key transforms, prefixes, templates, and sorted keys.
- list: Structured listing of directories and secrets, with optional censored values. Emits rows for directories (children) and secrets (keys or data) and supports Glazed outputs (JSON/YAML/CSV/table).
- seed: Seed KV data into Vault from a YAML spec, with data sourced from env and local files. Supports dry-run.
- interactive: Lightweight prompt to preview and write envrc/json/yaml for a path.

## Vault Layer (Parameters)

This app defines a reusable Glazed layer `vault`:
- vault-addr: Vault address (default: http://127.0.0.1:8200)
- vault-token: Optional token (empty by default)
- vault-token-source: auto|env|file|lookup (default: auto)
- vault-token-file: Path to token file (default: empty; lookup can fallback to ~/.vault-token)

Commands use InitializeStruct with the `vault` layer to unify configuration.

## Output Formats & Modes

- envrc: Emits export statements; by default, keys are sorted; per-section headers are added in batch mode.
- json/yaml: Keys can be sorted with `--sort-keys` for deterministic output.
- Modes (batch):
  - overwrite: Replace target file.
  - append: For YAML, appends as multi-doc (`---`); for other formats, raw append.
  - merge: Shallow merge of top-level keys for JSON/YAML.

## Aggregation Rules (batch)

- Stdout: Aggregates per job, then prints once:
  - JSON: merged object
  - YAML: merged object for `merge` or multi-doc for `append`
- Files: When a single output target is used, aggregated JSON/YAML is written once at the end under the chosen mode.

## Key Transform & Prefix

- transform-keys: Uppercases keys and converts `-` to `_` consistently across formats.
- prefix: Adds a prefix to emitted keys (applied before transform).
- env_map (batch): Explicit mapping `ENV_VAR -> source key` disables transform/prefix/include/exclude for that section.

## Zerolog Logging

Logging is configured via Glazed’s logging layer on the root command (stderr output). Use `--log-level debug` for detailed diagnostics. The application logs internal steps with structured debug logs and prints high-level progress lines to stdout where appropriate.

## Typical Workflows

- Preview envrc for a path: `generate --path ... --format envrc --dry-run`
- Produce merged JSON/YAML for multiple sections: `batch --format json|yaml --output -`
- Append multiple YAML documents: `batch --format yaml --output out.yaml --output-mode append`
- Seed Vault from env/files: `seed --config seed.yaml --dry-run`
- Structured listing: `list --path ... --output json|yaml|csv|table`
