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

This guide walks through building the CLI, configuring Vault settings and logging, and running common workflows using Glazed layers and structured outputs.

## 1) Build the application

```bash
cd vibes/2025-09-04/vault-envrc-generator/go-utility
GOCACHE=$(pwd)/.gocache go build -o vault-envrc-generator .
./vault-envrc-generator --help
```

## 2) Configure logging and Vault settings

Logging flags (on root):
- `--log-level`: trace|debug|info|warn|error|fatal (default: info)
- `--log-format`: text|json (default: text)
- `--log-file`: optional log file

Vault layer flags (available on commands):
- `--vault-addr`: Vault address (default http://127.0.0.1:8200)
- `--vault-token`, `--vault-token-source`, `--vault-token-file`

Example: enable debug logging and point at Vault:
```bash
./vault-envrc-generator --log-level debug \
  list --path secrets/ --depth 1 --vault-addr https://vault.example.com/
```

## 3) Preview generation for a single path

Generate `.envrc` without writing a file:
```bash
./vault-envrc-generator generate \
  --path secrets/environments/development/shared/database \
  --format envrc --dry-run \
  --vault-addr https://vault.example.com/
```

Generate JSON or YAML with sorted keys, write to file:
```bash
./vault-envrc-generator generate \
  --path secrets/.../providers/openai --format json --sort-keys \
  --output out/openai.json --vault-addr https://vault.example.com/
```

## 4) Batch workflows from YAML

Preview to stdout as JSON (merged):
```bash
./vault-envrc-generator batch -c batch.yaml \
  --format json --output - --dry-run --vault-addr https://vault.example.com/
```

Append YAML as multiple documents to a file:
```bash
./vault-envrc-generator batch -c batch.yaml \
  --format yaml --output out/bundle.yaml --output-mode append \
  --vault-addr https://vault.example.com/
```

Shallow-merge YAML keys to a single document:
```bash
./vault-envrc-generator batch -c batch.yaml \
  --format yaml --output out/merged.yaml --output-mode merge \
  --vault-addr https://vault.example.com/
```

## 5) Structured listing

List directories and secrets with structured rows, then choose an output format with Glazed:
```bash
# As JSON
./vault-envrc-generator list --path secrets/environments/development/ --depth 2 \
  --output json --vault-addr https://vault.example.com/

# As table
./vault-envrc-generator list --path secrets/... --depth 1 --output table \
  --vault-addr https://vault.example.com/
```

Use `--include-values` and `--censor` to show censored values instead of keys:
```bash
./vault-envrc-generator list --path secrets/... --include-values --censor XXXXX \
  --output yaml --vault-addr https://vault.example.com/
```

## 6) Seed Vault from local env and files

Dry-run to see planned writes:
```bash
./vault-envrc-generator seed -c seed.yaml --dry-run \
  --vault-addr https://vault.example.com/
```

Write for real (remove `--dry-run`):
```bash
./vault-envrc-generator seed -c seed.yaml --vault-addr https://vault.example.com/
```

## 7) Interactive preview

Quickly explore a path and write output:
```bash
./vault-envrc-generator interactive --vault-addr https://vault.example.com/
```
Follow the prompts to choose include/exclude, prefix, transform, and format.

## Notes

- Sorting applies to top-level keys for JSON/YAML. Nested maps preserve their natural order.
- For batch YAML append, multi-doc outputs use `---` between documents.
- Logging uses zerolog (stderr). Use `--log-level debug` for detailed internals.

