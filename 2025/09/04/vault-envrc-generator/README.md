# Vault .envrc Generator

Generate developer-friendly environment files (.envrc, JSON, YAML) from HashiCorp Vault secrets. Supports browsing, batch composition, YAML-driven seeding, and safe token resolution.

## Highlights

- 🔐 KV v1/v2 aware: KV v2-first fallback (no mount-list permission required)
- 🧭 Token sources: env, file (~/.vault-token), or lookup via `vault token lookup`
- 🧰 Interfaces: `generate`, `interactive`, `batch`, `list`, `seed`, `test`
- 🧩 Output: `.envrc`, `json`, `yaml`; envrc appends; JSON/YAML merge across jobs
- 🧱 Sections-based batch: job-level defaults + per-section overrides, with headers in `.envrc`
- ✍️ YAML seeding: write secrets into Vault from env, literals, files
- 🔎 KV v2-aware listing: YAML/text, optional censored values for leaf keys
- 🧩 Token templating for paths/outputs across batch and seed (via token context)
- 🧷 Fixed values & per-job base_path in batch: inject templated constants and job-local bases

## Quick Start

### Prerequisites

- HashiCorp Vault reachable and you have a token (dev policy at minimum)

### Install

```bash
go build -o vault-envrc-generator .
```

### Configure Vault address and token

```bash
export VAULT_ADDR="https://vault.example.com:8200"
export VAULT_TOKEN="$(cat ~/.vault-token)"   # or use env/file/lookup at runtime
```

### Common commands

```bash
# Connectivity check
./vault-envrc-generator test -v

# Browse and generate interactively
./vault-envrc-generator interactive

# Generate one path
./vault-envrc-generator generate --path secrets/environments/development/shared/database \
  --output out/.envrc --prefix DB_ --transform-keys

# Batch (sections schema)
./vault-envrc-generator batch --config go-utility/batch-load-dev.yaml --continue-on-error

# List what you can see (YAML with censored values)
./vault-envrc-generator list --path secrets/environments/development/shared/ \
  --format yaml --include-values --censor "***"

# Seed secrets from YAML
./vault-envrc-generator seed --config go-utility/seed-personal.yaml --dry-run
```

## Concepts

A few core concepts help understand how this tool behaves and why:

- KV Engines: Vault stores secrets under mounts. KV v2 wraps real data under `data/` for reads and `metadata/` for listings. This tool automatically attempts v2 and falls back to v1 to minimize required privileges.
- Token Resolution: Tokens can be taken from `VAULT_TOKEN`, read from `~/.vault-token`, or resolved via `vault token lookup` (current session). The default `auto` mode tries all three in a safe order.
- Key Transformation and Prefixing: Many developers prefer shell-safe env names. `transform_keys` converts keys to uppercase and replaces `-` with `_`, then `prefix` is prepended (e.g., `DB_`).
- Output Semantics: In batch, `.envrc` appends section content with headers; `json`/`yaml` shallow-merge top-level keys. Use `--sort-keys` for deterministic output.
- Sections-Based Batch: Define job-level defaults (output, format, etc.) and a list of sections each pointing at a Vault path. Each section can override defaults for precise control. The `.envrc` formatter emits friendly headers per section including the source path and description.

## Token Resolution

Global flags/ENV control how the token is found:

- `--vault-token-source`: `auto` (default), `env`, `file`, `lookup`
- `--vault-token-file`: path to token file (default: `~/.vault-token`)

Auto order: explicit flag/env → file → `vault token lookup`.

Notes:
- For `lookup`, this tool shells out to `vault token lookup -format=json` and extracts `.data.id`. Ensure the Vault CLI is installed and authenticated.
- For `file`, the default path is `~/.vault-token`. You can override with `--vault-token-file`.

## Commands

### generate
Reads a single path and writes formatted output.

```bash
vault-envrc-generator generate [flags]

Flags:
  -p, --path string          Vault secret path (required)
  -o, --output string        Output file path (default ".envrc")
      --prefix string        Prefix for names
      --exclude strings      Keys to exclude
      --include strings      Keys to include
      --transform-keys       Uppercase + replace '-'→'_' (see Transform Keys)
      --template string      Custom Go template (envrc)
  -f, --format string        envrc | json | yaml (default "envrc")
      --dry-run              Show output only
```

How it works:
- Reads the secret from Vault at `--path` (tries KV v2 then v1)
- Filters keys per `--include/--exclude`
- Applies `--transform-keys` and `--prefix`
- Formats into `envrc|json|yaml` (or a custom template)

Common examples:
```bash
# Only include username/password and add prefix
./vault-envrc-generator generate -p secrets/.../database --include username,password \
  --prefix DB_ --transform-keys --output out/.envrc

# Emit JSON for programmatic consumption
./vault-envrc-generator generate -p secrets/.../config --format json > config.json
```

### interactive
Guided mode to select a path, preview, and write.

### batch
Compose multiple outputs from multiple paths. Two schemas are supported.

#### Sections schema (recommended)

```yaml
jobs:
  - base_path: secrets/environments/development     # optional: prepended to relative section paths
  - name: "Dev envrc"
    description: "Aggregated development environment variables"
    output: "out/dev/.envrc"
    # merge-only semantics (envrc appends, json/yaml merge)
    format: envrc              # envrc | json | yaml
    transform_keys: true       # job-level default; sections can override
    sections:
      - name: db
        description: "Shared DB user/password"
        path: shared/database                       # becomes secrets/environments/development/shared/database
        include_keys: [username, password]
        prefix: DATABASE_
        # transform_keys: false  # optional override
      - name: google-oauth
        path: external-apis/development/google-oauth
        # Option A: include_keys (uses prefix/transform rules)
        # include_keys: [client_id, client_secret]
        # prefix: GOOGLE_
        # Option B: env_map (explicit mapping to env var names; no transform/prefix)
        env_map:
          GOOGLE_CLIENT_ID: client_id
          GOOGLE_CLIENT_SECRET: client_secret

      # Example of templated personal path using OIDC user ID
      - name: personal-core
        description: "Personal core config for current OIDC user"
        path: "secrets/environments/development/personal/{{ .Token.OIDCUserID }}/local/core"
        include_keys: [VAULT_ADDR]
```

Behavior:
- `.envrc` sections include headers with job/section name, source path, description, and a trailing blank line.
- Merge-only: `.envrc` appends; `json`/`yaml` merge keys (last write wins).

Tri-state `transform_keys` precedence:
- Section-level `transform_keys: true|false` overrides job.
- Job-level `transform_keys: true|false` is the default for all sections.
- If omitted at both levels, the default is `false`.

#### Legacy job schema

```yaml
jobs:
  - name: frontend
    path: secrets/app/frontend
    output: out/frontend.envrc
    prefix: FRONTEND_
    transform_keys: true
```

### list
KV v2-aware listing.

```bash
vault-envrc-generator list --path <prefix> [--depth N] [--prefix STR] \
  [--format yaml|text] [--include-values] [--censor "***"]
```

Examples:

```bash
# YAML with censored leaf keys
./vault-envrc-generator list --path secrets/environments/development/shared/ \
  --depth 1 --format yaml --include-values --censor "***"

# Text view
./vault-envrc-generator list --path secrets/environments/development/ --depth 2 --format text
```

### seed
Write secrets to Vault from a YAML spec (env/literal/file sources). KV v2-first, v1 fallback.

```yaml
base_path: secrets/environments/development/personal/{{ .Token.OIDCUserID }}/local
sets:
  - path: core
    data:
      VAULT_ADDR: https://vault.mento.co/
    env:
      OP_ACCOUNT: OP_ACCOUNT
      OP_VAULT: OP_VAULT
  - path: google
    env:
      client_email: GOOGLE_EMAIL
    files:
      private_key: ~/.keys/google-sa.pem
```

Run:

```bash
./vault-envrc-generator seed --config go-utility/seed-personal.yaml --dry-run
./vault-envrc-generator seed --config go-utility/seed-personal.yaml
```

### test
Connectivity, health, token introspection, and a simple read.

## Transform Keys

- Uppercases keys and replaces hyphens with underscores, prior to prefixing.
- Applies to all formats because transformation happens before formatting.
- Example: `prefix=MYAPP_`, `transform_keys=true`, key `client-id` → `MYAPP_CLIENT_ID`.

## Batch Format Details

### Job-level fields

- `name`: Human-friendly identifier for logs and headers.
- `description` (optional): Included in `.envrc` headers.
- `output`: File to write.
  (merge-only: `.envrc` appends; `json`/`yaml` merge)
- `format`: `envrc` | `json` | `yaml`.
- `transform_keys` (optional): Default for all sections (overridden by section-level value).
- `prefix`, `include_keys`, `exclude_keys`, `template`, `variables`: Defaults for sections.
- Templating: `base_path`, `output` (and legacy `path`) accept Go templates with token context.
  - Context fields under `.Token`: `Accessor`, `CreationTTL`, `DisplayName`, `EntityID`, `ExpireTime`, `ID`, `IssueTime`, `Meta[role]`, `Policies`, `Path`, `TTL`, `Type`, and `OIDCUserID` (extracted from `display_name` like `oidc-<id>`).
- `base_path`: (YAML top-level) If set, any section `path` that is not an absolute Vault path will be joined as `base_path/<section.path>`. Override via CLI: `--base-path`.
  - Per-job `base_path`: each job may also specify `base_path`; when present, it overrides the YAML top-level/CLI base for that job.
  - Absolute escaping: if a section `path` (or job `path`) is an absolute Vault path (prefix `secrets/`, `secret/`, etc.), it is used as-is and not joined with any base path.

### Sections

Each `section` accepts the same keys as a legacy job, plus `name` and `description`. Section-level values override job-level defaults. For `.envrc`, the emitted header shows `job: section`, the Vault `path`, and the optional `description`.

Key selection options per section:
- `include_keys`: choose keys and then apply `transform_keys`/`prefix`.
- `env_map`: explicit mapping of `ENV_VAR` → `source_key`. When `env_map` is used:
  - `transform_keys` and `prefix` are ignored (your env var names are used as-is)
  - `include_keys`/`exclude_keys` are ignored for that section
- Templating: `path` and `output` accept token-context templates, e.g., `secrets/.../{{ .Token.OIDCUserID }}/...`.
- `fixed`: map of `key: templated_string` injected into the section before selection/formatting. Useful for constants, computed values, or escaping Vault reads entirely (leave `path` empty to emit only fixed values).

### Token-based templating

The batch and seed commands support rendering Go templates using values derived from your current Vault token (`vault token lookup`). This lets you personalize paths without hardcoding identifiers.

- Requirements: the Vault CLI must be installed and authenticated (so `vault token lookup` works).
- Context available under `.Token`:
  - Identification: `DisplayName`, `EntityID`, `Type`, `ID` (token id)
  - Timing: `IssueTime`, `ExpireTime`, `CreationTTL`, `TTL`
  - Policy & metadata: `Policies` (array), `Meta` (map, e.g., `.Token.Meta.role`)
  - Path: `Path` (auth mount path that issued the token)
  - Extra convenience: `OIDCUserID` parsed from `DisplayName` when it looks like `oidc-<id>`

Examples:

```yaml
jobs:
  - base_path: secrets/environments/development/personal/{{ .Token.OIDCUserID }}/local
  - name: personal-envrc
    output: out/personal-{{ .Token.OIDCUserID }}.envrc
    # merge-only semantics (envrc appends; json/yaml merge)
    format: envrc
    sections:
      - name: core
        path: core
        include_keys: [VAULT_ADDR]
      - name: constants
        path: ""    # no Vault read; use fixed values only
        fixed:
          NOW_RFC3339: "{{ .Token.IssueTime }}"
          ENV_NAME: "development"
```

Notes:
- Treat token-derived values as sensitive. Avoid writing raw token IDs to filenames or logs.
- If a template references a missing field, rendering fails with a clear error.

### Concurrency & Safety

- When running with `--parallel`, each output file is protected by an in-process mutex to prevent races.
- `--continue-on-error` lets other sections/jobs continue if one fails (e.g., lacking permission on a path).

## Output Formats

### envrc

Emits `export KEY=VALUE` lines with a generated header per section in batch mode.

```bash
# === Dev envrc: db ===
# Source path: secrets/environments/development/shared/database
# Section: Shared DB user/password

export DATABASE_USERNAME=postgres
export DATABASE_PASSWORD=your_password_here

# === Dev envrc: google-oauth ===
# Source path: secrets/external-apis/development/google-oauth

export GOOGLE_CLIENT_ID=...
export GOOGLE_CLIENT_SECRET=...
```

### json / yaml

Maps of key→value. Merge-only semantics: later sections/jobs override keys.

Example merged JSON:
```json
{
  "DATABASE_USERNAME": "postgres",
  "DATABASE_PASSWORD": "...",
  "API_KEY": "..."
}
```

## Configuration

### ENV and config file

- `VAULT_ADDR`, `VAULT_TOKEN`
- Or a config file `~/.vault-envrc-generator.yaml`:

```yaml
vault:
  addr: https://vault.example.com:8200
  token: your-token
output: .envrc
verbose: false
```

## Security & Troubleshooting

- 403 on listing mounts is expected for non-admin tokens. The tool avoids requiring `sys/mounts`.
- 403 on secret paths means your token lacks `list`/`read` there.
- Use `test -v` and `list` to probe access without reading values.
- Ensure `VAULT_ADDR` is set; if using `lookup` token source, you must be logged in with the Vault CLI.
- If a `.envrc` file has duplicate exports, later sections will appear later in the file; standard shell sourcing semantics apply (last one wins).

## Development

```bash
go build -o vault-envrc-generator .
go test ./...
```

Project layout:

```
vibes/.../go-utility/
├── cmd/            # CLI commands (generate, batch, list, seed, test, interactive)
├── pkg/
│   ├── vault/      # KV v2-first client (Get/Put, List metadata)
│   └── envrc/      # Formatting and key transforms
└── go.mod
```

## Appendix: Example batch (sections)

```yaml
jobs:
  - name: dev-envrc
    description: "Aggregated development environment variables for .envrc"
    output: out/dev/.envrc
    # merge-only semantics (envrc appends; json/yaml merge)
    format: envrc
    transform_keys: true
    sections:
      - name: db
        path: secrets/environments/development/shared/database
        include_keys: [username, password]
        prefix: DATABASE_
      - name: google-oauth
        path: secrets/external-apis/development/google-oauth
        include_keys: [client_id, client_secret]
        prefix: GOOGLE_
```

## End-to-End Example

This example demonstrates seeding a personal namespace, exploring, and generating `.envrc`.

```bash
# 1) Seed personal namespace (dry-run first)
./vault-envrc-generator seed --config go-utility/seed-personal.yaml --dry-run
./vault-envrc-generator seed --config go-utility/seed-personal.yaml

# 2) Explore accessible development paths (YAML + censored values)
./vault-envrc-generator list --path secrets/environments/development/ --depth 2 \
  --format yaml --include-values --censor "***"

# 3) Batch-generate an envrc composed from several sections
./vault-envrc-generator batch --config go-utility/batch-load-dev.yaml --continue-on-error

# 4) Source it in your shell
source out/dev/.envrc
```
