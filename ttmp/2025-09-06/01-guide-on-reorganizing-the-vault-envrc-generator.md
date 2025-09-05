## Guide: Reorganizing the vault-envrc-generator (Go utility)

### Purpose and scope
This document proposes a modular reorganization of the current Go CLI under `vibes/2025-09-04/vault-envrc-generator/go-utility/` to maximize code reuse, testability, and clarity by extracting shared logic into `pkg/` packages (in the style of `pkg/envrc/generator.go`). It maps current responsibilities, highlights duplication, and outlines new packages, public APIs, and migration steps.

### Current layout and responsibilities

- `cmd/root.go`
  - Flags and Viper binding (`initConfig`), `Execute()`; global defaults.
- `cmd/generate.go`
  - `runGenerate`: resolve token, init Vault client, `GetSecrets`, configure `envrc.Generator`, generate content, handle dry-run, write output.
  - `countEnvVars(content string) int`.
- `cmd/interactive.go`
  - `runInteractive`: resolve token, init Vault client, `TestConnection`, prompt for path/options, generate preview using `envrc.Generator`, confirm, write file.
  - `promptForPath`, `promptForOptions`, `promptForKeys`, `promptConfirmation`.
- `cmd/list.go`
  - `runList`: resolve token, init Vault client, normalize path, recursively list entries (`walkVault`), optional censored values, output in YAML or text.
  - `normalizePath`, `walkVault`.
- `cmd/list_types.go`
  - `type ListEntry` model for `list` output.
- `cmd/batch.go` (large)
  - Types: `BatchConfig`, `TemplateContext`, `TokenContext`, `BatchSection`, `BatchJob`.
  - Globals: flags and overrides, `outputLocks` and `lockForPath`.
  - `runBatch`: load config (YAML), resolve token, build `vault.Client`, build template context from token, compute base path and overrides, sequential/parallel job processing.
  - `loadBatchConfig`, `processBatchSequential`, `processBatchParallel`.
  - `processJob`: fetch secrets per section, apply `fixed` and `variables`, handle `env_map`, build `envrc.Options`, generate content, add headers, write with `overwrite|append|merge` for `envrc|json|yaml`, per-path locking.
  - Helpers: `buildTemplateContext`, `renderTemplateString`, `isVaultAbsolute`, `combineBaseAndPath`.
- `cmd/seed.go`
  - Types: `SeedSpec`, `SeedSet`.
  - `runSeed`: parse spec, resolve token, init Vault client, build template context (calls `buildTemplateContext` from `batch.go`), resolve base path and templated set paths, collect data from literals/env/files, dry-run or `PutSecrets`.
  - `keysOf`.

- `pkg/envrc/generator.go`
  - `type Options`, `type Generator` and `NewGenerator`.
  - `(*Generator).Generate` routes to `generateEnvrc|generateJSON|generateYAML` after `filterSecrets`, `transformKeys`, `addPrefix`.
  - Helpers: `generateFromTemplate`, `formatValue`, `escapeValue`.

- `pkg/vault/client.go`
  - `type Client` wrapper for `api.Client`.
  - `NewClient`, `GetSecrets` (KV v2 first, fallback v1), `PutSecrets` (v2 then v1), `ListSecrets`, `GetClient`, `TestConnection`.
  - Internals: `getKVv1Secrets`, `getKVv2Secrets`, `putKVv1Secrets`, `putKVv2Secrets`, `parsePath`.

- `pkg/vault/token_loader.go`
  - `type TokenSource`, `const` variants, `ResolveToken(ctx, explicit, source, tokenFile, verbose)`, `lookupTokenViaCLI`.

### Pain points and duplication
- Repeated Vault connect flow (resolve token + new client) across `generate`, `interactive`, `list`, `batch`, `seed`.
- Template token introspection and rendering live in `cmd/batch.go` but are reused by `seed.go` (cross-file coupling within `cmd`).
- Vault path helpers (`isVaultAbsolute`, `combineBaseAndPath`, `normalizePath`) live in `cmd/*`.
- Output file writing (overwrite/append/merge) and JSON/YAML merge logic duplicated in multiple places within `cmd/batch.go`.
- Concurrency path locks (`outputLocks`) embedded in `cmd/batch.go`.
- Batch/Seed YAML schemas defined under `cmd/`, limiting reuse/testability.

### Proposed package structure
Keep `cmd/` thin. Introduce cohesive packages under `pkg/` with clear, testable APIs.

- `pkg/vault/`
  - keep: `client.go`, `token_loader.go`.
  - add: `context.go`
    - `type TemplateContext`, `type TokenContext` (moved from `cmd/batch.go`).
    - `func BuildTemplateContext(client *Client) (TemplateContext, error)`.
  - add: `path.go`
    - `func IsAbsoluteVaultPath(p string) bool` (from `isVaultAbsolute`).
    - `func JoinBaseAndPath(base, p string) string` (from `combineBaseAndPath`).
    - `func NormalizeListPath(p string) string` (from `normalizePath`).
  - add: `templates.go`
    - `func RenderTemplateString(s string, ctx TemplateContext) (string, error)` (from `renderTemplateString`).
  - optional: `connect.go`
    - `type ConnectConfig { Addr, Token, TokenFile string; Source TokenSource; Verbose bool }`.
    - `func Connect(ctx context.Context, cfg ConnectConfig) (*Client, string, error)` to DRY repeated setup.

- `pkg/output/`
  - `writer.go`
    - `type OutputMode string` with `const (
      OutputModeOverwrite OutputMode = "overwrite"
      OutputModeAppend    OutputMode = "append"
      OutputModeMerge     OutputMode = "merge"
      )`.
    - `type WriteOptions struct { Mode OutputMode; Format string }`.
    - `func Write(path string, content []byte, opts WriteOptions) error`.
    - Internals for per-path locking (moved `outputLocks` + `lockForPath`) and JSON/YAML merge helpers.

- `pkg/batch/`
  - `types.go`
    - `type Config`, `type Job`, `type Section` (moved + renamed from `BatchConfig|BatchJob|BatchSection`).
  - `processor.go`
    - `type Processor struct { Client *vault.Client; Verbose bool }`.
    - `type ProcessorOptions struct { BasePath, OutputOverride, OutputModeOverride, FormatOverride string; Parallel, ContinueOnError bool }`.
    - `func (p *Processor) Process(ctx context.Context, cfg *Config, opts ProcessorOptions) error` (internals contain the current `processBatchSequential|Parallel` and `processJob` logic, use `envrc.Generator` and `pkg/output`).
    - Keep header suppression logic local here; rely on `pkg/output` for actual writes.

- `pkg/seed/`
  - `spec.go`
    - `type Spec` (from `SeedSpec`), `type Set` (from `SeedSet`).
  - `runner.go`
    - `type Options struct { DryRun bool }`.
    - `func Run(ctx context.Context, client *vault.Client, spec *Spec, opts Options, verbose bool) error`.
    - Uses `vault.BuildTemplateContext`, `vault.RenderTemplateString`, `vault.JoinBaseAndPath`, environment and file reads, then `client.PutSecrets`.

- `pkg/listing/`
  - `types.go`: `type Entry` (from `ListEntry`).
  - `walker.go`: `func Walk(client *vault.Client, path string, depth int) ([]string, []error)` and re-exports `NormalizeListPath` via `vault.NormalizeListPath`.

- `pkg/envrc/`
  - keep `generator.go` as-is; optionally add typed format constants:
    - `const (
      FormatEnvrc = "envrc"
      FormatJSON  = "json"
      FormatYAML  = "yaml"
      )`.

### Public API sketches

```go
// pkg/vault/context.go
package vault

type TemplateContext struct { Token TokenContext }

type TokenContext struct {
    Accessor, CreationTTL, DisplayName, EntityID, ExpireTime string
    ID, IssueTime, Path, TTL, Type, OIDCUserID               string
    Policies []string
    Meta     map[string]string
}

func BuildTemplateContext(c *Client) (TemplateContext, error)
```

```go
// pkg/vault/templates.go
package vault

func RenderTemplateString(s string, tctx TemplateContext) (string, error)
```

```go
// pkg/vault/path.go
package vault

func IsAbsoluteVaultPath(p string) bool
func JoinBaseAndPath(base, p string) string
func NormalizeListPath(p string) string
```

```go
// pkg/output/writer.go
package output

type OutputMode string
const (
    OutputModeOverwrite OutputMode = "overwrite"
    OutputModeAppend    OutputMode = "append"
    OutputModeMerge     OutputMode = "merge"
)

type WriteOptions struct {
    Mode   OutputMode
    Format string // envrc|json|yaml
}

func Write(path string, content []byte, opts WriteOptions) error
```

```go
// pkg/batch/processor.go
package batch

type Config struct { BasePath string; Jobs []Job }

type Job struct {
    Name, Description, Path, Output, OutputMode, Prefix, Format, Template string
    ExcludeKeys, IncludeKeys []string
    Transform *bool
    Variables map[string]string
    Sections  []Section
    BasePath  string
    Fixed     map[string]string
}

type Section struct {
    Name, Description, Path, Prefix, Template, Format, Output string
    ExcludeKeys, IncludeKeys []string
    Transform *bool
    Variables map[string]string
    EnvMap    map[string]string
    Fixed     map[string]string
}

type Processor struct { Client *vault.Client; Verbose bool }

type ProcessorOptions struct {
    BasePath, OutputOverride, OutputModeOverride, FormatOverride string
    Parallel, ContinueOnError bool
}

func (p *Processor) Process(ctx context.Context, cfg *Config, opts ProcessorOptions) error
```

```go
// pkg/seed/runner.go
package seed

type Spec struct { BasePath string; Sets []Set }

type Set struct {
    Path  string
    Data  map[string]string
    Env   map[string]string
    Files map[string]string
}

type Options struct { DryRun bool }

func Run(ctx context.Context, client *vault.Client, spec *Spec, opts Options, verbose bool) error
```

```go
// pkg/listing/walker.go
package listing

type Entry struct {
    Path     string            `yaml:"path"`
    Type     string            `yaml:"type"` // directory|secret
    Children []string          `yaml:"children,omitempty"`
    Keys     []string          `yaml:"keys,omitempty"`
    Data     map[string]string `yaml:"data,omitempty"`
}

func Walk(client *vault.Client, path string, depth int) ([]string, []error)
```

### cmd/ after refactor (thin wrappers)

- `cmd/batch.go`: parse flags, load YAML into `batch.Config`, build `vault.Client`, call `batch.Processor.Process(ctx, cfg, opts)`.
- `cmd/seed.go`: parse flags, load YAML into `seed.Spec`, call `seed.Run(ctx, client, &spec, opts, verbose)`.
- `cmd/generate.go`: connect, `client.GetSecrets(path)`, build `envrc.Generator`, write via `pkg/output.Write`.
- `cmd/list.go`: connect, `listing.Walk`, format output; `Entry` type from `pkg/listing`.
- `cmd/interactive.go`: connect, keep prompts; reuse `envrc.Generator` and `pkg/output.Write`.

Minimal example of the new `cmd/batch.go` core:

```go
client, _ := vault.NewClient(addr, token)
proc := batch.Processor{Client: client, Verbose: viper.GetBool("verbose")}
opts := batch.ProcessorOptions{
  BasePath: viper.GetString("batch.base_path"),
  OutputOverride: viper.GetString("batch.output_override"),
  OutputModeOverride: viper.GetString("batch.output_mode_override"),
  FormatOverride: viper.GetString("batch.format_override"),
  Parallel: parallel, ContinueOnError: continueOnError,
}
if err := proc.Process(cmd.Context(), cfg, opts); err != nil { return err }
```

### Behavior parity notes
- Header suppression for `envrc` retains current semantics: suppress when appending to existing non-empty file and across multiple sections targeting the same path.
- `env_map` continues to disable transform, prefix, include/exclude for explicit mapping.
- JSON/YAML merge behavior matches current logic (shallow merge/override on key conflicts).
- Base path resolution and string templating preserve current behavior using token-derived `TemplateContext`.
- YAML schemas for batch and seed remain unchanged (only moved under `pkg/`).

### Benefits
- Clear separation of concerns; `cmd/` becomes orchestration and flag parsing only.
- Shared logic consolidated in `pkg/` for reuse across commands and future tools.
- Easier unit testing of `batch`, `seed`, `output`, `listing`, and `vault` helpers.
- Reduced cross-file coupling inside `cmd/` (e.g., `seed.go` no longer depends on helpers defined in `batch.go`).
- Safer concurrent writes with centralized per-path locking in `pkg/output`.

### Migration plan
1) Extract helpers to `pkg/vault`:
   - [ ] Move `TemplateContext`, `TokenContext`, `buildTemplateContext` → `vault/context.go` as `BuildTemplateContext`.
   - [ ] Move `renderTemplateString` → `vault/templates.go`.
   - [ ] Move `isVaultAbsolute`, `combineBaseAndPath`, `normalizePath` → `vault/path.go`.
2) Introduce `pkg/output`:
   - [ ] Implement `OutputMode`, `WriteOptions`, and `Write` with per-path locks and merge logic.
   - [ ] Replace direct file writes in `batch` and `generate` with `output.Write`.
3) Introduce `pkg/batch`:
   - [ ] Move `BatchConfig|BatchJob|BatchSection` → `batch/types.go`.
   - [ ] Move `processBatchSequential|Parallel|processJob` into `batch/processor.go`.
   - [ ] Keep CLI-only flag parsing in `cmd/batch.go` and delegate work to `batch.Processor`.
4) Introduce `pkg/seed`:
   - [ ] Move `SeedSpec|SeedSet` → `seed/spec.go`.
   - [ ] Extract `runSeed` core loop into `seed.Run` with `seed.Options{DryRun}`.
5) Introduce `pkg/listing`:
   - [ ] Move `ListEntry` → `listing/types.go`.
   - [ ] Move `walkVault` → `listing/walker.go`.
6) Optional convenience:
   - [ ] Add `vault.Connect(ctx, cfg)` to encapsulate token resolution + client creation.
7) Thin out `cmd/` files accordingly and update imports.
8) Add unit tests:
   - [ ] `pkg/output` for overwrite/append/merge.
   - [ ] `pkg/batch` for section/env_map/headers.
   - [ ] `pkg/seed` for env/file ingestion and templating.
   - [ ] `pkg/vault` helpers for path and templating.

### Risks and mitigations
- Refactor churn across multiple files: mitigate with incremental steps above and focused tests in `pkg/`.
- Subtle header suppression behavior: add tests to snapshot before/after content in `pkg/batch`.
- Merge semantics differences: preserve current shallow-merge logic; document clearly in code.

### References
- Existing reusable module: `pkg/envrc/generator.go` (kept as-is; expose format constants if desired).
- Vault client/token helpers: `pkg/vault/client.go`, `pkg/vault/token_loader.go` (expanded with context, path, and templating helpers).
