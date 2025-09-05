### Notes on refactoring the vault-envrc-generator to be reusable

#### Overview
This document summarizes the refactor of the Go CLI under `vibes/2025-09-04/vault-envrc-generator/go-utility/` into a more reusable, modular architecture. It covers what changed, what works now, what still needs work, and recommended next steps.

#### What changed (high-level)
- Extracted shared logic out of `cmd/*` into `pkg/*` packages.
- Removed parallel execution and consolidated sequential processing to eliminate deadlocks and simplify control flow.
- Introduced robust, centralized output handling (overwrite/append/merge), including JSON-merge semantics and YAML multi-document append.
- Applied key-transform logic consistently across formats (envrc/json/yaml).
- Added verbose logging in key subsystems (batch processing and output writing).

#### New packages and responsibilities
- `pkg/vault/`
  - `context.go`: `BuildTemplateContext(*vault.Client) (TemplateContext, error)`
  - `path.go`: `IsAbsoluteVaultPath`, `JoinBaseAndPath`, `NormalizeListPath`
  - `templates.go`: `RenderTemplateString`
- `pkg/envrc/`
  - `generator.go` (existing): `Options`, `Generator`, `Generate`
  - Change: `TransformKeys` now applies across all formats, not just `envrc`.
- `pkg/output/`
  - `writer.go`: `Write(path string, content []byte, opts WriteOptions) error`
    - Centralizes overwrite/append/merge logic with per-path locking
    - JSON is always merged
    - YAML append creates multiple documents (`---` separator when appending to non-empty file)
- `pkg/batch/`
  - `types.go`: `Config`, `Job`, `Section`
  - `processor.go`: `Processor.Process(cfg, opts)` orchestrates the whole batch
    - Aggregates per-output-path and flushes once at the end
    - Stdout aggregation for JSON and YAML
    - File aggregation via `pkg/output` and per-path locking
- `pkg/seed/`
  - `spec.go`: `Spec`, `Set`
  - `runner.go`: `Run(client, spec, opts, verbose)` handles seeding from env and files
- `pkg/listing/`
  - `types.go`: `Entry`
  - `walker.go`: `Walk(client, path, depth)` for listing paths

#### CLI changes and current responsibilities
- `cmd/root.go`: global flags, config initialization.
- `cmd/generate.go`: unchanged interface; now uses `pkg/output.Write` for writing.
- `cmd/interactive.go`: unchanged interface (now benefits from shared logic).
- `cmd/list.go`: uses `pkg/listing` for walking and types.
- `cmd/seed.go`: delegates to `pkg/seed.Run`.
- `cmd/batch.go`: thin wrapper that parses flags and delegates to `pkg/batch.Processor.Process`.
  - Removed legacy helpers, parallel flags, and in-file per-path locks.
  - Long help updated to highlight JSON/YAML support.

#### Behavior and rules (by format)
- envrc
  - Default behavior unchanged.
  - Per-section headers include job and section descriptions; content concatenated.
  - Header suppression works when appending to existing files.
- json
  - Always merged (shallow) across sections, both to stdout and files.
- yaml
  - `output_mode: merge` performs a shallow merge across sections.
  - `output_mode: append` appends as multiple YAML documents (separated by `---`), both to stdout (single flush at end) and to files.
- Key transform/prefix
  - `transform_keys` and `prefix` apply consistently to envrc/json/yaml (transform converts to UPPERCASE with `-` -> `_`).
  - When `env_map` is used, transform/prefix/include/exclude are disabled for that section (explicit mapping wins).

#### Logging and diagnostics
- `pkg/batch/processor.go` logs per-job and per-section steps with `-v`.
- `pkg/output/writer.go` logs lock acquisition, effective write mode, and write/merge operations with `Verbose`.

#### What is working
- Seed command: dry-run and actual writes via `pkg/seed.Run`.
- Batch command (sequential):
  - envrc generation with section headers; append/overwrite modes.
  - json output with forced merge.
  - yaml output with merge and multi-doc append.
  - Stdout handling for json/yaml: aggregate and print at end; envrc streams immediately.
- Consistent key transformation across formats.
- Deadlock issue eliminated by removing parallelism and centralizing write locks.

#### What is not working / caveats
- YAML merge is shallow (keys from later sections overwrite earlier ones). Deep merge is not implemented.
- For stdout in yaml append mode, output is printed once at the end (after all sections); this is expected but differs from streaming behavior.
- Some legacy `cmd/*` helpers removed; if any external scripts depended on those internals, they will need to update to the new `pkg/*` APIs.
- We removed parallel processing; if high throughput is necessary in the future, a worker-pool with strict, per-file write serialization could be reintroduced.

#### Validation performed
- Built the project successfully multiple times.
- Ran seed dry-run successfully.
- Ran batch with `envrc` (append/overwrite) and confirmed content and headers.
- Ran batch to stdout with `--format json --output -` and observed merged JSON.
- Ran batch to stdout with `--format yaml --output - --output-mode append` and observed verbose logs and single flush at end.
- Ran batch to file with `--format yaml --output out/tmp.yaml --output-mode append` and confirmed verbose logs showing section-by-section generation; the final write is aggregated per current logic.

Example commands used during validation:
```bash
# build and run
cd vibes/2025-09-04/vault-envrc-generator/go-utility && go build -o vault-envrc-generator .

# seed dry run
./vault-envrc-generator seed -c seed-personal.yaml --dry-run -v

# batch envrc
./vault-envrc-generator batch -c batch-personal.yaml -v

# batch stdout json (merged)
./vault-envrc-generator batch -c batch-personal.yaml --output - --format json -v

# batch stdout yaml (append as multi-doc)
./vault-envrc-generator batch -c batch-personal.yaml --output - --format yaml --output-mode append -v

# batch yaml append to file (multi-doc)
./vault-envrc-generator batch -c batch-personal.yaml --format yaml --output out/tmp.yaml --output-mode append -v
```

#### Where to continue / next steps
- Finalize cleanup:
  - Remove `cmd/list_types.go` (types now under `pkg/listing`).
  - Verify no lingering unused helpers remain in `cmd/*`.
- Add unit tests:
  - `pkg/output`: overwrite/append/merge, yaml multi-doc append, json forced merge.
  - `pkg/batch`: env_map behavior, header suppression, stdout aggregation.
  - `pkg/envrc/generator`: transform/prefix across formats.
  - `pkg/vault` helpers: template rendering and path joining.
- YAML merge depth:
  - Decide if you want deep-merge for YAML; if yes, add a deep-merge option.
- Performance:
  - If needed, reintroduce concurrency safely with a worker pool and per-target write queue (single writer per path).

#### Future enhancements
- Add `--deep-merge` for YAML to recursively merge nested maps.
- Provide `--sort-keys` for json/yaml to ensure deterministic output ordering.
- Add `--no-headers` option to envrc for completely headerless sections.
- Support templated `output_mode` and `format` (already templated through job/section overrides, but could be extended by variables).
- Add `--dry-run` to batch to preview writes for all formats (currently easy to emulate by `--output -`).

#### Knowledge learned / rationale
- Parallelism increases complexity for the value provided here; sequential execution removes deadlock and is fast enough for typical use.
- Centralizing output handling (locking, merge, append) simplifies correctness and unlocks format-specific rules.
- Aggregating per-output (and stdout) before writing avoids interleaving issues and gives deterministic outputs for multi-section jobs.
- Applying transform/prefix consistently across formats prevents confusing discrepancies (e.g., `OPENAI_api_key`).

#### Key files (post-refactor)
- `pkg/vault/context.go`, `pkg/vault/path.go`, `pkg/vault/templates.go`
- `pkg/envrc/generator.go`
- `pkg/output/writer.go`
- `pkg/batch/types.go`, `pkg/batch/processor.go`
- `pkg/seed/spec.go`, `pkg/seed/runner.go`
- `pkg/listing/types.go`, `pkg/listing/walker.go`
- `cmd/batch.go`, `cmd/seed.go`, `cmd/generate.go`, `cmd/list.go`, `cmd/interactive.go`, `cmd/root.go`

