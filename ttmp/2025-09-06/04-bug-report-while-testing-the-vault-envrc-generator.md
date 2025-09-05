# Bug Report — Testing vault-envrc-generator

Date: 2025-09-06
Scope: End-to-end testing of list, generate, batch, and seed commands against `https://vault.mento.co/` using repository-provided configs

## Environment
- Host: linux 6.8.0-78-generic, zsh
- Working dir: `vibes/2025-09-04/vault-envrc-generator/go-utility`
- VAULT_ADDR: `https://vault.mento.co/`
- Commands built with: `GOCACHE=$(pwd)/.gocache go build -o vault-envrc-generator .`

## Summary of What Works
- generate: envrc/json/yaml output; transform-keys; prefix; output to file; dry-run
- batch: dry-run to stdout (json and envrc), YAML merge to file, JSON merged stdout
- list: directory listing for existing paths with `--output table` and `--output yaml`
- seed: dry-run reports writes (debug logs) for all sets in `seed-personal.yaml`

## Issues Found

### 1) batch output override still prints envrc content to stdout
- Command:
  - `./vault-envrc-generator batch -c batch-personal.yaml --output override.envrc --output-mode overwrite --vault-addr https://vault.mento.co`
- Expected:
  - Creates `override.envrc` and does not print full envrc sections to stdout
- Actual:
  - Full envrc content printed to stdout; file `override.envrc` not found afterward
- Evidence:
  - Command output showed the envrc sections; `ls -la override.envrc` → file missing
- Notes:
  - Code path: `pkg/batch/processor.go`
  - In `processJob`, aggregated stdout behavior always prints content for non-json/yaml formats (envrc) and file write happens only when there is a single unique output path AND not dry-run. However, printing happens regardless.
  - Potential fix: buffer envrc sections for stdout only when output path is `-`, otherwise aggregate and write to file without printing.

### 2) list --include-values returns only warning and no data in some cases
- Commands:
  - `./vault-envrc-generator list --path secrets/environments/ --depth 2 --output json --vault-addr https://vault.mento.co`
  - `./vault-envrc-generator list --path secrets/environments/development/personal/105823507735936514181/local/slack --include-values --censor "XXXXX" --output yaml --vault-addr https://vault.mento.co`
- Expected:
  - Structured rows with censored values or at least keys
- Actual:
  - Only `Warnings (1) encountered during listing.` printed in some invocations
- Evidence:
  - The `list` command to `.../slack` returned only the warning line, no rows
- Notes:
  - Implementation: `cmds/list.go` uses Glazed middlewares; output formats are restricted to `yaml|text`; earlier tests using `--output table` relied on Glazed output layers on the root (works for directory overview).
  - For `--include-values`, code builds rows with a `data` map of censored values; but if `client.GetSecrets(e)` errors, it sets `data = map[string]interface{}{}` and still adds a row. The absence of rows suggests walker returned zero entries, likely due to `ListSecrets` at `.../slack` returning nil or empty.
  - `vault.NormalizeListPath` appends a trailing `/`, which is correct. Potential ACL limitations or KV engine metadata listing edge case may lead to empty list at leaf path.
  - Suggest: Improve messages by printing the path that caused warnings and return at least a row indicating inaccessible path with reason when `include-values` is set.

### 3) batch YAML append: file not found after run
- Command:
  - `./vault-envrc-generator batch -c batch-personal.yaml --format yaml --output out/tmp/append.yaml --output-mode append --vault-addr https://vault.mento.co`
- Expected:
  - `out/tmp/append.yaml` created (with multi-doc using `---` when appending repeatedly)
- Actual:
  - After run, `ls out/tmp` showed only `merge.yaml`; `append.yaml` missing
- Evidence:
  - `ls -l out/tmp` → no `append.yaml`
- Notes:
  - Code path: `pkg/batch/processor.go#L357-L426` writes aggregated JSON/YAML to file only when a single, non-stdout output path is used AND when content is aggregated via stdout aggregators. For YAML append mode, aggregator uses `stdoutYAMLDocs` (not `stdoutYAMLAgg`) and the file write branch prefers `stdoutJSONAgg` or `stdoutYAMLAgg` but not `stdoutYAMLDocs`, so nothing gets written. This explains missing append file.
  - Potential fix: When `format == yaml` and `mode == append`, concatenate doc strings with separators and write them via `output.Write` with `Append` mode, even if `stdoutYAMLDocs` is used.

### 4) batch envrc vs stdout aggregation semantics
- Observation:
  - Envrc sections are printed to stdout during batch even when a file output path is specified.
- Impact:
  - Hard to script; mixes stdout with content, leading to accidental leakage of secrets on console.
- Suggestion:
  - For envrc format, collect per-section content into an in-memory buffer and only print when output target is `-`. Otherwise, write to file only.

### 5) generate command argument parsing edge case (cosmetic)
- Observation:
  - A previous run showed a garbled command echo where the `--format` flag appeared mangled in the shell output. Actual generation worked and file output was correct.
- Status:
  - Likely a terminal paste artifact. No action unless reproducible.

## Additional Validation Performed
- generate envrc + transform/prefix: OK
- generate yaml/json sorted: OK
- generate to file: OK
- list directory level table/yaml: OK
- batch dry-run to stdout (json merged): OK
- batch yaml merge to file with ordered keys: OK (`out/tmp/merge.yaml` created)

## Suggested Fixes (Code Pointers)
1. batch envrc printing and file write
   - File: `pkg/batch/processor.go`
   - Areas: `processJob` aggregation and flush logic
   - Change:
     - Do not print envrc content to stdout when `renderedOutPath != "-"` and not dry-run
     - Accumulate envrc contents per section and write via `output.Write` when single output target

2. batch YAML append writing
   - File: `pkg/batch/processor.go` around `stdoutYAMLDocs` handling and file flush
   - Change:
     - If `format == yaml` and `mode == append` and `stdoutYAMLDocs` has content and only one output path, join docs with `---` and call `output.Write` with `OutputModeAppend`

3. list warn-only behavior visibility
   - Files: `cmds/list.go`, `pkg/listing/walker.go`, `pkg/vault/client.go`
   - Improvements:
     - Include warning details in stderr (path and error string) instead of only a count
     - Optionally return a row with `type: warning` for paths that could not be listed to make UI more informative

## Repro Steps (Concise)
1) Envrc printed instead of file write
```bash
./vault-envrc-generator batch -c batch-personal.yaml \
  --output override.envrc --output-mode overwrite \
  --vault-addr https://vault.mento.co
```

2) list include-values warning-only
```bash
./vault-envrc-generator list \
  --path secrets/environments/development/personal/105823507735936514181/local/slack \
  --include-values --censor "XXXXX" --output yaml \
  --vault-addr https://vault.mento.co
```

3) YAML append file missing
```bash
./vault-envrc-generator batch -c batch-personal.yaml \
  --format yaml --output out/tmp/append.yaml --output-mode append \
  --vault-addr https://vault.mento.co
ls -l out/tmp
```

## Impact Assessment
- stdout leakage risk for envrc in batch mode when output path is a file
- Missing append output reduces usefulness of YAML multi-doc workflows
- list UX: warning-only messages reduce debuggability for end users

## Recommendations
- Prioritize batch write/print semantics refactor (envrc, YAML append)
- Enhance list warning verbosity and optionally emit warning rows
- Add integration tests for batch modes: overwrite, append, merge (json/yaml/envrc)
- Document stdout vs file write semantics explicitly in Getting Started guide
