# tmux-dashboard YAML Guide

## Overview
The dashboard is defined by a YAML file that describes a tmux session, its windows (tabs), and panes. The main entry point reads the YAML, validates it, substitutes variables, and then applies it to tmux (or prints a dry‑run).

## Config structure (config.go)
```yaml
version: 2               # required, currently only 2 is supported
session: "my-session"   # tmux session name
include: ["extra.yml"] # optional list of additional config files
vars:                     # optional map of variables for templating
  VAR1: "value"

tabs:
  - name: "main"
    layout: "tiled"    # optional, default "tiled"
    panes:
      - cmd: "top"               # command to run in the pane
        refresh: 5                # optional refresh interval in seconds
        env:
          FOO: "bar"            # optional env vars
```

## Execution flow (main.go → Config → TmuxManager)
1. **Load config** – `LoadConfigWithIncludes` loads the primary YAML and any `include` files, merges tabs, and checks for duplicate tab names.
2. **Validate** – `Validate` ensures version 2, non‑empty session, at least one tab, valid layouts, and non‑empty pane commands.
3. **Substitute variables** – `SubstituteVars` merges global vars, `Vars` from the file and values passed via `--var` on the command line, then replaces `{{ .VAR }}` placeholders using `text/template`.
4. **Create manager** – `NewTmuxManager` creates a `gotmux` client unless `--dry-run` is set.
5. **Apply** – `ApplyConfig` kills an existing session, creates a new session, creates windows for each `tab`, splits panes, sends environment variables, runs commands (optionally wrapped in a refresh loop), and applies the layout.
6. **Dry‑run** – When `dryRun` is true `printDryRun` prints the exact `tmux` commands that would be executed.

## Command generation
- Commands are sent directly without `bash -lc` wrapping. For refresh loops, the command is executed repeatedly with a timestamp and clear screen. Environment variables are sent before the command with `export KEY=VALUE` and a `C-m` (Enter) to apply. Layouts are mapped to `gotmux` constants; unsupported layouts fall back to `tiled`.

## Usage
```bash
go run main.go -c examples/demo.yml        # run dashboard
go run main.go -c demo.yml --dry-run         # show tmux commands only
```

*Generated with Crush*