---
Title: Writing tmux-dashboard YAML
Slug: writing-tmux-dashboard-yaml
Short: Author YAML configs with includes, variables, tabs, panes, and layouts
Topics:
- tmux
- dashboard
- yaml
- authoring
Commands:
- render
- validate
- apply
Flags:
- --session
- --set
- --set-json
- --set-json-file
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Writing tmux-dashboard YAML

This page describes the configuration schema for tmux-dashboard and provides examples and best practices for composing dashboards with includes, variables, and validated layouts.

## File schema

```yaml
version: 2                 # required, must be 2
session: "my-session"      # required
include:                   # optional list of files; merged in order
  - path/to/another.yml
vars:                      # optional default variables (can be overridden)
  project: acme
tabs:                      # required, at least one
  - name: "Overview"       # required, unique across all includes
    layout: tiled          # optional; see valid values below
    panes:                 # required, at least one pane per tab
      - cmd: "htop"        # required command; Go template variables allowed
        refresh: 0         # optional seconds; repeats command if > 0
        env:               # optional env vars for the pane
          ENV: prod
```

## Includes and merging

- `include` accepts file paths (relative paths are resolved from the current file’s directory).
- Tabs from included files are appended first, then tabs from the current file.
- Duplicate tab `name` values across all includes are rejected.

## Variables and templating

- Define defaults under `vars:` and override at runtime via `--set`, `--set-json`, or `--set-json-file`.
- Variables are substituted in `cmd` strings using Go templates with the variables map. Example: `echo {{ .project }}`.
- Precedence: `--set-json` > `--set-json-file` > `--set` > file-level `vars`.

Example:

```yaml
vars:
  project: acme
tabs:
  - name: "Dev"
    panes:
      - cmd: "echo Project: {{ .project }}"
```

Runtime override:

```bash
tmux-dashboard render --set project=zeus examples/devdash.yml
```

## Tabs and panes

- Each tab needs a `name` and at least one `pane`.
- Each pane needs a `cmd`.
- `refresh` repeats the command every N seconds in a loop and prepends a timestamp; use for live dashboards.
- `env` sets environment variables in the pane before running the command.

Example pane with refresh and env:

```yaml
panes:
  - cmd: "kubectl get pods -A"
    refresh: 10
    env:
      KUBECONFIG: /home/user/.kube/config
```

## Layouts

Valid `layout` values:

- `tiled`
- `even-vertical`
- `even-horizontal`
- `main-vertical`
- `main-horizontal`

If omitted, `tiled` is used.

## Validation rules

- `version` must be 2
- `session` is required
- At least one `tab`
- Unique `tab.name` across all includes
- `layout` must be one of the valid values (if provided)
- Each `pane` must have a non-empty `cmd`; `refresh` must be >= 0

Validate a file:

```bash
tmux-dashboard validate examples/demo.yml
```

## Authoring tips

- Use `render` to inspect the final config after includes and variables:

```bash
tmux-dashboard render --set env=prod examples/devdash.yml
```

- Prefer small, focused `include` files (one concern per file) to keep large dashboards maintainable.
- Use unique, descriptive tab names; duplicates are not allowed.
- When using `refresh`, keep commands idempotent and fast.

See usage examples:

  glaze help using-tmux-dashboard


