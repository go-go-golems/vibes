---
Title: Using tmux-dashboard
Slug: using-tmux-dashboard
Short: Learn how to run, inspect, and validate dashboards, pass variables, and control sessions
Topics:
- tmux
- dashboard
- usage
- commands
Commands:
- apply
- render
- validate
Flags:
- --session
- --set
- --set-json
- --set-json-file
- --dry-run
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Using tmux-dashboard

tmux-dashboard turns a YAML file into a full tmux session with windows, panes, and commands. This page shows how to run dashboards, override variables, inspect the resolved configuration, and validate files before applying.

## Quick start

Run a configuration file and create the tmux session:

```bash
tmux-dashboard apply examples/devdash.yml
```

Preview what would be executed without changing your system:

```bash
tmux-dashboard apply --dry-run examples/devdash.yml
```

Override the session name:

```bash
tmux-dashboard apply --session dev examples/devdash.yml
```

Pass variables directly, from JSON, or from a JSON file:

```bash
tmux-dashboard apply --set project=acme --set env=prod examples/devdash.yml
tmux-dashboard apply --set-json '{"project":"acme","env":"prod"}' examples/devdash.yml
tmux-dashboard apply --set-json-file examples/vars.json examples/devdash.yml
```

## Commands

### apply <config.yml>

Create or recreate the tmux session from a YAML configuration.

- **Key flags:** `--session`, `--set`, `--set-json`, `--set-json-file`, `--dry-run`

Examples:

```bash
tmux-dashboard apply examples/demo.yml
tmux-dashboard apply --session my-session examples/sysdash.yml
tmux-dashboard apply --dry-run examples/network.yml
```

### render <config.yml>

Render the fully-resolved configuration after processing `include` files and variable substitution. Useful for debugging and reviewing the exact commands that will be used.

Examples:

```bash
tmux-dashboard render examples/demo.yml
tmux-dashboard render --set project=acme examples/devdash.yml
```

### validate <config.yml>

Validate syntax, schema, and logical consistency of a configuration file. Prints a short summary of the parsed session.

Examples:

```bash
tmux-dashboard validate examples/demo.yml
```

## Variables and precedence

Variables can be defined in the YAML under `vars:` and overridden from the CLI. Precedence is: `--set-json` > `--set-json-file` > `--set` > `vars` in the file. Variables are substituted into `cmd` strings using Go templates, for example `{{ .project }}`.

Example:

```bash
tmux-dashboard apply --set project=acme examples/devdash.yml
```

## Sessions and dry runs

- **--session**: Override the `session` from the file at runtime.
- **--dry-run**: Print the `tmux` commands that would be executed, without touching tmux.

```bash
tmux-dashboard apply --dry-run examples/devdash.yml | less
```

## Examples from this repository

Try the sample dashboards under `examples/`:

```bash
tmux-dashboard apply examples/demo.yml
tmux-dashboard apply examples/sysdash.yml
tmux-dashboard apply examples/network.yml
```

Use `render` to examine the resolved config:

```bash
tmux-dashboard render examples/devdash.yml
```

## Troubleshooting

- If validation fails with an unsupported version, make sure `version: 2` is set.
- Duplicate tab names across `include` files are rejected; ensure each tab has a unique `name`.
- Invalid `layout` values will be flagged during validation. See the schema details in “glaze help writing-tmux-dashboard-yaml”.

For schema and authoring guidance, see:

  glaze help writing-tmux-dashboard-yaml


