---
Title: Implementation Guide — YAML-configurable services
Ticket: MEN-CONFIG-YAML
Status: active
Topics:
    - configuration
    - yaml
    - services
DocType: implementation-guide
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: Design + plan to implement YAML-based service configuration, PWD & args support, and manager integration
LastUpdated: 2025-11-18T09:19:31.170395266-05:00
---

# Implementation Guide — YAML-configurable services

## 1. Purpose and Scope

This guide translates the analysis in `various/01-analysis-repository-configuration.md` into a concrete design and implementation plan to make mento-tui services configurable via YAML, including:
- Defining services (binary, port, env vars)
- Per-service working directory (PWD)
- Command-line arguments (list or space-separated string)
- Global defaults (e.g., working directory, buffer sizes)

Initial scope focuses on service definitions, PWD, and args. Future extensions (timing, process management, UI, logging) are tracked in `various/02-additional-configuration-options.md`.

## 2. High-Level Design

- Introduce a configuration loader for a YAML file (default `mento-tui.yaml`) into `internal/config`.
- Extend `models.Service` to include `WorkingDirectory` and `Args`.
- Update `services.Manager` to initialize services from loaded YAML instead of hardcoded values.
- Update service start logic to honor PWD and args.
- Add CLI flag to specify config path (default: `./mento-tui.yaml`).
- Provide an example config file at repository root.

## 3. YAML Schema (Initial Scope)

```yaml
# mento-tui.yaml
services:
  - name: "Identity Server"
    # Prefer 'ports' (list). 'port' (single) is also accepted and normalized.
    ports: [8083]
    binary_path: "./mock-binaries/identity-server"
    working_directory: "./identity-service"     # optional
    args: ["--port", "8083", "--debug"]         # or string: "--port 8083 --debug"
    env_vars:
      - "IDENTITY_SERVICE_PORT=8083"
    log_buffer_size: 1000                       # optional (falls back to global or default)

  - name: "Frontend (Vite)"
    ports: [5173]
    binary_path: "./mock-binaries/frontend"
    working_directory: "./frontend"
    args: ["--host", "0.0.0.0", "--port", "5173"]
    env_vars:
      - "VITE_PORT=5173"
    log_buffer_size: 1000                       # optional

  - name: "Mento Worker"
    ports: [8082, 9090]                         # multiple exposed ports supported
    binary_path: "./mock-binaries/worker"
    working_directory: "."
    args: "--config worker.yaml --verbose"
    env_vars:
      - "MENTO_SERVICE_PORT=8082"
    log_buffer_size: 1000                       # optional

global:
  working_directory: "."      # default PWD for services without explicit PWD (optional)
  log_buffer_size: 10000      # default global buffer size (optional)
```

Notes:
- Services may specify either `ports` (preferred) or a single `port`. If `port` is present, it will be normalized to `ports: [port]`. If both exist, they are merged (deduplicated).
- `working_directory` and `log_buffer_size` are optional at the service level; they fall back to `global` values, then to built-in defaults.

## 4. Go Types

```go
// internal/config/config.go
package config

type AppConfig struct {
    Global   GlobalConfig     `yaml:"global"`
    Services []ServiceConfig  `yaml:"services"`
}

type GlobalConfig struct {
    WorkingDirectory string `yaml:"working_directory"`
    LogBufferSize    int    `yaml:"log_buffer_size"`
}

type ServiceConfig struct {
    Name             string            `yaml:"name"`
    // Ports: preferred multi-port field. 'Port' (single) remains for convenience/BC and gets normalized.
    Ports            []int             `yaml:"ports"`
    Port             int               `yaml:"port"`
    BinaryPath       string            `yaml:"binary_path"`
    WorkingDirectory string            `yaml:"working_directory"` // optional
    ArgsList         []string          `yaml:"args"`          // when YAML provides a list
    ArgsString       string            `yaml:"-"`             // parse from raw for string case
    EnvVars          []string          `yaml:"env_vars"`
    LogBufferSize    int               `yaml:"log_buffer_size"`   // optional
}
```

Parsing strategy for args:
- If YAML node for `args` is a sequence → `ArgsList`.
- If YAML node is a string → split with `strings.Fields()` into `ArgsList`.

Normalization strategy for ports:
- If `Ports` is empty and `Port > 0`, set `Ports = []int{Port}`.
- If both are set, merge into `Ports` and deduplicate; ignore `Port` afterward.

## 5. Loader API

```go
// internal/config/config.go
func Load(path string) (*AppConfig, error)
func (ac *AppConfig) Validate() error // required fields, ranges, duplicates
```

Validation rules (initial):
- Service: `name`, (`ports` or `port`), and `binary_path` required.
- Ports in [1, 65535] and deduplicated.
- Duplicate service names rejected; duplicate ports across services optionally warned/error (policy).
- If `working_directory` set: should exist (warn first; can become strict later).
- `log_buffer_size` optional per service; falls back to `global.log_buffer_size`, else default 1000.

## 6. Manager Integration

- Change `services.NewManager()` to `services.NewManager(cfg *config.AppConfig)`.
- Map `config.ServiceConfig` to `models.Service` (including `WorkingDirectory`, `Args`, and `Ports`).
- Fallbacks:
  - `service.LogBuffer = models.NewLogBuffer(serviceCfg.LogBufferSize or cfg.Global.LogBufferSize or 1000)`
  - `manager.GlobalLog = models.NewLogBuffer(cfg.Global.LogBufferSize or 10000)`

Service start logic:

```go
// exec.Command with args
cmd := exec.Command(svc.BinaryPath, svc.Args...)

// working directory precedence: service -> global -> default
switch {
case svc.WorkingDirectory != "":
    cmd.Dir = svc.WorkingDirectory
case global.WorkingDirectory != "":
    cmd.Dir = global.WorkingDirectory
}

cmd.Env = append(os.Environ(), svc.EnvVars...)
```

UI updates:
- Replace single port display with joined list, e.g., `Ports: 8082, 9090`. If only one port, still display singular form for readability.

## 7. CLI Changes

- Add `--config` flag to `cmd/main.go` to pass config path (default `./mento-tui.yaml`).
- Remove hardcoded `os.Chdir("/home/ubuntu/mento-tui")`.
- On load errors: print helpful message and exit non-zero.

Pseudo-code:

```go
// cmd/main.go
configPath := flag.String("config", "./mento-tui.yaml", "path to config file")
flag.Parse()

cfg, err := config.Load(*configPath)
if err != nil {
    fmt.Fprintf(os.Stderr, "Error loading config: %v\n", err)
    os.Exit(1)
}

p := tea.NewProgram(ui.NewModel(cfg))
```

Update `ui.NewModel(cfg *config.AppConfig)` to pass cfg into `services.NewManager(cfg)`.

## 8. File Changes

- Create: `internal/config/config.go`, `internal/config/config_test.go`
- Modify: 
  - `internal/models/models.go` (add fields: `WorkingDirectory string`, `Args []string`, `Ports []int`)
  - `internal/services/manager.go` (constructor + start logic)
  - `cmd/main.go` (flags + loader)
  - `internal/ui/app.go` and `internal/ui/dashboard.go` (wire cfg and display multiple ports)
- Add: `mento-tui.yaml.example`
- Update: `README.md` (document configuration)

## 9. Step-by-Step Plan

- [ ] Add YAML dependency to `go.mod` (`gopkg.in/yaml.v3`)
- [ ] Create `internal/config/config.go` with `Load` and `Validate`
- [ ] Extend `models.Service` with `WorkingDirectory string`, `Args []string`, `Ports []int`
- [ ] Modify `services.NewManager(cfg)` to build services from config (normalize `port`/`ports`)
- [ ] Update `StartService` to use args and working directory
- [ ] Update `dashboard`/UI to render multiple ports
- [ ] Update `cmd/main.go` to accept `--config` and load config
- [ ] Thread `cfg` through `ui.NewModel(cfg)` and into manager
- [ ] Create `mento-tui.yaml.example` matching schema
- [ ] Update `README.md` with setup and example
- [ ] Add unit tests for loader and validation
- [ ] Add integration smoke test (load config, start/stop services)

## 10. Testing Strategy

Unit tests:
- Valid config loads and maps to services
- Invalid config: missing fields, port(s) range, duplicate names/ports
- Args parsing: list vs string
- Ports normalization: `port` only, `ports` only, and both combined/deduplicated
- Optional fields: missing per-service `working_directory` and `log_buffer_size` fall back correctly

Integration tests (manual/local acceptable):
- Start/stop single service and all services using example config
- Verify working directory affects relative file access (smoke)
- Display renders multiple ports correctly

## 11. Risks and Mitigations

- Incorrect args parsing → Prefer list format in examples; robust split for strings.
- Non-existent PWD → Warn and default to global/current dir.
- Relative paths brittle → Document that paths are resolved relative to process PWD.
- Ports collisions → Add optional validation to flag conflicts across services.

## 12. Follow-ups (Out of Scope for MVP)

Tracked in `various/02-additional-configuration-options.md`:
- Timing intervals, graceful shutdown, health checks, dependencies
- UI theming, logging options, keybindings
- Notifications, viewport, validation, performance tuning
