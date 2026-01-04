# Additional Configuration Options

This document explores what else could be made configurable beyond the initial service definitions, binary paths, working directories, and command-line arguments.

## Timing and Intervals

### Current Hardcoded Values

1. **UI Refresh Interval**: `1 second` (in `internal/ui/app.go:53`)
   ```go
   return tea.Tick(time.Second, func(t time.Time) tea.Msg {
   ```

2. **Process Monitoring Interval**: `2 seconds` (in `internal/services/manager.go:203`)
   ```go
   ticker := time.NewTicker(2 * time.Second)
   ```

3. **Startup Status Check Delay**: `3 seconds` (in `internal/services/manager.go:115`)
   ```go
   time.Sleep(3 * time.Second)
   ```

4. **Service Startup Delay**: `500ms` between services (in `internal/services/manager.go:168`)
   ```go
   time.Sleep(500 * time.Millisecond)
   ```

5. **Restart Delay**: `1 second` (in `internal/services/manager.go:159`)
   ```go
   time.Sleep(1 * time.Second)
   ```

### Proposed Configuration

```yaml
timing:
  ui_refresh_interval: "1s"          # UI update frequency
  process_monitor_interval: "2s"      # CPU/memory check frequency
  startup_status_delay: "3s"          # Wait before marking as running
  service_startup_delay: "500ms"      # Delay between starting services
  restart_delay: "1s"                 # Delay between stop and start on restart
```

**Benefits**: 
- Adjust for slower systems (longer intervals = less CPU)
- Faster feedback for development (shorter intervals)
- Fine-tune startup sequencing

## Process Management

### Current Behavior

- SIGTERM sent immediately on stop
- No timeout before SIGKILL
- No health checks
- No auto-restart on failure

### Proposed Configuration

```yaml
process_management:
  graceful_shutdown_timeout: "10s"    # Wait for SIGTERM before SIGKILL
  health_check_interval: "5s"          # Check if process is still alive
  health_check_endpoint: ""            # Optional HTTP endpoint to check
  auto_restart_on_failure: false       # Restart service if it crashes
  max_restart_attempts: 3              # Limit restart attempts
  restart_backoff: "exponential"       # exponential, linear, fixed
  restart_backoff_base: "2s"           # Base delay for exponential backoff
```

**Benefits**:
- More graceful shutdowns
- Better failure recovery
- Health monitoring
- Prevents restart loops

## UI/Display Configuration

### Current Hardcoded Values

All colors and styles are hardcoded in `internal/ui/styles.go`:
- Primary color: `#7D56F4`
- Secondary color: `#00BFFF`
- Success color: `#00FF00`
- Warning color: `#FFA500`
- Error color: `#FF0000`
- Muted color: `#666666`
- Border color: `#383838`

### Proposed Configuration

```yaml
ui:
  theme: "default"  # default, dark, light, custom
  colors:
    primary: "#7D56F4"
    secondary: "#00BFFF"
    success: "#00FF00"
    warning: "#FFA500"
    error: "#FF0000"
    muted: "#666666"
    border: "#383838"
  layout:
    default_width: 120
    default_height: 40
    service_card_padding: 2
    service_card_margin: 1
  display:
    show_timestamps: true
    timestamp_format: "15:04:05"      # Go time format
    show_pids: true
    show_ports: true
    show_cpu_memory: true
    uptime_format: "HH:MM:SS"         # or "human" for "2h 15m"
```

**Benefits**:
- Personalization/theming
- Accessibility (color blind friendly themes)
- Terminal size optimization
- Information density control

## Logging Configuration

### Current Hardcoded Values

- Global log buffer: `10000` lines
- Per-service log buffer: `1000` lines
- No log filtering
- No log retention policies

### Proposed Configuration

```yaml
logging:
  global_buffer_size: 10000
  service_buffer_size: 1000
  timestamp_format: "2006-01-02 15:04:05.000"
  log_levels: ["debug", "info", "warn", "error"]  # Filter by level
  filters:
    - pattern: ".*ERROR.*"              # Regex patterns to highlight
      style: "error"
    - pattern: ".*WARN.*"
      style: "warning"
  retention:
    max_age: "24h"                      # Keep logs for 24 hours
    max_size_mb: 100                    # Max log size
  export:
    enabled: false
    path: "./logs"
    format: "json"                      # json, text, csv
```

**Benefits**:
- Memory management
- Better log filtering/search
- Log export for analysis
- Highlighting important messages

## Service Behavior

### Current Limitations

- No service dependencies
- No auto-start on launch
- No health checks
- No startup order control

### Proposed Configuration

```yaml
services:
  - name: "Identity Server"
    # ... existing fields ...
    behavior:
      auto_start: false                # Start automatically on launch
      depends_on: []                   # Service names that must start first
      startup_timeout: "30s"           # Fail if not running after timeout
      health_check:
        enabled: true
        endpoint: "http://localhost:8083/health"
        interval: "5s"
        timeout: "2s"
      restart_policy:
        on_failure: "always"           # always, never, on-exit
        max_attempts: 5
        backoff: "exponential"
      priority: 1                      # Lower = starts first
```

**Benefits**:
- Proper service orchestration
- Dependency management
- Health monitoring
- Automatic recovery

## Keyboard Shortcuts

### Current Hardcoded Bindings

All shortcuts are hardcoded in `internal/ui/app.go`:
- `q`, `ctrl+c` - Quit
- `h`, `?` - Help
- `esc` - Back
- `t` - Tail logs
- `c`, `e` - Config
- `enter` - Start service
- `r` - Restart
- `s` - Stop
- `a` - Start all
- `x` - Stop all

### Proposed Configuration

```yaml
keybindings:
  quit: ["q", "ctrl+c"]
  help: ["h", "?"]
  back: ["esc"]
  tail_logs: ["t"]
  config: ["c", "e"]
  start_service: ["enter"]
  restart_service: ["r"]
  stop_service: ["s"]
  start_all: ["a"]
  stop_all: ["x"]
  # Custom bindings
  custom:
    - key: "f"
      action: "filter_logs"
    - key: "g"
      action: "goto_top"
```

**Benefits**:
- Personalization
- Accessibility (remap keys)
- Power user features
- Vim-style bindings option

## Notification/Alerts

### Current State

- No notifications
- No alert thresholds

### Proposed Configuration

```yaml
notifications:
  enabled: true
  channels:
    - type: "terminal"                  # terminal, desktop, sound
      on_service_failure: true
      on_service_start: false
      on_high_cpu: true
      cpu_threshold: 80.0
      on_high_memory: true
      memory_threshold_mb: 1000
  sound:
    enabled: false
    on_failure: "/path/to/alert.wav"
```

**Benefits**:
- Better monitoring awareness
- Alert on issues
- Configurable thresholds

## Window/Viewport Settings

### Current State

- No default window size
- No viewport configuration

### Proposed Configuration

```yaml
viewport:
  default_width: 120
  default_height: 40
  min_width: 80
  min_height: 24
  log_viewer:
    lines_per_page: 20
    auto_scroll: true
    follow_tail: true
  dashboard:
    services_per_page: 5
    show_all_services: true
```

**Benefits**:
- Better UX for different terminal sizes
- Scroll behavior control
- Information density management

## Validation and Safety

### Current State

- No port conflict detection
- No binary validation before start
- No resource limit checks

### Proposed Configuration

```yaml
validation:
  check_port_conflicts: true
  check_binary_exists: true
  check_working_directory: true
  resource_limits:
    max_cpu_percent: 95.0
    max_memory_mb: 2048
    warn_at_cpu_percent: 80.0
    warn_at_memory_mb: 1500
  startup_checks:
    - type: "port_available"
      port: 8083
    - type: "file_exists"
      path: "./config.yaml"
    - type: "directory_exists"
      path: "./data"
```

**Benefits**:
- Prevent common errors
- Resource monitoring
- Pre-flight checks
- Better error messages

## Performance Tuning

### Proposed Configuration

```yaml
performance:
  log_buffer_prealloc: true            # Pre-allocate log buffers
  concurrent_startup: false            # Start services in parallel
  max_concurrent_starts: 3             # Limit parallel starts
  process_poll_optimization: true      # Optimize process stats polling
  ui_render_throttle: "100ms"          # Throttle UI updates
```

**Benefits**:
- Better performance on slower systems
- Resource optimization
- Faster startup for many services

## Example: Complete Configuration File

```yaml
# mento-tui.yaml - Complete example

services:
  - name: "Identity Server"
    port: 8083
    binary_path: "./mock-binaries/identity-server"
    working_directory: "./identity-service"
    args: ["--port", "8083", "--debug"]
    env_vars:
      - "IDENTITY_SERVICE_PORT=8083"
    log_buffer_size: 1000
    behavior:
      auto_start: false
      depends_on: []
      startup_timeout: "30s"
      priority: 1

global:
  log_buffer_size: 10000
  working_directory: "."

timing:
  ui_refresh_interval: "1s"
  process_monitor_interval: "2s"
  startup_status_delay: "3s"
  service_startup_delay: "500ms"
  restart_delay: "1s"

process_management:
  graceful_shutdown_timeout: "10s"
  health_check_interval: "5s"
  auto_restart_on_failure: false
  max_restart_attempts: 3

ui:
  theme: "default"
  colors:
    primary: "#7D56F4"
    secondary: "#00BFFF"
    success: "#00FF00"
  display:
    show_timestamps: true
    timestamp_format: "15:04:05"
    uptime_format: "HH:MM:SS"

logging:
  global_buffer_size: 10000
  service_buffer_size: 1000
  timestamp_format: "2006-01-02 15:04:05.000"

keybindings:
  quit: ["q", "ctrl+c"]
  help: ["h", "?"]
  # ... other bindings

validation:
  check_port_conflicts: true
  check_binary_exists: true
```

## Priority Recommendations

### High Priority (Core Functionality)
1. ✅ Service definitions (binary, args, working dir) - **Already planned**
2. ⭐ Timing intervals - **Very useful for tuning**
3. ⭐ Process management (graceful shutdown, health checks) - **Production ready**
4. ⭐ Service behavior (dependencies, auto-start) - **Orchestration**

### Medium Priority (User Experience)
5. UI themes/colors - **Personalization**
6. Logging configuration - **Better log management**
7. Keyboard shortcuts - **Accessibility**

### Low Priority (Nice to Have)
8. Notifications - **Monitoring enhancement**
9. Viewport settings - **Fine-tuning**
10. Performance tuning - **Optimization**

## Implementation Considerations

1. **Backward Compatibility**: Default values should match current hardcoded behavior
2. **Validation**: Validate all timing values, colors, paths
3. **Documentation**: Document all configuration options
4. **Examples**: Provide example configs for common scenarios
5. **Migration**: Tool to generate config from current hardcoded values

