---
Title: Configuration YAML Reference
Slug: config-yaml-reference
Short: Complete reference for the mento-tui configuration YAML file format
Topics:
  - config
  - yaml
  - configuration
Commands:
  - mento-tui
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Configuration YAML Reference

The mento-tui application uses a YAML configuration file to define the services it manages. This file specifies service binaries, ports, environment variables, and global settings that control how mento-tui monitors and manages your services.

## Configuration File Structure

The configuration file consists of two main sections: `services` and `global`. The `services` section defines each service that mento-tui will manage, while the `global` section contains application-wide settings that apply to all services.

```yaml
services:
  - name: "Service Name"
    ports: [8080]
    binary_path: "./path/to/binary"
    env_vars:
      KEY: "value"
    log_buffer_size: 1000

global:
  working_directory: "."
  log_buffer_size: 10000
```

## Services Section

Each service entry defines a single process that mento-tui will start, monitor, and manage. Services run independently and can be started, stopped, or restarted individually from the TUI dashboard.

### Service Fields

**name** (required, string)
: A human-readable name for the service displayed in the TUI dashboard. This name appears in service lists, log viewers, and status displays.

**ports** (optional, list of integers)
: A list of TCP ports that the service listens on. Multiple ports can be specified for services that expose multiple endpoints. The TUI displays all configured ports for each service.

**port** (optional, integer)
: A single port number (legacy field, use `ports` for multiple ports). If both `port` and `ports` are specified, `ports` takes precedence.

**binary_path** (required, string)
: The file system path to the executable binary that mento-tui will launch for this service. Can be an absolute path or relative to the `global.working_directory`.

**working_directory** (optional, string)
: The working directory for this service's process. If not specified, uses `global.working_directory`. This directory becomes the current working directory when the service binary is executed.

**args** (optional, string or list)
: Command-line arguments to pass to the service binary. Can be specified as a single string (space-separated) or as a YAML list. The TUI passes these arguments directly to the binary when starting the service.

**env_vars** (optional, map)
: Environment variables to set for the service process. Specified as key-value pairs where keys are environment variable names and values are strings. The TUI sets these variables in the process environment before launching the binary.

**log_buffer_size** (optional, integer)
: Maximum number of log lines to buffer for this service. When the buffer is full, older lines are discarded. Defaults to `global.log_buffer_size` if not specified.

### Service Configuration Example

```yaml
services:
  - name: "Identity Server"
    ports: [8083]
    binary_path: "./mock-binaries/identity-server"
    env_vars:
      IDENTITY_SERVICE_PORT: "8083"
      IDENTITY_SERVICE_DB_DSN: "postgres://postgres:password@localhost:5432/identity_db"
      IDENTITY_SERVICE_LOG_LEVEL: "debug"
    log_buffer_size: 1000

  - name: "API Gateway"
    ports: [8080, 8443]
    binary_path: "/usr/local/bin/api-gateway"
    working_directory: "/var/lib/api-gateway"
    args:
      - "--config"
      - "/etc/api-gateway/config.yaml"
    env_vars:
      GATEWAY_PORT: "8080"
      GATEWAY_TLS_PORT: "8443"
      GATEWAY_LOG_LEVEL: "info"
```

## Global Section

The `global` section contains settings that apply to the entire mento-tui application and serve as defaults for individual services.

### Global Fields

**working_directory** (optional, string, default: ".")
: The default working directory for all services. Individual services can override this with their own `working_directory` field. Relative paths in `binary_path` are resolved relative to this directory.

**log_buffer_size** (optional, integer, default: 10000)
: The default maximum number of log lines to buffer per service. Individual services can override this with their own `log_buffer_size` field. When a service's log buffer is full, older lines are discarded to make room for new ones.

### Global Configuration Example

```yaml
global:
  working_directory: "/opt/mento-services"
  log_buffer_size: 5000
```

## Complete Configuration Example

Here's a complete example configuration file that demonstrates all available options:

```yaml
global:
  working_directory: "."
  log_buffer_size: 10000

services:
  - name: "Identity Server"
    ports: [8083]
    binary_path: "./mock-binaries/identity-server"
    env_vars:
      IDENTITY_SERVICE_PORT: "8083"
      IDENTITY_SERVICE_DB_DSN: "postgres://postgres:password@localhost:5432/identity_db"
      IDENTITY_SERVICE_LOG_LEVEL: "debug"
    log_buffer_size: 1000

  - name: "Frontend (Vite)"
    ports: [5173]
    binary_path: "./mock-binaries/frontend"
    env_vars:
      VITE_PORT: "5173"
      VITE_API_URL: "http://localhost:8082"
    log_buffer_size: 1000

  - name: "Mento Worker"
    ports: [8082]
    binary_path: "./mock-binaries/worker"
    working_directory: "/tmp/worker"
    args: "--verbose --workers 4"
    env_vars:
      MENTO_SERVICE_PORT: "8082"
      MENTO_SERVICE_LOG_LEVEL: "debug"
    log_buffer_size: 2000
```

## Configuration File Location

By default, mento-tui looks for a configuration file named `mento-tui.yaml` in the current working directory. You can specify a different configuration file using the `--config` flag:

```bash
mento-tui --config /path/to/my-config.yaml
```

## Environment Variable Ordering

The `env_vars` field preserves the order of environment variables as specified in the YAML file. This ordering is maintained when displaying configuration in the TUI and when setting environment variables for service processes. Use an ordered map structure in your YAML to control the display order.

