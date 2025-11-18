---
Title: Getting Started with mento-tui
Slug: getting-started
Short: Step-by-step guide to set up and start using mento-tui
Topics:
  - tutorial
  - getting-started
  - setup
Commands:
  - mento-tui
IsTopLevel: true
ShowPerDefault: true
SectionType: Tutorial
---

# Getting Started with mento-tui

This tutorial walks you through setting up mento-tui, creating your first configuration file, and managing services through the TUI dashboard. By the end, you'll be able to start, stop, and monitor multiple services from a single interface.

## Prerequisites

Before you begin, ensure you have:

- Go 1.18 or higher installed
- Terminal with ANSI color support
- Service binaries you want to manage (or use the included mock binaries for testing)

## Step 1: Install mento-tui

Build mento-tui from source:

```bash
# Clone or navigate to the project directory
cd mento-tui

# Download dependencies
go mod tidy

# Build the application
go build -o mento-tui cmd/main.go
```

Verify the installation:

```bash
./mento-tui --help
```

You should see the command help output with available options.

## Step 2: Create a Configuration File

Create your first configuration file named `mento-tui.yaml` in the current directory. Start with a simple configuration that defines one service:

```yaml
services:
  - name: "My Service"
    ports: [8080]
    binary_path: "./my-service-binary"
    env_vars:
      PORT: "8080"
      LOG_LEVEL: "info"
    log_buffer_size: 1000

global:
  working_directory: "."
  log_buffer_size: 10000
```

Replace `./my-service-binary` with the actual path to your service executable. The `name` field appears in the TUI dashboard, and `ports` lists the TCP ports your service listens on.

## Step 3: Launch the TUI

Start mento-tui with your configuration:

```bash
./mento-tui
```

The TUI dashboard appears, showing your configured service in a list. The service status shows as "Stopped" initially.

## Step 4: Start Your First Service

Use the arrow keys (**↑** or **↓**) to select your service in the dashboard. Press **Enter** to start it. Watch the status change from "Stopped" to "Starting" and then "Running".

Once running, you'll see:
- Process ID (PID)
- CPU usage percentage
- Memory consumption
- Uptime counter

## Step 5: View Service Logs

Press **t** or **l** to open the log viewer. You'll see real-time log output from your service. The log viewer shows:
- Timestamp for each log entry
- Service name
- Log message content

Press **Tab** to switch between service tabs (if you have multiple services), or press **Esc** to return to the dashboard.

## Step 6: View Service Configuration

Press **c** or **e** from the dashboard to open the configuration viewer. This displays all environment variables configured for the selected service. Scroll through the configuration using arrow keys to verify settings.

## Step 7: Stop and Restart Services

Stop your service by selecting it and pressing **s**. The status changes to "Stopping" and then "Stopped". Restart it by pressing **r**, which stops and immediately starts the service again.

## Step 8: Add More Services

Edit your `mento-tui.yaml` file to add additional services:

```yaml
services:
  - name: "Identity Server"
    ports: [8083]
    binary_path: "./identity-server"
    env_vars:
      PORT: "8083"
    log_buffer_size: 1000

  - name: "API Server"
    ports: [8080]
    binary_path: "./api-server"
    env_vars:
      PORT: "8080"
    log_buffer_size: 1000

global:
  working_directory: "."
  log_buffer_size: 10000
```

Restart mento-tui to load the new configuration. The dashboard now shows all configured services. Use **a** to start all services at once, or **x** to stop all services.

## Step 9: Explore Advanced Features

**Multiple Ports**: Configure services that listen on multiple ports:

```yaml
services:
  - name: "Gateway"
    ports: [8080, 8443]
    binary_path: "./gateway"
```

**Custom Working Directory**: Set a specific working directory for a service:

```yaml
services:
  - name: "Service"
    binary_path: "./service"
    working_directory: "/var/lib/service"
```

**Command-Line Arguments**: Pass arguments to service binaries:

```yaml
services:
  - name: "Service"
    binary_path: "./service"
    args: "--verbose --workers 4"
```

## Step 10: Get Help

Access help documentation anytime:

```bash
# From command line
./mento-tui help

# From within TUI
Press h or ? to show help screen
```

The help screen displays all available keyboard shortcuts and features.

## Next Steps

Now that you're familiar with the basics:

1. **Read the Configuration Reference**: Learn about all available configuration options (`mento-tui help config-yaml-reference`)

## Troubleshooting

**Service won't start**: Check that the `binary_path` is correct and the file is executable. View logs in the log viewer to see error messages.

**Configuration errors**: Verify your YAML syntax is correct. Common issues include incorrect indentation or missing required fields.

**Port conflicts**: Ensure ports aren't already in use by other processes. Use `netstat` or `lsof` to check port availability.

**Help needed**: Press **h** in the TUI or run `./mento-tui help` from the command line for assistance.

