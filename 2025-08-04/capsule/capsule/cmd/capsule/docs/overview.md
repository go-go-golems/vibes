---
Title: Capsule Overview
Slug: overview
Short: Introduction to the Capsule CLI tool for resource-constrained containers
SectionType: GeneralTopic
Topics:
- introduction
- overview
- getting-started
Commands:
- capsule
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
Order: 1
---

# Capsule Overview

Capsule is a CLI tool that wraps Docker to run binaries and Go programs with repeatable CPU, memory, and network constraints. It provides a simple interface for creating resource-limited execution environments.

## Key Features

- **Resource Constraints**: Precise CPU and memory limits using Docker's cgroup controls
- **Go Program Support**: Automatic building and containerization of Go programs
- **Container Management**: List, monitor, and manage capsule-controlled containers
- **Real-time Monitoring**: Live resource usage statistics
- **Automatic Cleanup**: Configurable container removal policies

## Quick Start

```bash
# Run a simple command with resource limits
capsule run --cpu 1.0 --mem 512m ubuntu:latest echo "Hello World"

# Build and run a Go program with constraints
capsule go ./my-program.go --cpu 0.5 --mem 256m -- -arg1 value1

# List running capsules
capsule ls

# Monitor resource usage
capsule stats
```

## Core Concepts

### Resource Constraints
Capsule uses Docker's built-in resource controls to enforce limits:
- **CPU**: Specified as number of cores (e.g., 0.5, 1.0, 2.0)
- **Memory**: Specified with units (e.g., 256m, 1g, 2g)
- **Processes**: Optional process count limits

### Container Lifecycle
- Containers are automatically labeled for management
- Default behavior removes containers on exit
- Detached mode available for long-running processes

### Go Program Integration
- Automatic compilation of Go source files
- Minimal container images using distroless base
- Resource constraints applied during execution

## Common Use Cases

1. **Development Testing**: Test applications under resource constraints
2. **Performance Validation**: Verify behavior with limited resources
3. **CI/CD Integration**: Consistent resource-limited testing environments
4. **Resource Planning**: Understand application resource requirements

## Next Steps

- Learn about specific commands: `capsule help commands`
- See examples: `capsule help examples`
- Understand resource constraints: `capsule help resources`
- Explore Go program support: `capsule help go-programs`

