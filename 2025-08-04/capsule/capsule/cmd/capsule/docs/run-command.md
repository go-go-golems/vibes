---
Title: Run Command
Slug: run-command
Short: Execute commands in resource-constrained containers
SectionType: Tutorial
Topics:
- commands
- run
- containers
- resources
Commands:
- run
Flags:
- --cpu
- --mem
- --name
- --rm
- --detach
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
Order: 2
---

# Run Command

The `run` command executes a command inside a Docker container with specified resource constraints. This is the core command for running applications in controlled environments.

## Syntax

```bash
capsule run [flags] IMAGE [COMMAND...]
```

## Flags

### Resource Constraints
- `--cpu FLOAT`: CPU limit as number of cores (default: 1.0)
- `--mem STRING`: Memory limit with units (default: "1g")
- `--pids INT`: Process limit (default: 1024)
- `--swap STRING`: Swap limit (default: "0")

### Container Options
- `--name STRING`: Container name for identification
- `--rm BOOL`: Remove container on exit (default: true)
- `--detach BOOL`: Run container in background (default: false)
- `--volume STRINGS`: Bind mount volumes (host:container)
- `--env STRINGS`: Environment variables (KEY=value)

## Examples

### Basic Usage

Run a simple command with default resource limits:
```bash
capsule run ubuntu:latest echo "Hello World"
```

### Resource Constraints

Limit CPU to 0.5 cores and memory to 256MB:
```bash
capsule run --cpu 0.5 --mem 256m alpine:latest stress --cpu 2 --timeout 10s
```

### Named Container

Create a named container for easier management:
```bash
capsule run --name my-test --cpu 1.0 --mem 512m ubuntu:latest sleep 30
```

### Detached Mode

Run a long-running process in the background:
```bash
capsule run --detach --name web-server --cpu 2.0 --mem 1g nginx:alpine
```

### Volume Mounting

Mount host directories into the container:
```bash
capsule run --volume ./data:/app/data --cpu 1.0 --mem 512m ubuntu:latest ls /app/data
```

### Environment Variables

Pass environment variables to the container:
```bash
capsule run --env "DEBUG=true" --env "PORT=8080" --cpu 1.0 --mem 512m node:alpine
```

## Resource Constraint Details

### CPU Limits
- Specified as floating-point number of cores
- Examples: 0.5 (half core), 1.0 (one core), 2.5 (two and half cores)
- Enforced using Docker's `--cpus` flag with cgroup controls

### Memory Limits
- Specified with units: b, k, m, g (bytes, kilobytes, megabytes, gigabytes)
- Examples: "256m", "1g", "2048m"
- Containers are killed (OOM) if they exceed the limit

### Process Limits
- Limits the number of processes/threads in the container
- Useful for preventing fork bombs or excessive process creation
- Default: 1024 processes

## Container Management

### Automatic Labeling
All containers created by capsule are automatically labeled:
- `capsule.managed=true`: Identifies capsule-managed containers
- `capsule.cpu=X.XX`: Records CPU limit
- `capsule.memory=XXXm`: Records memory limit
- `capsule.created=TIMESTAMP`: Creation timestamp

### Cleanup Behavior
- By default, containers are removed when they exit (`--rm=true`)
- Use `--rm=false` to keep containers for inspection
- Detached containers must be manually stopped and removed

## Error Handling

### Common Exit Codes
- `0`: Successful execution
- `125`: Docker daemon error
- `126`: Container command not executable
- `127`: Container command not found
- `137`: Container killed due to memory limit (OOM)

### Troubleshooting
- Use `capsule ls` to see running containers
- Use `capsule stats` to monitor resource usage
- Check Docker logs: `docker logs <container-name>`

## Integration with Other Commands

The run command works seamlessly with other capsule commands:
- Monitor with: `capsule stats <container-name>`
- List containers: `capsule ls`
- Stop containers: `capsule stop <container-name>`

## See Also

- `capsule help go-command`: Build and run Go programs
- `capsule help resources`: Understanding resource constraints
- `capsule help examples`: More usage examples

