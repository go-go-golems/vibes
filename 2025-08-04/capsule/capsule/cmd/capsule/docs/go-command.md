---
Title: Go Command
Slug: go-command
Short: Build and run Go programs in resource-constrained containers
SectionType: Tutorial
Topics:
- commands
- go
- golang
- build
- containers
Commands:
- go
Flags:
- --cpu
- --mem
- --build-flags
- --ldflags
- --tag
- --keep-image
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
Order: 3
---

# Go Command

The `go` command builds Go programs and runs them inside Docker containers with specified resource constraints. This command automates the entire build-containerize-run workflow.

## Syntax

```bash
capsule go [flags] PACKAGE [ARGS...]
```

## Flags

### Resource Constraints
- `--cpu FLOAT`: CPU limit as number of cores (default: 1.0)
- `--mem STRING`: Memory limit with units (default: "1g")

### Build Options
- `--build-flags STRING`: Additional flags for go build
- `--ldflags STRING`: Linker flags for go build
- `--tag STRING`: Tag for the built Docker image
- `--keep-image BOOL`: Keep the built image after run (default: false)

## Examples

### Basic Usage

Build and run a Go program with default settings:
```bash
capsule go ./main.go
```

### With Arguments

Pass arguments to the Go program:
```bash
capsule go ./server.go -- -port 8080 -config config.yaml
```

### Resource Constraints

Run with specific CPU and memory limits:
```bash
capsule go ./cpu-intensive.go --cpu 2.0 --mem 512m -- -workers 4
```

### Build Flags

Use custom build flags:
```bash
capsule go ./main.go --build-flags "-tags production" --ldflags "-s -w"
```

### Keep Image

Keep the built image for reuse:
```bash
capsule go ./main.go --keep-image --tag my-app:v1.0
```

### Complex Example

Build a web server with optimization and resource limits:
```bash
capsule go ./cmd/server \
  --cpu 1.5 \
  --mem 1g \
  --build-flags "-tags production" \
  --ldflags "-s -w -X main.version=1.0.0" \
  --tag web-server:latest \
  --keep-image \
  -- -port 8080 -workers 4
```

## Build Process

### Compilation
1. **Source Analysis**: Analyzes the Go package or file
2. **Dependency Resolution**: Resolves Go module dependencies
3. **Cross Compilation**: Builds for Linux/amd64 (container target)
4. **Binary Creation**: Creates statically linked binary

### Containerization
1. **Dockerfile Generation**: Creates minimal Dockerfile
2. **Base Image**: Uses `gcr.io/distroless/static` for security
3. **Binary Copy**: Copies compiled binary to container
4. **Entrypoint Setup**: Configures binary as entrypoint

### Example Generated Dockerfile
```dockerfile
FROM gcr.io/distroless/static
COPY main /main
ENTRYPOINT ["/main"]
```

## Build Flags Reference

### Common Build Flags
- `-tags STRING`: Build tags for conditional compilation
- `-race`: Enable race detector
- `-ldflags STRING`: Linker flags
- `-gcflags STRING`: Compiler flags

### Common Linker Flags
- `-s`: Strip symbol table and debug info
- `-w`: Strip DWARF debug info
- `-X key=value`: Set string variable at link time

### Examples
```bash
# Production build with size optimization
--build-flags "-tags prod" --ldflags "-s -w"

# Debug build with race detection
--build-flags "-race" --ldflags "-X main.debug=true"

# Version injection
--ldflags "-X main.version=1.0.0 -X main.buildTime=$(date -u +%Y%m%d%H%M%S)"
```

## Image Management

### Automatic Cleanup
- By default, built images are removed after execution
- Use `--keep-image` to preserve images for reuse
- Images are tagged with timestamp if no tag specified

### Image Naming
- Default: `capsule-go-<timestamp>`
- Custom: Use `--tag` flag for specific naming
- Registry: Can push to registries if needed

### Image Optimization
- Uses distroless base image for minimal size
- Statically linked binaries (no dependencies)
- Typical image size: 5-20MB depending on program

## Resource Constraints

### CPU Limits
- Applied during container execution, not build
- Build process uses host resources
- Runtime CPU throttling enforced by Docker

### Memory Limits
- Applied to running container only
- Build process memory not constrained
- OOM kill if program exceeds limit

## Error Handling

### Build Errors
- Go compilation errors displayed directly
- Module resolution issues shown
- Build context problems reported

### Runtime Errors
- Container execution errors reported
- Resource constraint violations logged
- Exit codes preserved from Go program

### Common Issues
1. **Module Issues**: Ensure go.mod is present
2. **Build Context**: Run from correct directory
3. **Cross Compilation**: Some CGO packages may fail
4. **Resource Limits**: Ensure adequate memory for program

## Performance Considerations

### Build Performance
- Builds are performed on host (not containerized)
- Go module cache is used for dependencies
- Incremental builds benefit from Go build cache

### Runtime Performance
- Distroless images have minimal overhead
- Static linking eliminates library dependencies
- Resource constraints may impact performance

## Integration Examples

### CI/CD Pipeline
```bash
# Test with resource constraints
capsule go ./... --cpu 0.5 --mem 256m -- -test.v

# Build production image
capsule go ./cmd/server \
  --build-flags "-tags prod" \
  --ldflags "-s -w" \
  --tag myapp:${VERSION} \
  --keep-image
```

### Development Workflow
```bash
# Quick test run
capsule go . -- -config dev.yaml

# Performance testing
capsule go ./benchmark.go --cpu 1.0 --mem 512m -- -duration 30s

# Resource validation
capsule go ./server.go --cpu 0.5 --mem 256m -- -load-test
```

## See Also

- `capsule help run-command`: General container execution
- `capsule help resources`: Understanding resource constraints
- `capsule help examples`: More usage examples
- `capsule help troubleshooting`: Common issues and solutions

