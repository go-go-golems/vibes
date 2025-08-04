---
Title: Troubleshooting
Slug: troubleshooting
Short: Common issues and solutions for Capsule
SectionType: GeneralTopic
Topics:
- troubleshooting
- errors
- debugging
- solutions
- common-issues
Commands:
- run
- go
- ls
- stats
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
Order: 6
---

# Troubleshooting

This guide helps you diagnose and resolve common issues when using Capsule.

## Common Error Messages

### Exit Code 137 (OOM Killed)
**Symptom**: Container exits with code 137
**Cause**: Memory limit exceeded
**Solution**: Increase memory limit or optimize memory usage

```bash
# Problem: Memory limit too low
capsule run --mem 128m ubuntu:latest stress --vm 1 --vm-bytes 200M
# Error: exit status 137

# Solution: Increase memory limit
capsule run --mem 256m ubuntu:latest stress --vm 1 --vm-bytes 200M
```

### Exit Code 125 (Docker Error)
**Symptom**: Docker daemon error
**Cause**: Docker configuration or permission issues
**Solution**: Check Docker installation and permissions

```bash
# Check Docker status
docker --version
docker info

# Test Docker access
docker run hello-world

# If permission denied, add user to docker group
sudo usermod -aG docker $USER
# Then logout and login again
```

### "Cannot find image" Error
**Symptom**: Unable to find image locally
**Cause**: Image not available locally or incorrect name
**Solution**: Pull image or verify name

```bash
# Pull image explicitly
docker pull ubuntu:latest

# Verify image name
docker images | grep ubuntu

# Use full registry path if needed
capsule run docker.io/library/ubuntu:latest echo "hello"
```

## Resource Constraint Issues

### CPU Performance Problems

#### Symptom: Application runs slower than expected
**Diagnosis**: CPU limit may be too restrictive
```bash
# Check current CPU usage
capsule stats container-name

# Compare with unlimited run
time capsule run ubuntu:latest stress --cpu 4 --timeout 10s
time capsule run --cpu 0.5 ubuntu:latest stress --cpu 4 --timeout 10s
```

**Solutions**:
1. Increase CPU allocation
2. Optimize application for lower CPU usage
3. Use CPU profiling to identify bottlenecks

#### Symptom: Multi-threaded application not utilizing all threads
**Cause**: CPU limit lower than thread count
```bash
# Problem: 4 threads with 0.5 CPU limit
capsule go ./multi-threaded.go --cpu 0.5 -- -threads 4

# Solution: Match CPU limit to thread needs
capsule go ./multi-threaded.go --cpu 2.0 -- -threads 4
```

### Memory Issues

#### Symptom: Frequent OOM kills
**Diagnosis**: Memory usage exceeds limits
```bash
# Monitor memory usage patterns
capsule stats container-name

# Check memory requirements without limits
docker run --rm ubuntu:latest stress --vm 1 --vm-bytes 500M --timeout 10s
```

**Solutions**:
1. Increase memory limit
2. Optimize memory usage in application
3. Use memory profiling tools

#### Symptom: Application fails to start
**Cause**: Insufficient memory for initialization
```bash
# Problem: JVM needs more memory to start
capsule run --mem 64m openjdk:11 java -version
# Error: Could not create the Java Virtual Machine

# Solution: Increase memory for JVM overhead
capsule run --mem 256m openjdk:11 java -version
```

## Go Program Issues

### Build Failures

#### Symptom: "go: cannot find main module"
**Cause**: Not in a Go module directory
**Solution**: Initialize Go module or specify correct path

```bash
# Initialize Go module
go mod init my-program

# Or specify full path to Go file
capsule go /full/path/to/program.go
```

#### Symptom: "package not found" errors
**Cause**: Missing dependencies
**Solution**: Ensure dependencies are available

```bash
# Download dependencies
go mod tidy

# Verify module status
go mod verify

# Run from module root
capsule go ./cmd/main.go
```

#### Symptom: CGO compilation errors
**Cause**: Cross-compilation issues with CGO
**Solution**: Disable CGO or use compatible packages

```bash
# Disable CGO for static compilation
CGO_ENABLED=0 capsule go ./main.go

# Or use pure Go alternatives
```

### Runtime Issues

#### Symptom: Go program exits immediately
**Cause**: Program completed successfully or panic
**Diagnosis**: Check program logic and error handling

```bash
# Add debug output
capsule go - << 'EOF'
package main
import (
    "fmt"
    "os"
)
func main() {
    fmt.Println("Program starting...")
    fmt.Printf("Args: %v\n", os.Args)
    // Your program logic here
    fmt.Println("Program completed")
}
EOF
```

## Container Management Issues

### Container Not Found

#### Symptom: "No such container" errors
**Cause**: Container name incorrect or container removed
**Solution**: Verify container exists

```bash
# List all capsule containers
capsule ls -a

# List all Docker containers
docker ps -a

# Use container ID instead of name
capsule stats 7b86a1c5ec28
```

### Permission Issues

#### Symptom: "Permission denied" in container
**Cause**: File permissions or user context
**Solution**: Adjust permissions or user

```bash
# Check file permissions
ls -la ./data

# Make files readable
chmod 644 ./data/*

# Run as specific user (if needed)
docker run --user $(id -u):$(id -g) ...
```

## Network Issues

### Connection Problems

#### Symptom: Cannot connect to external services
**Cause**: Network configuration or firewall
**Solution**: Test network connectivity

```bash
# Test basic connectivity
capsule run ubuntu:latest ping -c 3 google.com

# Test DNS resolution
capsule run ubuntu:latest nslookup google.com

# Check if host networking helps
capsule run --network host ubuntu:latest curl http://localhost:8080
```

### Port Binding Issues

#### Symptom: "Port already in use" errors
**Cause**: Port conflict with host or other containers
**Solution**: Use different ports or stop conflicting services

```bash
# Check what's using the port
netstat -tlnp | grep :8080
lsof -i :8080

# Use different port
capsule run -p 8081:8080 nginx:alpine

# Or stop conflicting service
sudo systemctl stop apache2
```

## Performance Debugging

### Slow Performance

#### General Performance Issues
1. **Check Resource Limits**: Ensure adequate CPU/memory
2. **Monitor Usage**: Use `capsule stats` to see actual usage
3. **Compare Baseline**: Run without constraints to establish baseline
4. **Profile Application**: Use appropriate profiling tools

```bash
# Performance comparison
echo "Without constraints:"
time docker run --rm ubuntu:latest stress --cpu 2 --timeout 5s

echo "With constraints:"
time capsule run --cpu 1.0 ubuntu:latest stress --cpu 2 --timeout 5s
```

#### Go Program Performance
```bash
# Enable Go profiling
capsule go ./main.go -- -cpuprofile=cpu.prof -memprofile=mem.prof

# Analyze profiles (after copying from container)
go tool pprof cpu.prof
go tool pprof mem.prof
```

### Resource Usage Analysis

#### High CPU Usage
```bash
# Monitor CPU usage over time
capsule stats container-name | tee cpu-usage.log

# Check for CPU-intensive processes
capsule run ubuntu:latest top -b -n 1
```

#### High Memory Usage
```bash
# Monitor memory patterns
watch -n 1 'capsule stats container-name'

# Check memory breakdown
capsule run ubuntu:latest cat /proc/meminfo
```

## Debugging Techniques

### Container Inspection

#### Examine Container State
```bash
# Get detailed container information
docker inspect container-name

# Check container logs
docker logs container-name

# Execute commands in running container
docker exec -it container-name /bin/bash
```

#### File System Inspection
```bash
# Mount host directory for inspection
capsule run --volume ./debug:/debug ubuntu:latest \
  /bin/bash -c "cp /app/logfile /debug/"

# Examine container file system
docker run --rm -it --entrypoint /bin/bash image-name
```

### Application Debugging

#### Debug Go Programs
```bash
# Add debug output
capsule go - << 'EOF'
package main
import (
    "fmt"
    "runtime"
    "time"
)
func main() {
    fmt.Printf("Go version: %s\n", runtime.Version())
    fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
    fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
    
    var m runtime.MemStats
    runtime.ReadMemStats(&m)
    fmt.Printf("Allocated memory: %d KB\n", m.Alloc/1024)
    
    time.Sleep(1 * time.Second)
    fmt.Println("Debug complete")
}
EOF
```

#### Environment Debugging
```bash
# Check environment variables
capsule run ubuntu:latest env

# Check system limits
capsule run ubuntu:latest ulimit -a

# Check cgroup settings
capsule run ubuntu:latest cat /sys/fs/cgroup/memory/memory.limit_in_bytes
```

## Getting Help

### Log Collection
When reporting issues, collect relevant information:

```bash
# System information
uname -a
docker --version
docker info

# Capsule version and help
capsule --version
capsule help

# Container information
capsule ls -a
docker ps -a

# Resource usage
capsule stats

# Docker logs
docker logs container-name
```

### Verbose Output
Enable verbose output for debugging:

```bash
# Docker debug mode
export DOCKER_BUILDKIT=0
docker run --rm -it ubuntu:latest /bin/bash

# Capsule debug (if available)
capsule --verbose run ubuntu:latest echo "debug"
```

### Community Resources
- Check GitHub issues for similar problems
- Review Docker documentation for container-specific issues
- Consult Go documentation for Go program issues
- Use Docker forums for Docker-related problems

## Prevention Tips

### Best Practices
1. **Start with generous limits** and reduce gradually
2. **Monitor resource usage** regularly
3. **Test with realistic workloads** before production
4. **Keep Docker and dependencies updated**
5. **Use specific image tags** instead of `latest`
6. **Implement proper error handling** in applications
7. **Document resource requirements** for your applications

### Regular Maintenance
```bash
# Clean up unused containers
docker container prune

# Clean up unused images
docker image prune

# Clean up unused volumes
docker volume prune

# Full cleanup (careful!)
docker system prune -a
```

## See Also

- `capsule help resources`: Understanding resource constraints
- `capsule help examples`: Working examples
- `capsule help run-command`: Run command reference
- `capsule help go-command`: Go command reference

