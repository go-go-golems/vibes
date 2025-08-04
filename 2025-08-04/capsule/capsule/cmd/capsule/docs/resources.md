---
Title: Resource Constraints
Slug: resources
Short: Understanding and using CPU, memory, and process limits
SectionType: GeneralTopic
Topics:
- resources
- cpu
- memory
- limits
- constraints
- performance
Commands:
- run
- go
Flags:
- --cpu
- --mem
- --pids
- --swap
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
Order: 4
---

# Resource Constraints

Capsule provides precise control over system resources using Docker's cgroup-based resource management. This ensures predictable performance and prevents resource exhaustion.

## CPU Constraints

### How CPU Limits Work
CPU limits are enforced using Docker's `--cpus` flag, which leverages Linux cgroups to control CPU time allocation.

### Specification Format
- **Float Values**: Number of CPU cores (e.g., 0.5, 1.0, 2.5)
- **Precision**: Up to 2 decimal places
- **Range**: 0.01 to available CPU cores

### Examples
```bash
# Half a CPU core
capsule run --cpu 0.5 ubuntu:latest stress --cpu 4

# One and a half cores
capsule run --cpu 1.5 ubuntu:latest stress --cpu 4

# Quarter core for lightweight tasks
capsule run --cpu 0.25 alpine:latest ping -c 10 google.com
```

### CPU Behavior
- **Throttling**: Processes are throttled when exceeding limit
- **Scheduling**: Fair scheduling among processes within limit
- **Burst Capability**: Can use more CPU briefly if available
- **Multi-threading**: Multiple threads share the CPU allocation

### Performance Impact
```bash
# Demonstration of CPU throttling
capsule go ./cpu-burner.go --cpu 0.5 -- -threads 4
# 4 threads will compete for 0.5 CPU cores
```

## Memory Constraints

### How Memory Limits Work
Memory limits use Docker's `--memory` flag with cgroup memory controller to enforce hard limits.

### Specification Format
- **Units**: b (bytes), k (kilobytes), m (megabytes), g (gigabytes)
- **Examples**: "256m", "1g", "2048m", "1.5g"
- **Case Insensitive**: "1G" same as "1g"

### Examples
```bash
# 256 megabytes
capsule run --mem 256m ubuntu:latest stress --vm 1 --vm-bytes 200M

# 1 gigabyte
capsule run --mem 1g ubuntu:latest stress --vm 1 --vm-bytes 800M

# 1.5 gigabytes
capsule run --mem 1.5g ubuntu:latest stress --vm 1 --vm-bytes 1G
```

### Memory Behavior
- **Hard Limit**: Process killed (OOM) when limit exceeded
- **No Swap**: By default, swap is disabled
- **Immediate Enforcement**: No grace period for memory violations
- **Exit Code 137**: Indicates OOM kill

### Memory Types
1. **RSS**: Resident Set Size (physical memory)
2. **Cache**: File system cache (reclaimable)
3. **Buffer**: Kernel buffers (reclaimable)
4. **Total**: RSS + Cache + Buffer

## Process Limits

### Process ID Limits
Control the maximum number of processes and threads in a container.

### Specification
```bash
# Limit to 100 processes
capsule run --pids 100 ubuntu:latest stress --fork 50

# Default limit (1024)
capsule run ubuntu:latest stress --fork 10
```

### Use Cases
- **Fork Bomb Prevention**: Prevent runaway process creation
- **Resource Planning**: Understand process requirements
- **Testing**: Validate behavior under process constraints

## Swap Constraints

### Swap Memory Control
Control swap usage in addition to physical memory.

### Specification
```bash
# No swap (default)
capsule run --swap 0 --mem 256m ubuntu:latest stress --vm 1 --vm-bytes 300M

# Allow 512MB swap
capsule run --swap 512m --mem 256m ubuntu:latest stress --vm 1 --vm-bytes 600M
```

### Swap Behavior
- **Default**: Swap disabled (`--swap 0`)
- **Total Limit**: Memory + Swap combined limit
- **Performance**: Swap usage impacts performance significantly

## Constraint Combinations

### Balanced Constraints
```bash
# Web server with balanced resources
capsule run \
  --cpu 1.0 \
  --mem 512m \
  --pids 256 \
  nginx:alpine
```

### CPU-Intensive Workload
```bash
# High CPU, moderate memory
capsule go ./compute-heavy.go \
  --cpu 4.0 \
  --mem 1g \
  -- -workers 8
```

### Memory-Intensive Workload
```bash
# Moderate CPU, high memory
capsule go ./data-processing.go \
  --cpu 1.0 \
  --mem 8g \
  -- -buffer-size 4GB
```

## Monitoring Resource Usage

### Real-time Monitoring
```bash
# Monitor all capsule containers
capsule stats

# Monitor specific container
capsule stats my-container

# Monitor with Docker directly
docker stats $(capsule ls -q)
```

### Resource Metrics
- **CPU %**: Percentage of allocated CPU used
- **Memory Usage/Limit**: Current usage vs. limit
- **Memory %**: Percentage of limit used
- **Network I/O**: Network traffic
- **Block I/O**: Disk I/O operations

## Best Practices

### CPU Allocation
1. **Start Conservative**: Begin with lower limits and increase as needed
2. **Consider Threads**: Multi-threaded apps need adequate CPU allocation
3. **Monitor Performance**: Use stats to validate CPU usage patterns
4. **Burst Workloads**: Allow headroom for temporary spikes

### Memory Allocation
1. **Know Your Application**: Understand memory requirements
2. **Add Buffer**: Include 20-30% buffer for safety
3. **Monitor Closely**: Memory violations are fatal (OOM kill)
4. **Test Thoroughly**: Validate under various load conditions

### Process Limits
1. **Understand Patterns**: Know how many processes your app creates
2. **Include Overhead**: Account for system processes
3. **Test Edge Cases**: Validate behavior at limits
4. **Monitor Trends**: Watch for unexpected process growth

## Troubleshooting

### CPU Issues
- **Symptoms**: Slow performance, high CPU wait times
- **Diagnosis**: Check if CPU limit is too restrictive
- **Solution**: Increase CPU allocation or optimize code

### Memory Issues
- **Symptoms**: Container killed, exit code 137
- **Diagnosis**: Memory usage exceeds limit
- **Solution**: Increase memory limit or optimize memory usage

### Process Issues
- **Symptoms**: "Cannot fork" errors, process creation failures
- **Diagnosis**: Process limit reached
- **Solution**: Increase process limit or reduce process creation

## Advanced Topics

### Cgroup Integration
Capsule leverages Docker's cgroup integration:
- **CPU**: Uses CFS (Completely Fair Scheduler)
- **Memory**: Uses memory cgroup controller
- **PIDs**: Uses pids cgroup controller

### Container Runtime Impact
- **Overhead**: Minimal overhead from Docker runtime
- **Isolation**: Strong isolation between containers
- **Host Impact**: Constraints protect host system

### Performance Tuning
- **CPU Affinity**: Docker can pin to specific CPU cores
- **NUMA Awareness**: Consider NUMA topology for large systems
- **Kernel Parameters**: Some limits configurable via kernel parameters

## See Also

- `capsule help run-command`: Using resource constraints with run
- `capsule help go-command`: Resource constraints for Go programs
- `capsule help monitoring`: Monitoring resource usage
- `capsule help troubleshooting`: Solving resource-related issues

