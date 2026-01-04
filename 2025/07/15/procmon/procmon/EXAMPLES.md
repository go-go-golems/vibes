# Process Monitor Examples

This document provides practical examples of using the Process Monitor tool for various system monitoring and debugging scenarios.

## Basic Usage Examples

### 1. Quick System Health Check

```bash
# Get an overview of system health
./procmon --system

# Output:
# System Information
# ==================
# Timestamp: 2024-01-15 14:30:45
# 
# Memory:
#   Total: 3.8 GB
#   Used: 32.8%
#   Pressure: none
#   Thrashing: Not detected
# 
# Thermal:
#   State: normal
# 
# Power:
#   AC Power: Disconnected
#   Power Saving: false
#   CPU Governor: 
#   CPU Frequency: 0 MHz (avg)
```

### 2. Find Top CPU Consumers

```bash
# List processes sorted by CPU usage
./procmon --list

# Output:
# PID      Name                 CPU%     Memory(MB) Threads  State    Command
# --------------------------------------------------------------------------------
# 1234     firefox              15.2     1024       45       Running  /usr/bin/firefox
# 5678     chrome               8.7      512        23       Running  /usr/bin/google-chrome
# 9012     python               3.4      256        8        Running  python script.py
```

### 3. Monitor Specific Process

```bash
# Monitor a specific process by PID
./procmon --pid 1234

# Monitor with custom update interval
./procmon --pid 1234 --update-interval 500ms
```

## Advanced Monitoring Scenarios

### 4. Debugging High CPU Usage

When you notice high system load, use procmon to identify the culprit:

```bash
# Step 1: Get overview of top processes
./procmon --list --min-cpu 5.0

# Step 2: Monitor specific high-CPU process interactively
./procmon --pid <high-cpu-pid>

# Step 3: In the interactive mode:
# - Press '2' to switch to Threads tab
# - Look for threads with high CPU usage
# - Press 's' to sort by different metrics
```

**Example Output in Threads Tab:**
```
Threads for Process: firefox (PID 1234)
TID      Name                 CPU%     State    Priority  Role
----------------------------------------------------------------
1234     MainThread           2.1      Running  0         Main Process
1235     Compositor           8.4      Running  0         GPU Rendering
1236     Content              12.3     Running  0         Web Content
1237     JS Helper            3.2      Running  0         JavaScript
```

### 5. Memory Leak Detection

Monitor memory usage over time to detect potential leaks:

```bash
# Start monitoring with SQLite logging (when implemented)
./procmon --log-to-sqlite memory_analysis.db

# Or monitor interactively and watch memory trends
./procmon --pid <suspect-process>
```

In the interactive mode:
- Switch to Memory tab (press '3')
- Watch for steadily increasing memory usage
- Look for processes with high virtual memory but low resident memory

### 6. System Performance Analysis

For comprehensive system analysis:

```bash
# Monitor all processes with detailed filtering
./procmon --min-cpu 1.0 --min-memory 50 --show-kernel

# Interactive mode for real-time analysis
./procmon
# Then navigate through tabs:
# Tab 1: Processes - Overall process view
# Tab 3: Memory - System memory pressure
# Tab 4: Thermal - Temperature monitoring
# Tab 5: Power - Battery and CPU frequency
# Tab 6: System - Health overview
```

## Troubleshooting Scenarios

### 7. Investigating System Slowdown

When your system feels sluggish:

```bash
# Step 1: Check for memory pressure
./procmon --system | grep -A 10 "Memory:"

# Step 2: Look for thrashing indicators
./procmon --system | grep "Thrashing"

# Step 3: Check thermal throttling
./procmon --system | grep -A 5 "Thermal:"

# Step 4: Interactive investigation
./procmon
# Navigate to Memory tab to see detailed pressure analysis
```

### 8. Battery Drain Investigation

For laptop users experiencing fast battery drain:

```bash
# Check power status and CPU frequency
./procmon --system | grep -A 10 "Power:"

# Monitor high-power processes
./procmon --list --min-cpu 2.0

# Interactive monitoring
./procmon
# Switch to Power tab (press '5') to see:
# - Battery status and capacity
# - CPU governor and frequency
# - Power saving mode status
```

### 9. Web Browser Performance Issues

Debugging Firefox or Chrome performance problems:

```bash
# Monitor browser process and all its children
./procmon --pid $(pgrep firefox | head -1)

# In interactive mode, examine:
# - Main browser process CPU usage
# - Individual tab processes (Content processes)
# - GPU process utilization
# - Extension processes
```

**Understanding Browser Processes:**
- **Main Process**: Browser UI and coordination
- **Content Processes**: Individual tabs and web content
- **GPU Process**: Hardware acceleration
- **Extension Processes**: Browser extensions

### 10. Server Performance Monitoring

For server environments:

```bash
# Continuous monitoring with minimal output
./procmon --list --min-cpu 5.0 > server_load.log

# System health checks
while true; do
    echo "$(date): $(./procmon --system | grep 'Used:')"
    sleep 60
done

# Memory pressure monitoring
./procmon --system | grep -E "(Memory:|Thrashing:)"
```

## Memory Pressure Testing

### 11. Testing Thrashing Detection

Use the included memory stress tools:

```bash
# Navigate to test directory
cd qemu-test/

# Test 1: Gradual memory allocation
./memory-stress-test.sh memory 512

# Test 2: Page fault pressure
./memory-stress-test.sh pagefault

# Test 3: Force swapping
./memory-stress-test.sh swap

# Test 4: Comprehensive test
./memory-stress-test.sh all

# Monitor during stress test
# In another terminal:
./procmon --system
```

### 12. Monitoring During Stress Test

While running memory stress tests:

```bash
# Terminal 1: Start stress test
cd qemu-test/
./memory-stress-test.sh memory 1024

# Terminal 2: Monitor with procmon
./procmon --system
# Watch for:
# - Memory usage increase
# - Pressure level changes
# - Thrashing detection
# - System health score changes
```

## Automation and Scripting

### 13. Automated Health Checks

Create scripts for regular system monitoring:

```bash
#!/bin/bash
# health_check.sh

echo "=== System Health Check $(date) ==="

# Memory status
echo "Memory Status:"
./procmon --system | grep -A 4 "Memory:"

# Top CPU consumers
echo -e "\nTop CPU Consumers:"
./procmon --list | head -10

# Thermal status
echo -e "\nThermal Status:"
./procmon --system | grep -A 3 "Thermal:"

echo "=================================="
```

### 14. Alert Scripts

Monitor for specific conditions:

```bash
#!/bin/bash
# memory_alert.sh

MEMORY_THRESHOLD=85
TEMP_THRESHOLD=80

# Check memory usage
MEMORY_USAGE=$(./procmon --system | grep "Used:" | awk '{print $2}' | sed 's/%//')

if [ "$MEMORY_USAGE" -gt "$MEMORY_THRESHOLD" ]; then
    echo "ALERT: High memory usage: ${MEMORY_USAGE}%"
    # Send notification, email, etc.
fi

# Check for thrashing
THRASHING=$(./procmon --system | grep "Thrashing:" | grep "DETECTED")
if [ -n "$THRASHING" ]; then
    echo "ALERT: Memory thrashing detected!"
fi
```

### 15. Performance Logging

Log performance metrics over time:

```bash
#!/bin/bash
# performance_logger.sh

LOG_FILE="system_performance.log"

while true; do
    TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')
    
    # Get system metrics
    MEMORY_USAGE=$(./procmon --system | grep "Used:" | awk '{print $2}')
    PROCESS_COUNT=$(./procmon --list | wc -l)
    TOP_CPU_PROCESS=$(./procmon --list | head -2 | tail -1 | awk '{print $2 ":" $3}')
    
    # Log to file
    echo "$TIMESTAMP,$MEMORY_USAGE,$PROCESS_COUNT,$TOP_CPU_PROCESS" >> $LOG_FILE
    
    sleep 60
done
```

## Integration Examples

### 16. Integration with System Monitoring

Combine with other monitoring tools:

```bash
# With htop for comparison
htop &
./procmon

# With iostat for I/O monitoring
iostat -x 1 &
./procmon --system

# With sar for historical data
sar -u 1 &
./procmon --list
```

### 17. Container Monitoring

Monitor containerized applications:

```bash
# Find container processes
docker ps --format "table {{.Names}}\t{{.ID}}"

# Get container PID
CONTAINER_PID=$(docker inspect <container_name> | jq '.[0].State.Pid')

# Monitor container
./procmon --pid $CONTAINER_PID
```

### 18. Development Workflow

For developers debugging applications:

```bash
# Start your application
./my_application &
APP_PID=$!

# Monitor during development
./procmon --pid $APP_PID --update-interval 250ms

# In interactive mode:
# - Monitor memory usage for leaks
# - Check thread creation patterns
# - Watch CPU usage during operations
# - Analyze performance during load tests
```

## Tips and Best Practices

### Performance Monitoring Tips

1. **Start with System Overview**: Always begin with `--system` to get overall health
2. **Use Filtering**: Apply `--min-cpu` and `--min-memory` to focus on significant processes
3. **Monitor Over Time**: Use interactive mode for real-time analysis
4. **Check Multiple Metrics**: Don't rely on CPU alone - memory, threads, and I/O matter
5. **Understand Process Relationships**: Parent-child relationships help identify root causes

### Troubleshooting Tips

1. **Memory Issues**: Look for high virtual memory, swap usage, and thrashing indicators
2. **CPU Issues**: Check both total CPU and per-thread usage patterns
3. **Thermal Issues**: Monitor temperature trends, not just current values
4. **Battery Issues**: Correlate high CPU usage with power consumption patterns
5. **System Responsiveness**: Watch for I/O wait and memory pressure indicators

### Automation Tips

1. **Regular Health Checks**: Schedule periodic system health monitoring
2. **Threshold Alerts**: Set up automated alerts for critical conditions
3. **Historical Logging**: Keep performance logs for trend analysis
4. **Integration**: Combine with existing monitoring infrastructure
5. **Documentation**: Document normal vs. abnormal patterns for your systems

---

These examples should help you get started with effective system monitoring using Process Monitor. For more advanced usage and customization options, refer to the main README.md file.

