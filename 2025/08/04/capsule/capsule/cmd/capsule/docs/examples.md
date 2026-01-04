---
Title: Examples
Slug: examples
Short: Practical examples and use cases for Capsule
SectionType: Example
Topics:
- examples
- use-cases
- tutorials
- workflows
Commands:
- run
- go
- ls
- stats
IsTemplate: false
IsTopLevel: true
ShowPerDefault: true
Order: 5
---

# Examples

This page provides practical examples and common use cases for the Capsule CLI tool.

## Basic Examples

### Hello World
```bash
# Simple command execution
capsule run ubuntu:latest echo "Hello from Capsule!"

# With resource constraints
capsule run --cpu 0.5 --mem 256m alpine:latest echo "Resource-constrained hello"
```

### File Operations
```bash
# List files with memory limit
capsule run --mem 128m ubuntu:latest ls -la /usr/bin

# Create and examine files
capsule run --volume ./data:/workspace ubuntu:latest \
  /bin/bash -c "echo 'test data' > /workspace/test.txt && cat /workspace/test.txt"
```

## Go Program Examples

### Simple Go Program
```bash
# Create a simple Go program
cat > hello.go << 'EOF'
package main
import "fmt"
func main() {
    fmt.Println("Hello from Go in Capsule!")
}
EOF

# Run with Capsule
capsule go hello.go
```

### Go Program with Arguments
```bash
# Create a program that accepts arguments
cat > greet.go << 'EOF'
package main
import (
    "fmt"
    "os"
)
func main() {
    if len(os.Args) > 1 {
        fmt.Printf("Hello, %s!\n", os.Args[1])
    } else {
        fmt.Println("Hello, World!")
    }
}
EOF

# Run with arguments
capsule go greet.go -- Alice
```

### Resource-Intensive Go Program
```bash
# CPU and memory intensive program
cat > burner.go << 'EOF'
package main
import (
    "flag"
    "fmt"
    "runtime"
    "sync"
    "time"
)

func main() {
    threads := flag.Int("threads", 2, "Number of CPU threads")
    memory := flag.Int("memory", 100, "Memory to allocate (MB)")
    duration := flag.Duration("duration", 10*time.Second, "Run duration")
    flag.Parse()

    fmt.Printf("Starting with %d threads, %d MB memory for %v\n", 
        *threads, *memory, *duration)
    
    // Allocate memory
    data := make([][]byte, *memory)
    for i := range data {
        data[i] = make([]byte, 1024*1024) // 1MB chunks
    }
    
    // CPU burning
    var wg sync.WaitGroup
    for i := 0; i < *threads; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            start := time.Now()
            for time.Since(start) < *duration {
                // Burn CPU
                for j := 0; j < 1000000; j++ {
                    _ = j * j
                }
            }
        }()
    }
    
    wg.Wait()
    fmt.Printf("Completed. Memory still allocated: %d MB\n", len(data))
}
EOF

# Run with different resource constraints
capsule go burner.go --cpu 1.0 --mem 512m -- -threads 4 -memory 200 -duration 5s
```

## Development Workflows

### Testing Under Resource Constraints
```bash
# Test application with limited resources
capsule go ./my-app.go --cpu 0.5 --mem 256m -- -test-mode

# Validate memory usage patterns
capsule go ./memory-test.go --mem 128m -- -allocate 100MB
```

### Performance Benchmarking
```bash
# Benchmark with different CPU allocations
for cpu in 0.5 1.0 2.0; do
    echo "Testing with $cpu CPU cores:"
    time capsule go ./benchmark.go --cpu $cpu -- -iterations 1000000
done
```

### CI/CD Integration
```bash
#!/bin/bash
# ci-test.sh - Resource-constrained testing script

set -e

echo "Running unit tests with resource constraints..."
capsule go ./... --cpu 1.0 --mem 512m -- -test.v -test.short

echo "Running integration tests..."
capsule go ./integration-test.go --cpu 2.0 --mem 1g -- -config test.yaml

echo "Performance validation..."
capsule go ./perf-test.go --cpu 0.5 --mem 256m -- -duration 30s

echo "All tests passed!"
```

## Container Management Examples

### Long-Running Services
```bash
# Start a web server in background
capsule run --detach --name web-server --cpu 1.0 --mem 512m \
  nginx:alpine

# Check if it's running
capsule ls

# Monitor resource usage
capsule stats web-server

# Stop when done
docker stop web-server
```

### Batch Processing
```bash
# Process multiple files with resource limits
for file in data/*.csv; do
    capsule run --cpu 0.5 --mem 256m --name "process-$(basename $file)" \
      python:alpine python -c "
import pandas as pd
df = pd.read_csv('/data/$(basename $file)')
print(f'Processed {len(df)} rows from $(basename $file)')
" &
done

# Wait for all to complete
wait

# Check results
capsule ls -a
```

## Monitoring Examples

### Resource Usage Monitoring
```bash
# Monitor all capsule containers
capsule stats &
STATS_PID=$!

# Run some workloads
capsule go ./cpu-intensive.go --cpu 1.0 --mem 512m &
capsule go ./memory-intensive.go --cpu 0.5 --mem 1g &

# Let them run for a while
sleep 30

# Stop monitoring
kill $STATS_PID
```

### Automated Resource Tracking
```bash
#!/bin/bash
# resource-tracker.sh

LOG_FILE="resource-usage.log"
echo "timestamp,container,cpu_percent,memory_usage,memory_limit" > $LOG_FILE

while true; do
    capsule ls --output json | jq -r '.[] | 
        "\(now | strftime("%Y-%m-%d %H:%M:%S")),\(.name),\(.cpu_percent),\(.memory_usage),\(.memory_limit)"' \
        >> $LOG_FILE
    sleep 5
done
```

## Advanced Use Cases

### Multi-Stage Processing Pipeline
```bash
#!/bin/bash
# pipeline.sh - Multi-stage data processing

# Stage 1: Data extraction (CPU intensive)
capsule go ./extract.go --cpu 2.0 --mem 1g -- -input raw_data.json -output stage1.json

# Stage 2: Data transformation (Memory intensive)
capsule go ./transform.go --cpu 1.0 --mem 2g -- -input stage1.json -output stage2.json

# Stage 3: Data loading (Balanced)
capsule go ./load.go --cpu 1.0 --mem 512m -- -input stage2.json -output final.db

echo "Pipeline completed!"
```

### Load Testing
```bash
# Load test with controlled resource usage
capsule go ./load-generator.go --cpu 2.0 --mem 1g -- \
  -target http://localhost:8080 \
  -concurrent 50 \
  -duration 5m \
  -rate 100rps
```

### Development Environment Simulation
```bash
# Simulate production constraints in development
capsule go ./my-service.go \
  --cpu 0.5 \
  --mem 256m \
  --volume ./config:/app/config \
  --env "ENV=production" \
  -- -config /app/config/prod.yaml
```

## Troubleshooting Examples

### Memory Limit Testing
```bash
# Test what happens when memory limit is exceeded
capsule go - --mem 128m << 'EOF'
package main
import "fmt"
func main() {
    data := make([][]byte, 200) // Try to allocate 200MB
    for i := range data {
        data[i] = make([]byte, 1024*1024)
        fmt.Printf("Allocated %d MB\n", i+1)
    }
}
EOF
# This will be killed with exit code 137 (OOM)
```

### CPU Constraint Validation
```bash
# Verify CPU constraints are working
capsule go - --cpu 0.5 << 'EOF'
package main
import (
    "fmt"
    "runtime"
    "sync"
    "time"
)
func main() {
    fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
    
    var wg sync.WaitGroup
    start := time.Now()
    
    // Start 4 CPU-intensive goroutines
    for i := 0; i < 4; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            counter := 0
            for time.Since(start) < 5*time.Second {
                counter++
            }
            fmt.Printf("Goroutine %d: %d iterations\n", id, counter)
        }(i)
    }
    
    wg.Wait()
    fmt.Printf("Total time: %v\n", time.Since(start))
}
EOF
```

## Integration Examples

### Docker Compose Integration
```yaml
# docker-compose.yml
version: '3.8'
services:
  app:
    build: .
    deploy:
      resources:
        limits:
          cpus: '1.0'
          memory: 512M
    # Use capsule for development testing:
    # capsule go ./app.go --cpu 1.0 --mem 512m
```

### Kubernetes Resource Requests
```yaml
# deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
spec:
  template:
    spec:
      containers:
      - name: app
        resources:
          requests:
            cpu: "0.5"
            memory: "256Mi"
          limits:
            cpu: "1.0"
            memory: "512Mi"
        # Test locally with:
        # capsule go ./app.go --cpu 1.0 --mem 512m
```

## See Also

- `capsule help run-command`: Detailed run command reference
- `capsule help go-command`: Go program execution details
- `capsule help resources`: Understanding resource constraints
- `capsule help troubleshooting`: Solving common issues

