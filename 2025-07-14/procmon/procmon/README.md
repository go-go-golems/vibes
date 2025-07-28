# Process Monitor (procmon)

A comprehensive CLI tool for monitoring processes, threads, system resources, and performance metrics on Linux systems. Built with Go, featuring an interactive terminal UI powered by Bubble Tea and advanced monitoring capabilities.

## Features

### Core Monitoring
- **Real-time process and thread monitoring** with detailed CPU usage per thread
- **Memory usage tracking** with resident and virtual memory statistics
- **System-wide resource monitoring** including memory pressure detection
- **Kernel thrashing detection** with confidence scoring and contributing factors
- **Well-known program analysis** for Firefox, Chrome, and other complex applications

### System Health
- **CPU temperature monitoring** via thermal sensors
- **Battery status and power management** tracking
- **CPU frequency and governor monitoring** 
- **Memory pressure analysis** with thrashing detection algorithms
- **Performance alerting** with configurable thresholds

### User Interface
- **Interactive Terminal UI** with tabbed interface for different views
- **Non-interactive modes** for scripting and automation
- **Real-time updates** with configurable refresh intervals
- **Keyboard navigation** and filtering options
- **Multiple output formats** (planned: JSON, CSV, YAML)

### Data Persistence
- **SQLite logging** for historical data analysis
- **Performance trend analysis** and anomaly detection
- **Alert history** and system health scoring
- **Configurable retention policies** for long-term monitoring

## Installation

### Prerequisites
- Go 1.21 or later
- Linux system with procfs support
- SQLite3 (for logging features)

### Building from Source

```bash
# Clone the repository
git clone https://github.com/procmon/procmon.git
cd procmon

# Install dependencies
go mod tidy

# Build the application
go build -o procmon cmd/simple/main.go

# Make executable
chmod +x procmon
```

### Quick Start

```bash
# Interactive monitoring (default mode)
./procmon

# List top processes
./procmon --list

# System information
./procmon --system

# Monitor specific process
./procmon --pid 1234

# Show version
./procmon --version
```

## Usage

### Interactive Mode

The default mode launches an interactive terminal UI with multiple tabs:

```bash
./procmon
```

**Navigation:**
- `Tab` / `←→` - Switch between tabs
- `1-6` - Jump directly to specific tabs
- `s` - Cycle through sort options
- `r` - Reverse sort order
- `k` - Toggle kernel process visibility
- `q` - Quit

**Tabs:**
1. **Processes** - List of running processes with CPU and memory usage
2. **Threads** - Detailed thread information for selected processes
3. **Memory** - System memory usage and pressure analysis
4. **Thermal** - Temperature sensors and thermal state
5. **Power** - Battery status and CPU frequency information
6. **System** - Overall system health overview

### Command Line Options

```bash
# Process monitoring options
./procmon --pid 1234                    # Monitor specific process
./procmon --update-interval 500ms       # Custom refresh rate
./procmon --min-cpu 1.0                 # Filter by minimum CPU usage
./procmon --min-memory 100              # Filter by minimum memory (MB)

# Output options
./procmon --list                        # Non-interactive process list
./procmon --system                      # System information snapshot
./procmon --temp-unit fahrenheit        # Temperature unit (celsius/fahrenheit/kelvin)

# Logging options (planned)
./procmon --log-to-sqlite monitor.db    # Enable SQLite logging
./procmon --history-size 1000           # Historical data points to keep
```

### Non-Interactive Modes

#### Process List
```bash
./procmon --list
```
Outputs a formatted table of running processes sorted by CPU usage.

#### System Information
```bash
./procmon --system
```
Displays comprehensive system health information including:
- Memory usage and pressure
- Thermal state and temperatures
- Power status and CPU frequency
- Battery information (if available)

## Architecture

### Core Components

- **Process Monitor** (`pkg/monitor/`) - Core process and thread monitoring
- **Memory Monitor** (`pkg/memory/`) - Memory pressure and thrashing detection
- **Thermal Monitor** (`pkg/thermal/`) - Temperature sensor monitoring
- **Power Monitor** (`pkg/power/`) - Battery and CPU frequency tracking
- **Performance Analyzer** (`pkg/analysis/`) - Trend analysis and alerting
- **SQLite Logger** (`pkg/storage/`) - Data persistence and historical analysis
- **Terminal UI** (`pkg/ui/`) - Interactive Bubble Tea interface

### Data Flow

1. **Data Collection** - Monitors read from `/proc`, `/sys`, and other kernel interfaces
2. **Processing** - Raw data is parsed, analyzed, and enriched with metadata
3. **Analysis** - Performance analyzer detects trends, anomalies, and generates alerts
4. **Storage** - Optional SQLite logging for historical data and trend analysis
5. **Presentation** - Data is displayed via interactive UI or formatted output

### Well-Known Program Analysis

The tool includes specialized analysis for common applications:

- **Firefox** - Identifies content processes, GPU processes, and extension processes
- **Chrome/Chromium** - Recognizes renderer processes, GPU processes, and utility processes
- **System Services** - Categorizes systemd services, kernel threads, and system daemons

## Memory Thrashing Detection

The tool implements sophisticated algorithms to detect kernel thrashing conditions:

### Detection Factors
- **Page Fault Rate** - Monitors major and minor page faults per second
- **Swap Usage** - Tracks swap in/out activity and overall swap utilization
- **Memory Pressure** - Analyzes available memory and allocation patterns
- **I/O Wait** - Monitors I/O wait percentage as indicator of storage pressure

### Confidence Scoring
Thrashing detection uses a weighted confidence score based on multiple factors:
- High page fault rates (30% weight)
- Excessive swap usage (25% weight)
- Memory pressure indicators (25% weight)
- High I/O wait times (20% weight)

### Thresholds (Configurable)
- Page fault rate: >1000 faults/second
- Swap usage: >80% of available swap
- Memory pressure: >80% memory utilization
- I/O wait: >20% of CPU time

## Performance Features

### Real-Time Monitoring
- Sub-second update intervals
- Efficient procfs parsing with minimal system impact
- Concurrent monitoring of multiple subsystems
- Smart caching to reduce filesystem overhead

### Historical Analysis
- Configurable data retention (default: 5 minutes in memory, 7 days in SQLite)
- Trend detection for memory leaks and performance degradation
- Alert generation with configurable cooldown periods
- System health scoring (0-100 scale)

### Resource Efficiency
- Low memory footprint (~10-20MB typical usage)
- Minimal CPU overhead (<1% on modern systems)
- Efficient data structures for high-frequency updates
- Optional SQLite logging with batched writes

## Testing and Validation

### Memory Pressure Testing

The project includes tools for testing thrashing detection:

```bash
# Navigate to test directory
cd qemu-test/

# Run memory stress tests
./memory-stress-test.sh memory 512    # Allocate 512MB
./memory-stress-test.sh pagefault     # Create page fault pressure
./memory-stress-test.sh swap          # Force swapping
./memory-stress-test.sh all           # Run all tests
```

### QEMU Testing Environment

For isolated testing, QEMU VMs can be used:

```bash
# Start test VM with limited memory
./start-test-vm.sh

# Monitor the VM or run tests on host system
./memory-stress-test.sh all
```

## Development

### Project Structure

```
procmon/
├── cmd/
│   ├── simple/main.go          # Simplified CLI entry point
│   └── procmon/main.go         # Full Glazed CLI (planned)
├── pkg/
│   ├── monitor/                # Process monitoring
│   ├── memory/                 # Memory pressure detection
│   ├── thermal/                # Temperature monitoring
│   ├── power/                  # Power and frequency monitoring
│   ├── analysis/               # Performance analysis
│   ├── storage/                # SQLite logging
│   ├── ui/                     # Bubble Tea interface
│   └── cli/                    # CLI commands (planned)
├── internal/
│   ├── procfs/                 # Procfs parsing utilities
│   └── sysfs/                  # Sysfs utilities
├── qemu-test/                  # Testing tools
└── docs/                       # Documentation
```

### Building and Testing

```bash
# Install dependencies
go mod tidy

# Run tests
go test ./...

# Build development version
go build -o procmon-dev cmd/simple/main.go

# Build with debug info
go build -gcflags="all=-N -l" -o procmon-debug cmd/simple/main.go

# Cross-compile for different architectures
GOOS=linux GOARCH=amd64 go build -o procmon-linux-amd64 cmd/simple/main.go
GOOS=linux GOARCH=arm64 go build -o procmon-linux-arm64 cmd/simple/main.go
```

### Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Code Style
- Follow standard Go formatting (`gofmt`)
- Use meaningful variable and function names
- Add comments for complex algorithms
- Include unit tests for new features
- Update documentation for user-facing changes

## Troubleshooting

### Common Issues

#### Permission Errors
```bash
# Some monitoring features require elevated privileges
sudo ./procmon

# Or add user to appropriate groups
sudo usermod -a -G adm,systemd-journal $USER
```

#### Missing Thermal Sensors
```
Error discovering hwmon sensors: open /sys/class/hwmon: no such file or directory
```
This is normal in virtualized environments or systems without hardware monitoring.

#### High CPU Usage
If procmon itself uses high CPU:
- Increase update interval: `--update-interval 2s`
- Reduce history size: `--history-size 100`
- Disable SQLite logging temporarily

#### Memory Pressure Not Detected
- Ensure sufficient system load
- Check if PSI (Pressure Stall Information) is available: `cat /proc/pressure/memory`
- Verify swap is enabled: `swapon --show`

### Debug Mode

```bash
# Enable verbose logging
PROCMON_DEBUG=1 ./procmon

# Profile memory usage
go tool pprof ./procmon http://localhost:6060/debug/pprof/heap

# Profile CPU usage
go tool pprof ./procmon http://localhost:6060/debug/pprof/profile
```

## Roadmap

### Version 1.1 (Planned)
- [ ] Complete Glazed CLI framework integration
- [ ] JSON/CSV/YAML output formats
- [ ] Configuration file support
- [ ] Plugin system for custom analyzers

### Version 1.2 (Planned)
- [ ] Network monitoring integration
- [ ] Container and cgroup support
- [ ] Web dashboard interface
- [ ] Prometheus metrics export

### Version 2.0 (Future)
- [ ] Distributed monitoring support
- [ ] Machine learning anomaly detection
- [ ] Advanced visualization
- [ ] Performance regression testing

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Charm Bracelet](https://charm.sh/) for the excellent Bubble Tea TUI framework
- [go-go-golems](https://github.com/go-go-golems) for the Glazed CLI framework
- Linux kernel developers for comprehensive procfs and sysfs interfaces
- The Go community for excellent tooling and libraries

## Support

- **Issues**: [GitHub Issues](https://github.com/procmon/procmon/issues)
- **Discussions**: [GitHub Discussions](https://github.com/procmon/procmon/discussions)
- **Documentation**: [Wiki](https://github.com/procmon/procmon/wiki)

---

**Process Monitor** - Advanced system monitoring for Linux systems

