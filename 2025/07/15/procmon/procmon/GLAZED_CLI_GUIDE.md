# Process Monitor - Glazed CLI Framework Guide

This guide demonstrates the rich CLI capabilities provided by the Glazed framework integration in Process Monitor. The tool supports both traditional text output and structured data output in multiple formats.

## Overview

Process Monitor (`procmon-glazed`) leverages the [Glazed framework](https://github.com/go-go-golems/glazed) to provide:

- **Rich Verbs:** Comprehensive command sets with extensive parameters
- **Structured Data Output:** JSON, CSV, YAML, and table formats
- **Dual Mode Operation:** Both human-readable and machine-readable output
- **Advanced Filtering:** Complex data filtering and sorting capabilities
- **Schema Support:** Built-in data schema validation and documentation

## Building the Application

```bash
# Build with CGO support for SQLite functionality
CGO_ENABLED=1 go build -o procmon-glazed cmd/glazed/main.go
```

## Command Overview

```bash
$ ./procmon-glazed --help
Process Monitor (procmon) is a comprehensive CLI tool for monitoring processes, 
threads, system resources, and performance metrics on Linux systems. 
It provides rich structured data output in multiple formats (JSON, CSV, YAML, tables)
and supports advanced monitoring capabilities including memory pressure detection,
thermal monitoring, and performance analysis.

Available Commands:
  analyze     Analyze system performance and detect issues
  export      Export historical monitoring data from SQLite database
  list        List running processes with CPU and memory usage
  monitor     Monitor a specific process and its threads in real-time
  system      Display comprehensive system health and resource information
  version     Show version information
```



## List Command

The `list` command provides comprehensive process listing with advanced filtering and sorting capabilities.

### Basic Usage

```bash
# List all processes (default: top 50 by CPU usage)
./procmon-glazed list

# List with structured output
./procmon-glazed list --structured
```

### Advanced Parameters

```bash
# Filter by minimum CPU usage (1% or higher)
./procmon-glazed list --min-cpu 1.0 --structured

# Filter by minimum memory usage (100MB or higher)
./procmon-glazed list --min-memory 100 --structured

# Sort by memory usage in descending order
./procmon-glazed list --list-sort-by memory --reverse --structured

# Show kernel threads and thread details
./procmon-glazed list --show-kernel --show-threads --limit 10 --structured

# Combine multiple filters
./procmon-glazed list --min-cpu 0.5 --min-memory 50 --list-sort-by memory --reverse --limit 20 --structured
```

### Output Formats

The structured output provides rich data including:

- **Process Information:** PID, PPID, name, command line
- **Resource Usage:** CPU usage (total, user, system), memory (resident, virtual, shared)
- **Process State:** State, priority, nice value, thread count
- **Timestamps:** Process start time in RFC3339 format
- **Thread Details:** Individual thread information when requested

### Example Output

**Regular Output:**
```
PID      Name                 CPU%     Memory(MB) Threads  State      Command
--------------------------------------------------------------------------------
1234     firefox              15.2     512        45       S          /usr/bin/firefox
5678     chrome               8.7      256        32       S          /usr/bin/google-chrome
```

**Structured Output:**
```
+------+------+----------+---------+----------+---------------+------------------+------------------+------------------+--------------+-------+----------+------+---------------------------+
| pid  | ppid | name     | command | cpu_usage| cpu_time_user | cpu_time_system  | memory_resident_mb| memory_virtual_mb| thread_count | state | priority | nice | start_time                |
+------+------+----------+---------+----------+---------------+------------------+------------------+------------------+--------------+-------+----------+------+---------------------------+
| 1234 | 1    | firefox  | /usr... | 15.2     | 120.5         | 45.2             | 512              | 1024             | 45           | S     | 20       | 0    | 2025-07-14T10:30:00-04:00 |
+------+------+----------+---------+----------+---------------+------------------+------------------+------------------+--------------+-------+----------+------+---------------------------+
```


## Monitor Command

The `monitor` command provides real-time monitoring of a specific process with detailed thread-level information.

### Basic Usage

```bash
# Monitor process with PID 1234 for 30 seconds
./procmon-glazed monitor --pid 1234 --duration 30s --structured

# Monitor with custom interval and thread details
./procmon-glazed monitor --pid 1234 --interval 500ms --duration 10s --show-threads --structured
```

### Parameters

- `--pid`: Target process ID (required)
- `--interval`: Update interval (default: 1s, examples: 500ms, 2s)
- `--duration`: Monitoring duration (default: 30s, examples: 1m, 5m)
- `--show-threads`: Include individual thread information (default: true)

### Real-time Data Stream

The monitor command provides continuous structured data output with:

- **Process Metrics:** Real-time CPU and memory usage
- **Thread Information:** Individual thread CPU usage and state
- **Error Handling:** Proper reporting when processes exit
- **Summary Statistics:** Final monitoring summary

### Example Output

```
+---------------------------+-----------------+------------+------------------+--------------+-------------------------+----------------------+-------------------+---------------+
| timestamp                 | monitoring_type | target_pid | duration_seconds | sample_count | sample_interval_seconds | status               | error             | sample_number |
+---------------------------+-----------------+------------+------------------+--------------+-------------------------+----------------------+-------------------+---------------+
| 2025-07-14T16:31:15-04:00 | process         | 1234       |                  |              |                         |                      |                   | 0             |
| 2025-07-14T16:31:16-04:00 | thread          | 1234       |                  |              |                         |                      |                   | 0             |
| 2025-07-14T16:31:17-04:00 | summary         | 1234       | 30               | 30           | 1                       | Monitoring completed |                   |               |
+---------------------------+-----------------+------------+------------------+--------------+-------------------------+----------------------+-------------------+---------------+
```

### Use Cases

- **Performance Debugging:** Monitor specific processes causing high CPU usage
- **Thread Analysis:** Identify which threads in multi-threaded applications consume resources
- **Real-time Monitoring:** Continuous monitoring for performance analysis
- **Automated Monitoring:** Structured output for integration with monitoring systems


## System Command

The `system` command provides comprehensive system health and resource information across multiple subsystems.

### Basic Usage

```bash
# Display system overview
./procmon-glazed system

# Structured system information
./procmon-glazed system --structured

# Detailed system information with sensors
./procmon-glazed system --show-details --show-sensors --structured
```

### Parameters

- `--temp-unit`: Temperature unit (celsius, fahrenheit, kelvin)
- `--show-details`: Show detailed breakdown of all subsystems
- `--show-sensors`: Show individual sensor readings
- `--show-history`: Show historical trends and analysis

### System Components Monitored

1. **Memory Subsystem**
   - Memory usage and pressure monitoring
   - Kernel thrashing detection
   - Memory pressure scoring

2. **Thermal Subsystem**
   - CPU temperature monitoring
   - Thermal sensor management
   - Temperature alert thresholds

3. **Power Subsystem**
   - Battery status monitoring
   - CPU governor information
   - Power management state

### Example Output

**Regular Output:**
```
System Information
==================
Timestamp: 2025-07-14 16:31:41

Memory:
  Status: Monitoring enabled
  Thrashing Detection: Active

Thermal:
  Status: Monitoring enabled
  Temperature Unit: celsius

Power:
  Status: Monitoring enabled
  CPU Governor: Available
```

**Structured Output:**
```
+---------------------------+-----------+--------------------+--------------------+-------------------------+------------------+-------------------+---------------------+---------------------+
| timestamp                 | component | status             | battery_monitoring | cpu_governor_monitoring | temperature_unit | sensor_monitoring | thrashing_detection | pressure_monitoring |
+---------------------------+-----------+--------------------+--------------------+-------------------------+------------------+-------------------+---------------------+---------------------+
| 2025-07-14T16:31:41-04:00 | memory    | monitoring_enabled |                    |                         |                  |                   | active              | enabled             |
| 2025-07-14T16:31:41-04:00 | thermal   | monitoring_enabled |                    |                         | celsius          | active            |                     |                     |
| 2025-07-14T16:31:41-04:00 | power     | monitoring_enabled | active             | enabled                 |                  |                   |                     |                     |
+---------------------------+-----------+--------------------+--------------------+-------------------------+------------------+-------------------+---------------------+---------------------+
```


## Analyze Command

The `analyze` command performs system performance analysis and issue detection over a specified time period.

### Basic Usage

```bash
# Analyze system for 30 seconds with 1-second intervals
./procmon-glazed analyze --duration 30s --interval 1s --structured

# Quick analysis with high-frequency sampling
./procmon-glazed analyze --duration 5s --interval 500ms --structured
```

### Parameters

- `--duration`: Analysis duration (examples: 30s, 1m, 5m)
- `--interval`: Sampling interval (examples: 500ms, 1s, 2s)

### Analysis Features

- **Performance Sampling:** Continuous system performance monitoring
- **Health Scoring:** Automated system health assessment
- **Trend Detection:** Performance trend analysis
- **Issue Identification:** Automatic detection of performance issues

### Example Output

```
+---------------------------+---------------+------------------+--------------+-------------------------+--------------+---------------------------------+
| timestamp                 | analysis_type | duration_seconds | sample_count | sample_interval_seconds | health_score | status                          |
+---------------------------+---------------+------------------+--------------+-------------------------+--------------+---------------------------------+
| 2025-07-14T16:31:54-04:00 | summary       | 30               | 30           | 1                       | 75           | Analysis completed successfully |
+---------------------------+---------------+------------------+--------------+-------------------------+--------------+---------------------------------+
```

## Export Command

The `export` command provides data export functionality from the SQLite monitoring database.

### Basic Usage

```bash
# Export all data
./procmon-glazed export --structured

# Export specific table
./procmon-glazed export --table processes --structured

# Export with time range filtering
./procmon-glazed export --start-time "2025-07-14T00:00:00Z" --end-time "2025-07-14T23:59:59Z" --structured
```

### Parameters

- `--database`: Path to SQLite database file (default: procmon.db)
- `--table`: Database table to export (all, processes, system_metrics, performance_events)
- `--start-time`: Start time for export (RFC3339 format)
- `--end-time`: End time for export (RFC3339 format)
- `--process-filter`: Filter by process name (supports wildcards)
- `--min-cpu`: Minimum CPU usage threshold for filtering
- `--limit`: Maximum number of records to export
- `--include-metadata`: Include metadata and schema information

### Supported Export Formats

- **JSON:** Machine-readable structured data
- **CSV:** Spreadsheet-compatible format
- **YAML:** Human-readable structured format
- **Table:** Console-friendly tabular output

### Example Output

```
+-------------+----------------------------------------------------------+-----------------------------------------------+------------------------+------------------------------------------------+
| export_type | message                                                  | supported_tables                              | supported_formats      | note                                           |
+-------------+----------------------------------------------------------+-----------------------------------------------+------------------------+------------------------------------------------+
| placeholder | Export functionality ready - database schema implemented | processes, system_metrics, performance_events | JSON, CSV, YAML, Table | Actual data export requires populated database |
+-------------+----------------------------------------------------------+-----------------------------------------------+------------------------+------------------------------------------------+
```


## Advanced Usage Examples

### Pipeline Integration

The structured output makes Process Monitor ideal for integration with other tools:

```bash
# Export to JSON for further processing
./procmon-glazed list --structured --output-format json > processes.json

# Monitor and log to file
./procmon-glazed monitor --pid 1234 --duration 1h --structured >> monitoring.log

# System health check with filtering
./procmon-glazed system --structured | grep -E "(critical|warning)"
```

### Automation and Scripting

```bash
#!/bin/bash
# Automated monitoring script

# Check system health
HEALTH_SCORE=$(./procmon-glazed analyze --duration 10s --structured | grep summary | awk '{print $12}')

if [ "$HEALTH_SCORE" -lt 50 ]; then
    echo "System health critical: $HEALTH_SCORE"
    # Trigger alerts or remediation
fi

# Monitor high CPU processes
./procmon-glazed list --min-cpu 10.0 --structured --limit 5
```

### Data Analysis Workflows

```bash
# Collect performance data
./procmon-glazed analyze --duration 5m --interval 1s --structured > performance_data.csv

# Export historical data for analysis
./procmon-glazed export --table system_metrics --start-time "2025-07-14T00:00:00Z" --structured > metrics.json

# Monitor specific processes over time
for pid in $(pgrep firefox); do
    ./procmon-glazed monitor --pid $pid --duration 30s --structured >> firefox_monitoring.log &
done
```

## Schema and Data Structure

### Process Data Schema

```yaml
process:
  pid: integer
  ppid: integer
  name: string
  command: string
  cpu_usage: float
  cpu_time_user: float
  cpu_time_system: float
  memory_resident_mb: integer
  memory_virtual_mb: integer
  memory_shared_mb: integer
  thread_count: integer
  state: string
  priority: integer
  nice: integer
  start_time: timestamp (RFC3339)
```

### Thread Data Schema

```yaml
thread:
  pid: integer
  tid: integer
  thread_name: string
  thread_cpu_usage: float
  thread_cpu_time_user: float
  thread_cpu_time_system: float
  thread_state: string
  thread_priority: integer
```

### System Component Schema

```yaml
system_component:
  timestamp: timestamp (RFC3339)
  component: string (memory|thermal|power)
  status: string
  battery_monitoring: string
  cpu_governor_monitoring: string
  temperature_unit: string
  sensor_monitoring: string
  thrashing_detection: string
  pressure_monitoring: string
```

## Integration with External Tools

### Prometheus Integration

```bash
# Export metrics in a format suitable for Prometheus
./procmon-glazed system --structured | convert_to_prometheus_format.py
```

### Grafana Dashboard

The structured output can be easily integrated with Grafana for visualization:

1. Export data to time-series database
2. Configure Grafana data source
3. Create dashboards using the rich metric data

### Log Aggregation

```bash
# Send structured logs to centralized logging
./procmon-glazed monitor --pid $PID --structured | logger -t procmon
```

## Best Practices

### Performance Monitoring

1. **Use appropriate intervals:** Balance between data granularity and system overhead
2. **Filter effectively:** Use CPU and memory thresholds to focus on relevant processes
3. **Structured output:** Always use `--structured` for automated processing
4. **Time-bound monitoring:** Set appropriate duration limits for continuous monitoring

### Data Management

1. **Regular exports:** Export historical data regularly to prevent database growth
2. **Filtering:** Use time-range and process filters to export relevant data
3. **Format selection:** Choose appropriate output format for your use case
4. **Schema validation:** Leverage built-in schema support for data validation

### Automation

1. **Error handling:** Check command exit codes in scripts
2. **Resource limits:** Set appropriate timeouts and limits
3. **Logging:** Capture both structured output and error messages
4. **Monitoring:** Set up alerts based on health scores and thresholds

## Troubleshooting

### Common Issues

1. **CGO Build Requirements:** Ensure GCC is installed for SQLite support
2. **Permission Issues:** Some monitoring features may require elevated privileges
3. **Process Not Found:** Handle cases where monitored processes exit
4. **Database Locks:** Ensure proper database access for export operations

### Debug Mode

```bash
# Enable verbose logging
./procmon-glazed list --print-parsed-parameters --structured

# Print command schema
./procmon-glazed list --print-schema

# Print YAML configuration
./procmon-glazed list --print-yaml
```

This comprehensive guide demonstrates the power and flexibility of the Glazed framework integration, providing rich CLI capabilities for process monitoring and system analysis.

