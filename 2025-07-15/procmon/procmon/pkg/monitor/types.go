package monitor

import (
	"time"
)

// ProcessInfo represents comprehensive information about a running process
type ProcessInfo struct {
	// Process identification
	PID         int32  `json:"pid"`
	PPID        int32  `json:"ppid"`
	Name        string `json:"name"`
	CommandLine string `json:"cmdline"`
	Executable  string `json:"executable"`

	// Resource utilization
	CPUUsage CPUUsageInfo    `json:"cpu_usage"`
	Memory   MemoryUsageInfo `json:"memory"`
	IO       IOUsageInfo     `json:"io"`

	// Process state
	State    ProcessState `json:"state"`
	Priority int32        `json:"priority"`
	Nice     int32        `json:"nice"`

	// Timing information
	StartTime time.Time     `json:"start_time"`
	CPUTime   time.Duration `json:"cpu_time"`

	// User and group information
	UID       uint32 `json:"uid"`
	GID       uint32 `json:"gid"`
	Username  string `json:"username"`
	Groupname string `json:"groupname"`

	// Thread information
	ThreadCount int32        `json:"thread_count"`
	Threads     []ThreadInfo `json:"threads"`

	// Metadata
	CollectionTime time.Time  `json:"collection_time"`
	Source         DataSource `json:"source"`
	Reliability    float64    `json:"reliability"`
}

// ThreadInfo represents detailed information about a specific thread
type ThreadInfo struct {
	// Thread identification
	TID  int32  `json:"tid"`
	PID  int32  `json:"pid"`
	Name string `json:"name"`

	// CPU information
	CPUUsage    CPUUsageInfo `json:"cpu_usage"`
	CPUAffinity []int32      `json:"cpu_affinity"`
	LastCPU     int32        `json:"last_cpu"`

	// Thread state
	State    ProcessState `json:"state"`
	Priority int32        `json:"priority"`
	Nice     int32        `json:"nice"`

	// Timing information
	UserTime   time.Duration `json:"user_time"`
	SystemTime time.Duration `json:"system_time"`

	// Memory information (thread-specific where available)
	StackSize uint64 `json:"stack_size"`

	// Metadata
	CollectionTime time.Time  `json:"collection_time"`
	Source         DataSource `json:"source"`
}

// CPUUsageInfo represents CPU usage statistics
type CPUUsageInfo struct {
	// Current usage percentages
	Total  float64 `json:"total"`
	User   float64 `json:"user"`
	System float64 `json:"system"`

	// Raw timing values (in clock ticks)
	UserTicks   uint64 `json:"user_ticks"`
	SystemTicks uint64 `json:"system_ticks"`

	// Historical data
	History TimeSeries[float64] `json:"history"`

	// Derived metrics
	Average1Min  float64 `json:"avg_1min"`
	Average5Min  float64 `json:"avg_5min"`
	Average15Min float64 `json:"avg_15min"`

	// Metadata
	SampleInterval time.Duration `json:"sample_interval"`
	LastUpdate     time.Time     `json:"last_update"`
}

// MemoryUsageInfo represents memory usage information
type MemoryUsageInfo struct {
	VirtualSize  uint64  `json:"virtual_size"`  // Virtual memory size
	ResidentSize uint64  `json:"resident_size"` // Resident set size
	SharedSize   uint64  `json:"shared_size"`   // Shared memory size
	SwapSize     uint64  `json:"swap_size"`     // Swap usage
	UsagePercent float64 `json:"usage_percent"` // Memory usage percentage

	// Historical data
	History TimeSeries[uint64] `json:"history"`
}

// IOUsageInfo represents I/O usage information
type IOUsageInfo struct {
	ReadBytes    uint64 `json:"read_bytes"`
	WriteBytes   uint64 `json:"write_bytes"`
	ReadOps      uint64 `json:"read_ops"`
	WriteOps     uint64 `json:"write_ops"`
	ReadRate     float64 `json:"read_rate"`  // Bytes per second
	WriteRate    float64 `json:"write_rate"` // Bytes per second

	// Historical data
	History TimeSeries[uint64] `json:"history"`
}

// ProcessState represents the current state of a process
type ProcessState string

const (
	ProcessStateRunning     ProcessState = "R"
	ProcessStateSleeping    ProcessState = "S"
	ProcessStateDiskSleep   ProcessState = "D"
	ProcessStateZombie      ProcessState = "Z"
	ProcessStateStopped     ProcessState = "T"
	ProcessStateTracingStop ProcessState = "t"
	ProcessStateDead        ProcessState = "X"
	ProcessStateWakekill    ProcessState = "W"
	ProcessStateParked      ProcessState = "P"
)

// String returns a human-readable description of the process state
func (ps ProcessState) String() string {
	switch ps {
	case ProcessStateRunning:
		return "Running"
	case ProcessStateSleeping:
		return "Sleeping"
	case ProcessStateDiskSleep:
		return "Disk Sleep"
	case ProcessStateZombie:
		return "Zombie"
	case ProcessStateStopped:
		return "Stopped"
	case ProcessStateTracingStop:
		return "Tracing Stop"
	case ProcessStateDead:
		return "Dead"
	case ProcessStateWakekill:
		return "Wakekill"
	case ProcessStateParked:
		return "Parked"
	default:
		return "Unknown"
	}
}

// TimeSeries represents a time-series data structure for historical monitoring data
type TimeSeries[T any] struct {
	// Data storage
	Data    []TimeSeriesPoint[T] `json:"data"`
	MaxSize int                  `json:"max_size"`

	// Metadata
	StartTime time.Time     `json:"start_time"`
	EndTime   time.Time     `json:"end_time"`
	Interval  time.Duration `json:"interval"`

	// Statistics
	Count int `json:"count"`

	// Configuration
	RetentionPolicy RetentionPolicy `json:"retention_policy"`
}

// TimeSeriesPoint represents a single data point in a time series
type TimeSeriesPoint[T any] struct {
	Timestamp time.Time `json:"timestamp"`
	Value     T         `json:"value"`
	Quality   float64   `json:"quality"` // Data quality indicator (0-1)
}

// RetentionPolicy defines how historical data is retained
type RetentionPolicy struct {
	MaxAge      time.Duration   `json:"max_age"`
	MaxPoints   int             `json:"max_points"`
	Aggregation AggregationType `json:"aggregation"`
}

type AggregationType string

const (
	AggregationNone    AggregationType = "none"
	AggregationAverage AggregationType = "average"
	AggregationMax     AggregationType = "max"
	AggregationMin     AggregationType = "min"
	AggregationSum     AggregationType = "sum"
)

// DataSource represents the source of monitoring data
type DataSource struct {
	Type        SourceType `json:"type"`
	Path        string     `json:"path"`
	Reliability float64    `json:"reliability"`
	LastAccess  time.Time  `json:"last_access"`
	ErrorCount  int        `json:"error_count"`
}

type SourceType string

const (
	SourceTypeProcFS      SourceType = "procfs"
	SourceTypeSysFS       SourceType = "sysfs"
	SourceTypeHWMon       SourceType = "hwmon"
	SourceTypeThermal     SourceType = "thermal"
	SourceTypePowerSupply SourceType = "power_supply"
	SourceTypeCommand     SourceType = "command"
)

// Add method to add a new data point to the time series
func (ts *TimeSeries[T]) Add(value T, timestamp time.Time, quality float64) {
	point := TimeSeriesPoint[T]{
		Timestamp: timestamp,
		Value:     value,
		Quality:   quality,
	}

	ts.Data = append(ts.Data, point)
	ts.Count++

	// Update time range
	if ts.Count == 1 {
		ts.StartTime = timestamp
	}
	ts.EndTime = timestamp

	// Apply retention policy
	ts.applyRetentionPolicy()
}

// applyRetentionPolicy removes old data points based on the retention policy
func (ts *TimeSeries[T]) applyRetentionPolicy() {
	if ts.RetentionPolicy.MaxPoints > 0 && len(ts.Data) > ts.RetentionPolicy.MaxPoints {
		// Remove oldest points
		excess := len(ts.Data) - ts.RetentionPolicy.MaxPoints
		ts.Data = ts.Data[excess:]
		ts.Count = len(ts.Data)
		if len(ts.Data) > 0 {
			ts.StartTime = ts.Data[0].Timestamp
		}
	}

	if ts.RetentionPolicy.MaxAge > 0 {
		cutoff := time.Now().Add(-ts.RetentionPolicy.MaxAge)
		// Remove points older than cutoff
		for i, point := range ts.Data {
			if point.Timestamp.After(cutoff) {
				ts.Data = ts.Data[i:]
				ts.Count = len(ts.Data)
				if len(ts.Data) > 0 {
					ts.StartTime = ts.Data[0].Timestamp
				}
				break
			}
		}
	}
}

// GetLatest returns the most recent data point
func (ts *TimeSeries[T]) GetLatest() (TimeSeriesPoint[T], bool) {
	if len(ts.Data) == 0 {
		var zero TimeSeriesPoint[T]
		return zero, false
	}
	return ts.Data[len(ts.Data)-1], true
}

// GetRange returns data points within the specified time range
func (ts *TimeSeries[T]) GetRange(start, end time.Time) []TimeSeriesPoint[T] {
	var result []TimeSeriesPoint[T]
	for _, point := range ts.Data {
		if point.Timestamp.After(start) && point.Timestamp.Before(end) {
			result = append(result, point)
		}
	}
	return result
}

