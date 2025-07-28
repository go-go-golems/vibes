package procfs

import "time"

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

// RawProcessInfo contains raw process information from procfs
type RawProcessInfo struct {
	// Process identification
	PID         int32  `json:"pid"`
	PPID        int32  `json:"ppid"`
	Name        string `json:"name"`
	CommandLine string `json:"cmdline"`
	Executable  string `json:"executable"`

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
	ThreadCount int32           `json:"thread_count"`
	Threads     []RawThreadInfo `json:"threads"`

	// CPU timing (raw values)
	UserTicks   uint64 `json:"user_ticks"`
	SystemTicks uint64 `json:"system_ticks"`

	// Memory information
	VirtualSize  uint64 `json:"virtual_size"`
	ResidentSize uint64 `json:"resident_size"`
	SwapSize     uint64 `json:"swap_size"`

	// Metadata
	CollectionTime time.Time `json:"collection_time"`
	Reliability    float64   `json:"reliability"`
}

// RawThreadInfo contains raw thread information from procfs
type RawThreadInfo struct {
	// Thread identification
	TID  int32  `json:"tid"`
	PID  int32  `json:"pid"`
	Name string `json:"name"`

	// Thread state
	State    ProcessState `json:"state"`
	Priority int32        `json:"priority"`
	Nice     int32        `json:"nice"`
	LastCPU  int32        `json:"last_cpu"`

	// Timing information
	UserTime   time.Duration `json:"user_time"`
	SystemTime time.Duration `json:"system_time"`

	// CPU timing (raw values)
	UserTicks   uint64 `json:"user_ticks"`
	SystemTicks uint64 `json:"system_ticks"`

	// Memory information (thread-specific where available)
	StackSize uint64 `json:"stack_size"`

	// Metadata
	CollectionTime time.Time `json:"collection_time"`
}

