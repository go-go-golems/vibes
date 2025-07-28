package monitor

import (
	"time"

	"github.com/procmon/procmon/internal/procfs"
)

// ConvertRawProcessInfo converts procfs.RawProcessInfo to monitor.ProcessInfo
func ConvertRawProcessInfo(raw *procfs.RawProcessInfo) *ProcessInfo {
	info := &ProcessInfo{
		PID:         raw.PID,
		PPID:        raw.PPID,
		Name:        raw.Name,
		CommandLine: raw.CommandLine,
		Executable:  raw.Executable,
		State:       ProcessState(raw.State),
		Priority:    raw.Priority,
		Nice:        raw.Nice,
		StartTime:   raw.StartTime,
		CPUTime:     raw.CPUTime,
		UID:         raw.UID,
		GID:         raw.GID,
		Username:    raw.Username,
		Groupname:   raw.Groupname,
		ThreadCount: raw.ThreadCount,
		CollectionTime: raw.CollectionTime,
		Source: DataSource{
			Type:        SourceTypeProcFS,
			Path:        "/proc/" + string(rune(raw.PID)),
			Reliability: raw.Reliability,
			LastAccess:  raw.CollectionTime,
		},
	}

	// Initialize CPU usage with raw values
	info.CPUUsage = CPUUsageInfo{
		UserTicks:      raw.UserTicks,
		SystemTicks:    raw.SystemTicks,
		SampleInterval: time.Second,
		LastUpdate:     raw.CollectionTime,
		History: TimeSeries[float64]{
			MaxSize: 300,
			RetentionPolicy: RetentionPolicy{
				MaxAge:    5 * time.Minute,
				MaxPoints: 300,
			},
		},
	}

	// Initialize memory usage
	info.Memory = MemoryUsageInfo{
		VirtualSize:  raw.VirtualSize,
		ResidentSize: raw.ResidentSize,
		SwapSize:     raw.SwapSize,
		History: TimeSeries[uint64]{
			MaxSize: 300,
			RetentionPolicy: RetentionPolicy{
				MaxAge:    5 * time.Minute,
				MaxPoints: 300,
			},
		},
	}

	// Initialize I/O usage
	info.IO = IOUsageInfo{
		History: TimeSeries[uint64]{
			MaxSize: 300,
			RetentionPolicy: RetentionPolicy{
				MaxAge:    5 * time.Minute,
				MaxPoints: 300,
			},
		},
	}

	// Convert threads
	info.Threads = make([]ThreadInfo, len(raw.Threads))
	for i, rawThread := range raw.Threads {
		info.Threads[i] = ConvertRawThreadInfo(&rawThread)
	}

	return info
}

// ConvertRawThreadInfo converts procfs.RawThreadInfo to monitor.ThreadInfo
func ConvertRawThreadInfo(raw *procfs.RawThreadInfo) ThreadInfo {
	return ThreadInfo{
		TID:        raw.TID,
		PID:        raw.PID,
		Name:       raw.Name,
		State:      ProcessState(raw.State),
		Priority:   raw.Priority,
		Nice:       raw.Nice,
		LastCPU:    raw.LastCPU,
		UserTime:   raw.UserTime,
		SystemTime: raw.SystemTime,
		StackSize:  raw.StackSize,
		CollectionTime: raw.CollectionTime,
		Source: DataSource{
			Type:        SourceTypeProcFS,
			Path:        "/proc/" + string(rune(raw.PID)) + "/task/" + string(rune(raw.TID)),
			Reliability: 1.0,
			LastAccess:  raw.CollectionTime,
		},
		CPUUsage: CPUUsageInfo{
			UserTicks:      raw.UserTicks,
			SystemTicks:    raw.SystemTicks,
			SampleInterval: time.Second,
			LastUpdate:     raw.CollectionTime,
			History: TimeSeries[float64]{
				MaxSize: 300,
				RetentionPolicy: RetentionPolicy{
					MaxAge:    5 * time.Minute,
					MaxPoints: 300,
				},
			},
		},
	}
}

