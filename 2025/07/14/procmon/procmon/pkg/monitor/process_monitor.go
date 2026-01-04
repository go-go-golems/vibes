package monitor

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/procmon/procmon/internal/procfs"
)

// ProcessMonitor manages process and thread monitoring
type ProcessMonitor struct {
	parser   *procfs.Parser
	interval time.Duration
	
	// Process tracking
	processes    map[int32]*ProcessInfo
	processMutex sync.RWMutex
	
	// Previous CPU measurements for calculating usage
	prevCPUMeasurements map[int32]CPUMeasurement
	prevSystemCPU       SystemCPUMeasurement
	
	// Configuration
	config MonitorConfig
	
	// Control channels
	stopChan chan struct{}
	doneChan chan struct{}
	
	// Callbacks
	callbacks []ProcessCallback
}

// CPUMeasurement stores CPU timing information for calculating usage
type CPUMeasurement struct {
	UserTicks   uint64
	SystemTicks uint64
	Timestamp   time.Time
}

// SystemCPUMeasurement stores system-wide CPU timing information
type SystemCPUMeasurement struct {
	User      uint64
	Nice      uint64
	System    uint64
	Idle      uint64
	IOWait    uint64
	IRQ       uint64
	SoftIRQ   uint64
	Steal     uint64
	Guest     uint64
	GuestNice uint64
	Timestamp time.Time
}

// MonitorConfig contains configuration options for the process monitor
type MonitorConfig struct {
	UpdateInterval    time.Duration
	IncludeKernel     bool
	IncludeThreads    bool
	MaxHistoryPoints  int
	CPUUsageWindow    time.Duration
	ProcessFilter     ProcessFilter
}

// ProcessFilter defines criteria for filtering processes
type ProcessFilter struct {
	MinCPUUsage     float64
	MinMemoryUsage  uint64
	NamePatterns    []string
	ExcludePatterns []string
	IncludeKernel   bool
}

// ProcessCallback is called when process information is updated
type ProcessCallback func(process *ProcessInfo, event ProcessEvent)

// ProcessEvent represents different types of process events
type ProcessEvent string

const (
	ProcessEventNew     ProcessEvent = "new"
	ProcessEventUpdated ProcessEvent = "updated"
	ProcessEventExited  ProcessEvent = "exited"
)

// DefaultMonitorConfig returns a default configuration
func DefaultMonitorConfig() MonitorConfig {
	return MonitorConfig{
		UpdateInterval:   time.Second,
		IncludeKernel:    false,
		IncludeThreads:   true,
		MaxHistoryPoints: 300, // 5 minutes at 1-second intervals
		CPUUsageWindow:   time.Minute,
		ProcessFilter: ProcessFilter{
			MinCPUUsage:    0.0,
			MinMemoryUsage: 0,
			IncludeKernel:  false,
		},
	}
}

// NewProcessMonitor creates a new process monitor
func NewProcessMonitor(config MonitorConfig) *ProcessMonitor {
	return &ProcessMonitor{
		parser:              procfs.NewParser(),
		interval:            config.UpdateInterval,
		processes:           make(map[int32]*ProcessInfo),
		prevCPUMeasurements: make(map[int32]CPUMeasurement),
		config:              config,
		stopChan:            make(chan struct{}),
		doneChan:            make(chan struct{}),
	}
}

// AddCallback adds a callback function to be called on process events
func (pm *ProcessMonitor) AddCallback(callback ProcessCallback) {
	pm.callbacks = append(pm.callbacks, callback)
}

// Start begins the monitoring process
func (pm *ProcessMonitor) Start(ctx context.Context) error {
	// Initial system CPU measurement
	if err := pm.updateSystemCPU(); err != nil {
		return fmt.Errorf("failed to get initial system CPU measurement: %w", err)
	}

	// Start monitoring goroutine
	go pm.monitorLoop(ctx)
	
	return nil
}

// Stop stops the monitoring process
func (pm *ProcessMonitor) Stop() {
	close(pm.stopChan)
	<-pm.doneChan
}

// GetProcesses returns a copy of all currently monitored processes
func (pm *ProcessMonitor) GetProcesses() map[int32]*ProcessInfo {
	pm.processMutex.RLock()
	defer pm.processMutex.RUnlock()
	
	// Create a copy to avoid race conditions
	result := make(map[int32]*ProcessInfo)
	for pid, process := range pm.processes {
		// Deep copy the process info
		processCopy := *process
		result[pid] = &processCopy
	}
	
	return result
}

// GetProcess returns information for a specific process
func (pm *ProcessMonitor) GetProcess(pid int32) (*ProcessInfo, bool) {
	pm.processMutex.RLock()
	defer pm.processMutex.RUnlock()
	
	process, exists := pm.processes[pid]
	if !exists {
		return nil, false
	}
	
	// Return a copy
	processCopy := *process
	return &processCopy, true
}

// GetProcessCount returns the number of currently monitored processes
func (pm *ProcessMonitor) GetProcessCount() int {
	pm.processMutex.RLock()
	defer pm.processMutex.RUnlock()
	
	return len(pm.processes)
}

// monitorLoop is the main monitoring loop
func (pm *ProcessMonitor) monitorLoop(ctx context.Context) {
	defer close(pm.doneChan)
	
	ticker := time.NewTicker(pm.interval)
	defer ticker.Stop()
	
	for {
		select {
		case <-ctx.Done():
			return
		case <-pm.stopChan:
			return
		case <-ticker.C:
			if err := pm.updateProcesses(); err != nil {
				// Log error but continue monitoring
				fmt.Printf("Error updating processes: %v\n", err)
			}
		}
	}
}

// updateProcesses updates information for all processes
func (pm *ProcessMonitor) updateProcesses() error {
	// Get list of current processes
	pids, err := pm.parser.GetProcessList()
	if err != nil {
		return fmt.Errorf("failed to get process list: %w", err)
	}
	
	// Update system CPU measurement
	if err := pm.updateSystemCPU(); err != nil {
		return fmt.Errorf("failed to update system CPU: %w", err)
	}
	
	pm.processMutex.Lock()
	defer pm.processMutex.Unlock()
	
	// Track which processes we've seen in this update
	seenPIDs := make(map[int32]bool)
	
	// Update existing processes and add new ones
	for _, pid := range pids {
		seenPIDs[pid] = true
		
		// Get process information
		rawProcessInfo, err := pm.parser.GetProcessInfo(pid)
		if err != nil {
			// Process might have exited, skip it
			continue
		}
		
		// Convert to monitor types
		processInfo := ConvertRawProcessInfo(rawProcessInfo)
		
		// Apply process filter
		if !pm.shouldIncludeProcess(processInfo) {
			continue
		}
		
		// Calculate CPU usage
		pm.calculateCPUUsage(processInfo)
		
		// Check if this is a new process or an update
		if existingProcess, exists := pm.processes[pid]; exists {
			// Update existing process
			pm.updateProcessHistory(existingProcess, processInfo)
			pm.processes[pid] = processInfo
			pm.notifyCallbacks(processInfo, ProcessEventUpdated)
		} else {
			// New process
			pm.processes[pid] = processInfo
			pm.notifyCallbacks(processInfo, ProcessEventNew)
		}
	}
	
	// Remove processes that no longer exist
	for pid, process := range pm.processes {
		if !seenPIDs[pid] {
			delete(pm.processes, pid)
			delete(pm.prevCPUMeasurements, pid)
			pm.notifyCallbacks(process, ProcessEventExited)
		}
	}
	
	return nil
}

// shouldIncludeProcess determines if a process should be included based on filters
func (pm *ProcessMonitor) shouldIncludeProcess(process *ProcessInfo) bool {
	filter := pm.config.ProcessFilter
	
	// Check kernel process filter
	if !filter.IncludeKernel && isKernelProcess(process) {
		return false
	}
	
	// Check minimum CPU usage
	if process.CPUUsage.Total < filter.MinCPUUsage {
		return false
	}
	
	// Check minimum memory usage
	if process.Memory.ResidentSize < filter.MinMemoryUsage {
		return false
	}
	
	// Check name patterns (if specified)
	if len(filter.NamePatterns) > 0 {
		matched := false
		for _, pattern := range filter.NamePatterns {
			if matchesPattern(process.Name, pattern) {
				matched = true
				break
			}
		}
		if !matched {
			return false
		}
	}
	
	// Check exclude patterns
	for _, pattern := range filter.ExcludePatterns {
		if matchesPattern(process.Name, pattern) {
			return false
		}
	}
	
	return true
}

// isKernelProcess determines if a process is a kernel process
func isKernelProcess(process *ProcessInfo) bool {
	// Kernel processes typically have names in square brackets
	return len(process.Name) > 2 && process.Name[0] == '[' && process.Name[len(process.Name)-1] == ']'
}

// matchesPattern performs simple pattern matching (supports * wildcard)
func matchesPattern(name, pattern string) bool {
	// Simple implementation - could be enhanced with regex support
	if pattern == "*" {
		return true
	}
	
	// For now, just do exact match or prefix/suffix matching
	if pattern[len(pattern)-1] == '*' {
		prefix := pattern[:len(pattern)-1]
		return len(name) >= len(prefix) && name[:len(prefix)] == prefix
	}
	
	if pattern[0] == '*' {
		suffix := pattern[1:]
		return len(name) >= len(suffix) && name[len(name)-len(suffix):] == suffix
	}
	
	return name == pattern
}

// calculateCPUUsage calculates CPU usage percentage for a process
func (pm *ProcessMonitor) calculateCPUUsage(process *ProcessInfo) {
	now := time.Now()
	
	// Get previous measurement
	prevMeasurement, hasPrev := pm.prevCPUMeasurements[process.PID]
	
	// Store current measurement
	currentMeasurement := CPUMeasurement{
		UserTicks:   process.CPUUsage.UserTicks,
		SystemTicks: process.CPUUsage.SystemTicks,
		Timestamp:   now,
	}
	pm.prevCPUMeasurements[process.PID] = currentMeasurement
	
	if !hasPrev {
		// No previous measurement, can't calculate usage yet
		process.CPUUsage.Total = 0.0
		process.CPUUsage.User = 0.0
		process.CPUUsage.System = 0.0
		return
	}
	
	// Calculate time difference
	timeDiff := now.Sub(prevMeasurement.Timestamp).Seconds()
	if timeDiff <= 0 {
		return
	}
	
	// Calculate tick differences
	userTickDiff := currentMeasurement.UserTicks - prevMeasurement.UserTicks
	systemTickDiff := currentMeasurement.SystemTicks - prevMeasurement.SystemTicks
	totalTickDiff := userTickDiff + systemTickDiff
	
	// Calculate CPU usage percentages
	// Convert ticks to seconds and then to percentage
	ticksPerSecond := float64(procfs.ClockTicksPerSecond)
	
	process.CPUUsage.User = (float64(userTickDiff) / ticksPerSecond) / timeDiff * 100.0
	process.CPUUsage.System = (float64(systemTickDiff) / ticksPerSecond) / timeDiff * 100.0
	process.CPUUsage.Total = (float64(totalTickDiff) / ticksPerSecond) / timeDiff * 100.0
	
	// Add to history
	process.CPUUsage.History.Add(process.CPUUsage.Total, now, 1.0)
	
	// Calculate moving averages
	pm.calculateMovingAverages(&process.CPUUsage)
}

// calculateMovingAverages calculates moving averages for CPU usage
func (pm *ProcessMonitor) calculateMovingAverages(cpuUsage *CPUUsageInfo) {
	now := time.Now()
	
	// 1-minute average
	oneMinAgo := now.Add(-time.Minute)
	oneMinData := cpuUsage.History.GetRange(oneMinAgo, now)
	if len(oneMinData) > 0 {
		sum := 0.0
		for _, point := range oneMinData {
			sum += point.Value
		}
		cpuUsage.Average1Min = sum / float64(len(oneMinData))
	}
	
	// 5-minute average
	fiveMinAgo := now.Add(-5 * time.Minute)
	fiveMinData := cpuUsage.History.GetRange(fiveMinAgo, now)
	if len(fiveMinData) > 0 {
		sum := 0.0
		for _, point := range fiveMinData {
			sum += point.Value
		}
		cpuUsage.Average5Min = sum / float64(len(fiveMinData))
	}
	
	// 15-minute average
	fifteenMinAgo := now.Add(-15 * time.Minute)
	fifteenMinData := cpuUsage.History.GetRange(fifteenMinAgo, now)
	if len(fifteenMinData) > 0 {
		sum := 0.0
		for _, point := range fifteenMinData {
			sum += point.Value
		}
		cpuUsage.Average15Min = sum / float64(len(fifteenMinData))
	}
}

// updateProcessHistory updates historical data for a process
func (pm *ProcessMonitor) updateProcessHistory(existing, updated *ProcessInfo) {
	// Copy history from existing process
	updated.CPUUsage.History = existing.CPUUsage.History
	updated.Memory.History = existing.Memory.History
	updated.IO.History = existing.IO.History
	
	// Add new data points
	now := time.Now()
	updated.Memory.History.Add(updated.Memory.ResidentSize, now, 1.0)
	updated.IO.History.Add(updated.IO.ReadBytes+updated.IO.WriteBytes, now, 1.0)
}

// updateSystemCPU updates system-wide CPU measurements
func (pm *ProcessMonitor) updateSystemCPU() error {
	// This would read from /proc/stat to get system CPU information
	// For now, we'll implement a basic version
	pm.prevSystemCPU = SystemCPUMeasurement{
		Timestamp: time.Now(),
	}
	return nil
}

// notifyCallbacks notifies all registered callbacks about a process event
func (pm *ProcessMonitor) notifyCallbacks(process *ProcessInfo, event ProcessEvent) {
	for _, callback := range pm.callbacks {
		// Run callback in a separate goroutine to avoid blocking
		go func(cb ProcessCallback) {
			defer func() {
				if r := recover(); r != nil {
					// Log panic but don't crash the monitor
					fmt.Printf("Callback panic: %v\n", r)
				}
			}()
			cb(process, event)
		}(callback)
	}
}

