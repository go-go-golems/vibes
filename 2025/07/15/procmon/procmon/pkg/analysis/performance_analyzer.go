package analysis

import (
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/procmon/procmon/pkg/memory"
	"github.com/procmon/procmon/pkg/monitor"
	"github.com/procmon/procmon/pkg/power"
	"github.com/procmon/procmon/pkg/thermal"
)

// PerformanceAnalyzer analyzes system performance and generates insights
type PerformanceAnalyzer struct {
	config AnalyzerConfig
	
	// Historical data for trend analysis
	processHistory    map[int32][]ProcessSnapshot
	systemHistory     []SystemSnapshot
	alertHistory      []PerformanceAlert
	
	// Alert thresholds
	thresholds AlertThresholds
}

// AnalyzerConfig contains configuration for the performance analyzer
type AnalyzerConfig struct {
	HistorySize         int
	AnalysisInterval    time.Duration
	AlertCooldown       time.Duration
	TrendAnalysisWindow time.Duration
}

// ProcessSnapshot represents a point-in-time snapshot of a process
type ProcessSnapshot struct {
	Timestamp    time.Time
	PID          int32
	Name         string
	CPUUsage     float64
	MemoryUsage  uint64
	ThreadCount  int32
	State        monitor.ProcessState
}

// SystemSnapshot represents a point-in-time snapshot of system state
type SystemSnapshot struct {
	Timestamp         time.Time
	MemoryUsage       float64
	MemoryPressure    memory.PressureLevel
	ThrashingDetected bool
	ThermalState      thermal.ThermalState
	CPUTemperature    float64
	PowerState        power.PowerState
	CPUFrequency      float64
}

// PerformanceAlert represents a performance issue or anomaly
type PerformanceAlert struct {
	ID          string
	Timestamp   time.Time
	Type        AlertType
	Severity    AlertSeverity
	Title       string
	Description string
	Process     *ProcessInfo
	Metric      MetricInfo
	Duration    time.Duration
	Resolved    bool
	ResolvedAt  time.Time
}

type AlertType string

const (
	AlertTypeCPUUsage        AlertType = "cpu_usage"
	AlertTypeMemoryUsage     AlertType = "memory_usage"
	AlertTypeMemoryLeak      AlertType = "memory_leak"
	AlertTypeThrashing       AlertType = "thrashing"
	AlertTypeThermal         AlertType = "thermal"
	AlertTypePowerConsumption AlertType = "power_consumption"
	AlertTypeProcessSpawn    AlertType = "process_spawn"
	AlertTypeProcessCrash    AlertType = "process_crash"
	AlertTypeThreadStorm     AlertType = "thread_storm"
	AlertTypeIOBottleneck    AlertType = "io_bottleneck"
)

type AlertSeverity string

const (
	AlertSeverityInfo     AlertSeverity = "info"
	AlertSeverityWarning  AlertSeverity = "warning"
	AlertSeverityCritical AlertSeverity = "critical"
)

// ProcessInfo contains process information for alerts
type ProcessInfo struct {
	PID         int32
	Name        string
	CommandLine string
	User        string
}

// MetricInfo contains metric information for alerts
type MetricInfo struct {
	Name      string
	Value     float64
	Threshold float64
	Unit      string
}

// AlertThresholds defines thresholds for various performance alerts
type AlertThresholds struct {
	CPUUsagePercent       float64
	MemoryUsagePercent    float64
	MemoryLeakMBPerHour   float64
	ThermalTemperature    float64
	ThreadCountPerProcess int32
	ProcessSpawnRate      int
	IOWaitPercent         float64
}

// DefaultAnalyzerConfig returns a default analyzer configuration
func DefaultAnalyzerConfig() AnalyzerConfig {
	return AnalyzerConfig{
		HistorySize:         1000,
		AnalysisInterval:    30 * time.Second,
		AlertCooldown:       5 * time.Minute,
		TrendAnalysisWindow: 10 * time.Minute,
	}
}

// DefaultAlertThresholds returns default alert thresholds
func DefaultAlertThresholds() AlertThresholds {
	return AlertThresholds{
		CPUUsagePercent:       80.0,
		MemoryUsagePercent:    85.0,
		MemoryLeakMBPerHour:   100.0,
		ThermalTemperature:    80.0,
		ThreadCountPerProcess: 100,
		ProcessSpawnRate:      10,
		IOWaitPercent:         20.0,
	}
}

// NewPerformanceAnalyzer creates a new performance analyzer
func NewPerformanceAnalyzer(config AnalyzerConfig, thresholds AlertThresholds) *PerformanceAnalyzer {
	return &PerformanceAnalyzer{
		config:         config,
		thresholds:     thresholds,
		processHistory: make(map[int32][]ProcessSnapshot),
		systemHistory:  make([]SystemSnapshot, 0, config.HistorySize),
		alertHistory:   make([]PerformanceAlert, 0, config.HistorySize),
	}
}

// AnalyzeSystem performs comprehensive system performance analysis
func (pa *PerformanceAnalyzer) AnalyzeSystem(
	processes map[int32]*monitor.ProcessInfo,
	systemMemory memory.SystemMemory,
	memoryPressure memory.MemoryPressure,
	thermalState thermal.ThermalState,
	cpuTemp thermal.Temperature,
	powerState power.PowerState,
	cpuFreq power.CPUFrequencyInfo,
) []PerformanceAlert {
	
	now := time.Now()
	var alerts []PerformanceAlert
	
	// Update historical data
	pa.updateHistory(processes, systemMemory, memoryPressure, thermalState, cpuTemp, powerState, cpuFreq, now)
	
	// Analyze individual processes
	processAlerts := pa.analyzeProcesses(processes, now)
	alerts = append(alerts, processAlerts...)
	
	// Analyze system-wide metrics
	systemAlerts := pa.analyzeSystemMetrics(systemMemory, memoryPressure, thermalState, cpuTemp, powerState, now)
	alerts = append(alerts, systemAlerts...)
	
	// Analyze trends
	trendAlerts := pa.analyzeTrends(now)
	alerts = append(alerts, trendAlerts...)
	
	// Store new alerts
	pa.alertHistory = append(pa.alertHistory, alerts...)
	if len(pa.alertHistory) > pa.config.HistorySize {
		pa.alertHistory = pa.alertHistory[len(pa.alertHistory)-pa.config.HistorySize:]
	}
	
	return alerts
}

// updateHistory updates historical data for trend analysis
func (pa *PerformanceAnalyzer) updateHistory(
	processes map[int32]*monitor.ProcessInfo,
	systemMemory memory.SystemMemory,
	memoryPressure memory.MemoryPressure,
	thermalState thermal.ThermalState,
	cpuTemp thermal.Temperature,
	powerState power.PowerState,
	cpuFreq power.CPUFrequencyInfo,
	timestamp time.Time,
) {
	// Update process history
	for pid, process := range processes {
		snapshot := ProcessSnapshot{
			Timestamp:   timestamp,
			PID:         pid,
			Name:        process.Name,
			CPUUsage:    process.CPUUsage.Total,
			MemoryUsage: process.Memory.ResidentSize,
			ThreadCount: process.ThreadCount,
			State:       process.State,
		}
		
		pa.processHistory[pid] = append(pa.processHistory[pid], snapshot)
		if len(pa.processHistory[pid]) > pa.config.HistorySize {
			pa.processHistory[pid] = pa.processHistory[pid][1:]
		}
	}
	
	// Clean up history for processes that no longer exist
	for pid := range pa.processHistory {
		if _, exists := processes[pid]; !exists {
			delete(pa.processHistory, pid)
		}
	}
	
	// Update system history
	systemSnapshot := SystemSnapshot{
		Timestamp:         timestamp,
		MemoryUsage:       systemMemory.UsagePercent,
		MemoryPressure:    memoryPressure.Level,
		ThrashingDetected: memoryPressure.Thrashing.Detected,
		ThermalState:      thermalState,
		CPUTemperature:    cpuTemp.Value,
		PowerState:        powerState,
		CPUFrequency:      cpuFreq.AverageFreq,
	}
	
	pa.systemHistory = append(pa.systemHistory, systemSnapshot)
	if len(pa.systemHistory) > pa.config.HistorySize {
		pa.systemHistory = pa.systemHistory[1:]
	}
}

// analyzeProcesses analyzes individual process performance
func (pa *PerformanceAnalyzer) analyzeProcesses(processes map[int32]*monitor.ProcessInfo, timestamp time.Time) []PerformanceAlert {
	var alerts []PerformanceAlert
	
	for _, process := range processes {
		// Skip kernel processes for most alerts
		if isKernelProcess(process) {
			continue
		}
		
		// Check CPU usage
		if process.CPUUsage.Total > pa.thresholds.CPUUsagePercent {
			if !pa.hasRecentAlert(AlertTypeCPUUsage, process.PID, timestamp) {
				alert := PerformanceAlert{
					ID:        fmt.Sprintf("cpu_%d_%d", process.PID, timestamp.Unix()),
					Timestamp: timestamp,
					Type:      AlertTypeCPUUsage,
					Severity:  pa.getCPUSeverity(process.CPUUsage.Total),
					Title:     fmt.Sprintf("High CPU usage: %s", process.Name),
					Description: fmt.Sprintf("Process %s (PID %d) is using %.1f%% CPU", 
						process.Name, process.PID, process.CPUUsage.Total),
					Process: &ProcessInfo{
						PID:         process.PID,
						Name:        process.Name,
						CommandLine: process.CommandLine,
						User:        process.Username,
					},
					Metric: MetricInfo{
						Name:      "cpu_usage_percent",
						Value:     process.CPUUsage.Total,
						Threshold: pa.thresholds.CPUUsagePercent,
						Unit:      "%",
					},
				}
				alerts = append(alerts, alert)
			}
		}
		
		// Check memory usage
		memoryPercent := float64(process.Memory.ResidentSize) / float64(1024*1024*1024) * 100 // Convert to GB percentage
		if memoryPercent > pa.thresholds.MemoryUsagePercent {
			if !pa.hasRecentAlert(AlertTypeMemoryUsage, process.PID, timestamp) {
				alert := PerformanceAlert{
					ID:        fmt.Sprintf("memory_%d_%d", process.PID, timestamp.Unix()),
					Timestamp: timestamp,
					Type:      AlertTypeMemoryUsage,
					Severity:  pa.getMemorySeverity(memoryPercent),
					Title:     fmt.Sprintf("High memory usage: %s", process.Name),
					Description: fmt.Sprintf("Process %s (PID %d) is using %.1f GB of memory", 
						process.Name, process.PID, float64(process.Memory.ResidentSize)/(1024*1024*1024)),
					Process: &ProcessInfo{
						PID:         process.PID,
						Name:        process.Name,
						CommandLine: process.CommandLine,
						User:        process.Username,
					},
					Metric: MetricInfo{
						Name:      "memory_usage_gb",
						Value:     float64(process.Memory.ResidentSize) / (1024 * 1024 * 1024),
						Threshold: pa.thresholds.MemoryUsagePercent,
						Unit:      "GB",
					},
				}
				alerts = append(alerts, alert)
			}
		}
		
		// Check thread count
		if process.ThreadCount > pa.thresholds.ThreadCountPerProcess {
			if !pa.hasRecentAlert(AlertTypeThreadStorm, process.PID, timestamp) {
				alert := PerformanceAlert{
					ID:        fmt.Sprintf("threads_%d_%d", process.PID, timestamp.Unix()),
					Timestamp: timestamp,
					Type:      AlertTypeThreadStorm,
					Severity:  AlertSeverityWarning,
					Title:     fmt.Sprintf("High thread count: %s", process.Name),
					Description: fmt.Sprintf("Process %s (PID %d) has %d threads", 
						process.Name, process.PID, process.ThreadCount),
					Process: &ProcessInfo{
						PID:         process.PID,
						Name:        process.Name,
						CommandLine: process.CommandLine,
						User:        process.Username,
					},
					Metric: MetricInfo{
						Name:      "thread_count",
						Value:     float64(process.ThreadCount),
						Threshold: float64(pa.thresholds.ThreadCountPerProcess),
						Unit:      "threads",
					},
				}
				alerts = append(alerts, alert)
			}
		}
	}
	
	return alerts
}

// analyzeSystemMetrics analyzes system-wide performance metrics
func (pa *PerformanceAnalyzer) analyzeSystemMetrics(
	systemMemory memory.SystemMemory,
	memoryPressure memory.MemoryPressure,
	thermalState thermal.ThermalState,
	cpuTemp thermal.Temperature,
	powerState power.PowerState,
	timestamp time.Time,
) []PerformanceAlert {
	var alerts []PerformanceAlert
	
	// Check memory pressure
	if memoryPressure.Level == memory.PressureLevelHigh || memoryPressure.Level == memory.PressureLevelCritical {
		if !pa.hasRecentSystemAlert(AlertTypeMemoryUsage, timestamp) {
			severity := AlertSeverityWarning
			if memoryPressure.Level == memory.PressureLevelCritical {
				severity = AlertSeverityCritical
			}
			
			alert := PerformanceAlert{
				ID:        fmt.Sprintf("system_memory_%d", timestamp.Unix()),
				Timestamp: timestamp,
				Type:      AlertTypeMemoryUsage,
				Severity:  severity,
				Title:     "High system memory pressure",
				Description: fmt.Sprintf("System memory pressure is %s (%.1f%% used)", 
					memoryPressure.Level, systemMemory.UsagePercent),
				Metric: MetricInfo{
					Name:      "memory_usage_percent",
					Value:     systemMemory.UsagePercent,
					Threshold: pa.thresholds.MemoryUsagePercent,
					Unit:      "%",
				},
			}
			alerts = append(alerts, alert)
		}
	}
	
	// Check thrashing
	if memoryPressure.Thrashing.Detected {
		if !pa.hasRecentSystemAlert(AlertTypeThrashing, timestamp) {
			alert := PerformanceAlert{
				ID:        fmt.Sprintf("thrashing_%d", timestamp.Unix()),
				Timestamp: timestamp,
				Type:      AlertTypeThrashing,
				Severity:  AlertSeverityCritical,
				Title:     "Memory thrashing detected",
				Description: fmt.Sprintf("System is thrashing with %.1f%% confidence. Factors: %v", 
					memoryPressure.Thrashing.Confidence, memoryPressure.Thrashing.Factors),
				Metric: MetricInfo{
					Name:      "thrashing_confidence",
					Value:     memoryPressure.Thrashing.Confidence,
					Threshold: 50.0,
					Unit:      "%",
				},
			}
			alerts = append(alerts, alert)
		}
	}
	
	// Check thermal state
	if thermalState == thermal.ThermalStateCritical || 
	   (cpuTemp.Value > 0 && cpuTemp.Value > pa.thresholds.ThermalTemperature) {
		if !pa.hasRecentSystemAlert(AlertTypeThermal, timestamp) {
			alert := PerformanceAlert{
				ID:        fmt.Sprintf("thermal_%d", timestamp.Unix()),
				Timestamp: timestamp,
				Type:      AlertTypeThermal,
				Severity:  AlertSeverityCritical,
				Title:     "High system temperature",
				Description: fmt.Sprintf("System thermal state is %s, CPU temperature: %.1f°%s", 
					thermalState, cpuTemp.Value, cpuTemp.Unit),
				Metric: MetricInfo{
					Name:      "cpu_temperature",
					Value:     cpuTemp.Value,
					Threshold: pa.thresholds.ThermalTemperature,
					Unit:      string(cpuTemp.Unit),
				},
			}
			alerts = append(alerts, alert)
		}
	}
	
	return alerts
}

// analyzeTrends analyzes historical trends for anomaly detection
func (pa *PerformanceAnalyzer) analyzeTrends(timestamp time.Time) []PerformanceAlert {
	var alerts []PerformanceAlert
	
	// Analyze memory leak trends
	memoryLeakAlerts := pa.detectMemoryLeaks(timestamp)
	alerts = append(alerts, memoryLeakAlerts...)
	
	// Analyze process spawn rate
	spawnRateAlerts := pa.detectHighProcessSpawnRate(timestamp)
	alerts = append(alerts, spawnRateAlerts...)
	
	return alerts
}

// detectMemoryLeaks detects potential memory leaks in processes
func (pa *PerformanceAnalyzer) detectMemoryLeaks(timestamp time.Time) []PerformanceAlert {
	var alerts []PerformanceAlert
	
	cutoffTime := timestamp.Add(-pa.config.TrendAnalysisWindow)
	
	for pid, history := range pa.processHistory {
		if len(history) < 10 { // Need sufficient data points
			continue
		}
		
		// Filter recent history
		var recentHistory []ProcessSnapshot
		for _, snapshot := range history {
			if snapshot.Timestamp.After(cutoffTime) {
				recentHistory = append(recentHistory, snapshot)
			}
		}
		
		if len(recentHistory) < 5 {
			continue
		}
		
		// Calculate memory growth rate
		firstSnapshot := recentHistory[0]
		lastSnapshot := recentHistory[len(recentHistory)-1]
		
		timeDiff := lastSnapshot.Timestamp.Sub(firstSnapshot.Timestamp).Hours()
		if timeDiff < 0.5 { // Need at least 30 minutes of data
			continue
		}
		
		memoryDiff := float64(lastSnapshot.MemoryUsage-firstSnapshot.MemoryUsage) / (1024 * 1024) // Convert to MB
		growthRate := memoryDiff / timeDiff // MB per hour
		
		if growthRate > pa.thresholds.MemoryLeakMBPerHour {
			if !pa.hasRecentAlert(AlertTypeMemoryLeak, pid, timestamp) {
				alert := PerformanceAlert{
					ID:        fmt.Sprintf("memory_leak_%d_%d", pid, timestamp.Unix()),
					Timestamp: timestamp,
					Type:      AlertTypeMemoryLeak,
					Severity:  AlertSeverityWarning,
					Title:     fmt.Sprintf("Potential memory leak: %s", lastSnapshot.Name),
					Description: fmt.Sprintf("Process %s (PID %d) memory usage increased by %.1f MB/hour", 
						lastSnapshot.Name, pid, growthRate),
					Process: &ProcessInfo{
						PID:  pid,
						Name: lastSnapshot.Name,
					},
					Metric: MetricInfo{
						Name:      "memory_growth_rate",
						Value:     growthRate,
						Threshold: pa.thresholds.MemoryLeakMBPerHour,
						Unit:      "MB/hour",
					},
				}
				alerts = append(alerts, alert)
			}
		}
	}
	
	return alerts
}

// detectHighProcessSpawnRate detects unusually high process spawn rates
func (pa *PerformanceAnalyzer) detectHighProcessSpawnRate(timestamp time.Time) []PerformanceAlert {
	var alerts []PerformanceAlert
	
	if len(pa.systemHistory) < 10 {
		return alerts
	}
	
	cutoffTime := timestamp.Add(-5 * time.Minute) // Look at last 5 minutes
	
	// Count unique processes in recent history
	processNames := make(map[string]int)
	for _, snapshot := range pa.systemHistory {
		if snapshot.Timestamp.After(cutoffTime) {
			// This is a simplified approach - in reality we'd track process creation events
			for pid, history := range pa.processHistory {
				if len(history) > 0 && history[0].Timestamp.After(cutoffTime) {
					processNames[history[0].Name]++
				}
				_ = pid // Avoid unused variable warning
			}
		}
	}
	
	// Check for processes with high spawn rates
	for name, count := range processNames {
		if count > pa.thresholds.ProcessSpawnRate {
			if !pa.hasRecentSystemAlert(AlertTypeProcessSpawn, timestamp) {
				alert := PerformanceAlert{
					ID:        fmt.Sprintf("process_spawn_%s_%d", name, timestamp.Unix()),
					Timestamp: timestamp,
					Type:      AlertTypeProcessSpawn,
					Severity:  AlertSeverityWarning,
					Title:     fmt.Sprintf("High process spawn rate: %s", name),
					Description: fmt.Sprintf("Process %s spawned %d times in the last 5 minutes", name, count),
					Metric: MetricInfo{
						Name:      "spawn_rate",
						Value:     float64(count),
						Threshold: float64(pa.thresholds.ProcessSpawnRate),
						Unit:      "processes/5min",
					},
				}
				alerts = append(alerts, alert)
			}
		}
	}
	
	return alerts
}

// hasRecentAlert checks if there's a recent alert of the same type for a process
func (pa *PerformanceAnalyzer) hasRecentAlert(alertType AlertType, pid int32, timestamp time.Time) bool {
	cutoffTime := timestamp.Add(-pa.config.AlertCooldown)
	
	for _, alert := range pa.alertHistory {
		if alert.Type == alertType && 
		   alert.Process != nil && 
		   alert.Process.PID == pid && 
		   alert.Timestamp.After(cutoffTime) {
			return true
		}
	}
	
	return false
}

// hasRecentSystemAlert checks if there's a recent system-wide alert of the same type
func (pa *PerformanceAnalyzer) hasRecentSystemAlert(alertType AlertType, timestamp time.Time) bool {
	cutoffTime := timestamp.Add(-pa.config.AlertCooldown)
	
	for _, alert := range pa.alertHistory {
		if alert.Type == alertType && 
		   alert.Process == nil && 
		   alert.Timestamp.After(cutoffTime) {
			return true
		}
	}
	
	return false
}

// getCPUSeverity determines alert severity based on CPU usage
func (pa *PerformanceAnalyzer) getCPUSeverity(cpuUsage float64) AlertSeverity {
	if cpuUsage > 95.0 {
		return AlertSeverityCritical
	} else if cpuUsage > 85.0 {
		return AlertSeverityWarning
	}
	return AlertSeverityInfo
}

// getMemorySeverity determines alert severity based on memory usage
func (pa *PerformanceAnalyzer) getMemorySeverity(memoryPercent float64) AlertSeverity {
	if memoryPercent > 95.0 {
		return AlertSeverityCritical
	} else if memoryPercent > 85.0 {
		return AlertSeverityWarning
	}
	return AlertSeverityInfo
}

// GetTopProcessesByCPU returns top processes by CPU usage
func (pa *PerformanceAnalyzer) GetTopProcessesByCPU(processes map[int32]*monitor.ProcessInfo, limit int) []*monitor.ProcessInfo {
	var processList []*monitor.ProcessInfo
	for _, process := range processes {
		if !isKernelProcess(process) {
			processList = append(processList, process)
		}
	}
	
	sort.Slice(processList, func(i, j int) bool {
		return processList[i].CPUUsage.Total > processList[j].CPUUsage.Total
	})
	
	if len(processList) > limit {
		processList = processList[:limit]
	}
	
	return processList
}

// GetTopProcessesByMemory returns top processes by memory usage
func (pa *PerformanceAnalyzer) GetTopProcessesByMemory(processes map[int32]*monitor.ProcessInfo, limit int) []*monitor.ProcessInfo {
	var processList []*monitor.ProcessInfo
	for _, process := range processes {
		if !isKernelProcess(process) {
			processList = append(processList, process)
		}
	}
	
	sort.Slice(processList, func(i, j int) bool {
		return processList[i].Memory.ResidentSize > processList[j].Memory.ResidentSize
	})
	
	if len(processList) > limit {
		processList = processList[:limit]
	}
	
	return processList
}

// GetSystemHealthScore calculates an overall system health score (0-100)
func (pa *PerformanceAnalyzer) GetSystemHealthScore(
	systemMemory memory.SystemMemory,
	memoryPressure memory.MemoryPressure,
	thermalState thermal.ThermalState,
	cpuTemp thermal.Temperature,
) float64 {
	score := 100.0
	
	// Memory health (30% weight)
	memoryScore := 100.0 - systemMemory.UsagePercent
	if memoryPressure.Thrashing.Detected {
		memoryScore -= 30.0
	}
	score = score*0.7 + memoryScore*0.3
	
	// Thermal health (20% weight)
	thermalScore := 100.0
	if thermalState == thermal.ThermalStateWarning {
		thermalScore = 70.0
	} else if thermalState == thermal.ThermalStateCritical {
		thermalScore = 30.0
	}
	if cpuTemp.Value > 0 && cpuTemp.Value > pa.thresholds.ThermalTemperature {
		thermalScore -= 20.0
	}
	score = score*0.8 + thermalScore*0.2
	
	// Ensure score is within bounds
	if score < 0 {
		score = 0
	}
	if score > 100 {
		score = 100
	}
	
	return score
}

// GetRecentAlerts returns recent alerts within the specified time window
func (pa *PerformanceAnalyzer) GetRecentAlerts(since time.Time) []PerformanceAlert {
	var recentAlerts []PerformanceAlert
	
	for _, alert := range pa.alertHistory {
		if alert.Timestamp.After(since) {
			recentAlerts = append(recentAlerts, alert)
		}
	}
	
	// Sort by timestamp (newest first)
	sort.Slice(recentAlerts, func(i, j int) bool {
		return recentAlerts[i].Timestamp.After(recentAlerts[j].Timestamp)
	})
	
	return recentAlerts
}

// Helper function to check if a process is a kernel process
func isKernelProcess(process *monitor.ProcessInfo) bool {
	return len(process.Name) > 2 && 
		   process.Name[0] == '[' && 
		   process.Name[len(process.Name)-1] == ']'
}

// GenerateSystemReport generates a comprehensive system performance report
func (pa *PerformanceAnalyzer) GenerateSystemReport(
	processes map[int32]*monitor.ProcessInfo,
	systemMemory memory.SystemMemory,
	memoryPressure memory.MemoryPressure,
	thermalState thermal.ThermalState,
	cpuTemp thermal.Temperature,
	powerState power.PowerState,
	cpuFreq power.CPUFrequencyInfo,
) string {
	var report strings.Builder
	
	report.WriteString("System Performance Report\n")
	report.WriteString("========================\n\n")
	
	// System overview
	healthScore := pa.GetSystemHealthScore(systemMemory, memoryPressure, thermalState, cpuTemp)
	report.WriteString(fmt.Sprintf("Overall Health Score: %.1f/100\n", healthScore))
	report.WriteString(fmt.Sprintf("Timestamp: %s\n\n", time.Now().Format("2006-01-02 15:04:05")))
	
	// Memory status
	report.WriteString("Memory Status:\n")
	report.WriteString(fmt.Sprintf("  Usage: %.1f%% (%.1f GB / %.1f GB)\n", 
		systemMemory.UsagePercent,
		float64(systemMemory.Used)/(1024*1024*1024),
		float64(systemMemory.Total)/(1024*1024*1024)))
	report.WriteString(fmt.Sprintf("  Pressure: %s\n", memoryPressure.Level))
	if memoryPressure.Thrashing.Detected {
		report.WriteString(fmt.Sprintf("  Thrashing: DETECTED (%.1f%% confidence)\n", memoryPressure.Thrashing.Confidence))
	}
	report.WriteString("\n")
	
	// Thermal status
	report.WriteString("Thermal Status:\n")
	report.WriteString(fmt.Sprintf("  State: %s\n", thermalState))
	if cpuTemp.Value > 0 {
		report.WriteString(fmt.Sprintf("  CPU Temperature: %.1f°%s\n", cpuTemp.Value, cpuTemp.Unit))
	}
	report.WriteString("\n")
	
	// Power status
	report.WriteString("Power Status:\n")
	acStatus := "Disconnected"
	if powerState.ACConnected {
		acStatus = "Connected"
	}
	report.WriteString(fmt.Sprintf("  AC Power: %s\n", acStatus))
	report.WriteString(fmt.Sprintf("  CPU Governor: %s\n", cpuFreq.Governor))
	report.WriteString(fmt.Sprintf("  CPU Frequency: %.0f MHz (avg)\n", cpuFreq.AverageFreq))
	report.WriteString("\n")
	
	// Top processes by CPU
	topCPU := pa.GetTopProcessesByCPU(processes, 5)
	report.WriteString("Top Processes by CPU:\n")
	for i, process := range topCPU {
		report.WriteString(fmt.Sprintf("  %d. %s (PID %d): %.1f%%\n", 
			i+1, process.Name, process.PID, process.CPUUsage.Total))
	}
	report.WriteString("\n")
	
	// Top processes by memory
	topMemory := pa.GetTopProcessesByMemory(processes, 5)
	report.WriteString("Top Processes by Memory:\n")
	for i, process := range topMemory {
		memoryMB := float64(process.Memory.ResidentSize) / (1024 * 1024)
		report.WriteString(fmt.Sprintf("  %d. %s (PID %d): %.1f MB\n", 
			i+1, process.Name, process.PID, memoryMB))
	}
	report.WriteString("\n")
	
	// Recent alerts
	recentAlerts := pa.GetRecentAlerts(time.Now().Add(-time.Hour))
	if len(recentAlerts) > 0 {
		report.WriteString("Recent Alerts (last hour):\n")
		for _, alert := range recentAlerts {
			report.WriteString(fmt.Sprintf("  [%s] %s: %s\n", 
				alert.Severity, alert.Timestamp.Format("15:04:05"), alert.Title))
		}
	} else {
		report.WriteString("No recent alerts.\n")
	}
	
	return report.String()
}

