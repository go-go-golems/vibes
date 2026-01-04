package memory

import (
	"bufio"
	"context"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

// MemoryMonitor manages system memory monitoring and thrashing detection
type MemoryMonitor struct {
	// Configuration
	config MemoryConfig
	
	// Current state
	systemMemory    SystemMemory
	swapInfo        SwapInfo
	pressure        MemoryPressure
	memoryMutex     sync.RWMutex
	
	// Historical data for thrashing detection
	pageFaultHistory []PageFaultSample
	memoryHistory    []MemorySample
	historyMutex     sync.RWMutex
	
	// Control channels
	stopChan chan struct{}
	doneChan chan struct{}
	
	// Callbacks
	callbacks []MemoryCallback
}

// MemoryConfig contains configuration for memory monitoring
type MemoryConfig struct {
	UpdateInterval       time.Duration
	HistorySize          int
	ThrashingThreshold   ThrashingThresholds
	PressureThresholds   PressureThresholds
}

// ThrashingThresholds defines thresholds for thrashing detection
type ThrashingThresholds struct {
	PageFaultRate    float64 // Page faults per second
	SwapUsage        float64 // Percentage of swap used
	MemoryPressure   float64 // Memory pressure score
	IOWaitThreshold  float64 // I/O wait percentage
	MinDuration      time.Duration // Minimum duration to consider thrashing
}

// PressureThresholds defines memory pressure level thresholds
type PressureThresholds struct {
	LowMemory      float64 // Percentage of available memory
	ModerateMemory float64
	HighMemory     float64
	CriticalMemory float64
}

// SystemMemory represents system-wide memory statistics
type SystemMemory struct {
	Total        uint64    `json:"total"`         // Bytes
	Free         uint64    `json:"free"`          // Bytes
	Available    uint64    `json:"available"`     // Bytes
	Used         uint64    `json:"used"`          // Bytes
	Cached       uint64    `json:"cached"`        // Bytes
	Buffers      uint64    `json:"buffers"`       // Bytes
	Shared       uint64    `json:"shared"`        // Bytes
	SReclaimable uint64    `json:"s_reclaimable"` // Bytes
	SUnreclaim   uint64    `json:"s_unreclaim"`   // Bytes
	
	// Derived metrics
	UsagePercent      float64   `json:"usage_percent"`
	AvailablePercent  float64   `json:"available_percent"`
	
	// Metadata
	LastUpdate        time.Time `json:"last_update"`
}

// SwapInfo represents swap space information
type SwapInfo struct {
	Total        uint64    `json:"total"`         // Bytes
	Free         uint64    `json:"free"`          // Bytes
	Used         uint64    `json:"used"`          // Bytes
	Cached       uint64    `json:"cached"`        // Bytes
	
	// Derived metrics
	UsagePercent float64   `json:"usage_percent"`
	
	// Metadata
	LastUpdate   time.Time `json:"last_update"`
}

// MemoryPressure represents memory pressure and thrashing indicators
type MemoryPressure struct {
	// Pressure indicators
	Level        PressureLevel `json:"level"`
	Score        float64       `json:"score"`        // 0-100
	
	// Page fault statistics
	PageFaults   PageFaultInfo `json:"page_faults"`
	
	// Thrashing detection
	Thrashing    ThrashingInfo `json:"thrashing"`
	
	// I/O wait statistics
	IOWait       IOWaitInfo    `json:"io_wait"`
	
	// Metadata
	LastUpdate   time.Time     `json:"last_update"`
}

type PressureLevel string

const (
	PressureLevelNone     PressureLevel = "none"
	PressureLevelLow      PressureLevel = "low"
	PressureLevelModerate PressureLevel = "moderate"
	PressureLevelHigh     PressureLevel = "high"
	PressureLevelCritical PressureLevel = "critical"
)

// PageFaultInfo represents page fault statistics
type PageFaultInfo struct {
	MinorFaults  uint64    `json:"minor_faults"`
	MajorFaults  uint64    `json:"major_faults"`
	FaultRate    float64   `json:"fault_rate"`    // Faults per second
	LastUpdate   time.Time `json:"last_update"`
}

// ThrashingInfo represents thrashing detection information
type ThrashingInfo struct {
	Detected     bool              `json:"detected"`
	Confidence   float64           `json:"confidence"`    // 0-100
	Duration     time.Duration     `json:"duration"`
	Severity     ThrashingSeverity `json:"severity"`
	StartTime    time.Time         `json:"start_time"`
	
	// Contributing factors
	Factors      []ThrashingFactor `json:"factors"`
}

type ThrashingSeverity string

const (
	ThrashingSeverityNone     ThrashingSeverity = "none"
	ThrashingSeverityMild     ThrashingSeverity = "mild"
	ThrashingSeverityModerate ThrashingSeverity = "moderate"
	ThrashingSeveritySevere   ThrashingSeverity = "severe"
)

type ThrashingFactor string

const (
	FactorHighPageFaults   ThrashingFactor = "high_page_faults"
	FactorLowMemory        ThrashingFactor = "low_memory"
	FactorHighSwapUsage    ThrashingFactor = "high_swap_usage"
	FactorHighIOWait       ThrashingFactor = "high_io_wait"
)

// IOWaitInfo represents I/O wait statistics
type IOWaitInfo struct {
	Percentage   float64   `json:"percentage"`
	LastUpdate   time.Time `json:"last_update"`
}

// PageFaultSample represents a historical page fault measurement
type PageFaultSample struct {
	Timestamp    time.Time `json:"timestamp"`
	MinorFaults  uint64    `json:"minor_faults"`
	MajorFaults  uint64    `json:"major_faults"`
}

// MemorySample represents a historical memory measurement
type MemorySample struct {
	Timestamp    time.Time `json:"timestamp"`
	Available    uint64    `json:"available"`
	SwapUsed     uint64    `json:"swap_used"`
	IOWait       float64   `json:"io_wait"`
}

// MemoryCallback is called when memory events occur
type MemoryCallback func(event MemoryEvent, data interface{})

// MemoryEvent represents different types of memory events
type MemoryEvent string

const (
	MemoryEventPressureChange MemoryEvent = "pressure_change"
	MemoryEventThrashingStart MemoryEvent = "thrashing_start"
	MemoryEventThrashingEnd   MemoryEvent = "thrashing_end"
	MemoryEventLowMemory      MemoryEvent = "low_memory"
	MemoryEventCriticalMemory MemoryEvent = "critical_memory"
)

// DefaultMemoryConfig returns a default memory monitoring configuration
func DefaultMemoryConfig() MemoryConfig {
	return MemoryConfig{
		UpdateInterval: time.Second,
		HistorySize:    300, // 5 minutes at 1-second intervals
		ThrashingThreshold: ThrashingThresholds{
			PageFaultRate:   1000.0, // 1000 page faults per second
			SwapUsage:       80.0,   // 80% swap usage
			MemoryPressure:  80.0,   // 80% memory pressure
			IOWaitThreshold: 20.0,   // 20% I/O wait
			MinDuration:     5 * time.Second,
		},
		PressureThresholds: PressureThresholds{
			LowMemory:      20.0, // 20% available memory
			ModerateMemory: 10.0, // 10% available memory
			HighMemory:     5.0,  // 5% available memory
			CriticalMemory: 2.0,  // 2% available memory
		},
	}
}

// NewMemoryMonitor creates a new memory monitor
func NewMemoryMonitor(config MemoryConfig) *MemoryMonitor {
	return &MemoryMonitor{
		config:           config,
		pageFaultHistory: make([]PageFaultSample, 0, config.HistorySize),
		memoryHistory:    make([]MemorySample, 0, config.HistorySize),
		stopChan:         make(chan struct{}),
		doneChan:         make(chan struct{}),
	}
}

// AddCallback adds a callback function for memory events
func (mm *MemoryMonitor) AddCallback(callback MemoryCallback) {
	mm.callbacks = append(mm.callbacks, callback)
}

// Start begins memory monitoring
func (mm *MemoryMonitor) Start(ctx context.Context) error {
	// Initial memory reading
	if err := mm.updateMemoryInfo(); err != nil {
		return fmt.Errorf("failed to get initial memory information: %w", err)
	}

	// Start monitoring goroutine
	go mm.monitorLoop(ctx)
	
	return nil
}

// Stop stops memory monitoring
func (mm *MemoryMonitor) Stop() {
	close(mm.stopChan)
	<-mm.doneChan
}

// GetSystemMemory returns current system memory information
func (mm *MemoryMonitor) GetSystemMemory() SystemMemory {
	mm.memoryMutex.RLock()
	defer mm.memoryMutex.RUnlock()
	return mm.systemMemory
}

// GetSwapInfo returns current swap information
func (mm *MemoryMonitor) GetSwapInfo() SwapInfo {
	mm.memoryMutex.RLock()
	defer mm.memoryMutex.RUnlock()
	return mm.swapInfo
}

// GetMemoryPressure returns current memory pressure information
func (mm *MemoryMonitor) GetMemoryPressure() MemoryPressure {
	mm.memoryMutex.RLock()
	defer mm.memoryMutex.RUnlock()
	return mm.pressure
}

// monitorLoop is the main monitoring loop
func (mm *MemoryMonitor) monitorLoop(ctx context.Context) {
	defer close(mm.doneChan)
	
	ticker := time.NewTicker(mm.config.UpdateInterval)
	defer ticker.Stop()
	
	for {
		select {
		case <-ctx.Done():
			return
		case <-mm.stopChan:
			return
		case <-ticker.C:
			if err := mm.updateMemoryInfo(); err != nil {
				// Log error but continue monitoring
				fmt.Printf("Error updating memory info: %v\n", err)
			}
		}
	}
}

// updateMemoryInfo updates all memory-related information
func (mm *MemoryMonitor) updateMemoryInfo() error {
	now := time.Now()
	
	// Read system memory information
	systemMem, err := mm.readSystemMemory()
	if err != nil {
		return fmt.Errorf("failed to read system memory: %w", err)
	}
	
	// Read swap information
	swapInfo, err := mm.readSwapInfo()
	if err != nil {
		return fmt.Errorf("failed to read swap info: %w", err)
	}
	
	// Read page fault information
	pageFaults, err := mm.readPageFaults()
	if err != nil {
		return fmt.Errorf("failed to read page faults: %w", err)
	}
	
	// Read I/O wait information
	ioWait, err := mm.readIOWait()
	if err != nil {
		return fmt.Errorf("failed to read I/O wait: %w", err)
	}
	
	// Update historical data
	mm.updateHistory(now, systemMem, swapInfo, pageFaults, ioWait)
	
	// Calculate memory pressure
	pressure := mm.calculateMemoryPressure(systemMem, swapInfo, pageFaults, ioWait)
	
	// Detect thrashing
	thrashing := mm.detectThrashing()
	pressure.Thrashing = thrashing
	
	// Update current state
	mm.memoryMutex.Lock()
	prevPressureLevel := mm.pressure.Level
	prevThrashing := mm.pressure.Thrashing.Detected
	
	mm.systemMemory = systemMem
	mm.swapInfo = swapInfo
	mm.pressure = pressure
	mm.memoryMutex.Unlock()
	
	// Notify callbacks of changes
	mm.notifyCallbacks(prevPressureLevel, prevThrashing, pressure)
	
	return nil
}

// readSystemMemory reads system memory information from /proc/meminfo
func (mm *MemoryMonitor) readSystemMemory() (SystemMemory, error) {
	file, err := os.Open("/proc/meminfo")
	if err != nil {
		return SystemMemory{}, err
	}
	defer file.Close()

	var mem SystemMemory
	mem.LastUpdate = time.Now()
	
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		if len(parts) < 2 {
			continue
		}
		
		key := strings.TrimSuffix(parts[0], ":")
		value, err := strconv.ParseUint(parts[1], 10, 64)
		if err != nil {
			continue
		}
		
		// Convert from kB to bytes
		value *= 1024
		
		switch key {
		case "MemTotal":
			mem.Total = value
		case "MemFree":
			mem.Free = value
		case "MemAvailable":
			mem.Available = value
		case "Cached":
			mem.Cached = value
		case "Buffers":
			mem.Buffers = value
		case "Shmem":
			mem.Shared = value
		case "SReclaimable":
			mem.SReclaimable = value
		case "SUnreclaim":
			mem.SUnreclaim = value
		}
	}
	
	// Calculate derived metrics
	mem.Used = mem.Total - mem.Available
	if mem.Total > 0 {
		mem.UsagePercent = float64(mem.Used) / float64(mem.Total) * 100.0
		mem.AvailablePercent = float64(mem.Available) / float64(mem.Total) * 100.0
	}
	
	return mem, scanner.Err()
}

// readSwapInfo reads swap information from /proc/meminfo
func (mm *MemoryMonitor) readSwapInfo() (SwapInfo, error) {
	file, err := os.Open("/proc/meminfo")
	if err != nil {
		return SwapInfo{}, err
	}
	defer file.Close()

	var swap SwapInfo
	swap.LastUpdate = time.Now()
	
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		if len(parts) < 2 {
			continue
		}
		
		key := strings.TrimSuffix(parts[0], ":")
		value, err := strconv.ParseUint(parts[1], 10, 64)
		if err != nil {
			continue
		}
		
		// Convert from kB to bytes
		value *= 1024
		
		switch key {
		case "SwapTotal":
			swap.Total = value
		case "SwapFree":
			swap.Free = value
		case "SwapCached":
			swap.Cached = value
		}
	}
	
	// Calculate derived metrics
	swap.Used = swap.Total - swap.Free
	if swap.Total > 0 {
		swap.UsagePercent = float64(swap.Used) / float64(swap.Total) * 100.0
	}
	
	return swap, scanner.Err()
}

// readPageFaults reads page fault information from /proc/vmstat
func (mm *MemoryMonitor) readPageFaults() (PageFaultInfo, error) {
	file, err := os.Open("/proc/vmstat")
	if err != nil {
		return PageFaultInfo{}, err
	}
	defer file.Close()

	var faults PageFaultInfo
	faults.LastUpdate = time.Now()
	
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		if len(parts) < 2 {
			continue
		}
		
		key := parts[0]
		value, err := strconv.ParseUint(parts[1], 10, 64)
		if err != nil {
			continue
		}
		
		switch key {
		case "pgfault":
			faults.MinorFaults = value
		case "pgmajfault":
			faults.MajorFaults = value
		}
	}
	
	return faults, scanner.Err()
}

// readIOWait reads I/O wait information from /proc/stat
func (mm *MemoryMonitor) readIOWait() (IOWaitInfo, error) {
	file, err := os.Open("/proc/stat")
	if err != nil {
		return IOWaitInfo{}, err
	}
	defer file.Close()

	var ioWait IOWaitInfo
	ioWait.LastUpdate = time.Now()
	
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if !strings.HasPrefix(line, "cpu ") {
			continue
		}
		
		parts := strings.Fields(line)
		if len(parts) < 6 {
			continue
		}
		
		// CPU fields: user, nice, system, idle, iowait, irq, softirq, ...
		var total, iowaitTicks uint64
		for i := 1; i < len(parts) && i < 8; i++ {
			if val, err := strconv.ParseUint(parts[i], 10, 64); err == nil {
				total += val
				if i == 5 { // iowait is the 5th field (0-indexed)
					iowaitTicks = val
				}
			}
		}
		
		if total > 0 {
			ioWait.Percentage = float64(iowaitTicks) / float64(total) * 100.0
		}
		
		break
	}
	
	return ioWait, scanner.Err()
}

// updateHistory updates historical data for trend analysis
func (mm *MemoryMonitor) updateHistory(timestamp time.Time, systemMem SystemMemory, 
	swapInfo SwapInfo, pageFaults PageFaultInfo, ioWait IOWaitInfo) {
	
	mm.historyMutex.Lock()
	defer mm.historyMutex.Unlock()
	
	// Add page fault sample
	pageFaultSample := PageFaultSample{
		Timestamp:   timestamp,
		MinorFaults: pageFaults.MinorFaults,
		MajorFaults: pageFaults.MajorFaults,
	}
	mm.pageFaultHistory = append(mm.pageFaultHistory, pageFaultSample)
	
	// Add memory sample
	memorySample := MemorySample{
		Timestamp: timestamp,
		Available: systemMem.Available,
		SwapUsed:  swapInfo.Used,
		IOWait:    ioWait.Percentage,
	}
	mm.memoryHistory = append(mm.memoryHistory, memorySample)
	
	// Trim history to configured size
	if len(mm.pageFaultHistory) > mm.config.HistorySize {
		mm.pageFaultHistory = mm.pageFaultHistory[len(mm.pageFaultHistory)-mm.config.HistorySize:]
	}
	if len(mm.memoryHistory) > mm.config.HistorySize {
		mm.memoryHistory = mm.memoryHistory[len(mm.memoryHistory)-mm.config.HistorySize:]
	}
}

// calculateMemoryPressure calculates the current memory pressure level
func (mm *MemoryMonitor) calculateMemoryPressure(systemMem SystemMemory, swapInfo SwapInfo,
	pageFaults PageFaultInfo, ioWait IOWaitInfo) MemoryPressure {
	
	pressure := MemoryPressure{
		PageFaults: pageFaults,
		IOWait:     ioWait,
		LastUpdate: time.Now(),
	}
	
	// Calculate page fault rate
	mm.historyMutex.RLock()
	if len(mm.pageFaultHistory) >= 2 {
		recent := mm.pageFaultHistory[len(mm.pageFaultHistory)-1]
		previous := mm.pageFaultHistory[len(mm.pageFaultHistory)-2]
		
		timeDiff := recent.Timestamp.Sub(previous.Timestamp).Seconds()
		if timeDiff > 0 {
			totalFaultDiff := (recent.MinorFaults + recent.MajorFaults) - 
				(previous.MinorFaults + previous.MajorFaults)
			pressure.PageFaults.FaultRate = float64(totalFaultDiff) / timeDiff
		}
	}
	mm.historyMutex.RUnlock()
	
	// Determine pressure level based on available memory percentage
	availablePercent := systemMem.AvailablePercent
	thresholds := mm.config.PressureThresholds
	
	switch {
	case availablePercent <= thresholds.CriticalMemory:
		pressure.Level = PressureLevelCritical
		pressure.Score = 95.0
	case availablePercent <= thresholds.HighMemory:
		pressure.Level = PressureLevelHigh
		pressure.Score = 80.0
	case availablePercent <= thresholds.ModerateMemory:
		pressure.Level = PressureLevelModerate
		pressure.Score = 60.0
	case availablePercent <= thresholds.LowMemory:
		pressure.Level = PressureLevelLow
		pressure.Score = 30.0
	default:
		pressure.Level = PressureLevelNone
		pressure.Score = 10.0
	}
	
	// Adjust score based on other factors
	if swapInfo.UsagePercent > 50.0 {
		pressure.Score += 10.0
	}
	if pressure.PageFaults.FaultRate > mm.config.ThrashingThreshold.PageFaultRate {
		pressure.Score += 15.0
	}
	if ioWait.Percentage > mm.config.ThrashingThreshold.IOWaitThreshold {
		pressure.Score += 10.0
	}
	
	// Cap score at 100
	if pressure.Score > 100.0 {
		pressure.Score = 100.0
	}
	
	return pressure
}

// detectThrashing detects if the system is currently thrashing
func (mm *MemoryMonitor) detectThrashing() ThrashingInfo {
	mm.historyMutex.RLock()
	defer mm.historyMutex.RUnlock()
	
	thrashing := ThrashingInfo{
		Detected:   false,
		Confidence: 0.0,
		Severity:   ThrashingSeverityNone,
		Factors:    []ThrashingFactor{},
	}
	
	if len(mm.memoryHistory) < 5 || len(mm.pageFaultHistory) < 5 {
		return thrashing
	}
	
	// Analyze recent history for thrashing indicators
	recentSamples := 5
	recentMemory := mm.memoryHistory[len(mm.memoryHistory)-recentSamples:]
	recentPageFaults := mm.pageFaultHistory[len(mm.pageFaultHistory)-recentSamples:]
	
	// Check for high page fault rate
	var totalFaultRate float64
	for i := 1; i < len(recentPageFaults); i++ {
		timeDiff := recentPageFaults[i].Timestamp.Sub(recentPageFaults[i-1].Timestamp).Seconds()
		if timeDiff > 0 {
			faultDiff := (recentPageFaults[i].MinorFaults + recentPageFaults[i].MajorFaults) -
				(recentPageFaults[i-1].MinorFaults + recentPageFaults[i-1].MajorFaults)
			totalFaultRate += float64(faultDiff) / timeDiff
		}
	}
	avgFaultRate := totalFaultRate / float64(len(recentPageFaults)-1)
	
	// Check for high swap usage
	var avgSwapUsage float64
	for _, sample := range recentMemory {
		if mm.swapInfo.Total > 0 {
			avgSwapUsage += float64(sample.SwapUsed) / float64(mm.swapInfo.Total) * 100.0
		}
	}
	avgSwapUsage /= float64(len(recentMemory))
	
	// Check for low available memory
	var avgAvailablePercent float64
	for _, sample := range recentMemory {
		if mm.systemMemory.Total > 0 {
			availablePercent := float64(sample.Available) / float64(mm.systemMemory.Total) * 100.0
			avgAvailablePercent += availablePercent
		}
	}
	avgAvailablePercent /= float64(len(recentMemory))
	
	// Check for high I/O wait
	var avgIOWait float64
	for _, sample := range recentMemory {
		avgIOWait += sample.IOWait
	}
	avgIOWait /= float64(len(recentMemory))
	
	// Evaluate thrashing factors
	thresholds := mm.config.ThrashingThreshold
	confidence := 0.0
	
	if avgFaultRate > thresholds.PageFaultRate {
		thrashing.Factors = append(thrashing.Factors, FactorHighPageFaults)
		confidence += 30.0
	}
	
	if avgSwapUsage > thresholds.SwapUsage {
		thrashing.Factors = append(thrashing.Factors, FactorHighSwapUsage)
		confidence += 25.0
	}
	
	if avgAvailablePercent < (100.0 - thresholds.MemoryPressure) {
		thrashing.Factors = append(thrashing.Factors, FactorLowMemory)
		confidence += 25.0
	}
	
	if avgIOWait > thresholds.IOWaitThreshold {
		thrashing.Factors = append(thrashing.Factors, FactorHighIOWait)
		confidence += 20.0
	}
	
	// Determine if thrashing is detected
	if confidence >= 50.0 && len(thrashing.Factors) >= 2 {
		thrashing.Detected = true
		thrashing.Confidence = confidence
		
		// Determine severity
		switch {
		case confidence >= 80.0:
			thrashing.Severity = ThrashingSeveritySevere
		case confidence >= 65.0:
			thrashing.Severity = ThrashingSeverityModerate
		default:
			thrashing.Severity = ThrashingSeverityMild
		}
		
		// Set start time if not already set
		if mm.pressure.Thrashing.StartTime.IsZero() {
			thrashing.StartTime = time.Now()
		} else {
			thrashing.StartTime = mm.pressure.Thrashing.StartTime
			thrashing.Duration = time.Since(thrashing.StartTime)
		}
	}
	
	return thrashing
}

// notifyCallbacks notifies all registered callbacks about memory events
func (mm *MemoryMonitor) notifyCallbacks(prevPressureLevel PressureLevel, 
	prevThrashing bool, currentPressure MemoryPressure) {
	
	// Pressure level change
	if prevPressureLevel != currentPressure.Level {
		mm.notifyCallback(MemoryEventPressureChange, currentPressure)
	}
	
	// Thrashing state change
	if !prevThrashing && currentPressure.Thrashing.Detected {
		mm.notifyCallback(MemoryEventThrashingStart, currentPressure.Thrashing)
	} else if prevThrashing && !currentPressure.Thrashing.Detected {
		mm.notifyCallback(MemoryEventThrashingEnd, currentPressure.Thrashing)
	}
	
	// Memory level alerts
	switch currentPressure.Level {
	case PressureLevelHigh, PressureLevelCritical:
		if prevPressureLevel != PressureLevelHigh && prevPressureLevel != PressureLevelCritical {
			if currentPressure.Level == PressureLevelCritical {
				mm.notifyCallback(MemoryEventCriticalMemory, currentPressure)
			} else {
				mm.notifyCallback(MemoryEventLowMemory, currentPressure)
			}
		}
	}
}

// notifyCallback notifies a single callback
func (mm *MemoryMonitor) notifyCallback(event MemoryEvent, data interface{}) {
	for _, callback := range mm.callbacks {
		go func(cb MemoryCallback) {
			defer func() {
				if r := recover(); r != nil {
					fmt.Printf("Memory callback panic: %v\n", r)
				}
			}()
			cb(event, data)
		}(callback)
	}
}

