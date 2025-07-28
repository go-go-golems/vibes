package power

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"
)

// PowerMonitor manages power, battery, and CPU frequency monitoring
type PowerMonitor struct {
	// Configuration
	config PowerConfig
	
	// Current state
	batteries    map[string]*BatteryInfo
	cpuFreq      CPUFrequencyInfo
	powerState   PowerState
	powerMutex   sync.RWMutex
	
	// Control channels
	stopChan chan struct{}
	doneChan chan struct{}
	
	// Callbacks
	callbacks []PowerCallback
}

// PowerConfig contains configuration for power monitoring
type PowerConfig struct {
	UpdateInterval    time.Duration
	HistorySize       int
	BatteryPaths      []string
	CPUFreqPaths      []string
	AlertThresholds   PowerAlertThresholds
}

// PowerAlertThresholds defines power-related alert thresholds
type PowerAlertThresholds struct {
	LowBattery      float64 // Battery percentage
	CriticalBattery float64 // Battery percentage
	HighTemperature float64 // CPU temperature for throttling
}

// BatteryInfo represents information about a specific battery
type BatteryInfo struct {
	ID           string        `json:"id"`
	Name         string        `json:"name"`
	
	// Current state
	Status       BatteryStatus `json:"status"`
	Capacity     float64       `json:"capacity"`     // Percentage
	Energy       EnergyInfo    `json:"energy"`
	Voltage      float64       `json:"voltage"`      // Volts
	Current      float64       `json:"current"`      // Amperes
	Power        float64       `json:"power"`        // Watts
	
	// Health information
	Health       float64       `json:"health"`       // Percentage
	CycleCount   int32         `json:"cycle_count"`
	
	// Time estimates
	TimeToEmpty  time.Duration `json:"time_to_empty"`
	TimeToFull   time.Duration `json:"time_to_full"`
	
	// Historical data
	History      []BatteryReading `json:"history"`
	
	// Metadata
	Technology   string        `json:"technology"`
	Manufacturer string        `json:"manufacturer"`
	Model        string        `json:"model"`
	SerialNumber string        `json:"serial_number"`
	LastUpdate   time.Time     `json:"last_update"`
	Available    bool          `json:"available"`
}

// EnergyInfo represents energy-related battery information
type EnergyInfo struct {
	Current      float64 `json:"current"`      // Wh
	Full         float64 `json:"full"`         // Wh
	Design       float64 `json:"design"`       // Wh
	Rate         float64 `json:"rate"`         // W
}

type BatteryStatus string

const (
	BatteryStatusCharging    BatteryStatus = "charging"
	BatteryStatusDischarging BatteryStatus = "discharging"
	BatteryStatusFull        BatteryStatus = "full"
	BatteryStatusNotCharging BatteryStatus = "not_charging"
	BatteryStatusUnknown     BatteryStatus = "unknown"
)

// CPUFrequencyInfo represents CPU frequency and governor information
type CPUFrequencyInfo struct {
	// Per-core frequency information
	Cores        []CPUCoreFreq `json:"cores"`
	
	// System-wide frequency statistics
	AverageFreq  float64       `json:"average_freq"`
	MinFreq      float64       `json:"min_freq"`
	MaxFreq      float64       `json:"max_freq"`
	
	// Governor information
	Governor     string        `json:"governor"`
	AvailableGov []string      `json:"available_governors"`
	
	// Historical data
	History      []FrequencyReading `json:"history"`
	
	// Metadata
	LastUpdate   time.Time     `json:"last_update"`
}

// CPUCoreFreq represents frequency information for a single CPU core
type CPUCoreFreq struct {
	CoreID       int32   `json:"core_id"`
	CurrentFreq  float64 `json:"current_freq"`  // MHz
	MinFreq      float64 `json:"min_freq"`      // MHz
	MaxFreq      float64 `json:"max_freq"`      // MHz
	Governor     string  `json:"governor"`
	Available    bool    `json:"available"`
}

// PowerState represents overall system power state
type PowerState struct {
	ACConnected     bool      `json:"ac_connected"`
	BatteryPresent  bool      `json:"battery_present"`
	PowerSaving     bool      `json:"power_saving"`
	ThermalState    string    `json:"thermal_state"`
	LastUpdate      time.Time `json:"last_update"`
}

// BatteryReading represents a historical battery reading
type BatteryReading struct {
	Timestamp time.Time `json:"timestamp"`
	Capacity  float64   `json:"capacity"`
	Power     float64   `json:"power"`
	Voltage   float64   `json:"voltage"`
}

// FrequencyReading represents a historical frequency reading
type FrequencyReading struct {
	Timestamp   time.Time `json:"timestamp"`
	AverageFreq float64   `json:"average_freq"`
	Governor    string    `json:"governor"`
}

// PowerCallback is called when power events occur
type PowerCallback func(event PowerEvent, data interface{})

// PowerEvent represents different types of power events
type PowerEvent string

const (
	PowerEventBatteryLow      PowerEvent = "battery_low"
	PowerEventBatteryCritical PowerEvent = "battery_critical"
	PowerEventACConnected     PowerEvent = "ac_connected"
	PowerEventACDisconnected  PowerEvent = "ac_disconnected"
	PowerEventGovernorChange  PowerEvent = "governor_change"
	PowerEventFrequencyChange PowerEvent = "frequency_change"
)

// DefaultPowerConfig returns a default power monitoring configuration
func DefaultPowerConfig() PowerConfig {
	return PowerConfig{
		UpdateInterval: 2 * time.Second,
		HistorySize:    150, // 5 minutes at 2-second intervals
		BatteryPaths: []string{
			"/sys/class/power_supply",
		},
		CPUFreqPaths: []string{
			"/sys/devices/system/cpu",
		},
		AlertThresholds: PowerAlertThresholds{
			LowBattery:      20.0,
			CriticalBattery: 5.0,
			HighTemperature: 80.0,
		},
	}
}

// NewPowerMonitor creates a new power monitor
func NewPowerMonitor(config PowerConfig) *PowerMonitor {
	return &PowerMonitor{
		config:     config,
		batteries:  make(map[string]*BatteryInfo),
		stopChan:   make(chan struct{}),
		doneChan:   make(chan struct{}),
	}
}

// AddCallback adds a callback function for power events
func (pm *PowerMonitor) AddCallback(callback PowerCallback) {
	pm.callbacks = append(pm.callbacks, callback)
}

// Start begins power monitoring
func (pm *PowerMonitor) Start(ctx context.Context) error {
	// Discover power supplies and CPU frequency info
	if err := pm.discoverPowerSources(); err != nil {
		return fmt.Errorf("failed to discover power sources: %w", err)
	}

	// Initial readings
	if err := pm.updatePowerInfo(); err != nil {
		return fmt.Errorf("failed to get initial power information: %w", err)
	}

	// Start monitoring goroutine
	go pm.monitorLoop(ctx)
	
	return nil
}

// Stop stops power monitoring
func (pm *PowerMonitor) Stop() {
	close(pm.stopChan)
	<-pm.doneChan
}

// GetBatteries returns a copy of all battery information
func (pm *PowerMonitor) GetBatteries() map[string]*BatteryInfo {
	pm.powerMutex.RLock()
	defer pm.powerMutex.RUnlock()
	
	result := make(map[string]*BatteryInfo)
	for id, battery := range pm.batteries {
		batteryCopy := *battery
		result[id] = &batteryCopy
	}
	
	return result
}

// GetCPUFrequency returns current CPU frequency information
func (pm *PowerMonitor) GetCPUFrequency() CPUFrequencyInfo {
	pm.powerMutex.RLock()
	defer pm.powerMutex.RUnlock()
	return pm.cpuFreq
}

// GetPowerState returns current power state
func (pm *PowerMonitor) GetPowerState() PowerState {
	pm.powerMutex.RLock()
	defer pm.powerMutex.RUnlock()
	return pm.powerState
}

// monitorLoop is the main monitoring loop
func (pm *PowerMonitor) monitorLoop(ctx context.Context) {
	defer close(pm.doneChan)
	
	ticker := time.NewTicker(pm.config.UpdateInterval)
	defer ticker.Stop()
	
	// Periodic discovery (every 30 seconds)
	discoveryTicker := time.NewTicker(30 * time.Second)
	defer discoveryTicker.Stop()
	
	for {
		select {
		case <-ctx.Done():
			return
		case <-pm.stopChan:
			return
		case <-ticker.C:
			if err := pm.updatePowerInfo(); err != nil {
				fmt.Printf("Error updating power info: %v\n", err)
			}
		case <-discoveryTicker.C:
			if err := pm.discoverPowerSources(); err != nil {
				fmt.Printf("Error discovering power sources: %v\n", err)
			}
		}
	}
}

// discoverPowerSources discovers available power supplies and CPU frequency controls
func (pm *PowerMonitor) discoverPowerSources() error {
	// Discover batteries
	if err := pm.discoverBatteries(); err != nil {
		fmt.Printf("Error discovering batteries: %v\n", err)
	}
	
	// Discover CPU frequency controls
	if err := pm.discoverCPUFreq(); err != nil {
		fmt.Printf("Error discovering CPU frequency controls: %v\n", err)
	}
	
	return nil
}

// discoverBatteries discovers available batteries
func (pm *PowerMonitor) discoverBatteries() error {
	powerSupplyPath := "/sys/class/power_supply"
	
	entries, err := os.ReadDir(powerSupplyPath)
	if err != nil {
		return err
	}
	
	pm.powerMutex.Lock()
	defer pm.powerMutex.Unlock()
	
	for _, entry := range entries {
		supplyPath := filepath.Join(powerSupplyPath, entry.Name())
		
		// Check if this is a battery
		typeFile := filepath.Join(supplyPath, "type")
		typeData, err := os.ReadFile(typeFile)
		if err != nil {
			continue
		}
		
		supplyType := strings.TrimSpace(string(typeData))
		if supplyType != "Battery" {
			continue
		}
		
		// Create or update battery info
		if _, exists := pm.batteries[entry.Name()]; !exists {
			battery := &BatteryInfo{
				ID:        entry.Name(),
				Name:      entry.Name(),
				Available: true,
				History:   make([]BatteryReading, 0, pm.config.HistorySize),
			}
			
			// Read static battery information
			pm.readBatteryMetadata(battery, supplyPath)
			pm.batteries[entry.Name()] = battery
		}
	}
	
	return nil
}

// readBatteryMetadata reads static battery metadata
func (pm *PowerMonitor) readBatteryMetadata(battery *BatteryInfo, supplyPath string) {
	// Read manufacturer
	if data, err := os.ReadFile(filepath.Join(supplyPath, "manufacturer")); err == nil {
		battery.Manufacturer = strings.TrimSpace(string(data))
	}
	
	// Read model
	if data, err := os.ReadFile(filepath.Join(supplyPath, "model_name")); err == nil {
		battery.Model = strings.TrimSpace(string(data))
	}
	
	// Read technology
	if data, err := os.ReadFile(filepath.Join(supplyPath, "technology")); err == nil {
		battery.Technology = strings.TrimSpace(string(data))
	}
	
	// Read serial number
	if data, err := os.ReadFile(filepath.Join(supplyPath, "serial_number")); err == nil {
		battery.SerialNumber = strings.TrimSpace(string(data))
	}
	
	// Read cycle count
	if data, err := os.ReadFile(filepath.Join(supplyPath, "cycle_count")); err == nil {
		if count, err := strconv.ParseInt(strings.TrimSpace(string(data)), 10, 32); err == nil {
			battery.CycleCount = int32(count)
		}
	}
}

// discoverCPUFreq discovers CPU frequency controls
func (pm *PowerMonitor) discoverCPUFreq() error {
	cpuPath := "/sys/devices/system/cpu"
	
	entries, err := os.ReadDir(cpuPath)
	if err != nil {
		return err
	}
	
	pm.powerMutex.Lock()
	defer pm.powerMutex.Unlock()
	
	var cores []CPUCoreFreq
	
	for _, entry := range entries {
		if !strings.HasPrefix(entry.Name(), "cpu") {
			continue
		}
		
		// Extract CPU number
		cpuNumStr := strings.TrimPrefix(entry.Name(), "cpu")
		cpuNum, err := strconv.ParseInt(cpuNumStr, 10, 32)
		if err != nil {
			continue
		}
		
		cpuFreqPath := filepath.Join(cpuPath, entry.Name(), "cpufreq")
		if _, err := os.Stat(cpuFreqPath); os.IsNotExist(err) {
			continue
		}
		
		core := CPUCoreFreq{
			CoreID:    int32(cpuNum),
			Available: true,
		}
		
		// Read frequency limits
		if data, err := os.ReadFile(filepath.Join(cpuFreqPath, "cpuinfo_min_freq")); err == nil {
			if freq, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				core.MinFreq = freq / 1000.0 // Convert kHz to MHz
			}
		}
		
		if data, err := os.ReadFile(filepath.Join(cpuFreqPath, "cpuinfo_max_freq")); err == nil {
			if freq, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				core.MaxFreq = freq / 1000.0 // Convert kHz to MHz
			}
		}
		
		// Read governor
		if data, err := os.ReadFile(filepath.Join(cpuFreqPath, "scaling_governor")); err == nil {
			core.Governor = strings.TrimSpace(string(data))
		}
		
		cores = append(cores, core)
	}
	
	pm.cpuFreq.Cores = cores
	
	// Read available governors (from first core)
	if len(cores) > 0 {
		firstCPUPath := filepath.Join(cpuPath, "cpu0", "cpufreq", "scaling_available_governors")
		if data, err := os.ReadFile(firstCPUPath); err == nil {
			governors := strings.Fields(strings.TrimSpace(string(data)))
			pm.cpuFreq.AvailableGov = governors
		}
	}
	
	return nil
}

// updatePowerInfo updates all power-related information
func (pm *PowerMonitor) updatePowerInfo() error {
	now := time.Now()
	
	// Update battery information
	if err := pm.updateBatteries(now); err != nil {
		fmt.Printf("Error updating batteries: %v\n", err)
	}
	
	// Update CPU frequency information
	if err := pm.updateCPUFreq(now); err != nil {
		fmt.Printf("Error updating CPU frequency: %v\n", err)
	}
	
	// Update power state
	pm.updatePowerState(now)
	
	return nil
}

// updateBatteries updates battery information
func (pm *PowerMonitor) updateBatteries(now time.Time) error {
	pm.powerMutex.Lock()
	defer pm.powerMutex.Unlock()
	
	powerSupplyPath := "/sys/class/power_supply"
	
	for id, battery := range pm.batteries {
		supplyPath := filepath.Join(powerSupplyPath, id)
		
		// Read current battery status
		if data, err := os.ReadFile(filepath.Join(supplyPath, "status")); err == nil {
			status := strings.TrimSpace(string(data))
			switch strings.ToLower(status) {
			case "charging":
				battery.Status = BatteryStatusCharging
			case "discharging":
				battery.Status = BatteryStatusDischarging
			case "full":
				battery.Status = BatteryStatusFull
			case "not charging":
				battery.Status = BatteryStatusNotCharging
			default:
				battery.Status = BatteryStatusUnknown
			}
		}
		
		// Read capacity
		if data, err := os.ReadFile(filepath.Join(supplyPath, "capacity")); err == nil {
			if capacity, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				battery.Capacity = capacity
			}
		}
		
		// Read voltage
		if data, err := os.ReadFile(filepath.Join(supplyPath, "voltage_now")); err == nil {
			if voltage, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				battery.Voltage = voltage / 1000000.0 // Convert µV to V
			}
		}
		
		// Read current
		if data, err := os.ReadFile(filepath.Join(supplyPath, "current_now")); err == nil {
			if current, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				battery.Current = current / 1000000.0 // Convert µA to A
			}
		}
		
		// Calculate power
		battery.Power = battery.Voltage * battery.Current
		
		// Read energy information
		if data, err := os.ReadFile(filepath.Join(supplyPath, "energy_now")); err == nil {
			if energy, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				battery.Energy.Current = energy / 1000000.0 // Convert µWh to Wh
			}
		}
		
		if data, err := os.ReadFile(filepath.Join(supplyPath, "energy_full")); err == nil {
			if energy, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				battery.Energy.Full = energy / 1000000.0 // Convert µWh to Wh
			}
		}
		
		if data, err := os.ReadFile(filepath.Join(supplyPath, "energy_full_design")); err == nil {
			if energy, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				battery.Energy.Design = energy / 1000000.0 // Convert µWh to Wh
			}
		}
		
		// Calculate health
		if battery.Energy.Design > 0 {
			battery.Health = (battery.Energy.Full / battery.Energy.Design) * 100.0
		}
		
		// Calculate time estimates
		if battery.Power > 0 {
			if battery.Status == BatteryStatusDischarging {
				battery.TimeToEmpty = time.Duration(battery.Energy.Current/battery.Power*3600) * time.Second
			} else if battery.Status == BatteryStatusCharging {
				remaining := battery.Energy.Full - battery.Energy.Current
				battery.TimeToFull = time.Duration(remaining/battery.Power*3600) * time.Second
			}
		}
		
		battery.LastUpdate = now
		
		// Add to history
		reading := BatteryReading{
			Timestamp: now,
			Capacity:  battery.Capacity,
			Power:     battery.Power,
			Voltage:   battery.Voltage,
		}
		battery.History = append(battery.History, reading)
		
		// Trim history
		if len(battery.History) > pm.config.HistorySize {
			battery.History = battery.History[len(battery.History)-pm.config.HistorySize:]
		}
		
		// Check for alerts
		pm.checkBatteryAlerts(battery)
	}
	
	return nil
}

// updateCPUFreq updates CPU frequency information
func (pm *PowerMonitor) updateCPUFreq(now time.Time) error {
	pm.powerMutex.Lock()
	defer pm.powerMutex.Unlock()
	
	cpuPath := "/sys/devices/system/cpu"
	var totalFreq float64
	var activeCount int
	
	for i := range pm.cpuFreq.Cores {
		core := &pm.cpuFreq.Cores[i]
		
		cpuFreqPath := filepath.Join(cpuPath, fmt.Sprintf("cpu%d", core.CoreID), "cpufreq")
		
		// Read current frequency
		if data, err := os.ReadFile(filepath.Join(cpuFreqPath, "scaling_cur_freq")); err == nil {
			if freq, err := strconv.ParseFloat(strings.TrimSpace(string(data)), 64); err == nil {
				core.CurrentFreq = freq / 1000.0 // Convert kHz to MHz
				totalFreq += core.CurrentFreq
				activeCount++
			}
		}
		
		// Read governor
		if data, err := os.ReadFile(filepath.Join(cpuFreqPath, "scaling_governor")); err == nil {
			newGovernor := strings.TrimSpace(string(data))
			if core.Governor != newGovernor {
				oldGovernor := core.Governor
				core.Governor = newGovernor
				pm.notifyCallback(PowerEventGovernorChange, map[string]interface{}{
					"core":         core.CoreID,
					"old_governor": oldGovernor,
					"new_governor": newGovernor,
				})
			}
		}
	}
	
	// Calculate average frequency
	if activeCount > 0 {
		newAvgFreq := totalFreq / float64(activeCount)
		if abs(pm.cpuFreq.AverageFreq-newAvgFreq) > 50.0 { // Significant change (50 MHz)
			pm.notifyCallback(PowerEventFrequencyChange, map[string]interface{}{
				"old_frequency": pm.cpuFreq.AverageFreq,
				"new_frequency": newAvgFreq,
			})
		}
		pm.cpuFreq.AverageFreq = newAvgFreq
	}
	
	// Calculate min/max frequencies
	if len(pm.cpuFreq.Cores) > 0 {
		pm.cpuFreq.MinFreq = pm.cpuFreq.Cores[0].CurrentFreq
		pm.cpuFreq.MaxFreq = pm.cpuFreq.Cores[0].CurrentFreq
		
		for _, core := range pm.cpuFreq.Cores {
			if core.CurrentFreq < pm.cpuFreq.MinFreq {
				pm.cpuFreq.MinFreq = core.CurrentFreq
			}
			if core.CurrentFreq > pm.cpuFreq.MaxFreq {
				pm.cpuFreq.MaxFreq = core.CurrentFreq
			}
		}
		
		// Set overall governor (use first core's governor)
		pm.cpuFreq.Governor = pm.cpuFreq.Cores[0].Governor
	}
	
	pm.cpuFreq.LastUpdate = now
	
	// Add to history
	reading := FrequencyReading{
		Timestamp:   now,
		AverageFreq: pm.cpuFreq.AverageFreq,
		Governor:    pm.cpuFreq.Governor,
	}
	pm.cpuFreq.History = append(pm.cpuFreq.History, reading)
	
	// Trim history
	if len(pm.cpuFreq.History) > pm.config.HistorySize {
		pm.cpuFreq.History = pm.cpuFreq.History[len(pm.cpuFreq.History)-pm.config.HistorySize:]
	}
	
	return nil
}

// updatePowerState updates overall power state
func (pm *PowerMonitor) updatePowerState(now time.Time) {
	pm.powerMutex.Lock()
	defer pm.powerMutex.Unlock()
	
	prevACConnected := pm.powerState.ACConnected
	
	// Check for AC adapter
	pm.powerState.ACConnected = false
	pm.powerState.BatteryPresent = len(pm.batteries) > 0
	
	// Check AC adapters
	powerSupplyPath := "/sys/class/power_supply"
	if entries, err := os.ReadDir(powerSupplyPath); err == nil {
		for _, entry := range entries {
			supplyPath := filepath.Join(powerSupplyPath, entry.Name())
			
			// Check if this is an AC adapter
			if typeData, err := os.ReadFile(filepath.Join(supplyPath, "type")); err == nil {
				supplyType := strings.TrimSpace(string(typeData))
				if supplyType == "Mains" || supplyType == "ADP1" {
					if onlineData, err := os.ReadFile(filepath.Join(supplyPath, "online")); err == nil {
						online := strings.TrimSpace(string(onlineData))
						if online == "1" {
							pm.powerState.ACConnected = true
							break
						}
					}
				}
			}
		}
	}
	
	// Detect power saving mode (simplified heuristic)
	pm.powerState.PowerSaving = !pm.powerState.ACConnected && pm.powerState.BatteryPresent
	
	pm.powerState.LastUpdate = now
	
	// Notify of AC connection changes
	if prevACConnected != pm.powerState.ACConnected {
		if pm.powerState.ACConnected {
			pm.notifyCallback(PowerEventACConnected, pm.powerState)
		} else {
			pm.notifyCallback(PowerEventACDisconnected, pm.powerState)
		}
	}
}

// checkBatteryAlerts checks for battery-related alerts
func (pm *PowerMonitor) checkBatteryAlerts(battery *BatteryInfo) {
	if battery.Capacity <= pm.config.AlertThresholds.CriticalBattery {
		pm.notifyCallback(PowerEventBatteryCritical, battery)
	} else if battery.Capacity <= pm.config.AlertThresholds.LowBattery {
		pm.notifyCallback(PowerEventBatteryLow, battery)
	}
}

// notifyCallback notifies all callbacks of a power event
func (pm *PowerMonitor) notifyCallback(event PowerEvent, data interface{}) {
	for _, callback := range pm.callbacks {
		go func(cb PowerCallback) {
			defer func() {
				if r := recover(); r != nil {
					fmt.Printf("Power callback panic: %v\n", r)
				}
			}()
			cb(event, data)
		}(callback)
	}
}

// abs returns the absolute value of a float64
func abs(x float64) float64 {
	if x < 0 {
		return -x
	}
	return x
}

