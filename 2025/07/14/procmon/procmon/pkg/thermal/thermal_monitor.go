package thermal

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

// ThermalMonitor manages system temperature monitoring
type ThermalMonitor struct {
	// Configuration
	config ThermalConfig
	
	// Current state
	sensors      map[string]*ThermalSensor
	overallState ThermalState
	sensorMutex  sync.RWMutex
	
	// Control channels
	stopChan chan struct{}
	doneChan chan struct{}
	
	// Callbacks
	callbacks []ThermalCallback
}

// ThermalConfig contains configuration for thermal monitoring
type ThermalConfig struct {
	UpdateInterval    time.Duration
	HistorySize       int
	TemperatureUnit   TemperatureUnit
	AlertThresholds   AlertThresholds
	SensorPaths       []string // Additional sensor paths to scan
}

// AlertThresholds defines temperature alert thresholds
type AlertThresholds struct {
	Warning  Temperature `json:"warning"`
	Critical Temperature `json:"critical"`
}

// ThermalSensor represents an individual temperature sensor
type ThermalSensor struct {
	ID          string      `json:"id"`
	Name        string      `json:"name"`
	Type        SensorType  `json:"type"`
	Location    string      `json:"location"`
	
	// Current reading
	Temperature Temperature `json:"temperature"`
	
	// Thresholds
	Critical    Temperature `json:"critical"`
	Warning     Temperature `json:"warning"`
	
	// Historical data
	History     []TemperatureReading `json:"history"`
	
	// Sensor metadata
	Accuracy    float64     `json:"accuracy"`
	Available   bool        `json:"available"`
	LastUpdate  time.Time   `json:"last_update"`
	
	// Source information
	SourcePath  string      `json:"source_path"`
	SourceType  SourceType  `json:"source_type"`
}

// Temperature represents a temperature value with unit information
type Temperature struct {
	Value float64        `json:"value"`
	Unit  TemperatureUnit `json:"unit"`
}

type TemperatureUnit string

const (
	Celsius    TemperatureUnit = "C"
	Fahrenheit TemperatureUnit = "F"
	Kelvin     TemperatureUnit = "K"
)

type SensorType string

const (
	SensorTypeCPU     SensorType = "cpu"
	SensorTypeGPU     SensorType = "gpu"
	SensorTypeSystem  SensorType = "system"
	SensorTypeAmbient SensorType = "ambient"
	SensorTypeFan     SensorType = "fan"
	SensorTypeUnknown SensorType = "unknown"
)

type SourceType string

const (
	SourceTypeHWMon   SourceType = "hwmon"
	SourceTypeThermal SourceType = "thermal"
	SourceTypeACPI    SourceType = "acpi"
)

type ThermalState string

const (
	ThermalStateNormal   ThermalState = "normal"
	ThermalStateWarning  ThermalState = "warning"
	ThermalStateCritical ThermalState = "critical"
)

// TemperatureReading represents a historical temperature reading
type TemperatureReading struct {
	Timestamp   time.Time   `json:"timestamp"`
	Temperature Temperature `json:"temperature"`
	Quality     float64     `json:"quality"`
}

// ThermalCallback is called when thermal events occur
type ThermalCallback func(event ThermalEvent, sensor *ThermalSensor)

// ThermalEvent represents different types of thermal events
type ThermalEvent string

const (
	ThermalEventTemperatureChange ThermalEvent = "temperature_change"
	ThermalEventWarningThreshold  ThermalEvent = "warning_threshold"
	ThermalEventCriticalThreshold ThermalEvent = "critical_threshold"
	ThermalEventSensorAdded       ThermalEvent = "sensor_added"
	ThermalEventSensorRemoved     ThermalEvent = "sensor_removed"
)

// DefaultThermalConfig returns a default thermal monitoring configuration
func DefaultThermalConfig() ThermalConfig {
	return ThermalConfig{
		UpdateInterval:  2 * time.Second,
		HistorySize:     150, // 5 minutes at 2-second intervals
		TemperatureUnit: Celsius,
		AlertThresholds: AlertThresholds{
			Warning:  Temperature{Value: 70.0, Unit: Celsius},
			Critical: Temperature{Value: 85.0, Unit: Celsius},
		},
		SensorPaths: []string{
			"/sys/class/thermal",
			"/sys/class/hwmon",
		},
	}
}

// NewThermalMonitor creates a new thermal monitor
func NewThermalMonitor(config ThermalConfig) *ThermalMonitor {
	return &ThermalMonitor{
		config:       config,
		sensors:      make(map[string]*ThermalSensor),
		overallState: ThermalStateNormal,
		stopChan:     make(chan struct{}),
		doneChan:     make(chan struct{}),
	}
}

// AddCallback adds a callback function for thermal events
func (tm *ThermalMonitor) AddCallback(callback ThermalCallback) {
	tm.callbacks = append(tm.callbacks, callback)
}

// Start begins thermal monitoring
func (tm *ThermalMonitor) Start(ctx context.Context) error {
	// Discover sensors
	if err := tm.discoverSensors(); err != nil {
		return fmt.Errorf("failed to discover thermal sensors: %w", err)
	}

	// Initial temperature reading
	if err := tm.updateTemperatures(); err != nil {
		return fmt.Errorf("failed to get initial temperature readings: %w", err)
	}

	// Start monitoring goroutine
	go tm.monitorLoop(ctx)
	
	return nil
}

// Stop stops thermal monitoring
func (tm *ThermalMonitor) Stop() {
	close(tm.stopChan)
	<-tm.doneChan
}

// GetSensors returns a copy of all thermal sensors
func (tm *ThermalMonitor) GetSensors() map[string]*ThermalSensor {
	tm.sensorMutex.RLock()
	defer tm.sensorMutex.RUnlock()
	
	result := make(map[string]*ThermalSensor)
	for id, sensor := range tm.sensors {
		sensorCopy := *sensor
		result[id] = &sensorCopy
	}
	
	return result
}

// GetOverallState returns the overall thermal state
func (tm *ThermalMonitor) GetOverallState() ThermalState {
	tm.sensorMutex.RLock()
	defer tm.sensorMutex.RUnlock()
	return tm.overallState
}

// GetCPUTemperature returns the average CPU temperature
func (tm *ThermalMonitor) GetCPUTemperature() (Temperature, bool) {
	tm.sensorMutex.RLock()
	defer tm.sensorMutex.RUnlock()
	
	var totalTemp float64
	var count int
	
	for _, sensor := range tm.sensors {
		if sensor.Type == SensorTypeCPU && sensor.Available {
			totalTemp += sensor.Temperature.Value
			count++
		}
	}
	
	if count == 0 {
		return Temperature{}, false
	}
	
	return Temperature{
		Value: totalTemp / float64(count),
		Unit:  tm.config.TemperatureUnit,
	}, true
}

// monitorLoop is the main monitoring loop
func (tm *ThermalMonitor) monitorLoop(ctx context.Context) {
	defer close(tm.doneChan)
	
	ticker := time.NewTicker(tm.config.UpdateInterval)
	defer ticker.Stop()
	
	// Periodic sensor discovery (every 30 seconds)
	discoveryTicker := time.NewTicker(30 * time.Second)
	defer discoveryTicker.Stop()
	
	for {
		select {
		case <-ctx.Done():
			return
		case <-tm.stopChan:
			return
		case <-ticker.C:
			if err := tm.updateTemperatures(); err != nil {
				fmt.Printf("Error updating temperatures: %v\n", err)
			}
		case <-discoveryTicker.C:
			if err := tm.discoverSensors(); err != nil {
				fmt.Printf("Error discovering sensors: %v\n", err)
			}
		}
	}
}

// discoverSensors discovers available thermal sensors
func (tm *ThermalMonitor) discoverSensors() error {
	newSensors := make(map[string]*ThermalSensor)
	
	// Discover thermal zone sensors
	if err := tm.discoverThermalZones(newSensors); err != nil {
		fmt.Printf("Error discovering thermal zones: %v\n", err)
	}
	
	// Discover hwmon sensors
	if err := tm.discoverHWMonSensors(newSensors); err != nil {
		fmt.Printf("Error discovering hwmon sensors: %v\n", err)
	}
	
	// Update sensor map
	tm.sensorMutex.Lock()
	defer tm.sensorMutex.Unlock()
	
	// Check for new sensors
	for id, sensor := range newSensors {
		if _, exists := tm.sensors[id]; !exists {
			tm.sensors[id] = sensor
			tm.notifyCallback(ThermalEventSensorAdded, sensor)
		} else {
			// Update existing sensor metadata
			tm.sensors[id].Name = sensor.Name
			tm.sensors[id].Type = sensor.Type
			tm.sensors[id].Location = sensor.Location
		}
	}
	
	// Check for removed sensors
	for id, sensor := range tm.sensors {
		if _, exists := newSensors[id]; !exists {
			delete(tm.sensors, id)
			tm.notifyCallback(ThermalEventSensorRemoved, sensor)
		}
	}
	
	return nil
}

// discoverThermalZones discovers thermal zone sensors
func (tm *ThermalMonitor) discoverThermalZones(sensors map[string]*ThermalSensor) error {
	thermalPath := "/sys/class/thermal"
	
	entries, err := os.ReadDir(thermalPath)
	if err != nil {
		return err
	}
	
	for _, entry := range entries {
		if !strings.HasPrefix(entry.Name(), "thermal_zone") {
			continue
		}
		
		zonePath := filepath.Join(thermalPath, entry.Name())
		
		// Read zone type
		typeData, err := os.ReadFile(filepath.Join(zonePath, "type"))
		if err != nil {
			continue
		}
		zoneType := strings.TrimSpace(string(typeData))
		
		// Create sensor
		sensor := &ThermalSensor{
			ID:         entry.Name(),
			Name:       zoneType,
			Type:       tm.classifySensorType(zoneType),
			Location:   zoneType,
			SourcePath: filepath.Join(zonePath, "temp"),
			SourceType: SourceTypeThermal,
			Available:  true,
			Accuracy:   1.0,
			History:    make([]TemperatureReading, 0, tm.config.HistorySize),
		}
		
		// Read thresholds if available
		if criticalData, err := os.ReadFile(filepath.Join(zonePath, "trip_point_0_temp")); err == nil {
			if temp, err := strconv.ParseFloat(strings.TrimSpace(string(criticalData)), 64); err == nil {
				sensor.Critical = Temperature{Value: temp / 1000.0, Unit: Celsius}
			}
		}
		
		sensors[sensor.ID] = sensor
	}
	
	return nil
}

// discoverHWMonSensors discovers hardware monitoring sensors
func (tm *ThermalMonitor) discoverHWMonSensors(sensors map[string]*ThermalSensor) error {
	hwmonPath := "/sys/class/hwmon"
	
	entries, err := os.ReadDir(hwmonPath)
	if err != nil {
		return err
	}
	
	for _, entry := range entries {
		if !strings.HasPrefix(entry.Name(), "hwmon") {
			continue
		}
		
		hwmonDir := filepath.Join(hwmonPath, entry.Name())
		
		// Read device name
		var deviceName string
		if nameData, err := os.ReadFile(filepath.Join(hwmonDir, "name")); err == nil {
			deviceName = strings.TrimSpace(string(nameData))
		} else {
			deviceName = entry.Name()
		}
		
		// Find temperature inputs
		tempFiles, err := filepath.Glob(filepath.Join(hwmonDir, "temp*_input"))
		if err != nil {
			continue
		}
		
		for _, tempFile := range tempFiles {
			// Extract temperature number
			basename := filepath.Base(tempFile)
			tempNum := strings.TrimSuffix(strings.TrimPrefix(basename, "temp"), "_input")
			
			// Read temperature label if available
			var label string
			labelFile := filepath.Join(hwmonDir, fmt.Sprintf("temp%s_label", tempNum))
			if labelData, err := os.ReadFile(labelFile); err == nil {
				label = strings.TrimSpace(string(labelData))
			} else {
				label = fmt.Sprintf("%s_temp%s", deviceName, tempNum)
			}
			
			// Create sensor
			sensorID := fmt.Sprintf("%s_temp%s", entry.Name(), tempNum)
			sensor := &ThermalSensor{
				ID:         sensorID,
				Name:       label,
				Type:       tm.classifySensorType(label),
				Location:   fmt.Sprintf("%s/%s", deviceName, label),
				SourcePath: tempFile,
				SourceType: SourceTypeHWMon,
				Available:  true,
				Accuracy:   1.0,
				History:    make([]TemperatureReading, 0, tm.config.HistorySize),
			}
			
			// Read thresholds if available
			criticalFile := filepath.Join(hwmonDir, fmt.Sprintf("temp%s_crit", tempNum))
			if criticalData, err := os.ReadFile(criticalFile); err == nil {
				if temp, err := strconv.ParseFloat(strings.TrimSpace(string(criticalData)), 64); err == nil {
					sensor.Critical = Temperature{Value: temp / 1000.0, Unit: Celsius}
				}
			}
			
			maxFile := filepath.Join(hwmonDir, fmt.Sprintf("temp%s_max", tempNum))
			if maxData, err := os.ReadFile(maxFile); err == nil {
				if temp, err := strconv.ParseFloat(strings.TrimSpace(string(maxData)), 64); err == nil {
					sensor.Warning = Temperature{Value: temp / 1000.0, Unit: Celsius}
				}
			}
			
			sensors[sensor.ID] = sensor
		}
	}
	
	return nil
}

// classifySensorType attempts to classify the sensor type based on its name
func (tm *ThermalMonitor) classifySensorType(name string) SensorType {
	nameLower := strings.ToLower(name)
	
	switch {
	case strings.Contains(nameLower, "cpu") || strings.Contains(nameLower, "core") ||
		 strings.Contains(nameLower, "processor"):
		return SensorTypeCPU
	case strings.Contains(nameLower, "gpu") || strings.Contains(nameLower, "graphics") ||
		 strings.Contains(nameLower, "video"):
		return SensorTypeGPU
	case strings.Contains(nameLower, "ambient") || strings.Contains(nameLower, "room"):
		return SensorTypeAmbient
	case strings.Contains(nameLower, "fan"):
		return SensorTypeFan
	case strings.Contains(nameLower, "system") || strings.Contains(nameLower, "motherboard") ||
		 strings.Contains(nameLower, "chassis"):
		return SensorTypeSystem
	default:
		return SensorTypeUnknown
	}
}

// updateTemperatures updates temperature readings for all sensors
func (tm *ThermalMonitor) updateTemperatures() error {
	tm.sensorMutex.Lock()
	defer tm.sensorMutex.Unlock()
	
	now := time.Now()
	overallState := ThermalStateNormal
	
	for _, sensor := range tm.sensors {
		// Read temperature
		temp, err := tm.readTemperature(sensor.SourcePath)
		if err != nil {
			sensor.Available = false
			continue
		}
		
		sensor.Available = true
		sensor.LastUpdate = now
		
		// Convert to configured unit
		convertedTemp := tm.convertTemperature(temp, sensor.SourceType, tm.config.TemperatureUnit)
		
		// Check for significant change
		prevTemp := sensor.Temperature
		sensor.Temperature = convertedTemp
		
		// Add to history
		reading := TemperatureReading{
			Timestamp:   now,
			Temperature: convertedTemp,
			Quality:     1.0,
		}
		sensor.History = append(sensor.History, reading)
		
		// Trim history
		if len(sensor.History) > tm.config.HistorySize {
			sensor.History = sensor.History[len(sensor.History)-tm.config.HistorySize:]
		}
		
		// Check thresholds
		tm.checkThresholds(sensor, prevTemp)
		
		// Update overall state
		if sensor.Critical.Value > 0 && convertedTemp.Value >= sensor.Critical.Value {
			overallState = ThermalStateCritical
		} else if sensor.Warning.Value > 0 && convertedTemp.Value >= sensor.Warning.Value {
			if overallState != ThermalStateCritical {
				overallState = ThermalStateWarning
			}
		} else if convertedTemp.Value >= tm.config.AlertThresholds.Critical.Value {
			overallState = ThermalStateCritical
		} else if convertedTemp.Value >= tm.config.AlertThresholds.Warning.Value {
			if overallState != ThermalStateCritical {
				overallState = ThermalStateWarning
			}
		}
	}
	
	tm.overallState = overallState
	return nil
}

// readTemperature reads temperature from a sensor file
func (tm *ThermalMonitor) readTemperature(sensorPath string) (Temperature, error) {
	data, err := os.ReadFile(sensorPath)
	if err != nil {
		return Temperature{}, err
	}
	
	tempStr := strings.TrimSpace(string(data))
	tempMilliC, err := strconv.ParseFloat(tempStr, 64)
	if err != nil {
		return Temperature{}, err
	}
	
	// Convert from millidegrees Celsius to Celsius
	tempC := tempMilliC / 1000.0
	
	return Temperature{Value: tempC, Unit: Celsius}, nil
}

// convertTemperature converts temperature to the specified unit
func (tm *ThermalMonitor) convertTemperature(temp Temperature, sourceType SourceType, targetUnit TemperatureUnit) Temperature {
	// First convert to Celsius if needed
	var celsius float64
	switch temp.Unit {
	case Celsius:
		celsius = temp.Value
	case Fahrenheit:
		celsius = (temp.Value - 32.0) * 5.0 / 9.0
	case Kelvin:
		celsius = temp.Value - 273.15
	}
	
	// Then convert to target unit
	var result float64
	switch targetUnit {
	case Celsius:
		result = celsius
	case Fahrenheit:
		result = celsius*9.0/5.0 + 32.0
	case Kelvin:
		result = celsius + 273.15
	}
	
	return Temperature{Value: result, Unit: targetUnit}
}

// checkThresholds checks if temperature thresholds have been crossed
func (tm *ThermalMonitor) checkThresholds(sensor *ThermalSensor, prevTemp Temperature) {
	currentTemp := sensor.Temperature
	
	// Check for threshold crossings
	if sensor.Critical.Value > 0 {
		if prevTemp.Value < sensor.Critical.Value && currentTemp.Value >= sensor.Critical.Value {
			tm.notifyCallback(ThermalEventCriticalThreshold, sensor)
		}
	} else if currentTemp.Value >= tm.config.AlertThresholds.Critical.Value {
		if prevTemp.Value < tm.config.AlertThresholds.Critical.Value {
			tm.notifyCallback(ThermalEventCriticalThreshold, sensor)
		}
	}
	
	if sensor.Warning.Value > 0 {
		if prevTemp.Value < sensor.Warning.Value && currentTemp.Value >= sensor.Warning.Value {
			tm.notifyCallback(ThermalEventWarningThreshold, sensor)
		}
	} else if currentTemp.Value >= tm.config.AlertThresholds.Warning.Value {
		if prevTemp.Value < tm.config.AlertThresholds.Warning.Value {
			tm.notifyCallback(ThermalEventWarningThreshold, sensor)
		}
	}
	
	// Notify of temperature changes (for significant changes)
	if abs(currentTemp.Value-prevTemp.Value) >= 2.0 {
		tm.notifyCallback(ThermalEventTemperatureChange, sensor)
	}
}

// notifyCallback notifies all callbacks of a thermal event
func (tm *ThermalMonitor) notifyCallback(event ThermalEvent, sensor *ThermalSensor) {
	for _, callback := range tm.callbacks {
		go func(cb ThermalCallback) {
			defer func() {
				if r := recover(); r != nil {
					fmt.Printf("Thermal callback panic: %v\n", r)
				}
			}()
			cb(event, sensor)
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

