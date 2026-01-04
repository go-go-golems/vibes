package storage

import (
	"database/sql"
	"fmt"
	"time"

	_ "github.com/mattn/go-sqlite3"

	"github.com/procmon/procmon/pkg/memory"
	"github.com/procmon/procmon/pkg/monitor"
	"github.com/procmon/procmon/pkg/power"
	"github.com/procmon/procmon/pkg/thermal"
)

// SQLiteLogger handles logging monitoring data to SQLite database
type SQLiteLogger struct {
	db       *sql.DB
	dbPath   string
	config   LoggerConfig
}

// LoggerConfig contains configuration for the SQLite logger
type LoggerConfig struct {
	BatchSize       int
	FlushInterval   time.Duration
	RetentionPeriod time.Duration
	LogLevel        LogLevel
}

type LogLevel int

const (
	LogLevelBasic LogLevel = iota
	LogLevelDetailed
	LogLevelVerbose
)

// NewSQLiteLogger creates a new SQLite logger
func NewSQLiteLogger(dbPath string, config LoggerConfig) (*SQLiteLogger, error) {
	logger := &SQLiteLogger{
		dbPath: dbPath,
		config: config,
	}

	if err := logger.initialize(); err != nil {
		return nil, fmt.Errorf("failed to initialize SQLite logger: %w", err)
	}

	return logger, nil
}

// DefaultLoggerConfig returns a default logger configuration
func DefaultLoggerConfig() LoggerConfig {
	return LoggerConfig{
		BatchSize:       100,
		FlushInterval:   30 * time.Second,
		RetentionPeriod: 7 * 24 * time.Hour, // 7 days
		LogLevel:        LogLevelDetailed,
	}
}

// initialize sets up the database and creates tables
func (l *SQLiteLogger) initialize() error {
	var err error
	l.db, err = sql.Open("sqlite3", l.dbPath)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}

	// Enable WAL mode for better concurrent access
	if _, err := l.db.Exec("PRAGMA journal_mode=WAL"); err != nil {
		return fmt.Errorf("failed to enable WAL mode: %w", err)
	}

	// Create tables
	if err := l.createTables(); err != nil {
		return fmt.Errorf("failed to create tables: %w", err)
	}

	return nil
}

// createTables creates all necessary database tables
func (l *SQLiteLogger) createTables() error {
	tables := []string{
		// System snapshots table
		`CREATE TABLE IF NOT EXISTS system_snapshots (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			memory_total_bytes INTEGER,
			memory_used_bytes INTEGER,
			memory_available_bytes INTEGER,
			memory_usage_percent REAL,
			memory_pressure_level TEXT,
			memory_pressure_score REAL,
			thrashing_detected BOOLEAN,
			thrashing_confidence REAL,
			thermal_state TEXT,
			cpu_temperature REAL,
			temperature_unit TEXT,
			ac_connected BOOLEAN,
			power_saving BOOLEAN,
			cpu_governor TEXT,
			cpu_freq_avg_mhz REAL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,

		// Process snapshots table
		`CREATE TABLE IF NOT EXISTS process_snapshots (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			system_snapshot_id INTEGER,
			timestamp DATETIME NOT NULL,
			pid INTEGER NOT NULL,
			ppid INTEGER,
			name TEXT NOT NULL,
			command_line TEXT,
			state TEXT,
			cpu_usage_percent REAL,
			memory_rss_bytes INTEGER,
			memory_vss_bytes INTEGER,
			thread_count INTEGER,
			start_time DATETIME,
			user_time_ms INTEGER,
			system_time_ms INTEGER,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (system_snapshot_id) REFERENCES system_snapshots(id)
		)`,

		// Thread details table
		`CREATE TABLE IF NOT EXISTS thread_details (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			process_snapshot_id INTEGER,
			timestamp DATETIME NOT NULL,
			tid INTEGER NOT NULL,
			pid INTEGER NOT NULL,
			name TEXT,
			state TEXT,
			cpu_usage_percent REAL,
			priority INTEGER,
			nice_value INTEGER,
			user_time_ms INTEGER,
			system_time_ms INTEGER,
			role TEXT,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (process_snapshot_id) REFERENCES process_snapshots(id)
		)`,

		// Memory events table
		`CREATE TABLE IF NOT EXISTS memory_events (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			event_type TEXT NOT NULL,
			severity TEXT,
			description TEXT,
			page_fault_rate REAL,
			swap_usage_percent REAL,
			io_wait_percent REAL,
			thrashing_factors TEXT,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,

		// Thermal events table
		`CREATE TABLE IF NOT EXISTS thermal_events (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			sensor_id TEXT NOT NULL,
			sensor_name TEXT,
			sensor_type TEXT,
			temperature REAL,
			temperature_unit TEXT,
			threshold_type TEXT,
			threshold_value REAL,
			event_type TEXT NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,

		// Power events table
		`CREATE TABLE IF NOT EXISTS power_events (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			event_type TEXT NOT NULL,
			battery_id TEXT,
			battery_capacity REAL,
			battery_status TEXT,
			ac_connected BOOLEAN,
			governor_old TEXT,
			governor_new TEXT,
			frequency_old_mhz REAL,
			frequency_new_mhz REAL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,

		// Battery history table
		`CREATE TABLE IF NOT EXISTS battery_history (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			battery_id TEXT NOT NULL,
			capacity_percent REAL,
			voltage REAL,
			current_amperes REAL,
			power_watts REAL,
			status TEXT,
			health_percent REAL,
			cycle_count INTEGER,
			time_to_empty_minutes INTEGER,
			time_to_full_minutes INTEGER,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,

		// CPU frequency history table
		`CREATE TABLE IF NOT EXISTS cpu_frequency_history (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			core_id INTEGER,
			frequency_mhz REAL,
			governor TEXT,
			min_freq_mhz REAL,
			max_freq_mhz REAL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,

		// Performance alerts table
		`CREATE TABLE IF NOT EXISTS performance_alerts (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			timestamp DATETIME NOT NULL,
			alert_type TEXT NOT NULL,
			severity TEXT NOT NULL,
			title TEXT NOT NULL,
			description TEXT,
			process_pid INTEGER,
			process_name TEXT,
			metric_name TEXT,
			metric_value REAL,
			threshold_value REAL,
			duration_seconds INTEGER,
			resolved BOOLEAN DEFAULT FALSE,
			resolved_at DATETIME,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		)`,
	}

	for _, table := range tables {
		if _, err := l.db.Exec(table); err != nil {
			return fmt.Errorf("failed to create table: %w", err)
		}
	}

	// Create indexes for better query performance
	indexes := []string{
		"CREATE INDEX IF NOT EXISTS idx_system_snapshots_timestamp ON system_snapshots(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_process_snapshots_timestamp ON process_snapshots(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_process_snapshots_pid ON process_snapshots(pid)",
		"CREATE INDEX IF NOT EXISTS idx_thread_details_timestamp ON thread_details(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_thread_details_pid ON thread_details(pid)",
		"CREATE INDEX IF NOT EXISTS idx_memory_events_timestamp ON memory_events(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_thermal_events_timestamp ON thermal_events(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_power_events_timestamp ON power_events(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_battery_history_timestamp ON battery_history(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_cpu_frequency_history_timestamp ON cpu_frequency_history(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_performance_alerts_timestamp ON performance_alerts(timestamp)",
		"CREATE INDEX IF NOT EXISTS idx_performance_alerts_resolved ON performance_alerts(resolved)",
	}

	for _, index := range indexes {
		if _, err := l.db.Exec(index); err != nil {
			return fmt.Errorf("failed to create index: %w", err)
		}
	}

	return nil
}

// LogSystemSnapshot logs a complete system snapshot
func (l *SQLiteLogger) LogSystemSnapshot(
	systemMemory memory.SystemMemory,
	memoryPressure memory.MemoryPressure,
	thermalState thermal.ThermalState,
	cpuTemp thermal.Temperature,
	powerState power.PowerState,
	cpuFreq power.CPUFrequencyInfo,
	processes map[int32]*monitor.ProcessInfo,
) error {
	tx, err := l.db.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	// Insert system snapshot
	systemResult, err := tx.Exec(`
		INSERT INTO system_snapshots (
			timestamp, memory_total_bytes, memory_used_bytes, memory_available_bytes,
			memory_usage_percent, memory_pressure_level, memory_pressure_score,
			thrashing_detected, thrashing_confidence, thermal_state, cpu_temperature,
			temperature_unit, ac_connected, power_saving, cpu_governor, cpu_freq_avg_mhz
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		time.Now(),
		systemMemory.Total,
		systemMemory.Used,
		systemMemory.Available,
		systemMemory.UsagePercent,
		string(memoryPressure.Level),
		memoryPressure.Score,
		memoryPressure.Thrashing.Detected,
		memoryPressure.Thrashing.Confidence,
		string(thermalState),
		cpuTemp.Value,
		string(cpuTemp.Unit),
		powerState.ACConnected,
		powerState.PowerSaving,
		cpuFreq.Governor,
		cpuFreq.AverageFreq,
	)
	if err != nil {
		return fmt.Errorf("failed to insert system snapshot: %w", err)
	}

	systemSnapshotID, err := systemResult.LastInsertId()
	if err != nil {
		return fmt.Errorf("failed to get system snapshot ID: %w", err)
	}

	// Insert process snapshots
	if l.config.LogLevel >= LogLevelDetailed {
		for _, process := range processes {
			processResult, err := tx.Exec(`
				INSERT INTO process_snapshots (
					system_snapshot_id, timestamp, pid, ppid, name, command_line,
					state, cpu_usage_percent, memory_rss_bytes, memory_vss_bytes,
					thread_count, start_time, user_time_ms, system_time_ms
				) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
				systemSnapshotID,
				time.Now(),
				process.PID,
				process.PPID,
				process.Name,
				process.CommandLine,
				string(process.State),
				process.CPUUsage.Total,
				process.Memory.ResidentSize,
				process.Memory.VirtualSize,
				process.ThreadCount,
				process.StartTime,
				process.CPUUsage.UserTicks,
				process.CPUUsage.SystemTicks,
			)
			if err != nil {
				return fmt.Errorf("failed to insert process snapshot for PID %d: %w", process.PID, err)
			}

			// Insert thread details if verbose logging is enabled
			if l.config.LogLevel >= LogLevelVerbose {
				processSnapshotID, err := processResult.LastInsertId()
				if err != nil {
					return fmt.Errorf("failed to get process snapshot ID: %w", err)
				}

				for _, thread := range process.Threads {
					_, err := tx.Exec(`
						INSERT INTO thread_details (
							process_snapshot_id, timestamp, tid, pid, name, state,
							cpu_usage_percent, priority, nice_value, user_time_ms,
							system_time_ms, role
						) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
						processSnapshotID,
						time.Now(),
						thread.TID,
						thread.PID,
						thread.Name,
						string(thread.State),
						thread.CPUUsage.Total,
						thread.Priority,
						thread.Nice,
						thread.UserTime.Milliseconds(),
						thread.SystemTime.Milliseconds(),
						"", // Role would come from analysis
					)
					if err != nil {
						return fmt.Errorf("failed to insert thread detail for TID %d: %w", thread.TID, err)
					}
				}
			}
		}
	}

	return tx.Commit()
}

// LogMemoryEvent logs a memory-related event
func (l *SQLiteLogger) LogMemoryEvent(event memory.MemoryEvent, data interface{}) error {
	var eventType, severity, description string
	var pageFaultRate, swapUsagePercent, ioWaitPercent float64
	var thrashingFactors string

	switch event {
	case memory.MemoryEventThrashingStart:
		eventType = "thrashing_start"
		severity = "warning"
		if thrashing, ok := data.(memory.ThrashingInfo); ok {
			description = fmt.Sprintf("Thrashing detected with %.1f%% confidence", thrashing.Confidence)
			thrashingFactors = fmt.Sprintf("%v", thrashing.Factors)
		}
	case memory.MemoryEventThrashingEnd:
		eventType = "thrashing_end"
		severity = "info"
		description = "Thrashing condition resolved"
	case memory.MemoryEventLowMemory:
		eventType = "low_memory"
		severity = "warning"
		description = "Low memory condition detected"
	case memory.MemoryEventCriticalMemory:
		eventType = "critical_memory"
		severity = "critical"
		description = "Critical memory condition detected"
	}

	_, err := l.db.Exec(`
		INSERT INTO memory_events (
			timestamp, event_type, severity, description, page_fault_rate,
			swap_usage_percent, io_wait_percent, thrashing_factors
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
		time.Now(),
		eventType,
		severity,
		description,
		pageFaultRate,
		swapUsagePercent,
		ioWaitPercent,
		thrashingFactors,
	)

	return err
}

// LogThermalEvent logs a thermal-related event
func (l *SQLiteLogger) LogThermalEvent(event thermal.ThermalEvent, sensor *thermal.ThermalSensor) error {
	var eventType string
	switch event {
	case thermal.ThermalEventWarningThreshold:
		eventType = "warning_threshold"
	case thermal.ThermalEventCriticalThreshold:
		eventType = "critical_threshold"
	case thermal.ThermalEventTemperatureChange:
		eventType = "temperature_change"
	case thermal.ThermalEventSensorAdded:
		eventType = "sensor_added"
	case thermal.ThermalEventSensorRemoved:
		eventType = "sensor_removed"
	}

	_, err := l.db.Exec(`
		INSERT INTO thermal_events (
			timestamp, sensor_id, sensor_name, sensor_type, temperature,
			temperature_unit, event_type
		) VALUES (?, ?, ?, ?, ?, ?, ?)`,
		time.Now(),
		sensor.ID,
		sensor.Name,
		string(sensor.Type),
		sensor.Temperature.Value,
		string(sensor.Temperature.Unit),
		eventType,
	)

	return err
}

// LogPowerEvent logs a power-related event
func (l *SQLiteLogger) LogPowerEvent(event power.PowerEvent, data interface{}) error {
	var eventType string
	var batteryID, batteryStatus string
	var batteryCapacity float64
	var acConnected bool
	var governorOld, governorNew string
	var frequencyOld, frequencyNew float64

	switch event {
	case power.PowerEventBatteryLow:
		eventType = "battery_low"
		if battery, ok := data.(*power.BatteryInfo); ok {
			batteryID = battery.ID
			batteryCapacity = battery.Capacity
			batteryStatus = string(battery.Status)
		}
	case power.PowerEventBatteryCritical:
		eventType = "battery_critical"
		if battery, ok := data.(*power.BatteryInfo); ok {
			batteryID = battery.ID
			batteryCapacity = battery.Capacity
			batteryStatus = string(battery.Status)
		}
	case power.PowerEventACConnected:
		eventType = "ac_connected"
		if powerState, ok := data.(power.PowerState); ok {
			acConnected = powerState.ACConnected
		}
	case power.PowerEventACDisconnected:
		eventType = "ac_disconnected"
		if powerState, ok := data.(power.PowerState); ok {
			acConnected = powerState.ACConnected
		}
	case power.PowerEventGovernorChange:
		eventType = "governor_change"
		if govData, ok := data.(map[string]interface{}); ok {
			if old, exists := govData["old_governor"]; exists {
				governorOld = old.(string)
			}
			if new, exists := govData["new_governor"]; exists {
				governorNew = new.(string)
			}
		}
	case power.PowerEventFrequencyChange:
		eventType = "frequency_change"
		if freqData, ok := data.(map[string]interface{}); ok {
			if old, exists := freqData["old_frequency"]; exists {
				frequencyOld = old.(float64)
			}
			if new, exists := freqData["new_frequency"]; exists {
				frequencyNew = new.(float64)
			}
		}
	}

	_, err := l.db.Exec(`
		INSERT INTO power_events (
			timestamp, event_type, battery_id, battery_capacity, battery_status,
			ac_connected, governor_old, governor_new, frequency_old_mhz, frequency_new_mhz
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		time.Now(),
		eventType,
		batteryID,
		batteryCapacity,
		batteryStatus,
		acConnected,
		governorOld,
		governorNew,
		frequencyOld,
		frequencyNew,
	)

	return err
}

// LogBatteryHistory logs battery status history
func (l *SQLiteLogger) LogBatteryHistory(batteries map[string]*power.BatteryInfo) error {
	tx, err := l.db.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	for _, battery := range batteries {
		if !battery.Available {
			continue
		}

		var timeToEmptyMinutes, timeToFullMinutes int
		if battery.TimeToEmpty > 0 {
			timeToEmptyMinutes = int(battery.TimeToEmpty.Minutes())
		}
		if battery.TimeToFull > 0 {
			timeToFullMinutes = int(battery.TimeToFull.Minutes())
		}

		_, err := tx.Exec(`
			INSERT INTO battery_history (
				timestamp, battery_id, capacity_percent, voltage, current_amperes,
				power_watts, status, health_percent, cycle_count, time_to_empty_minutes,
				time_to_full_minutes
			) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			time.Now(),
			battery.ID,
			battery.Capacity,
			battery.Voltage,
			battery.Current,
			battery.Power,
			string(battery.Status),
			battery.Health,
			battery.CycleCount,
			timeToEmptyMinutes,
			timeToFullMinutes,
		)
		if err != nil {
			return fmt.Errorf("failed to insert battery history for %s: %w", battery.ID, err)
		}
	}

	return tx.Commit()
}

// LogCPUFrequencyHistory logs CPU frequency history
func (l *SQLiteLogger) LogCPUFrequencyHistory(cpuFreq power.CPUFrequencyInfo) error {
	tx, err := l.db.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer tx.Rollback()

	for _, core := range cpuFreq.Cores {
		if !core.Available {
			continue
		}

		_, err := tx.Exec(`
			INSERT INTO cpu_frequency_history (
				timestamp, core_id, frequency_mhz, governor, min_freq_mhz, max_freq_mhz
			) VALUES (?, ?, ?, ?, ?, ?)`,
			time.Now(),
			core.CoreID,
			core.CurrentFreq,
			core.Governor,
			core.MinFreq,
			core.MaxFreq,
		)
		if err != nil {
			return fmt.Errorf("failed to insert CPU frequency history for core %d: %w", core.CoreID, err)
		}
	}

	return tx.Commit()
}

// LogPerformanceAlert logs a performance alert
func (l *SQLiteLogger) LogPerformanceAlert(alertType, severity, title, description string,
	processPID int32, processName, metricName string, metricValue, thresholdValue float64,
	duration time.Duration) error {

	_, err := l.db.Exec(`
		INSERT INTO performance_alerts (
			timestamp, alert_type, severity, title, description, process_pid,
			process_name, metric_name, metric_value, threshold_value, duration_seconds
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		time.Now(),
		alertType,
		severity,
		title,
		description,
		processPID,
		processName,
		metricName,
		metricValue,
		thresholdValue,
		int(duration.Seconds()),
	)

	return err
}

// CleanupOldData removes old data based on retention policy
func (l *SQLiteLogger) CleanupOldData() error {
	cutoffTime := time.Now().Add(-l.config.RetentionPeriod)

	tables := []string{
		"system_snapshots",
		"process_snapshots",
		"thread_details",
		"memory_events",
		"thermal_events",
		"power_events",
		"battery_history",
		"cpu_frequency_history",
		"performance_alerts",
	}

	tx, err := l.db.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin cleanup transaction: %w", err)
	}
	defer tx.Rollback()

	for _, table := range tables {
		_, err := tx.Exec(fmt.Sprintf("DELETE FROM %s WHERE timestamp < ?", table), cutoffTime)
		if err != nil {
			return fmt.Errorf("failed to cleanup table %s: %w", table, err)
		}
	}

	// Vacuum to reclaim space
	if _, err := tx.Exec("VACUUM"); err != nil {
		return fmt.Errorf("failed to vacuum database: %w", err)
	}

	return tx.Commit()
}

// Close closes the database connection
func (l *SQLiteLogger) Close() error {
	if l.db != nil {
		return l.db.Close()
	}
	return nil
}

// GetSystemStats returns basic statistics about logged data
func (l *SQLiteLogger) GetSystemStats() (map[string]interface{}, error) {
	stats := make(map[string]interface{})

	// Count records in each table
	tables := []string{
		"system_snapshots",
		"process_snapshots",
		"thread_details",
		"memory_events",
		"thermal_events",
		"power_events",
		"battery_history",
		"cpu_frequency_history",
		"performance_alerts",
	}

	for _, table := range tables {
		var count int
		err := l.db.QueryRow(fmt.Sprintf("SELECT COUNT(*) FROM %s", table)).Scan(&count)
		if err != nil {
			return nil, fmt.Errorf("failed to count records in %s: %w", table, err)
		}
		stats[table+"_count"] = count
	}

	// Get date range
	var oldestTimestamp, newestTimestamp time.Time
	err := l.db.QueryRow("SELECT MIN(timestamp), MAX(timestamp) FROM system_snapshots").Scan(&oldestTimestamp, &newestTimestamp)
	if err != nil && err != sql.ErrNoRows {
		return nil, fmt.Errorf("failed to get timestamp range: %w", err)
	}

	if !oldestTimestamp.IsZero() {
		stats["oldest_record"] = oldestTimestamp
		stats["newest_record"] = newestTimestamp
		stats["data_span_hours"] = newestTimestamp.Sub(oldestTimestamp).Hours()
	}

	// Get database file size
	var pageCount, pageSize int
	err = l.db.QueryRow("PRAGMA page_count").Scan(&pageCount)
	if err != nil {
		return nil, fmt.Errorf("failed to get page count: %w", err)
	}
	err = l.db.QueryRow("PRAGMA page_size").Scan(&pageSize)
	if err != nil {
		return nil, fmt.Errorf("failed to get page size: %w", err)
	}

	stats["database_size_bytes"] = pageCount * pageSize
	stats["database_size_mb"] = float64(pageCount*pageSize) / (1024 * 1024)

	return stats, nil
}

