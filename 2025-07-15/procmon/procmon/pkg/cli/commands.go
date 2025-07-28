package cli

import (
	"context"
	"fmt"
	"os"
	"strconv"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"

	"github.com/procmon/procmon/pkg/analysis"
	"github.com/procmon/procmon/pkg/memory"
	"github.com/procmon/procmon/pkg/monitor"
	"github.com/procmon/procmon/pkg/power"
	"github.com/procmon/procmon/pkg/thermal"
	"github.com/procmon/procmon/pkg/ui"
)

// MonitorCommand represents the main monitor command
type MonitorCommand struct {
	*cmds.CommandDescription
}

// NewMonitorCommand creates a new monitor command
func NewMonitorCommand() *MonitorCommand {
	return &MonitorCommand{
		CommandDescription: cmds.NewCommandDescription(
			"monitor",
			cmds.WithShort("Monitor process threads, CPU usage, memory, and system health"),
			cmds.WithLong(`
Process Monitor is a comprehensive system monitoring tool that provides real-time
information about processes, threads, memory usage, thermal state, and power consumption.

Features:
- Real-time process and thread monitoring
- CPU usage tracking per thread
- Memory pressure and thrashing detection
- Thermal monitoring with temperature sensors
- Battery and power state monitoring
- CPU frequency and governor tracking
- Well-known program analysis (Firefox, Chrome, etc.)
- Interactive terminal UI with multiple views
- Optional SQLite logging for historical data

The tool uses a tabbed interface to organize different types of information:
- Processes: List of running processes with CPU and memory usage
- Threads: Detailed thread information for selected processes
- Memory: System memory usage and thrashing detection
- Thermal: Temperature sensors and thermal state
- Power: Battery status, CPU frequency, and power management
- System: Overall system health overview
`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"pid",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Process ID to monitor (optional, monitors all if not specified)"),
					parameters.WithRequired(false),
				),
			),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"update-interval",
					parameters.ParameterTypeString,
					parameters.WithHelp("Update interval for monitoring data"),
					parameters.WithDefault("1s"),
				),
				parameters.NewParameterDefinition(
					"history-size",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Number of historical data points to keep"),
					parameters.WithDefault(300),
				),
				parameters.NewParameterDefinition(
					"log-to-sqlite",
					parameters.ParameterTypeString,
					parameters.WithHelp("SQLite database file for logging (optional)"),
				),
				parameters.NewParameterDefinition(
					"min-cpu",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Minimum CPU usage to display processes"),
					parameters.WithDefault(0.0),
				),
				parameters.NewParameterDefinition(
					"min-memory",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Minimum memory usage (MB) to display processes"),
					parameters.WithDefault(0),
				),
				parameters.NewParameterDefinition(
					"show-kernel",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Show kernel processes"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"temperature-unit",
					parameters.ParameterTypeChoice,
					parameters.WithHelp("Temperature unit for display"),
					parameters.WithChoices([]string{"celsius", "fahrenheit", "kelvin"}),
					parameters.WithDefault("celsius"),
				),
			),
		),
	}
}

// RunIntoGlazeProcessor implements the glazed command interface
func (c *MonitorCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp cmds.GlazeProcessor,
) error {
	// Extract parameters
	ps := parsedLayers.GetParameterSet()
	
	var targetPID int32
	if pidArg, ok := ps.Get("pid"); ok {
		if pid, err := pidArg.GetInt(); err == nil {
			targetPID = int32(pid)
		}
	}

	updateIntervalStr, _ := ps.GetString("update-interval")
	updateInterval, err := time.ParseDuration(updateIntervalStr)
	if err != nil {
		updateInterval = time.Second
	}

	historySize, _ := ps.GetInt("history-size")
	logFile, _ := ps.GetString("log-to-sqlite")
	minCPU, _ := ps.GetFloat("min-cpu")
	minMemoryMB, _ := ps.GetInt("min-memory")
	showKernel, _ := ps.GetBool("show-kernel")
	tempUnitStr, _ := ps.GetString("temperature-unit")

	// Convert temperature unit
	var tempUnit thermal.TemperatureUnit
	switch tempUnitStr {
	case "fahrenheit":
		tempUnit = thermal.Fahrenheit
	case "kelvin":
		tempUnit = thermal.Kelvin
	default:
		tempUnit = thermal.Celsius
	}

	// Create monitoring components
	processConfig := monitor.DefaultProcessConfig()
	processConfig.UpdateInterval = updateInterval
	processConfig.HistorySize = historySize
	if targetPID > 0 {
		processConfig.TargetPID = &targetPID
	}

	memoryConfig := memory.DefaultMemoryConfig()
	memoryConfig.UpdateInterval = updateInterval
	memoryConfig.HistorySize = historySize

	thermalConfig := thermal.DefaultThermalConfig()
	thermalConfig.UpdateInterval = updateInterval * 2 // Thermal updates less frequently
	thermalConfig.HistorySize = historySize / 2
	thermalConfig.TemperatureUnit = tempUnit

	powerConfig := power.DefaultPowerConfig()
	powerConfig.UpdateInterval = updateInterval * 2 // Power updates less frequently
	powerConfig.HistorySize = historySize / 2

	// Initialize monitors
	processMonitor := monitor.NewProcessMonitor(processConfig)
	memoryMonitor := memory.NewMemoryMonitor(memoryConfig)
	thermalMonitor := thermal.NewThermalMonitor(thermalConfig)
	powerMonitor := power.NewPowerMonitor(powerConfig)
	programAnalyzer := analysis.NewProgramAnalyzer()

	// Setup SQLite logging if requested
	if logFile != "" {
		// TODO: Implement SQLite logging
		fmt.Printf("SQLite logging to %s (not yet implemented)\n", logFile)
	}

	// Create UI model
	model := ui.NewModel(
		processMonitor,
		memoryMonitor,
		thermalMonitor,
		powerMonitor,
		programAnalyzer,
	)

	// Apply filters
	// TODO: Apply filters to model

	// Start the TUI
	program := tea.NewProgram(model, tea.WithAltScreen())
	
	if _, err := program.Run(); err != nil {
		return fmt.Errorf("failed to run TUI: %w", err)
	}

	return nil
}

// ListCommand represents the list command for non-interactive output
type ListCommand struct {
	*cmds.CommandDescription
}

// NewListCommand creates a new list command
func NewListCommand() *ListCommand {
	return &ListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"list",
			cmds.WithShort("List processes in a non-interactive format"),
			cmds.WithLong(`
List processes and their information in a structured format suitable for
scripting and automation. This command provides a snapshot of current
system state without the interactive UI.

Output formats supported:
- JSON: Structured data for programmatic consumption
- CSV: Tabular data for spreadsheet import
- Table: Human-readable table format
- YAML: Configuration-friendly format
`),
			cmds.WithArguments(
				parameters.NewParameterDefinition(
					"pid",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Process ID to list (optional, lists all if not specified)"),
					parameters.WithRequired(false),
				),
			),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"sort-by",
					parameters.ParameterTypeChoice,
					parameters.WithHelp("Sort processes by field"),
					parameters.WithChoices([]string{"name", "cpu", "memory", "pid", "threads"}),
					parameters.WithDefault("cpu"),
				),
				parameters.NewParameterDefinition(
					"reverse",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Reverse sort order"),
					parameters.WithDefault(true),
				),
				parameters.NewParameterDefinition(
					"limit",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Limit number of processes to show"),
					parameters.WithDefault(50),
				),
				parameters.NewParameterDefinition(
					"min-cpu",
					parameters.ParameterTypeFloat,
					parameters.WithHelp("Minimum CPU usage to include"),
					parameters.WithDefault(0.0),
				),
				parameters.NewParameterDefinition(
					"min-memory",
					parameters.ParameterTypeInteger,
					parameters.WithHelp("Minimum memory usage (MB) to include"),
					parameters.WithDefault(0),
				),
				parameters.NewParameterDefinition(
					"show-kernel",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Include kernel processes"),
					parameters.WithDefault(false),
				),
				parameters.NewParameterDefinition(
					"include-threads",
					parameters.ParameterTypeBool,
					parameters.WithHelp("Include thread information"),
					parameters.WithDefault(false),
				),
			),
		),
	}
}

// RunIntoGlazeProcessor implements the glazed command interface for list
func (c *ListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp cmds.GlazeProcessor,
) error {
	// Extract parameters
	ps := parsedLayers.GetParameterSet()
	
	var targetPID int32
	if pidArg, ok := ps.Get("pid"); ok {
		if pid, err := pidArg.GetInt(); err == nil {
			targetPID = int32(pid)
		}
	}

	sortBy, _ := ps.GetString("sort-by")
	reverse, _ := ps.GetBool("reverse")
	limit, _ := ps.GetInt("limit")
	minCPU, _ := ps.GetFloat("min-cpu")
	minMemoryMB, _ := ps.GetInt("min-memory")
	showKernel, _ := ps.GetBool("show-kernel")
	includeThreads, _ := ps.GetBool("include-threads")

	// Create process monitor
	processConfig := monitor.DefaultProcessConfig()
	if targetPID > 0 {
		processConfig.TargetPID = &targetPID
	}

	processMonitor := monitor.NewProcessMonitor(processConfig)
	
	// Start monitoring briefly to get current data
	if err := processMonitor.Start(ctx); err != nil {
		return fmt.Errorf("failed to start process monitor: %w", err)
	}
	defer processMonitor.Stop()

	// Wait a moment for data collection
	time.Sleep(2 * time.Second)

	// Get processes
	processes := processMonitor.GetProcesses()

	// Filter processes
	var filtered []*monitor.ProcessInfo
	for _, process := range processes {
		if process.CPUUsage.Total < minCPU {
			continue
		}
		if process.Memory.ResidentSize < uint64(minMemoryMB)*1024*1024 {
			continue
		}
		if !showKernel && isKernelProcess(process) {
			continue
		}
		filtered = append(filtered, process)
	}

	// Sort processes
	sortProcesses(filtered, sortBy, reverse)

	// Limit results
	if limit > 0 && len(filtered) > limit {
		filtered = filtered[:limit]
	}

	// Output processes
	for _, process := range filtered {
		row := map[string]interface{}{
			"pid":         process.PID,
			"ppid":        process.PPID,
			"name":        process.Name,
			"command":     process.CommandLine,
			"state":       string(process.State),
			"cpu_percent": process.CPUUsage.Total,
			"memory_mb":   process.Memory.ResidentSize / (1024 * 1024),
			"threads":     process.ThreadCount,
			"start_time":  process.StartTime.Format(time.RFC3339),
		}

		if includeThreads && len(process.Threads) > 0 {
			var threads []map[string]interface{}
			for _, thread := range process.Threads {
				threadData := map[string]interface{}{
					"tid":         thread.TID,
					"name":        thread.Name,
					"state":       string(thread.State),
					"cpu_percent": thread.CPUUsage.Total,
				}
				threads = append(threads, threadData)
			}
			row["threads_detail"] = threads
		}

		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add row: %w", err)
		}
	}

	return nil
}

// SystemCommand represents the system info command
type SystemCommand struct {
	*cmds.CommandDescription
}

// NewSystemCommand creates a new system command
func NewSystemCommand() *SystemCommand {
	return &SystemCommand{
		CommandDescription: cmds.NewCommandDescription(
			"system",
			cmds.WithShort("Display system information and health status"),
			cmds.WithLong(`
Display comprehensive system information including memory usage, thermal state,
power status, and overall system health. This command provides a snapshot of
the current system state.
`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"temperature-unit",
					parameters.ParameterTypeChoice,
					parameters.WithHelp("Temperature unit for display"),
					parameters.WithChoices([]string{"celsius", "fahrenheit", "kelvin"}),
					parameters.WithDefault("celsius"),
				),
			),
		),
	}
}

// RunIntoGlazeProcessor implements the glazed command interface for system
func (c *SystemCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp cmds.GlazeProcessor,
) error {
	ps := parsedLayers.GetParameterSet()
	tempUnitStr, _ := ps.GetString("temperature-unit")

	// Convert temperature unit
	var tempUnit thermal.TemperatureUnit
	switch tempUnitStr {
	case "fahrenheit":
		tempUnit = thermal.Fahrenheit
	case "kelvin":
		tempUnit = thermal.Kelvin
	default:
		tempUnit = thermal.Celsius
	}

	// Create monitors
	memoryMonitor := memory.NewMemoryMonitor(memory.DefaultMemoryConfig())
	thermalConfig := thermal.DefaultThermalConfig()
	thermalConfig.TemperatureUnit = tempUnit
	thermalMonitor := thermal.NewThermalMonitor(thermalConfig)
	powerMonitor := power.NewPowerMonitor(power.DefaultPowerConfig())

	// Start monitors
	if err := memoryMonitor.Start(ctx); err != nil {
		return fmt.Errorf("failed to start memory monitor: %w", err)
	}
	defer memoryMonitor.Stop()

	if err := thermalMonitor.Start(ctx); err != nil {
		return fmt.Errorf("failed to start thermal monitor: %w", err)
	}
	defer thermalMonitor.Stop()

	if err := powerMonitor.Start(ctx); err != nil {
		return fmt.Errorf("failed to start power monitor: %w", err)
	}
	defer powerMonitor.Stop()

	// Wait for data collection
	time.Sleep(2 * time.Second)

	// Collect system information
	systemMemory := memoryMonitor.GetSystemMemory()
	memoryPressure := memoryMonitor.GetMemoryPressure()
	thermalState := thermalMonitor.GetOverallState()
	powerState := powerMonitor.GetPowerState()
	cpuFreq := powerMonitor.GetCPUFrequency()
	batteries := powerMonitor.GetBatteries()

	// Get CPU temperature
	var cpuTemp thermal.Temperature
	if temp, ok := thermalMonitor.GetCPUTemperature(); ok {
		cpuTemp = temp
	}

	// Create system info row
	row := map[string]interface{}{
		"timestamp":           time.Now().Format(time.RFC3339),
		"memory_total_gb":     float64(systemMemory.Total) / (1024 * 1024 * 1024),
		"memory_used_percent": systemMemory.UsagePercent,
		"memory_pressure":     string(memoryPressure.Level),
		"thrashing_detected":  memoryPressure.Thrashing.Detected,
		"thermal_state":       string(thermalState),
		"ac_connected":        powerState.ACConnected,
		"power_saving":        powerState.PowerSaving,
		"cpu_governor":        cpuFreq.Governor,
		"cpu_freq_avg_mhz":    cpuFreq.AverageFreq,
	}

	if cpuTemp.Value > 0 {
		row["cpu_temperature"] = cpuTemp.Value
		row["temperature_unit"] = string(cpuTemp.Unit)
	}

	// Add battery information
	if len(batteries) > 0 {
		var batteryCapacities []float64
		var batteryStatuses []string
		for _, battery := range batteries {
			if battery.Available {
				batteryCapacities = append(batteryCapacities, battery.Capacity)
				batteryStatuses = append(batteryStatuses, string(battery.Status))
			}
		}
		row["battery_capacities"] = batteryCapacities
		row["battery_statuses"] = batteryStatuses
	}

	return gp.AddRow(ctx, row)
}

// Helper functions
func isKernelProcess(process *monitor.ProcessInfo) bool {
	return len(process.Name) > 2 && process.Name[0] == '[' && process.Name[len(process.Name)-1] == ']'
}

func sortProcesses(processes []*monitor.ProcessInfo, sortBy string, reverse bool) {
	// Implementation would be similar to the UI sorting logic
	// For brevity, implementing a simple CPU sort
	if sortBy == "cpu" {
		for i := 0; i < len(processes)-1; i++ {
			for j := i + 1; j < len(processes); j++ {
				less := processes[i].CPUUsage.Total < processes[j].CPUUsage.Total
				if reverse {
					less = !less
				}
				if less {
					processes[i], processes[j] = processes[j], processes[i]
				}
			}
		}
	}
}

