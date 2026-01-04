package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/charmbracelet/bubbletea"

	"github.com/procmon/procmon/pkg/analysis"
	"github.com/procmon/procmon/pkg/memory"
	"github.com/procmon/procmon/pkg/monitor"
	"github.com/procmon/procmon/pkg/power"
	"github.com/procmon/procmon/pkg/thermal"
	"github.com/procmon/procmon/pkg/ui"
)

func main() {
	// Command line flags
	var (
		pidFlag         = flag.Int("pid", 0, "Process ID to monitor (0 for all processes)")
		updateInterval  = flag.Duration("update-interval", time.Second, "Update interval for monitoring")
		historySize     = flag.Int("history-size", 300, "Number of historical data points to keep")
		tempUnit        = flag.String("temp-unit", "celsius", "Temperature unit (celsius, fahrenheit, kelvin)")
		showVersion     = flag.Bool("version", false, "Show version information")
		listMode        = flag.Bool("list", false, "List processes in non-interactive mode")
		systemMode      = flag.Bool("system", false, "Show system information")
	)
	flag.Parse()

	if *showVersion {
		fmt.Printf("Process Monitor v1.0.0\n")
		fmt.Printf("Build: 2024-01-15\n")
		fmt.Printf("Go: %s\n", "go1.24.5")
		return
	}

	ctx := context.Background()

	// Convert temperature unit
	var tempUnitEnum thermal.TemperatureUnit
	switch *tempUnit {
	case "fahrenheit":
		tempUnitEnum = thermal.Fahrenheit
	case "kelvin":
		tempUnitEnum = thermal.Kelvin
	default:
		tempUnitEnum = thermal.Celsius
	}

	// Create monitoring components
	processConfig := monitor.DefaultMonitorConfig()
	processConfig.UpdateInterval = *updateInterval
	processConfig.MaxHistoryPoints = *historySize
	if *pidFlag > 0 {
		// For now, we'll handle PID filtering in the UI layer
		// TODO: Add TargetPID support to MonitorConfig
	}

	memoryConfig := memory.DefaultMemoryConfig()
	memoryConfig.UpdateInterval = *updateInterval
	memoryConfig.HistorySize = *historySize

	thermalConfig := thermal.DefaultThermalConfig()
	thermalConfig.UpdateInterval = *updateInterval * 2
	thermalConfig.HistorySize = *historySize / 2
	thermalConfig.TemperatureUnit = tempUnitEnum

	powerConfig := power.DefaultPowerConfig()
	powerConfig.UpdateInterval = *updateInterval * 2
	powerConfig.HistorySize = *historySize / 2

	// Initialize monitors
	processMonitor := monitor.NewProcessMonitor(processConfig)
	memoryMonitor := memory.NewMemoryMonitor(memoryConfig)
	thermalMonitor := thermal.NewThermalMonitor(thermalConfig)
	powerMonitor := power.NewPowerMonitor(powerConfig)
	programAnalyzer := analysis.NewProgramAnalyzer()

	if *listMode {
		// Non-interactive list mode
		if err := runListMode(ctx, processMonitor); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		return
	}

	if *systemMode {
		// System information mode
		if err := runSystemMode(ctx, memoryMonitor, thermalMonitor, powerMonitor); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		return
	}

	// Interactive TUI mode
	model := ui.NewModel(
		processMonitor,
		memoryMonitor,
		thermalMonitor,
		powerMonitor,
		programAnalyzer,
	)

	program := tea.NewProgram(model, tea.WithAltScreen())
	
	if _, err := program.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Error running TUI: %v\n", err)
		os.Exit(1)
	}
}

func runListMode(ctx context.Context, processMonitor *monitor.ProcessMonitor) error {
	// Start monitoring briefly to get current data
	if err := processMonitor.Start(ctx); err != nil {
		return fmt.Errorf("failed to start process monitor: %w", err)
	}
	defer processMonitor.Stop()

	// Wait a moment for data collection
	time.Sleep(2 * time.Second)

	// Get processes
	processes := processMonitor.GetProcesses()

	// Print header
	fmt.Printf("%-8s %-20s %-8s %-10s %-8s %-8s %s\n",
		"PID", "Name", "CPU%", "Memory(MB)", "Threads", "State", "Command")
	fmt.Println(strings.Repeat("-", 80))

	// Sort by CPU usage (descending)
	var sortedProcesses []*monitor.ProcessInfo
	for _, process := range processes {
		sortedProcesses = append(sortedProcesses, process)
	}

	// Simple bubble sort by CPU usage
	for i := 0; i < len(sortedProcesses)-1; i++ {
		for j := i + 1; j < len(sortedProcesses); j++ {
			if sortedProcesses[i].CPUUsage.Total < sortedProcesses[j].CPUUsage.Total {
				sortedProcesses[i], sortedProcesses[j] = sortedProcesses[j], sortedProcesses[i]
			}
		}
	}

	// Print top 20 processes
	count := 0
	for _, process := range sortedProcesses {
		if count >= 20 {
			break
		}
		
		memoryMB := process.Memory.ResidentSize / (1024 * 1024)
		command := process.CommandLine
		if len(command) > 40 {
			command = command[:37] + "..."
		}
		
		fmt.Printf("%-8d %-20s %-8.1f %-10d %-8d %-8s %s\n",
			process.PID,
			truncateString(process.Name, 20),
			process.CPUUsage.Total,
			memoryMB,
			process.ThreadCount,
			process.State,
			command)
		
		count++
	}

	return nil
}

func runSystemMode(ctx context.Context, memoryMonitor *memory.MemoryMonitor, 
	thermalMonitor *thermal.ThermalMonitor, powerMonitor *power.PowerMonitor) error {
	
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

	// Print system information
	fmt.Println("System Information")
	fmt.Println("==================")
	fmt.Printf("Timestamp: %s\n", time.Now().Format("2006-01-02 15:04:05"))
	fmt.Println()

	fmt.Println("Memory:")
	fmt.Printf("  Total: %.1f GB\n", float64(systemMemory.Total)/(1024*1024*1024))
	fmt.Printf("  Used: %.1f%%\n", systemMemory.UsagePercent)
	fmt.Printf("  Pressure: %s\n", memoryPressure.Level)
	if memoryPressure.Thrashing.Detected {
		fmt.Printf("  Thrashing: DETECTED (%.1f%% confidence)\n", memoryPressure.Thrashing.Confidence)
	} else {
		fmt.Println("  Thrashing: Not detected")
	}
	fmt.Println()

	fmt.Println("Thermal:")
	fmt.Printf("  State: %s\n", thermalState)
	if cpuTemp.Value > 0 {
		fmt.Printf("  CPU Temperature: %.1f°%s\n", cpuTemp.Value, cpuTemp.Unit)
	}
	fmt.Println()

	fmt.Println("Power:")
	acStatus := "Disconnected"
	if powerState.ACConnected {
		acStatus = "Connected"
	}
	fmt.Printf("  AC Power: %s\n", acStatus)
	fmt.Printf("  Power Saving: %t\n", powerState.PowerSaving)
	fmt.Printf("  CPU Governor: %s\n", cpuFreq.Governor)
	fmt.Printf("  CPU Frequency: %.0f MHz (avg)\n", cpuFreq.AverageFreq)

	if len(batteries) > 0 {
		fmt.Println("  Batteries:")
		for _, battery := range batteries {
			if battery.Available {
				fmt.Printf("    %s: %.1f%% (%s)\n", 
					battery.Name, battery.Capacity, battery.Status)
			}
		}
	}

	return nil
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

