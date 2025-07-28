package ui

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"

	"github.com/procmon/procmon/pkg/analysis"
	"github.com/procmon/procmon/pkg/memory"
	"github.com/procmon/procmon/pkg/monitor"
	"github.com/procmon/procmon/pkg/power"
	"github.com/procmon/procmon/pkg/thermal"
)

// Model represents the main UI model for the process monitor
type Model struct {
	// Monitoring components
	processMonitor  *monitor.ProcessMonitor
	memoryMonitor   *memory.MemoryMonitor
	thermalMonitor  *thermal.ThermalMonitor
	powerMonitor    *power.PowerMonitor
	programAnalyzer *analysis.ProgramAnalyzer

	// UI state
	currentTab    Tab
	tabs          []TabInfo
	width         int
	height        int
	lastUpdate    time.Time
	updateTicker  *time.Ticker

	// Data
	processes     map[int32]*monitor.ProcessInfo
	systemMemory  memory.SystemMemory
	memoryPressure memory.MemoryPressure
	cpuTemp       thermal.Temperature
	thermalState  thermal.ThermalState
	batteries     map[string]*power.BatteryInfo
	cpuFreq       power.CPUFrequencyInfo
	powerState    power.PowerState

	// Filtering and sorting
	processFilter ProcessFilter
	sortBy        SortBy
	sortDesc      bool

	// Context for cancellation
	ctx    context.Context
	cancel context.CancelFunc

	// Error state
	err error
}

// Tab represents a UI tab index
type Tab int

// Tab represents a UI tab structure
type TabInfo struct {
	Name    string
	Content TabContent
}

type TabContent int

const (
	TabProcesses TabContent = iota
	TabThreads
	TabMemory
	TabThermal
	TabPower
	TabSystem
)

// ProcessFilter defines filtering options for the process list
type ProcessFilter struct {
	MinCPU    float64
	MinMemory uint64
	ShowKernel bool
	NameFilter string
}

// SortBy defines sorting options for processes
type SortBy int

const (
	SortByName SortBy = iota
	SortByCPU
	SortByMemory
	SortByPID
	SortByThreads
)

// NewModel creates a new UI model
func NewModel(
	processMonitor *monitor.ProcessMonitor,
	memoryMonitor *memory.MemoryMonitor,
	thermalMonitor *thermal.ThermalMonitor,
	powerMonitor *power.PowerMonitor,
	programAnalyzer *analysis.ProgramAnalyzer,
) *Model {
	ctx, cancel := context.WithCancel(context.Background())

	model := &Model{
		processMonitor:  processMonitor,
		memoryMonitor:   memoryMonitor,
		thermalMonitor:  thermalMonitor,
		powerMonitor:    powerMonitor,
		programAnalyzer: programAnalyzer,
		ctx:             ctx,
		cancel:          cancel,
		currentTab:      0,
		tabs: []TabInfo{
			{Name: "Processes", Content: TabProcesses},
			{Name: "Threads", Content: TabThreads},
			{Name: "Memory", Content: TabMemory},
			{Name: "Thermal", Content: TabThermal},
			{Name: "Power", Content: TabPower},
			{Name: "System", Content: TabSystem},
		},
		processFilter: ProcessFilter{
			MinCPU:     0.0,
			MinMemory:  0,
			ShowKernel: false,
		},
		sortBy:   SortByCPU,
		sortDesc: true,
		processes: make(map[int32]*monitor.ProcessInfo),
		batteries: make(map[string]*power.BatteryInfo),
	}

	return model
}

// Init initializes the model
func (m *Model) Init() tea.Cmd {
	// Start monitoring components
	if err := m.processMonitor.Start(m.ctx); err != nil {
		m.err = fmt.Errorf("failed to start process monitor: %w", err)
		return tea.Quit
	}

	if err := m.memoryMonitor.Start(m.ctx); err != nil {
		m.err = fmt.Errorf("failed to start memory monitor: %w", err)
		return tea.Quit
	}

	if err := m.thermalMonitor.Start(m.ctx); err != nil {
		m.err = fmt.Errorf("failed to start thermal monitor: %w", err)
		return tea.Quit
	}

	if err := m.powerMonitor.Start(m.ctx); err != nil {
		m.err = fmt.Errorf("failed to start power monitor: %w", err)
		return tea.Quit
	}

	// Start update ticker
	m.updateTicker = time.NewTicker(time.Second)

	return tea.Batch(
		m.tickCmd(),
		m.updateDataCmd(),
	)
}

// Update handles messages
func (m *Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case tea.KeyMsg:
		return m.handleKeyPress(msg)

	case tickMsg:
		return m, tea.Batch(
			m.tickCmd(),
			m.updateDataCmd(),
		)

	case updateDataMsg:
		m.updateData()
		return m, nil

	case tea.QuitMsg:
		return m, m.cleanup()

	default:
		return m, nil
	}
}

// View renders the UI
func (m *Model) View() string {
	if m.err != nil {
		return fmt.Sprintf("Error: %v\n\nPress 'q' to quit.", m.err)
	}

	if m.width == 0 || m.height == 0 {
		return "Initializing..."
	}

	// Render header
	header := m.renderHeader()

	// Render tabs
	tabs := m.renderTabs()

	// Render content
	content := m.renderContent()

	// Render footer
	footer := m.renderFooter()

	return lipgloss.JoinVertical(
		lipgloss.Left,
		header,
		tabs,
		content,
		footer,
	)
}

// handleKeyPress handles keyboard input
func (m *Model) handleKeyPress(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c":
		return m, tea.Quit

	case "tab", "right":
		m.currentTab = (m.currentTab + 1) % Tab(len(m.tabs))
		return m, nil

	case "shift+tab", "left":
		m.currentTab = (m.currentTab - 1 + Tab(len(m.tabs))) % Tab(len(m.tabs))
		return m, nil

	case "1":
		m.currentTab = 0
		return m, nil
	case "2":
		m.currentTab = 1
		return m, nil
	case "3":
		m.currentTab = 2
		return m, nil
	case "4":
		m.currentTab = 3
		return m, nil
	case "5":
		m.currentTab = 4
		return m, nil
	case "6":
		m.currentTab = 5
		return m, nil

	case "s":
		// Cycle through sort options
		m.sortBy = (m.sortBy + 1) % 5
		return m, nil

	case "r":
		// Reverse sort order
		m.sortDesc = !m.sortDesc
		return m, nil

	case "k":
		// Toggle kernel processes
		m.processFilter.ShowKernel = !m.processFilter.ShowKernel
		return m, nil

	default:
		return m, nil
	}
}

// renderHeader renders the application header
func (m *Model) renderHeader() string {
	title := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("205")).
		Render("Process Monitor")

	timestamp := lipgloss.NewStyle().
		Foreground(lipgloss.Color("241")).
		Render(m.lastUpdate.Format("15:04:05"))

	processCount := fmt.Sprintf("Processes: %d", len(m.processes))
	
	var memoryUsage string
	if m.systemMemory.Total > 0 {
		memoryUsage = fmt.Sprintf("Memory: %.1f%%", m.systemMemory.UsagePercent)
	}

	var cpuTempStr string
	if m.cpuTemp.Value > 0 {
		cpuTempStr = fmt.Sprintf("CPU: %.1f°%s", m.cpuTemp.Value, m.cpuTemp.Unit)
	}

	info := strings.Join([]string{processCount, memoryUsage, cpuTempStr}, " | ")

	headerStyle := lipgloss.NewStyle().
		Width(m.width).
		Padding(0, 1).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("62"))

	return headerStyle.Render(
		lipgloss.JoinHorizontal(
			lipgloss.Left,
			title,
			lipgloss.NewStyle().Width(m.width-lipgloss.Width(title)-lipgloss.Width(timestamp)-lipgloss.Width(info)-6).Render(""),
			info,
			"  ",
			timestamp,
		),
	)
}

// renderTabs renders the tab bar
func (m *Model) renderTabs() string {
	var tabs []string

	for i, tab := range m.tabs {
		style := lipgloss.NewStyle().
			Padding(0, 2).
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("238"))

		if Tab(i) == m.currentTab {
			style = style.
				Bold(true).
				Foreground(lipgloss.Color("205")).
				BorderForeground(lipgloss.Color("205"))
		} else {
			style = style.
				Foreground(lipgloss.Color("241"))
		}

		tabs = append(tabs, style.Render(tab.Name))
	}

	return lipgloss.JoinHorizontal(lipgloss.Left, tabs...)
}

// renderContent renders the main content area
func (m *Model) renderContent() string {
	contentHeight := m.height - 8 // Account for header, tabs, and footer

	switch m.tabs[m.currentTab].Content {
	case TabProcesses:
		return m.renderProcesses(contentHeight)
	case TabThreads:
		return m.renderThreads(contentHeight)
	case TabMemory:
		return m.renderMemory(contentHeight)
	case TabThermal:
		return m.renderThermal(contentHeight)
	case TabPower:
		return m.renderPower(contentHeight)
	case TabSystem:
		return m.renderSystem(contentHeight)
	default:
		return "Unknown tab"
	}
}

// renderFooter renders the footer with help text
func (m *Model) renderFooter() string {
	help := "Tab/←→: Switch tabs | 1-6: Direct tab | s: Sort | r: Reverse | k: Toggle kernel | q: Quit"

	footerStyle := lipgloss.NewStyle().
		Width(m.width).
		Padding(0, 1).
		Foreground(lipgloss.Color("241")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("238"))

	return footerStyle.Render(help)
}

// renderProcesses renders the process list
func (m *Model) renderProcesses(height int) string {
	if len(m.processes) == 0 {
		return "No processes found"
	}

	// Filter and sort processes
	filtered := m.filterProcesses()
	sorted := m.sortProcesses(filtered)

	// Create table header
	header := fmt.Sprintf("%-8s %-20s %-8s %-8s %-8s %-8s %s",
		"PID", "Name", "CPU%", "Memory", "Threads", "State", "Command")

	var rows []string
	rows = append(rows, header)
	rows = append(rows, strings.Repeat("-", m.width-4))

	// Limit to available height
	maxRows := height - 3 // Account for header and separator
	if len(sorted) > maxRows {
		sorted = sorted[:maxRows]
	}

	for _, process := range sorted {
		memoryMB := process.Memory.ResidentSize / (1024 * 1024)
		
		row := fmt.Sprintf("%-8d %-20s %-8.1f %-8d %-8d %-8s %s",
			process.PID,
			truncateString(process.Name, 20),
			process.CPUUsage.Total,
			memoryMB,
			process.ThreadCount,
			process.State,
			truncateString(process.CommandLine, 40))

		rows = append(rows, row)
	}

	return strings.Join(rows, "\n")
}

// renderThreads renders thread information for the selected process
func (m *Model) renderThreads(height int) string {
	// For now, show threads from the first process with threads
	for _, process := range m.processes {
		if len(process.Threads) > 0 {
			header := fmt.Sprintf("Threads for %s (PID: %d)", process.Name, process.PID)
			
			var rows []string
			rows = append(rows, header)
			rows = append(rows, strings.Repeat("-", len(header)))

			threadHeader := fmt.Sprintf("%-8s %-20s %-8s %-8s %s",
				"TID", "Name", "CPU%", "State", "Role")
			rows = append(rows, threadHeader)

			for _, thread := range process.Threads {
				// Analyze thread role if possible
				analysis := m.programAnalyzer.AnalyzeProcess(process)
				var role string
				for _, threadRole := range analysis.ThreadRoles {
					if threadRole.TID == thread.TID {
						role = threadRole.Role
						break
					}
				}
				if role == "" {
					role = "Worker"
				}

				row := fmt.Sprintf("%-8d %-20s %-8.1f %-8s %s",
					thread.TID,
					truncateString(thread.Name, 20),
					thread.CPUUsage.Total,
					thread.State,
					role)

				rows = append(rows, row)

				if len(rows) >= height-1 {
					break
				}
			}

			return strings.Join(rows, "\n")
		}
	}

	return "No threads to display"
}

// renderMemory renders memory information
func (m *Model) renderMemory(height int) string {
	var rows []string

	// System memory
	rows = append(rows, "System Memory:")
	rows = append(rows, fmt.Sprintf("  Total: %s", formatBytes(m.systemMemory.Total)))
	rows = append(rows, fmt.Sprintf("  Used:  %s (%.1f%%)", 
		formatBytes(m.systemMemory.Used), m.systemMemory.UsagePercent))
	rows = append(rows, fmt.Sprintf("  Free:  %s", formatBytes(m.systemMemory.Free)))
	rows = append(rows, fmt.Sprintf("  Available: %s", formatBytes(m.systemMemory.Available)))
	rows = append(rows, "")

	// Memory pressure
	rows = append(rows, "Memory Pressure:")
	rows = append(rows, fmt.Sprintf("  Level: %s", m.memoryPressure.Level))
	rows = append(rows, fmt.Sprintf("  Score: %.1f/100", m.memoryPressure.Score))
	rows = append(rows, fmt.Sprintf("  Page Fault Rate: %.1f/sec", m.memoryPressure.PageFaults.FaultRate))
	rows = append(rows, "")

	// Thrashing detection
	rows = append(rows, "Thrashing Detection:")
	if m.memoryPressure.Thrashing.Detected {
		rows = append(rows, fmt.Sprintf("  Status: DETECTED (%.1f%% confidence)", 
			m.memoryPressure.Thrashing.Confidence))
		rows = append(rows, fmt.Sprintf("  Severity: %s", m.memoryPressure.Thrashing.Severity))
		rows = append(rows, fmt.Sprintf("  Duration: %v", m.memoryPressure.Thrashing.Duration))
		rows = append(rows, fmt.Sprintf("  Factors: %v", m.memoryPressure.Thrashing.Factors))
	} else {
		rows = append(rows, "  Status: Not detected")
	}

	return strings.Join(rows, "\n")
}

// renderThermal renders thermal information
func (m *Model) renderThermal(height int) string {
	sensors := m.thermalMonitor.GetSensors()
	
	var rows []string
	rows = append(rows, fmt.Sprintf("Thermal State: %s", m.thermalState))
	rows = append(rows, "")

	if len(sensors) == 0 {
		rows = append(rows, "No thermal sensors found")
		return strings.Join(rows, "\n")
	}

	rows = append(rows, "Temperature Sensors:")
	for _, sensor := range sensors {
		if sensor.Available {
			status := "OK"
			if sensor.Critical.Value > 0 && sensor.Temperature.Value >= sensor.Critical.Value {
				status = "CRITICAL"
			} else if sensor.Warning.Value > 0 && sensor.Temperature.Value >= sensor.Warning.Value {
				status = "WARNING"
			}

			row := fmt.Sprintf("  %-20s %.1f°%s (%s)",
				truncateString(sensor.Name, 20),
				sensor.Temperature.Value,
				sensor.Temperature.Unit,
				status)
			rows = append(rows, row)
		}
	}

	return strings.Join(rows, "\n")
}

// renderPower renders power and battery information
func (m *Model) renderPower(height int) string {
	var rows []string

	// Power state
	rows = append(rows, "Power State:")
	acStatus := "Disconnected"
	if m.powerState.ACConnected {
		acStatus = "Connected"
	}
	rows = append(rows, fmt.Sprintf("  AC Power: %s", acStatus))
	rows = append(rows, fmt.Sprintf("  Power Saving: %t", m.powerState.PowerSaving))
	rows = append(rows, "")

	// Batteries
	if len(m.batteries) > 0 {
		rows = append(rows, "Batteries:")
		for _, battery := range m.batteries {
			if battery.Available {
				rows = append(rows, fmt.Sprintf("  %s:", battery.Name))
				rows = append(rows, fmt.Sprintf("    Status: %s", battery.Status))
				rows = append(rows, fmt.Sprintf("    Capacity: %.1f%%", battery.Capacity))
				rows = append(rows, fmt.Sprintf("    Health: %.1f%%", battery.Health))
				if battery.TimeToEmpty > 0 {
					rows = append(rows, fmt.Sprintf("    Time to Empty: %v", battery.TimeToEmpty))
				}
				if battery.TimeToFull > 0 {
					rows = append(rows, fmt.Sprintf("    Time to Full: %v", battery.TimeToFull))
				}
				rows = append(rows, "")
			}
		}
	} else {
		rows = append(rows, "No batteries found")
		rows = append(rows, "")
	}

	// CPU frequency
	rows = append(rows, "CPU Frequency:")
	rows = append(rows, fmt.Sprintf("  Governor: %s", m.cpuFreq.Governor))
	rows = append(rows, fmt.Sprintf("  Average: %.0f MHz", m.cpuFreq.AverageFreq))
	rows = append(rows, fmt.Sprintf("  Range: %.0f - %.0f MHz", m.cpuFreq.MinFreq, m.cpuFreq.MaxFreq))

	return strings.Join(rows, "\n")
}

// renderSystem renders system overview
func (m *Model) renderSystem(height int) string {
	var rows []string

	rows = append(rows, "System Overview:")
	rows = append(rows, "")

	// Process summary
	kernelCount := 0
	userCount := 0
	for _, process := range m.processes {
		if isKernelProcess(process) {
			kernelCount++
		} else {
			userCount++
		}
	}

	rows = append(rows, "Processes:")
	rows = append(rows, fmt.Sprintf("  Total: %d", len(m.processes)))
	rows = append(rows, fmt.Sprintf("  User: %d", userCount))
	rows = append(rows, fmt.Sprintf("  Kernel: %d", kernelCount))
	rows = append(rows, "")

	// Memory summary
	rows = append(rows, "Memory:")
	rows = append(rows, fmt.Sprintf("  Usage: %.1f%%", m.systemMemory.UsagePercent))
	rows = append(rows, fmt.Sprintf("  Pressure: %s", m.memoryPressure.Level))
	if m.memoryPressure.Thrashing.Detected {
		rows = append(rows, "  Thrashing: DETECTED")
	}
	rows = append(rows, "")

	// Thermal summary
	rows = append(rows, "Thermal:")
	rows = append(rows, fmt.Sprintf("  State: %s", m.thermalState))
	if m.cpuTemp.Value > 0 {
		rows = append(rows, fmt.Sprintf("  CPU Temperature: %.1f°%s", m.cpuTemp.Value, m.cpuTemp.Unit))
	}

	return strings.Join(rows, "\n")
}

// Helper functions

func (m *Model) filterProcesses() []*monitor.ProcessInfo {
	var filtered []*monitor.ProcessInfo

	for _, process := range m.processes {
		// Apply filters
		if process.CPUUsage.Total < m.processFilter.MinCPU {
			continue
		}
		if process.Memory.ResidentSize < m.processFilter.MinMemory {
			continue
		}
		if !m.processFilter.ShowKernel && isKernelProcess(process) {
			continue
		}
		if m.processFilter.NameFilter != "" && 
		   !strings.Contains(strings.ToLower(process.Name), 
		                    strings.ToLower(m.processFilter.NameFilter)) {
			continue
		}

		filtered = append(filtered, process)
	}

	return filtered
}

func (m *Model) sortProcesses(processes []*monitor.ProcessInfo) []*monitor.ProcessInfo {
	sort.Slice(processes, func(i, j int) bool {
		var less bool

		switch m.sortBy {
		case SortByName:
			less = processes[i].Name < processes[j].Name
		case SortByCPU:
			less = processes[i].CPUUsage.Total < processes[j].CPUUsage.Total
		case SortByMemory:
			less = processes[i].Memory.ResidentSize < processes[j].Memory.ResidentSize
		case SortByPID:
			less = processes[i].PID < processes[j].PID
		case SortByThreads:
			less = processes[i].ThreadCount < processes[j].ThreadCount
		}

		if m.sortDesc {
			return !less
		}
		return less
	})

	return processes
}

func (m *Model) updateData() {
	m.lastUpdate = time.Now()
	m.processes = m.processMonitor.GetProcesses()
	m.systemMemory = m.memoryMonitor.GetSystemMemory()
	m.memoryPressure = m.memoryMonitor.GetMemoryPressure()
	m.batteries = m.powerMonitor.GetBatteries()
	m.cpuFreq = m.powerMonitor.GetCPUFrequency()
	m.powerState = m.powerMonitor.GetPowerState()
	m.thermalState = m.thermalMonitor.GetOverallState()

	// Get CPU temperature
	if temp, ok := m.thermalMonitor.GetCPUTemperature(); ok {
		m.cpuTemp = temp
	}
}

func (m *Model) cleanup() tea.Cmd {
	if m.updateTicker != nil {
		m.updateTicker.Stop()
	}

	m.cancel()

	// Stop monitoring components
	m.processMonitor.Stop()
	m.memoryMonitor.Stop()
	m.thermalMonitor.Stop()
	m.powerMonitor.Stop()

	return tea.Quit
}

// Message types
type tickMsg time.Time
type updateDataMsg struct{}

func (m *Model) tickCmd() tea.Cmd {
	return tea.Tick(time.Second, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

func (m *Model) updateDataCmd() tea.Cmd {
	return func() tea.Msg {
		return updateDataMsg{}
	}
}

// Utility functions
func isKernelProcess(process *monitor.ProcessInfo) bool {
	return len(process.Name) > 2 && process.Name[0] == '[' && process.Name[len(process.Name)-1] == ']'
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

func formatBytes(bytes uint64) string {
	const unit = 1024
	if bytes < unit {
		return fmt.Sprintf("%d B", bytes)
	}
	div, exp := int64(unit), 0
	for n := bytes / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(bytes)/float64(div), "KMGTPE"[exp])
}

