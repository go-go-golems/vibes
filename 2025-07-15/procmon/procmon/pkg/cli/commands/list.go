package commands

import (
	"context"
	"fmt"
	"sort"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/procmon/procmon/pkg/monitor"
)

type ListCommand struct {
	*cmds.CommandDescription
}

func NewListCommand() (*ListCommand, error) {
	return &ListCommand{
		CommandDescription: cmds.NewCommandDescription(
			"list",
			cmds.WithShort("List running processes with CPU and memory usage"),
			cmds.WithLong(`List all running processes with detailed information including:
- Process ID (PID) and parent process ID (PPID)
- Process name and command line
- CPU usage percentage
- Memory usage (resident and virtual)
- Thread count and process state
- Well-known program analysis for complex applications

The output can be filtered and sorted by various criteria.`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"min-cpu",
					parameters.ParameterTypeFloat,
					parameters.WithDefault(0.0),
					parameters.WithHelp("Minimum CPU usage percentage to display"),
				),
				parameters.NewParameterDefinition(
					"min-memory",
					parameters.ParameterTypeInteger,
					parameters.WithDefault(0),
					parameters.WithHelp("Minimum memory usage in MB to display"),
				),
				parameters.NewParameterDefinition(
					"list-sort-by",
					parameters.ParameterTypeChoice,
					parameters.WithDefault("cpu"),
					parameters.WithChoices("cpu", "memory", "pid", "name", "threads"),
					parameters.WithHelp("Sort processes by specified field"),
				),
				parameters.NewParameterDefinition(
					"reverse",
					parameters.ParameterTypeBool,
					parameters.WithDefault(false),
					parameters.WithHelp("Reverse sort order"),
				),
				parameters.NewParameterDefinition(
					"limit",
					parameters.ParameterTypeInteger,
					parameters.WithDefault(50),
					parameters.WithHelp("Maximum number of processes to display"),
				),
				parameters.NewParameterDefinition(
					"show-kernel",
					parameters.ParameterTypeBool,
					parameters.WithDefault(false),
					parameters.WithHelp("Include kernel threads in output"),
				),
				parameters.NewParameterDefinition(
					"show-threads",
					parameters.ParameterTypeBool,
					parameters.WithDefault(false),
					parameters.WithHelp("Show thread information for each process"),
				),
			),
		),
	}, nil
}

// BareCommand implementation for classic text output
func (c *ListCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	params := struct {
		MinCPU     float64 `glazed.parameter:"min-cpu"`
		MinMemory  int     `glazed.parameter:"min-memory"`
		SortBy     string  `glazed.parameter:"list-sort-by"`
		Reverse    bool    `glazed.parameter:"reverse"`
		Limit      int     `glazed.parameter:"limit"`
		ShowKernel bool    `glazed.parameter:"show-kernel"`
		ShowThreads bool   `glazed.parameter:"show-threads"`
	}{}

	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &params)
	if err != nil {
		return fmt.Errorf("failed to parse parameters: %w", err)
	}

	// Create process monitor
	processMonitor := monitor.NewProcessMonitor(monitor.DefaultMonitorConfig())
	
	// Get process information as map
	processMap := processMonitor.GetProcesses()
	
	// Convert map to slice
	processes := make([]monitor.ProcessInfo, 0, len(processMap))
	for _, proc := range processMap {
		processes = append(processes, *proc)
	}

	// Filter processes
	filtered := filterProcesses(processes, params.MinCPU, params.MinMemory, params.ShowKernel)
	
	// Sort processes
	sortProcesses(filtered, params.SortBy, params.Reverse)
	
	// Limit results
	if params.Limit > 0 && len(filtered) > params.Limit {
		filtered = filtered[:params.Limit]
	}

	// Print header
	fmt.Printf("%-8s %-20s %-8s %-10s %-8s %-10s %s\n",
		"PID", "Name", "CPU%", "Memory(MB)", "Threads", "State", "Command")
	fmt.Println("--------------------------------------------------------------------------------")

	// Print processes
	for _, proc := range filtered {
		memoryMB := proc.Memory.ResidentSize / (1024 * 1024)
		command := truncateString(proc.CommandLine, 40)
		
		fmt.Printf("%-8d %-20s %-8.1f %-10d %-8d %-10s %s\n",
			proc.PID, truncateString(proc.Name, 20), proc.CPUUsage.Total,
			memoryMB, proc.ThreadCount, proc.State, command)
		
		// Show threads if requested
		if params.ShowThreads && len(proc.Threads) > 0 {
			for _, thread := range proc.Threads {
				fmt.Printf("  └─ %-6d %-18s %-8.1f %-10s %-8s %s\n",
					thread.TID, truncateString(thread.Name, 18), thread.CPUUsage.Total,
					"-", thread.State, "thread")
			}
		}
	}

	fmt.Printf("\nTotal processes: %d (showing %d)\n", len(processes), len(filtered))
	return nil
}

// GlazeCommand implementation for structured data output
func (c *ListCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	params := struct {
		MinCPU     float64 `glazed.parameter:"min-cpu"`
		MinMemory  int     `glazed.parameter:"min-memory"`
		SortBy     string  `glazed.parameter:"list-sort-by"`
		Reverse    bool    `glazed.parameter:"reverse"`
		Limit      int     `glazed.parameter:"limit"`
		ShowKernel bool    `glazed.parameter:"show-kernel"`
		ShowThreads bool   `glazed.parameter:"show-threads"`
	}{}

	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &params)
	if err != nil {
		return fmt.Errorf("failed to parse parameters: %w", err)
	}

	// Create process monitor
	processMonitor := monitor.NewProcessMonitor(monitor.DefaultMonitorConfig())
	
	// Get process information as map
	processMap := processMonitor.GetProcesses()
	
	// Convert map to slice
	processes := make([]monitor.ProcessInfo, 0, len(processMap))
	for _, proc := range processMap {
		processes = append(processes, *proc)
	}

	// Filter processes
	filtered := filterProcesses(processes, params.MinCPU, params.MinMemory, params.ShowKernel)
	
	// Sort processes
	sortProcesses(filtered, params.SortBy, params.Reverse)
	
	// Limit results
	if params.Limit > 0 && len(filtered) > params.Limit {
		filtered = filtered[:params.Limit]
	}

	// Output structured data
	for _, proc := range filtered {
		row := types.NewRow(
			types.MRP("pid", proc.PID),
			types.MRP("ppid", proc.PPID),
			types.MRP("name", proc.Name),
			types.MRP("command", proc.CommandLine),
			types.MRP("cpu_usage", proc.CPUUsage.Total),
			types.MRP("cpu_time_user", proc.CPUUsage.User),
			types.MRP("cpu_time_system", proc.CPUUsage.System),
			types.MRP("memory_resident_mb", proc.Memory.ResidentSize/(1024*1024)),
			types.MRP("memory_virtual_mb", proc.Memory.VirtualSize/(1024*1024)),
			types.MRP("memory_shared_mb", proc.Memory.SharedSize/(1024*1024)),
			types.MRP("thread_count", proc.ThreadCount),
			types.MRP("state", string(proc.State)),
			types.MRP("priority", proc.Priority),
			types.MRP("nice", proc.Nice),
			types.MRP("start_time", proc.StartTime.Format(time.RFC3339)),
		)
		
		err := gp.AddRow(ctx, row)
		if err != nil {
			return fmt.Errorf("failed to add process row: %w", err)
		}

		// Add thread information if requested
		if params.ShowThreads {
			for _, thread := range proc.Threads {
				threadRow := types.NewRow(
					types.MRP("pid", proc.PID),
					types.MRP("tid", thread.TID),
					types.MRP("thread_name", thread.Name),
					types.MRP("thread_cpu_usage", thread.CPUUsage.Total),
					types.MRP("thread_cpu_time_user", thread.CPUUsage.User),
					types.MRP("thread_cpu_time_system", thread.CPUUsage.System),
					types.MRP("thread_state", string(thread.State)),
					types.MRP("thread_priority", thread.Priority),
				)
				
				err := gp.AddRow(ctx, threadRow)
				if err != nil {
					return fmt.Errorf("failed to add thread row: %w", err)
				}
			}
		}
	}

	return nil
}

// Helper functions
func filterProcesses(processes []monitor.ProcessInfo, minCPU float64, minMemory int, showKernel bool) []monitor.ProcessInfo {
	var filtered []monitor.ProcessInfo
	minMemoryBytes := uint64(minMemory * 1024 * 1024)
	
	for _, proc := range processes {
		// Filter by CPU usage
		if proc.CPUUsage.Total < minCPU {
			continue
		}
		
		// Filter by memory usage
		if proc.Memory.ResidentSize < minMemoryBytes {
			continue
		}
		
		// Filter kernel threads if not requested (simple heuristic)
		if !showKernel && proc.PID < 100 {
			continue
		}
		
		filtered = append(filtered, proc)
	}
	
	return filtered
}

func sortProcesses(processes []monitor.ProcessInfo, sortBy string, reverse bool) {
	sort.Slice(processes, func(i, j int) bool {
		var less bool
		
		switch sortBy {
		case "cpu":
			less = processes[i].CPUUsage.Total < processes[j].CPUUsage.Total
		case "memory":
			less = processes[i].Memory.ResidentSize < processes[j].Memory.ResidentSize
		case "pid":
			less = processes[i].PID < processes[j].PID
		case "name":
			less = processes[i].Name < processes[j].Name
		case "threads":
			less = processes[i].ThreadCount < processes[j].ThreadCount
		default:
			less = processes[i].CPUUsage.Total < processes[j].CPUUsage.Total
		}
		
		if reverse {
			return !less
		}
		return less
	})
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

