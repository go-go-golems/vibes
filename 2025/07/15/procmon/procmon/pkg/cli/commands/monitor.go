package commands

import (
	"context"
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/procmon/procmon/pkg/monitor"
)

type MonitorCommand struct {
	*cmds.CommandDescription
}

func NewMonitorCommand() (*MonitorCommand, error) {
	return &MonitorCommand{
		CommandDescription: cmds.NewCommandDescription(
			"monitor",
			cmds.WithShort("Monitor a specific process and its threads in real-time"),
			cmds.WithLong(`Monitor a specific process by PID with real-time updates including:
- Process CPU and memory usage
- Individual thread information and CPU usage
- Well-known program analysis for complex applications
- Real-time updates with configurable refresh interval

This command provides continuous monitoring of a single process with detailed thread-level information.`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"pid",
					parameters.ParameterTypeInteger,
					parameters.WithRequired(true),
					parameters.WithHelp("Process ID to monitor"),
				),
				parameters.NewParameterDefinition(
					"interval",
					parameters.ParameterTypeString,
					parameters.WithDefault("1s"),
					parameters.WithHelp("Update interval (e.g., 500ms, 1s, 2s)"),
				),
				parameters.NewParameterDefinition(
					"duration",
					parameters.ParameterTypeString,
					parameters.WithDefault("30s"),
					parameters.WithHelp("Monitoring duration (e.g., 30s, 1m, 5m)"),
				),
				parameters.NewParameterDefinition(
					"show-threads",
					parameters.ParameterTypeBool,
					parameters.WithDefault(true),
					parameters.WithHelp("Show individual thread information"),
				),
			),
		),
	}, nil
}

// GlazeCommand implementation for structured data output
func (c *MonitorCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	params := struct {
		PID         int32  `glazed.parameter:"pid"`
		Interval    string `glazed.parameter:"interval"`
		Duration    string `glazed.parameter:"duration"`
		ShowThreads bool   `glazed.parameter:"show-threads"`
	}{}

	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &params)
	if err != nil {
		return fmt.Errorf("failed to parse parameters: %w", err)
	}

	// Parse interval and duration
	interval, err := time.ParseDuration(params.Interval)
	if err != nil {
		return fmt.Errorf("invalid interval: %w", err)
	}

	duration, err := time.ParseDuration(params.Duration)
	if err != nil {
		return fmt.Errorf("invalid duration: %w", err)
	}

	// Create process monitor
	processMonitor := monitor.NewProcessMonitor(monitor.DefaultMonitorConfig())

	// Setup monitoring context
	monitorCtx, cancel := context.WithTimeout(ctx, duration)
	defer cancel()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	sampleCount := 0

	// Monitoring loop
	for {
		select {
		case <-monitorCtx.Done():
			// Output summary
			summaryRow := types.NewRow(
				types.MRP("timestamp", time.Now().Format(time.RFC3339)),
				types.MRP("monitoring_type", "summary"),
				types.MRP("target_pid", params.PID),
				types.MRP("duration_seconds", duration.Seconds()),
				types.MRP("sample_count", sampleCount),
				types.MRP("sample_interval_seconds", interval.Seconds()),
				types.MRP("status", "Monitoring completed"),
			)
			
			err = gp.AddRow(ctx, summaryRow)
			if err != nil {
				return fmt.Errorf("failed to add summary row: %w", err)
			}
			return nil

		case <-ticker.C:
			timestamp := time.Now()
			
			// Get process information
			processMap := processMonitor.GetProcesses()
			targetProcess, exists := processMap[params.PID]
			
			if !exists {
				// Process not found
				notFoundRow := types.NewRow(
					types.MRP("timestamp", timestamp.Format(time.RFC3339)),
					types.MRP("monitoring_type", "error"),
					types.MRP("target_pid", params.PID),
					types.MRP("error", "Process not found"),
					types.MRP("sample_number", sampleCount),
				)
				
				err = gp.AddRow(ctx, notFoundRow)
				if err != nil {
					return fmt.Errorf("failed to add error row: %w", err)
				}
				sampleCount++
				continue
			}

			// Output process information
			processRow := types.NewRow(
				types.MRP("timestamp", timestamp.Format(time.RFC3339)),
				types.MRP("monitoring_type", "process"),
				types.MRP("pid", targetProcess.PID),
				types.MRP("name", targetProcess.Name),
				types.MRP("command", targetProcess.CommandLine),
				types.MRP("cpu_usage", targetProcess.CPUUsage.Total),
				types.MRP("cpu_user", targetProcess.CPUUsage.User),
				types.MRP("cpu_system", targetProcess.CPUUsage.System),
				types.MRP("memory_resident_mb", targetProcess.Memory.ResidentSize/(1024*1024)),
				types.MRP("memory_virtual_mb", targetProcess.Memory.VirtualSize/(1024*1024)),
				types.MRP("thread_count", targetProcess.ThreadCount),
				types.MRP("state", string(targetProcess.State)),
				types.MRP("sample_number", sampleCount),
			)
			
			err = gp.AddRow(ctx, processRow)
			if err != nil {
				return fmt.Errorf("failed to add process row: %w", err)
			}

			// Output thread information if requested
			if params.ShowThreads && len(targetProcess.Threads) > 0 {
				for _, thread := range targetProcess.Threads {
					threadRow := types.NewRow(
						types.MRP("timestamp", timestamp.Format(time.RFC3339)),
						types.MRP("monitoring_type", "thread"),
						types.MRP("pid", targetProcess.PID),
						types.MRP("tid", thread.TID),
						types.MRP("thread_name", thread.Name),
						types.MRP("thread_cpu_usage", thread.CPUUsage.Total),
						types.MRP("thread_cpu_user", thread.CPUUsage.User),
						types.MRP("thread_cpu_system", thread.CPUUsage.System),
						types.MRP("thread_state", string(thread.State)),
						types.MRP("thread_priority", thread.Priority),
						types.MRP("sample_number", sampleCount),
					)
					
					err = gp.AddRow(ctx, threadRow)
					if err != nil {
						return fmt.Errorf("failed to add thread row: %w", err)
					}
				}
			}

			sampleCount++
		}
	}
}

