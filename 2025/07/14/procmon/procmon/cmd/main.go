package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/procmon/procmon/pkg/analysis"
	"github.com/procmon/procmon/pkg/monitor"
)

func main() {
	// Create a context that can be cancelled
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Set up signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Create monitor configuration
	config := monitor.DefaultMonitorConfig()
	config.UpdateInterval = time.Second
	config.IncludeThreads = true
	config.ProcessFilter.IncludeKernel = false

	// Create process monitor
	processMonitor := monitor.NewProcessMonitor(config)

	// Create program analyzer
	programAnalyzer := analysis.NewProgramAnalyzer()

	// Add callback to demonstrate functionality
	processMonitor.AddCallback(func(process *monitor.ProcessInfo, event monitor.ProcessEvent) {
		switch event {
		case monitor.ProcessEventNew:
			fmt.Printf("New process: %s (PID: %d)\n", process.Name, process.PID)
			
			// Analyze the process if it's a well-known program
			if analysis := programAnalyzer.AnalyzeProcess(process); analysis.ProgramType != "unknown" {
				fmt.Printf("  Program type: %s\n", analysis.ProgramType)
				fmt.Printf("  Version: %s\n", analysis.Version)
				fmt.Printf("  Thread roles: %d\n", len(analysis.ThreadRoles))
			}
			
		case monitor.ProcessEventExited:
			fmt.Printf("Process exited: %s (PID: %d)\n", process.Name, process.PID)
		}
	})

	// Start monitoring
	fmt.Println("Starting process monitor...")
	if err := processMonitor.Start(ctx); err != nil {
		fmt.Printf("Failed to start process monitor: %v\n", err)
		os.Exit(1)
	}

	// Print initial process list
	go func() {
		time.Sleep(2 * time.Second) // Wait for initial scan
		processes := processMonitor.GetProcesses()
		fmt.Printf("\nCurrently monitoring %d processes:\n", len(processes))
		
		for _, process := range processes {
			fmt.Printf("  %s (PID: %d, CPU: %.1f%%, Memory: %d MB, Threads: %d)\n",
				process.Name,
				process.PID,
				process.CPUUsage.Total,
				process.Memory.ResidentSize/(1024*1024),
				process.ThreadCount)
		}
		fmt.Println()
	}()

	// Print periodic updates
	go func() {
		ticker := time.NewTicker(10 * time.Second)
		defer ticker.Stop()
		
		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
				processes := processMonitor.GetProcesses()
				fmt.Printf("Status: Monitoring %d processes\n", len(processes))
				
				// Show top 5 CPU consumers
				topProcesses := getTopCPUProcesses(processes, 5)
				if len(topProcesses) > 0 {
					fmt.Println("Top CPU consumers:")
					for i, process := range topProcesses {
						fmt.Printf("  %d. %s (PID: %d) - %.1f%% CPU\n",
							i+1, process.Name, process.PID, process.CPUUsage.Total)
					}
				}
				fmt.Println()
			}
		}
	}()

	// Wait for signal
	<-sigChan
	fmt.Println("\nShutting down...")

	// Stop monitoring
	processMonitor.Stop()
	fmt.Println("Process monitor stopped.")
}

// getTopCPUProcesses returns the top N processes by CPU usage
func getTopCPUProcesses(processes map[int32]*monitor.ProcessInfo, n int) []*monitor.ProcessInfo {
	// Convert map to slice
	var processList []*monitor.ProcessInfo
	for _, process := range processes {
		processList = append(processList, process)
	}

	// Sort by CPU usage (simple bubble sort for demonstration)
	for i := 0; i < len(processList)-1; i++ {
		for j := 0; j < len(processList)-i-1; j++ {
			if processList[j].CPUUsage.Total < processList[j+1].CPUUsage.Total {
				processList[j], processList[j+1] = processList[j+1], processList[j]
			}
		}
	}

	// Return top N
	if len(processList) > n {
		return processList[:n]
	}
	return processList
}

