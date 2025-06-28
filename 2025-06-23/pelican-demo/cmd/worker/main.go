package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"

	"github.com/google/uuid"

	"pelican-demo/internal/genome"
	"pelican-demo/internal/progress"
)

// CLISink implements progress.Sink for command line output
type CLISink struct {
	verbose bool
}

// Send prints progress events to stdout
func (c *CLISink) Send(event progress.Event) error {
	timestamp := event.Ts.Format("15:04:05")
	
	switch event.Stage {
	case "fetch":
		if event.RateLimited {
			fmt.Printf("[%s] 🐌 FETCH (Rate Limited): %d/%d records fetched\n", 
				timestamp, event.Fetched, 200)
		} else {
			fmt.Printf("[%s] 📥 FETCH: %d/%d records fetched\n", 
				timestamp, event.Fetched, 200)
		}
	case "analyze":
		fmt.Printf("[%s] 🧬 ANALYZE: %d/%d records indexed\n", 
			timestamp, event.Indexed, 200)
	case "done":
		fmt.Printf("[%s] ✅ COMPLETED: %d fetched, %d indexed\n", 
			timestamp, event.Fetched, event.Indexed)
	case "error":
		fmt.Printf("[%s] ❌ ERROR: %s\n", timestamp, event.Err)
	}
	
	if c.verbose {
		fmt.Printf("    Job ID: %s\n", event.JobID)
	}
	
	return nil
}

func main() {
	var (
		species = flag.String("species", "brown_pelican", "Pelican species to sequence")
		verbose = flag.Bool("verbose", false, "Enable verbose output")
		jobID   = flag.String("job-id", "", "Custom job ID (generates UUID if not provided)")
	)
	flag.Parse()
	
	// Validate species
	speciesInfo := genome.GetSpeciesInfo()
	if _, exists := speciesInfo[*species]; !exists {
		fmt.Printf("Invalid species: %s\n", *species)
		fmt.Println("Available species:")
		for name, info := range speciesInfo {
			fmt.Printf("  %s: %s\n", name, info)
		}
		os.Exit(1)
	}
	
	// Generate job ID if not provided
	if *jobID == "" {
		*jobID = uuid.New().String()
	}
	
	fmt.Printf("🦆 Pelican Genome Sequencer CLI\n")
	fmt.Printf("Species: %s\n", speciesInfo[*species])
	fmt.Printf("Job ID: %s\n", *jobID)
	fmt.Printf("Starting sequencing...\n\n")
	
	// Create CLI sink
	sink := &CLISink{verbose: *verbose}
	
	// Set up context with cancellation
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	
	// Handle interrupt signals
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	
	go func() {
		<-sigChan
		fmt.Printf("\n🛑 Received interrupt signal, stopping...\n")
		cancel()
	}()
	
	// Run genome sequencing
	cfg := genome.DefaultConfig(*species)
	if err := genome.Run(ctx, sink, *jobID, cfg); err != nil {
		if err == context.Canceled {
			fmt.Printf("Sequencing cancelled by user\n")
			os.Exit(130) // Standard exit code for SIGINT
		}
		log.Fatalf("Sequencing failed: %v", err)
	}
	
	fmt.Printf("\n🎉 Genome sequencing completed successfully!\n")
}

