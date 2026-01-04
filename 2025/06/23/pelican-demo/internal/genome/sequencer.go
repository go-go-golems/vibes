package genome

import (
	"context"
	"fmt"
	"math/rand"
	"time"

	"pelican-demo/internal/progress"
)

// Config holds configuration for genome sequencing
type Config struct {
	Species      string
	TotalRecords int
	BatchSize    int
	FetchDelay   time.Duration
	AnalyzeDelay time.Duration
}

// DefaultConfig returns a default configuration
func DefaultConfig(species string) Config {
	return Config{
		Species:      species,
		TotalRecords: 200,
		BatchSize:    10,
		FetchDelay:   50 * time.Millisecond,
		AnalyzeDelay: 30 * time.Millisecond,
	}
}

// Run executes the genome sequencing simulation
func Run(ctx context.Context, sink progress.Sink, jobID string, cfg Config) error {
	// Initialize counters
	fetched := 0
	indexed := 0
	
	// Send initial event
	if err := sink.Send(progress.Event{
		JobID:   jobID,
		Stage:   "fetch",
		Fetched: fetched,
		Indexed: indexed,
	}); err != nil {
		return fmt.Errorf("failed to send initial event: %w", err)
	}
	
	// Fetch phase - simulate downloading gene fragments
	for fetched < cfg.TotalRecords {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}
		
		// Simulate batch fetching
		batchSize := cfg.BatchSize
		if fetched+batchSize > cfg.TotalRecords {
			batchSize = cfg.TotalRecords - fetched
		}
		
		// Simulate network delay and occasional rate limiting
		time.Sleep(cfg.FetchDelay)
		rateLimited := rand.Float32() < 0.1 // 10% chance of rate limiting
		
		if rateLimited {
			// Simulate rate limit delay
			time.Sleep(200 * time.Millisecond)
		}
		
		fetched += batchSize
		
		// Send progress event
		if err := sink.Send(progress.Event{
			JobID:       jobID,
			Stage:       "fetch",
			Fetched:     fetched,
			Indexed:     indexed,
			RateLimited: rateLimited,
		}); err != nil {
			return fmt.Errorf("failed to send fetch event: %w", err)
		}
	}
	
	// Analysis phase - simulate indexing gene sequences
	for indexed < cfg.TotalRecords {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}
		
		// Simulate batch analysis
		batchSize := cfg.BatchSize
		if indexed+batchSize > cfg.TotalRecords {
			batchSize = cfg.TotalRecords - indexed
		}
		
		// Simulate analysis delay
		time.Sleep(cfg.AnalyzeDelay)
		
		indexed += batchSize
		
		// Send progress event
		if err := sink.Send(progress.Event{
			JobID:   jobID,
			Stage:   "analyze",
			Fetched: fetched,
			Indexed: indexed,
		}); err != nil {
			return fmt.Errorf("failed to send analyze event: %w", err)
		}
	}
	
	// Send completion event
	if err := sink.Send(progress.Event{
		JobID:   jobID,
		Stage:   "done",
		Fetched: fetched,
		Indexed: indexed,
	}); err != nil {
		return fmt.Errorf("failed to send completion event: %w", err)
	}
	
	return nil
}

// GetSpeciesInfo returns information about supported pelican species
func GetSpeciesInfo() map[string]string {
	return map[string]string{
		"brown_pelican":     "Pelecanus occidentalis - Found along coasts of the Americas",
		"peruvian_pelican":  "Pelecanus thagus - Native to the Pacific coast of South America",
		"dalmatian_pelican": "Pelecanus crispus - Largest pelican species, found in Europe and Asia",
		"american_white_pelican": "Pelecanus erythrorhynchos - Large North American pelican",
		"australian_pelican": "Pelecanus conspicillatus - Found across Australia and New Guinea",
	}
}

