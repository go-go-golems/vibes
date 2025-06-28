package genome

import (
	"context"
	"testing"
	"time"

	"pelican-demo/internal/progress"
)

// TestSink implements progress.Sink for testing
type TestSink struct {
	events []progress.Event
}

func (t *TestSink) Send(event progress.Event) error {
	t.events = append(t.events, event)
	return nil
}

func TestGenomeSequencing(t *testing.T) {
	// Create test sink
	sink := &TestSink{}
	
	// Create test configuration with smaller numbers for faster testing
	cfg := Config{
		Species:      "brown_pelican",
		TotalRecords: 20,
		BatchSize:    5,
		FetchDelay:   1 * time.Millisecond,
		AnalyzeDelay: 1 * time.Millisecond,
	}
	
	// Run genome sequencing
	ctx := context.Background()
	jobID := "test-job-123"
	
	err := Run(ctx, sink, jobID, cfg)
	if err != nil {
		t.Fatalf("Genome sequencing failed: %v", err)
	}
	
	// Verify events were sent
	if len(sink.events) == 0 {
		t.Fatal("No events were sent")
	}
	
	// Check that we have the expected event types
	var hasInitial, hasFetch, hasAnalyze, hasDone bool
	
	for _, event := range sink.events {
		if event.JobID != jobID {
			t.Errorf("Expected job ID %s, got %s", jobID, event.JobID)
		}
		
		switch event.Stage {
		case "fetch":
			if event.Fetched == 0 && event.Indexed == 0 {
				hasInitial = true
			} else {
				hasFetch = true
			}
		case "analyze":
			hasAnalyze = true
		case "done":
			hasDone = true
			// Verify final counts
			if event.Fetched != cfg.TotalRecords {
				t.Errorf("Expected %d fetched records, got %d", cfg.TotalRecords, event.Fetched)
			}
			if event.Indexed != cfg.TotalRecords {
				t.Errorf("Expected %d indexed records, got %d", cfg.TotalRecords, event.Indexed)
			}
		}
	}
	
	// Verify we got all expected event types
	if !hasInitial {
		t.Error("Missing initial event")
	}
	if !hasFetch {
		t.Error("Missing fetch events")
	}
	if !hasAnalyze {
		t.Error("Missing analyze events")
	}
	if !hasDone {
		t.Error("Missing done event")
	}
}

func TestGenomeSequencingCancellation(t *testing.T) {
	sink := &TestSink{}
	cfg := DefaultConfig("test_species")
	
	// Create context that will be cancelled
	ctx, cancel := context.WithCancel(context.Background())
	
	// Cancel after a short delay
	go func() {
		time.Sleep(10 * time.Millisecond)
		cancel()
	}()
	
	err := Run(ctx, sink, "test-job", cfg)
	if err != context.Canceled {
		t.Errorf("Expected context.Canceled error, got %v", err)
	}
}

func TestGetSpeciesInfo(t *testing.T) {
	species := GetSpeciesInfo()
	
	// Check that we have some expected species
	expectedSpecies := []string{
		"brown_pelican",
		"peruvian_pelican",
		"dalmatian_pelican",
	}
	
	for _, expected := range expectedSpecies {
		if _, exists := species[expected]; !exists {
			t.Errorf("Expected species %s not found", expected)
		}
	}
	
	// Check that all entries have descriptions
	for name, description := range species {
		if description == "" {
			t.Errorf("Species %s has empty description", name)
		}
	}
}

