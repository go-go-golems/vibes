package search

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
)

// DefaultSearcher implements the Searcher interface
type DefaultSearcher struct {
	engines map[string]SearchEngine
	mu      sync.RWMutex
}

// NewSearcher creates a new DefaultSearcher instance
func NewSearcher() *DefaultSearcher {
	searcher := &DefaultSearcher{
		engines: make(map[string]SearchEngine),
	}

	// Initialize with default engines
	searcher.initializeDefaultEngines()

	return searcher
}

// initializeDefaultEngines sets up the default search engines
func (s *DefaultSearcher) initializeDefaultEngines() {
	// Add demo engine for testing
	demoEngine := NewDemoEngine()
	s.engines["demo"] = demoEngine
	s.engines["all"] = demoEngine // Default to demo for "all" searches
}

// Search performs a product search using the specified parameters
func (s *DefaultSearcher) Search(ctx context.Context, params SearchParams) (SearchResults, error) {
	log.Debug().
		Str("query", params.Query).
		Str("site", params.Site).
		Int("max_results", params.MaxResults).
		Msg("Starting search")

	s.mu.RLock()
	defer s.mu.RUnlock()

	var allResults SearchResults
	var searchErrors []error

	// Determine which engines to use
	engines := s.getEnginesForSite(params.Site)
	if len(engines) == 0 {
		return nil, fmt.Errorf("no search engines available for site: %s", params.Site)
	}

	// Search using each engine
	for siteName, engine := range engines {
		log.Debug().Str("site", siteName).Msg("Searching with engine")

		// Create site-specific parameters
		siteParams := params
		siteParams.Site = siteName

		results, err := engine.Search(siteParams)
		if err != nil {
			log.Warn().Err(err).Str("site", siteName).Msg("Search failed for site")
			searchErrors = append(searchErrors, errors.Wrapf(err, "search failed for site %s", siteName))
			continue
		}

		// Apply filters
		filteredResults := s.applyFilters(results, params)
		allResults = append(allResults, filteredResults...)

		log.Debug().
			Str("site", siteName).
			Int("results", len(filteredResults)).
			Msg("Search completed for site")
	}

	// Limit results if specified
	if params.MaxResults > 0 && len(allResults) > params.MaxResults {
		allResults = allResults[:params.MaxResults]
	}

	// Sort results by relevance/price
	s.sortResults(allResults)

	log.Info().
		Int("total_results", len(allResults)).
		Int("errors", len(searchErrors)).
		Msg("Search completed")

	if len(allResults) == 0 && len(searchErrors) > 0 {
		return nil, fmt.Errorf("all searches failed: %v", searchErrors)
	}

	return allResults, nil
}

// getEnginesForSite returns the appropriate engines for the specified site
func (s *DefaultSearcher) getEnginesForSite(site string) map[string]SearchEngine {
	engines := make(map[string]SearchEngine)

	if site == "all" {
		// Return all available engines
		for name, engine := range s.engines {
			if name != "all" { // Avoid infinite recursion
				engines[name] = engine
			}
		}
	} else {
		// Return specific engine if available
		if engine, exists := s.engines[site]; exists {
			engines[site] = engine
		}
	}

	return engines
}

// applyFilters applies price and other filters to search results
func (s *DefaultSearcher) applyFilters(results SearchResults, params SearchParams) SearchResults {
	var filtered SearchResults

	for _, result := range results {
		// Apply price filters
		if params.MinPrice > 0 && result.Price < params.MinPrice {
			continue
		}
		if params.MaxPrice > 0 && result.Price > params.MaxPrice {
			continue
		}

		// Add timestamp
		result.SearchedAt = time.Now()

		filtered = append(filtered, result)
	}

	return filtered
}

// sortResults sorts the results by price (ascending)
func (s *DefaultSearcher) sortResults(results SearchResults) {
	// Simple bubble sort for demonstration
	n := len(results)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			if results[j].Price > results[j+1].Price {
				results[j], results[j+1] = results[j+1], results[j]
			}
		}
	}
}

// AddEngine adds a new search engine
func (s *DefaultSearcher) AddEngine(name string, engine SearchEngine) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if _, exists := s.engines[name]; exists {
		return fmt.Errorf("engine %s already exists", name)
	}

	s.engines[name] = engine
	log.Info().Str("engine", name).Msg("Search engine added")

	return nil
}

// GetAvailableSites returns a list of all available sites
func (s *DefaultSearcher) GetAvailableSites() []string {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var sites []string
	for name := range s.engines {
		if name != "all" {
			sites = append(sites, name)
		}
	}

	return sites
}

// GetSupportedSites returns supported sites as a formatted string
func (s *DefaultSearcher) GetSupportedSites() string {
	sites := s.GetAvailableSites()
	if len(sites) == 0 {
		return "none"
	}
	return strings.Join(sites, ", ")
}

