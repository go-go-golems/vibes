package scholarly

import (
	"arxiv-libgen-cli/pkg/arxiv"
	"arxiv-libgen-cli/pkg/common"
	"arxiv-libgen-cli/pkg/crossref"
	"arxiv-libgen-cli/pkg/openalex"
	"fmt"
	"strconv"
	"strings"

	"github.com/rs/zerolog/log"
)

// SearchWorks searches for works across different providers
func SearchWorks(req SearchWorksRequest) (*SearchWorksResponse, error) {
	if req.Query == "" {
		return nil, fmt.Errorf("query cannot be empty")
	}

	if req.Limit <= 0 {
		req.Limit = 20 // Default limit
	}

	log.Debug().Str("source", req.Source).Str("query", req.Query).Int("limit", req.Limit).Msg("Searching works")

	// Dispatch to the appropriate provider
	switch strings.ToLower(req.Source) {
	case "arxiv":
		return searchArxiv(req)
	case "crossref":
		return searchCrossref(req)
	case "openalex":
		return searchOpenAlex(req)
	default:
		return nil, fmt.Errorf("unsupported source: %s", req.Source)
	}
}

// searchArxiv searches for works in Arxiv
func searchArxiv(req SearchWorksRequest) (*SearchWorksResponse, error) {
	client := arxiv.NewClient()

	params := common.SearchParams{
		Query:      req.Query,
		MaxResults: req.Limit,
	}

	// Convert filter map to appropriate arxiv query parameters if needed
	if len(req.Filter) > 0 {
		filterQueries := []string{}
		for k, v := range req.Filter {
			switch k {
			case "category":
				filterQueries = append(filterQueries, fmt.Sprintf("cat:%s", v))
			case "author":
				filterQueries = append(filterQueries, fmt.Sprintf("au:\"%s\"", v))
			case "title":
				filterQueries = append(filterQueries, fmt.Sprintf("ti:\"%s\"", v))
			}
		}
		
		if len(filterQueries) > 0 {
			// Combine with the main query using AND
			if !strings.Contains(params.Query, ":") {
				// If query doesn't have a prefix, assume all fields
				params.Query = "all:" + params.Query
			}
			params.Query = fmt.Sprintf("%s AND %s", params.Query, strings.Join(filterQueries, " AND "))
		}
	}

	results, err := client.Search(params)
	if err != nil {
		return nil, fmt.Errorf("arxiv search error: %w", err)
	}

	works := make([]Work, 0, len(results))
	for _, result := range results {
		// Parse year from published date (format: YYYY-MM-DD)
		year := 0
		if len(result.Published) >= 4 {
			year, _ = strconv.Atoi(result.Published[:4])
		}

		work := Work{
			ID:         result.SourceURL,
			DOI:        result.DOI,
			Title:      result.Title,
			Authors:    result.Authors,
			Year:       year,
			IsOA:       true, // arXiv is always OA
			Abstract:   result.Abstract,
			SourceName: "arxiv",
			PDFURL:     result.PDFURL,
		}
		works = append(works, work)
	}

	return &SearchWorksResponse{Works: works}, nil
}

// searchCrossref searches for works in Crossref
func searchCrossref(req SearchWorksRequest) (*SearchWorksResponse, error) {
	client := crossref.NewClient("") // No mailto for now, could add as an option in the future

	params := common.SearchParams{
		Query:      req.Query,
		MaxResults: req.Limit,
	}

	// Apply filters if present
	if len(req.Filter) > 0 {
		params.Filters = req.Filter
	}

	// Execute search
	results, err := client.Search(params)
	if err != nil {
		return nil, fmt.Errorf("crossref search error: %w", err)
	}

	// Convert to common Work format
	works := make([]Work, 0, len(results))
	for _, result := range results {
		year := 0
		if y, ok := result.Metadata["year"].(int); ok {
			year = y
		}

		citationCount := 0
		if c, ok := result.Metadata["is-referenced-by-count"].(int); ok {
			citationCount = c
		}

		work := Work{
			ID:            result.DOI, // Crossref uses DOI as ID
			DOI:           result.DOI,
			Title:         result.Title,
			Authors:       result.Authors,
			Year:          year,
			CitationCount: citationCount,
			SourceName:    "crossref",
		}
		works = append(works, work)
	}

	return &SearchWorksResponse{Works: works}, nil
}

// searchOpenAlex searches for works in OpenAlex
func searchOpenAlex(req SearchWorksRequest) (*SearchWorksResponse, error) {
	client := openalex.NewClient("") // No mailto for now, could add as an option in the future

	params := common.SearchParams{
		Query:      req.Query,
		MaxResults: req.Limit,
	}

	// Apply filters if present
	if len(req.Filter) > 0 {
		params.Filters = req.Filter
	}

	// Execute search
	results, err := client.Search(params)
	if err != nil {
		return nil, fmt.Errorf("openalex search error: %w", err)
	}

	// Convert to common Work format
	works := make([]Work, 0, len(results))
	for _, result := range results {
		year := 0
		if y, ok := result.Metadata["publication_year"].(int); ok {
			year = y
		}

		isOA := false
		if oa, ok := result.Metadata["is_oa"].(bool); ok {
			isOA = oa
		}

		work := Work{
			ID:            result.SourceURL, // OpenAlex ID
			DOI:           result.DOI,
			Title:         result.Title,
			Authors:       result.Authors,
			Year:          year,
			IsOA:          isOA,
			CitationCount: result.Citations,
			Abstract:      result.Abstract,
			SourceName:    "openalex",
			PDFURL:        result.PDFURL,
		}
		works = append(works, work)
	}

	return &SearchWorksResponse{Works: works}, nil
}