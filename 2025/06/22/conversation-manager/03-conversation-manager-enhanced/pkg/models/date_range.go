package models

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// DateRange represents a date range for filtering
type DateRange struct {
	Start *time.Time `json:"start,omitempty"`
	End   *time.Time `json:"end,omitempty"`
	Label string     `json:"label"`
}

// DateRangeType represents different types of date ranges
type DateRangeType int

const (
	DateRangeCustom DateRangeType = iota
	DateRangeToday
	DateRangeYesterday
	DateRangeThisWeek
	DateRangeLastWeek
	DateRangeThisMonth
	DateRangeLastMonth
	DateRangeThisYear
	DateRangeLastYear
	DateRangeLast7Days
	DateRangeLast30Days
	DateRangeLast90Days
)

// DateRangeParser handles parsing of date range expressions
type DateRangeParser struct {
	now time.Time
}

// NewDateRangeParser creates a new date range parser
func NewDateRangeParser() *DateRangeParser {
	return &DateRangeParser{
		now: time.Now(),
	}
}

// ParseDateRange parses a date range expression
func (p *DateRangeParser) ParseDateRange(expr string) (*DateRange, error) {
	expr = strings.TrimSpace(strings.ToLower(expr))
	
	// Handle predefined ranges
	if dr := p.parsePredefinedRange(expr); dr != nil {
		return dr, nil
	}
	
	// Handle relative ranges
	if dr := p.parseRelativeRange(expr); dr != nil {
		return dr, nil
	}
	
	// Handle absolute date ranges
	if dr := p.parseAbsoluteRange(expr); dr != nil {
		return dr, nil
	}
	
	return nil, fmt.Errorf("unable to parse date range: %s", expr)
}

// parsePredefinedRange parses predefined date ranges
func (p *DateRangeParser) parsePredefinedRange(expr string) *DateRange {
	now := p.now
	
	switch expr {
	case "today":
		start := time.Date(now.Year(), now.Month(), now.Day(), 0, 0, 0, 0, now.Location())
		end := start.Add(24 * time.Hour)
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Today",
		}
		
	case "yesterday":
		yesterday := now.AddDate(0, 0, -1)
		start := time.Date(yesterday.Year(), yesterday.Month(), yesterday.Day(), 0, 0, 0, 0, yesterday.Location())
		end := start.Add(24 * time.Hour)
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Yesterday",
		}
		
	case "this-week", "thisweek":
		weekday := int(now.Weekday())
		if weekday == 0 { // Sunday
			weekday = 7
		}
		start := now.AddDate(0, 0, -(weekday-1))
		start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
		end := start.AddDate(0, 0, 7)
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "This Week",
		}
		
	case "last-week", "lastweek":
		weekday := int(now.Weekday())
		if weekday == 0 { // Sunday
			weekday = 7
		}
		thisWeekStart := now.AddDate(0, 0, -(weekday-1))
		start := thisWeekStart.AddDate(0, 0, -7)
		start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
		end := start.AddDate(0, 0, 7)
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Last Week",
		}
		
	case "this-month", "thismonth":
		start := time.Date(now.Year(), now.Month(), 1, 0, 0, 0, 0, now.Location())
		end := start.AddDate(0, 1, 0)
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "This Month",
		}
		
	case "last-month", "lastmonth":
		thisMonthStart := time.Date(now.Year(), now.Month(), 1, 0, 0, 0, 0, now.Location())
		start := thisMonthStart.AddDate(0, -1, 0)
		end := thisMonthStart
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Last Month",
		}
		
	case "this-year", "thisyear":
		start := time.Date(now.Year(), 1, 1, 0, 0, 0, 0, now.Location())
		end := start.AddDate(1, 0, 0)
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "This Year",
		}
		
	case "last-year", "lastyear":
		start := time.Date(now.Year()-1, 1, 1, 0, 0, 0, 0, now.Location())
		end := time.Date(now.Year(), 1, 1, 0, 0, 0, 0, now.Location())
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Last Year",
		}
	}
	
	return nil
}

// parseRelativeRange parses relative date ranges like "last-7-days", "last-30-days"
func (p *DateRangeParser) parseRelativeRange(expr string) *DateRange {
	now := p.now
	
	// Pattern: last-N-days, last-N-weeks, last-N-months
	relativePattern := regexp.MustCompile(`^last-(\d+)-(days?|weeks?|months?)$`)
	matches := relativePattern.FindStringSubmatch(expr)
	
	if len(matches) == 3 {
		num, err := strconv.Atoi(matches[1])
		if err != nil {
			return nil
		}
		
		unit := matches[2]
		var start time.Time
		var label string
		
		switch {
		case strings.HasPrefix(unit, "day"):
			start = now.AddDate(0, 0, -num)
			label = fmt.Sprintf("Last %d Days", num)
		case strings.HasPrefix(unit, "week"):
			start = now.AddDate(0, 0, -num*7)
			label = fmt.Sprintf("Last %d Weeks", num)
		case strings.HasPrefix(unit, "month"):
			start = now.AddDate(0, -num, 0)
			label = fmt.Sprintf("Last %d Months", num)
		default:
			return nil
		}
		
		start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
		end := time.Date(now.Year(), now.Month(), now.Day(), 23, 59, 59, 999999999, now.Location())
		
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: label,
		}
	}
	
	// Handle common shortcuts
	switch expr {
	case "last-7-days", "last7days", "week":
		start := now.AddDate(0, 0, -7)
		start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
		end := time.Date(now.Year(), now.Month(), now.Day(), 23, 59, 59, 999999999, now.Location())
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Last 7 Days",
		}
		
	case "last-30-days", "last30days", "month":
		start := now.AddDate(0, 0, -30)
		start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
		end := time.Date(now.Year(), now.Month(), now.Day(), 23, 59, 59, 999999999, now.Location())
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Last 30 Days",
		}
		
	case "last-90-days", "last90days", "quarter":
		start := now.AddDate(0, 0, -90)
		start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
		end := time.Date(now.Year(), now.Month(), now.Day(), 23, 59, 59, 999999999, now.Location())
		return &DateRange{
			Start: &start,
			End:   &end,
			Label: "Last 90 Days",
		}
	}
	
	return nil
}

// parseAbsoluteRange parses absolute date ranges
func (p *DateRangeParser) parseAbsoluteRange(expr string) *DateRange {
	// Pattern: YYYY-MM-DD to YYYY-MM-DD
	rangePattern := regexp.MustCompile(`^(\d{4}-\d{2}-\d{2})\s+to\s+(\d{4}-\d{2}-\d{2})$`)
	matches := rangePattern.FindStringSubmatch(expr)
	
	if len(matches) == 3 {
		start, err1 := time.Parse("2006-01-02", matches[1])
		end, err2 := time.Parse("2006-01-02", matches[2])
		
		if err1 == nil && err2 == nil {
			// Set start to beginning of day, end to end of day
			start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
			end = time.Date(end.Year(), end.Month(), end.Day(), 23, 59, 59, 999999999, end.Location())
			
			return &DateRange{
				Start: &start,
				End:   &end,
				Label: fmt.Sprintf("%s to %s", matches[1], matches[2]),
			}
		}
	}
	
	// Pattern: single date (YYYY-MM-DD)
	datePattern := regexp.MustCompile(`^\d{4}-\d{2}-\d{2}$`)
	if datePattern.MatchString(expr) {
		date, err := time.Parse("2006-01-02", expr)
		if err == nil {
			start := time.Date(date.Year(), date.Month(), date.Day(), 0, 0, 0, 0, date.Location())
			end := start.Add(24 * time.Hour)
			
			return &DateRange{
				Start: &start,
				End:   &end,
				Label: expr,
			}
		}
	}
	
	return nil
}

// GetPredefinedDateRanges returns a list of predefined date ranges
func GetPredefinedDateRanges() []string {
	return []string{
		"today",
		"yesterday",
		"this-week",
		"last-week",
		"this-month",
		"last-month",
		"this-year",
		"last-year",
		"last-7-days",
		"last-30-days",
		"last-90-days",
	}
}

// IsInRange checks if a timestamp falls within the date range
func (dr *DateRange) IsInRange(timestamp time.Time) bool {
	if dr.Start != nil && timestamp.Before(*dr.Start) {
		return false
	}
	
	if dr.End != nil && timestamp.After(*dr.End) {
		return false
	}
	
	return true
}

// String returns a string representation of the date range
func (dr *DateRange) String() string {
	if dr.Label != "" {
		return dr.Label
	}
	
	if dr.Start != nil && dr.End != nil {
		return fmt.Sprintf("%s to %s", 
			dr.Start.Format("2006-01-02"), 
			dr.End.Format("2006-01-02"))
	}
	
	if dr.Start != nil {
		return fmt.Sprintf("after %s", dr.Start.Format("2006-01-02"))
	}
	
	if dr.End != nil {
		return fmt.Sprintf("before %s", dr.End.Format("2006-01-02"))
	}
	
	return "all time"
}

// DateRangeSearchTerm represents a date range search term
type DateRangeSearchTerm struct {
	Type  string     // "after", "before", "range", "on"
	Range *DateRange
}

// ParseDateRangeSearchTerms parses date range search terms from a query
func ParseDateRangeSearchTerms(query string) []DateRangeSearchTerm {
	var terms []DateRangeSearchTerm
	parser := NewDateRangeParser()
	
	// Pattern: after:DATE, before:DATE, on:DATE, range:RANGE
	afterPattern := regexp.MustCompile(`after:([^\s]+)`)
	beforePattern := regexp.MustCompile(`before:([^\s]+)`)
	onPattern := regexp.MustCompile(`on:([^\s]+)`)
	rangePattern := regexp.MustCompile(`range:([^\s]+)`)
	
	// Parse "after:" terms
	afterMatches := afterPattern.FindAllStringSubmatch(query, -1)
	for _, match := range afterMatches {
		if dateRange, err := parser.ParseDateRange(match[1]); err == nil {
			// Convert to "after" range (no end date)
			terms = append(terms, DateRangeSearchTerm{
				Type: "after",
				Range: &DateRange{
					Start: dateRange.Start,
					End:   nil,
					Label: "after " + dateRange.Label,
				},
			})
		}
	}
	
	// Parse "before:" terms
	beforeMatches := beforePattern.FindAllStringSubmatch(query, -1)
	for _, match := range beforeMatches {
		if dateRange, err := parser.ParseDateRange(match[1]); err == nil {
			// Convert to "before" range (no start date)
			terms = append(terms, DateRangeSearchTerm{
				Type: "before",
				Range: &DateRange{
					Start: nil,
					End:   dateRange.Start, // Use start as the "before" cutoff
					Label: "before " + dateRange.Label,
				},
			})
		}
	}
	
	// Parse "on:" terms
	onMatches := onPattern.FindAllStringSubmatch(query, -1)
	for _, match := range onMatches {
		if dateRange, err := parser.ParseDateRange(match[1]); err == nil {
			terms = append(terms, DateRangeSearchTerm{
				Type:  "on",
				Range: dateRange,
			})
		}
	}
	
	// Parse "range:" terms
	rangeMatches := rangePattern.FindAllStringSubmatch(query, -1)
	for _, match := range rangeMatches {
		if dateRange, err := parser.ParseDateRange(match[1]); err == nil {
			terms = append(terms, DateRangeSearchTerm{
				Type:  "range",
				Range: dateRange,
			})
		}
	}
	
	return terms
}

// CleanDateRangeQuery removes date range terms from a search query
func CleanDateRangeQuery(query string) string {
	// Remove date range patterns
	patterns := []string{
		`after:[^\s]+`,
		`before:[^\s]+`,
		`on:[^\s]+`,
		`range:[^\s]+`,
	}
	
	for _, pattern := range patterns {
		re := regexp.MustCompile(pattern)
		query = re.ReplaceAllString(query, "")
	}
	
	// Clean up extra whitespace
	query = regexp.MustCompile(`\s+`).ReplaceAllString(strings.TrimSpace(query), " ")
	
	return query
}

