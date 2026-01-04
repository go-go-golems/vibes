package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"strings"
	"time"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]

	switch command {
	case "list":
		runListCommand(args)
	case "stats":
		runStatsCommand(args)
	case "search":
		runSearchCommand(args)
	case "fields":
		runFieldsCommand(args)
	case "cleanup":
		runCleanupCommand(args)
	case "help", "--help", "-h":
		printUsage()
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		printUsage()
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Println(`logctl - SQLite log database introspection tool

USAGE:
    logctl <command> [options]

COMMANDS:
    list     List log entries with optional filtering
    stats    Show statistics about log entries  
    search   Search log entries by message content
    fields   Show information about log fields
    cleanup  Remove old log entries
    help     Show this help message

EXAMPLES:
    logctl list                    # List recent logs
    logctl list --level error      # List error logs only
    logctl stats                   # Show log statistics
    logctl search "database"       # Search for logs containing "database"
    logctl fields                  # Show field information
    logctl cleanup --dry-run 7d    # Show what would be deleted (7 days old)

For more detailed help on each command, use: logctl <command> --help`)
}

func runListCommand(args []string) {
	fs := flag.NewFlagSet("list", flag.ExitOnError)
	dbPath := fs.String("database", "./logs.db", "Path to SQLite database file")
	level := fs.String("level", "", "Filter by log level (debug,info,warn,error)")
	message := fs.String("message", "", "Filter by message content")
	since := fs.String("since", "", "Show logs since this time (duration like '1h' or RFC3339)")
	limit := fs.Int("limit", 100, "Maximum number of results")
	output := fs.String("output", "table", "Output format (table, json)")
	
	fs.Parse(args)

	// Open database
	db, err := NewDatabase(*dbPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	// Create querier
	querier := NewLogQuerier(db)

	// Parse options
	options := QueryOptions{
		Limit:     *limit,
		OrderDesc: true,
	}

	if *level != "" {
		options.Levels = strings.Split(*level, ",")
	}

	if *message != "" {
		options.Message = *message
	}

	if *since != "" {
		startTime, err := parseTimeOrDuration(*since)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Invalid since time: %v\n", err)
			os.Exit(1)
		}
		options.StartTime = &startTime
	}

	// Query logs
	logs, err := querier.QueryLogs(options)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error querying logs: %v\n", err)
		os.Exit(1)
	}

	// Output results
	if *output == "json" {
		outputJSON(logs)
	} else {
		outputTable(logs)
	}
}

func runStatsCommand(args []string) {
	fs := flag.NewFlagSet("stats", flag.ExitOnError)
	dbPath := fs.String("database", "./logs.db", "Path to SQLite database file")
	output := fs.String("output", "table", "Output format (table, json)")
	
	fs.Parse(args)

	// Open database
	db, err := NewDatabase(*dbPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	// Create querier
	querier := NewLogQuerier(db)

	// Get statistics
	stats, err := querier.GetLogStats(QueryOptions{})
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting stats: %v\n", err)
		os.Exit(1)
	}

	// Output results
	if *output == "json" {
		data, _ := json.MarshalIndent(stats, "", "  ")
		fmt.Println(string(data))
	} else {
		fmt.Printf("Total logs: %d\n", stats.TotalCount)
		fmt.Println("\nLevel counts:")
		for level, count := range stats.LevelCounts {
			fmt.Printf("  %s: %d\n", level, count)
		}
		if !stats.TimeRange.Start.IsZero() {
			fmt.Printf("\nTime range: %s to %s\n", 
				stats.TimeRange.Start.Format(time.RFC3339),
				stats.TimeRange.End.Format(time.RFC3339))
		}
	}
}

func runSearchCommand(args []string) {
	fs := flag.NewFlagSet("search", flag.ExitOnError)
	dbPath := fs.String("database", "./logs.db", "Path to SQLite database file")
	limit := fs.Int("limit", 50, "Maximum number of results")
	output := fs.String("output", "table", "Output format (table, json)")
	
	fs.Parse(args)

	if len(fs.Args()) == 0 {
		fmt.Fprintf(os.Stderr, "Search query is required\n")
		os.Exit(1)
	}

	query := fs.Args()[0]

	// Open database
	db, err := NewDatabase(*dbPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	// Create querier
	querier := NewLogQuerier(db)

	// Search logs
	options := QueryOptions{
		Message:   query,
		Limit:     *limit,
		OrderDesc: true,
	}

	logs, err := querier.QueryLogs(options)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error searching logs: %v\n", err)
		os.Exit(1)
	}

	// Output results
	if *output == "json" {
		outputJSON(logs)
	} else {
		outputTable(logs)
	}
}

func runFieldsCommand(args []string) {
	fs := flag.NewFlagSet("fields", flag.ExitOnError)
	dbPath := fs.String("database", "./logs.db", "Path to SQLite database file")
	output := fs.String("output", "table", "Output format (table, json)")
	
	fs.Parse(args)

	// Open database
	db, err := NewDatabase(*dbPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	// Create querier
	querier := NewLogQuerier(db)

	// Get field information
	fields, err := querier.GetFieldInfo()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting field info: %v\n", err)
		os.Exit(1)
	}

	// Output results
	if *output == "json" {
		data, _ := json.MarshalIndent(fields, "", "  ")
		fmt.Println(string(data))
	} else {
		fmt.Printf("%-20s %-10s %-8s %s\n", "NAME", "TYPE", "COUNT", "SAMPLE VALUE")
		fmt.Println(strings.Repeat("-", 60))
		for _, field := range fields {
			sampleValue := field.SampleValue
			if len(sampleValue) > 20 {
				sampleValue = sampleValue[:17] + "..."
			}
			fmt.Printf("%-20s %-10s %-8d %s\n", field.Name, field.Type, field.Count, sampleValue)
		}
	}
}

func runCleanupCommand(args []string) {
	fs := flag.NewFlagSet("cleanup", flag.ExitOnError)
	dbPath := fs.String("database", "./logs.db", "Path to SQLite database file")
	dryRun := fs.Bool("dry-run", false, "Show what would be deleted without deleting")
	
	fs.Parse(args)

	if len(fs.Args()) == 0 {
		fmt.Fprintf(os.Stderr, "Duration is required (e.g., '7d', '24h')\n")
		os.Exit(1)
	}

	durationStr := fs.Args()[0]
	duration, err := time.ParseDuration(durationStr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Invalid duration: %v\n", err)
		os.Exit(1)
	}

	// Open database
	db, err := NewDatabase(*dbPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening database: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	// Create querier
	querier := NewLogQuerier(db)

	if *dryRun {
		// Count what would be deleted
		cutoffTime := time.Now().Add(-duration)
		options := QueryOptions{
			EndTime: &cutoffTime,
		}
		count, err := querier.CountLogs(options)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error counting logs: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Would delete %d logs older than %s (cutoff: %s)\n", 
			count, duration, cutoffTime.Format(time.RFC3339))
	} else {
		// Actually delete
		deleted, err := querier.DeleteOldLogs(duration)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error deleting logs: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Deleted %d logs older than %s\n", deleted, duration)
	}
}

func outputJSON(data interface{}) {
	jsonData, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error marshaling JSON: %v\n", err)
		os.Exit(1)
	}
	fmt.Println(string(jsonData))
}

func outputTable(logs []LogResult) {
	if len(logs) == 0 {
		fmt.Println("No logs found")
		return
	}

	fmt.Printf("%-6s %-20s %-8s %-50s %-20s\n", "ID", "TIMESTAMP", "LEVEL", "MESSAGE", "CALLER")
	fmt.Println(strings.Repeat("-", 110))
	
	for _, log := range logs {
		message := log.Message
		if len(message) > 47 {
			message = message[:44] + "..."
		}
		caller := log.Caller
		if len(caller) > 17 {
			caller = caller[:14] + "..."
		}
		fmt.Printf("%-6d %-20s %-8s %-50s %-20s\n", 
			log.ID, 
			log.Timestamp.Format("2006-01-02 15:04:05"), 
			log.Level, 
			message,
			caller)
	}
}

func parseTimeOrDuration(input string) (time.Time, error) {
	// Try parsing as RFC3339 first
	if t, err := time.Parse(time.RFC3339, input); err == nil {
		return t, nil
	}

	// Try parsing as duration (relative to now)
	if duration, err := time.ParseDuration(input); err == nil {
		return time.Now().Add(-duration), nil
	}

	return time.Time{}, fmt.Errorf("invalid time format: %s", input)
}

