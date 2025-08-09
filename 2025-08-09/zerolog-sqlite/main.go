package main

import (
	"fmt"
	"log"
	"time"

	"github.com/rs/zerolog"
)

func main() {
	// Initialize database
	db, err := NewDatabase("./logs.db")
	if err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	defer db.Close()

	// Create SQLite writer
	sqliteWriter, err := NewSQLiteWriter(db, WithBatchSize(1))
	if err != nil {
		log.Fatalf("Failed to create SQLite writer: %v", err)
	}
	defer sqliteWriter.Close()

	// Create zerolog logger with SQLite backend
	logger := zerolog.New(sqliteWriter).With().Timestamp().Caller().Logger()

	// Example 1: Basic logging
	fmt.Println("=== Example 1: Basic Logging ===")
	logger.Info().Msg("Application started")
	logger.Debug().Msg("Debug message")
	logger.Warn().Msg("Warning message")
	logger.Error().Msg("Error message")

	// Example 2: Structured logging
	fmt.Println("=== Example 2: Structured Logging ===")
	logger.Info().
		Str("user_id", "12345").
		Int("age", 30).
		Bool("active", true).
		Float64("score", 95.5).
		Msg("User login")

	logger.Info().
		Str("service", "payment").
		Str("transaction_id", "txn_67890").
		Float64("amount", 99.99).
		Str("currency", "USD").
		Msg("Payment processed")

	// Example 3: Complex structured data
	fmt.Println("=== Example 3: Complex Structured Data ===")
	logger.Info().
		Str("event", "order_created").
		Str("order_id", "ord_123456").
		Interface("items", []map[string]interface{}{
			{"name": "Widget A", "price": 19.99, "quantity": 2},
			{"name": "Widget B", "price": 29.99, "quantity": 1},
		}).
		Interface("customer", map[string]interface{}{
			"id":    "cust_789",
			"name":  "John Doe",
			"email": "john@example.com",
		}).
		Msg("Order created successfully")

	// Example 4: Error logging with stack trace
	fmt.Println("=== Example 4: Error Logging ===")
	err = fmt.Errorf("database connection failed")
	logger.Error().
		Err(err).
		Str("database", "postgres").
		Str("host", "localhost").
		Int("port", 5432).
		Msg("Failed to connect to database")

	// Wait a moment to ensure all logs are written
	time.Sleep(100 * time.Millisecond)

	// Example 5: Querying logs
	fmt.Println("\n=== Example 5: Querying Logs ===")
	querier := NewLogQuerier(db)

	// Query all logs
	fmt.Println("--- All Logs ---")
	allLogs, err := querier.QueryLogs(QueryOptions{
		Limit:     10,
		OrderDesc: true,
	})
	if err != nil {
		log.Printf("Failed to query all logs: %v", err)
	} else {
		for _, logEntry := range allLogs {
			fmt.Printf("[%s] %s: %s\n", 
				logEntry.Timestamp.Format("15:04:05"), 
				logEntry.Level, 
				logEntry.Message)
		}
	}

	// Query logs by level
	fmt.Println("\n--- Error Logs Only ---")
	errorLogs, err := querier.QueryLogs(QueryOptions{
		Levels: []string{"error"},
		Limit:  5,
	})
	if err != nil {
		log.Printf("Failed to query error logs: %v", err)
	} else {
		for _, logEntry := range errorLogs {
			fmt.Printf("[%s] %s: %s (Fields: %v)\n", 
				logEntry.Timestamp.Format("15:04:05"), 
				logEntry.Level, 
				logEntry.Message,
				logEntry.Fields)
		}
	}

	// Query logs with field filters
	fmt.Println("\n--- Logs with user_id field ---")
	userLogs, err := querier.QueryLogs(QueryOptions{
		Fields: map[string]string{
			"user_id": "12345",
		},
		Limit: 5,
	})
	if err != nil {
		log.Printf("Failed to query user logs: %v", err)
	} else {
		for _, logEntry := range userLogs {
			fmt.Printf("[%s] %s: %s (Fields: %v)\n", 
				logEntry.Timestamp.Format("15:04:05"), 
				logEntry.Level, 
				logEntry.Message,
				logEntry.Fields)
		}
	}

	// Query logs by time range
	fmt.Println("\n--- Recent Logs (last 5 minutes) ---")
	fiveMinutesAgo := time.Now().Add(-5 * time.Minute)
	recentLogs, err := querier.QueryLogs(QueryOptions{
		StartTime: &fiveMinutesAgo,
		Limit:     10,
		OrderDesc: true,
	})
	if err != nil {
		log.Printf("Failed to query recent logs: %v", err)
	} else {
		for _, logEntry := range recentLogs {
			fmt.Printf("[%s] %s: %s\n", 
				logEntry.Timestamp.Format("15:04:05"), 
				logEntry.Level, 
				logEntry.Message)
		}
	}

	// Get log statistics
	fmt.Println("\n--- Log Statistics ---")
	stats, err := querier.GetLogStats(QueryOptions{})
	if err != nil {
		log.Printf("Failed to get log stats: %v", err)
	} else {
		fmt.Printf("Total logs: %d\n", stats.TotalCount)
		fmt.Printf("Level counts:\n")
		for level, count := range stats.LevelCounts {
			fmt.Printf("  %s: %d\n", level, count)
		}
		if !stats.TimeRange.Start.IsZero() && !stats.TimeRange.End.IsZero() {
			fmt.Printf("Time range: %s to %s\n", 
				stats.TimeRange.Start.Format("15:04:05"), 
				stats.TimeRange.End.Format("15:04:05"))
		}
	}

	// Example 6: Performance test
	fmt.Println("\n=== Example 6: Performance Test ===")
	performanceTest(logger, querier)

	fmt.Println("\n=== Examples completed successfully! ===")
	fmt.Println("Database file: ./logs.db")
	fmt.Println("You can inspect the database using any SQLite browser or CLI tool.")
}

func performanceTest(logger zerolog.Logger, querier *LogQuerier) {
	fmt.Println("Running performance test with 1000 log entries...")
	
	start := time.Now()
	
	// Log 1000 entries
	for i := 0; i < 1000; i++ {
		logger.Info().
			Int("iteration", i).
			Str("test_type", "performance").
			Float64("random_value", float64(i)*1.23).
			Bool("even", i%2 == 0).
			Msgf("Performance test log entry %d", i)
	}
	
	writeTime := time.Since(start)
	fmt.Printf("Write performance: 1000 logs in %v (%.2f logs/sec)\n", 
		writeTime, 1000.0/writeTime.Seconds())
	
	// Query performance test
	start = time.Now()
	
	logs, err := querier.QueryLogs(QueryOptions{
		Fields: map[string]string{
			"test_type": "performance",
		},
		Limit: 100,
	})
	
	queryTime := time.Since(start)
	
	if err != nil {
		fmt.Printf("Query failed: %v\n", err)
	} else {
		fmt.Printf("Query performance: Retrieved %d logs in %v\n", 
			len(logs), queryTime)
	}
	
	// Count performance test
	start = time.Now()
	count, err := querier.CountLogs(QueryOptions{
		Fields: map[string]string{
			"test_type": "performance",
		},
	})
	countTime := time.Since(start)
	
	if err != nil {
		fmt.Printf("Count failed: %v\n", err)
	} else {
		fmt.Printf("Count performance: Counted %d logs in %v\n", 
			count, countTime)
	}
}

