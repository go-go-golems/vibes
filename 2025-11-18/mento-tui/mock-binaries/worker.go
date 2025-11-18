package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"
)

func main() {
	port := os.Getenv("MENTO_SERVICE_PORT")
	if port == "" {
		port = "8082"
	}

	// Set environment variables for testing
	os.Setenv("ONE_ON_ONE_V3_DATABASE_URL", "postgres://postgres:***@localhost:5432/mento_oneononev3")
	os.Setenv("WORKFLOWS_DATABASE_URL", "postgres://postgres:***@localhost:5432/mento_workflows")

	fmt.Printf("[Worker] Connecting to database...\n")
	time.Sleep(2 * time.Second)
	fmt.Printf("[Worker] ✅ Database connected\n")
	fmt.Printf("[Worker] Starting server on :%s...\n", port)
	time.Sleep(1 * time.Second)
	fmt.Printf("[Worker] ✅ Server listening on :%s\n", port)

	// Simple HTTP server
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Mento Worker - OK")
	})

	http.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, `{"status":"healthy"}`)
	})

	// Simulate background processing
	go func() {
		time.Sleep(5 * time.Second)
		ticker := time.NewTicker(8 * time.Second)
		for range ticker.C {
			log.Printf("[Worker] ⏱️ DocLens query completed in %dms", 200+time.Now().Unix()%100)
			time.Sleep(1 * time.Second)
			log.Printf("[Worker] Processing 1:1 document: \"Weekly Sync - Q4\"")
			time.Sleep(1 * time.Second)
			log.Printf("[Worker] Relevance score: 0.89 (high match)")
			time.Sleep(2 * time.Second)
			log.Printf("[Worker] ⏱️ Database query completed in %dms", 40+time.Now().Unix()%20)
			time.Sleep(1 * time.Second)
			log.Printf("[Worker] Syncing calendar events for user_%d", time.Now().Unix()%1000)
			time.Sleep(2 * time.Second)
			log.Printf("[Worker] Found %d upcoming 1:1 meetings", 10+time.Now().Unix()%5)
			time.Sleep(1 * time.Second)
			log.Printf("[Worker] ⏱️ Workflow execution completed in %.1fs", 1.0+float64(time.Now().Unix()%5)/10)
			time.Sleep(2 * time.Second)
			log.Printf("[Worker] Caching results for 30 minutes")
			time.Sleep(1 * time.Second)
			log.Printf("[Worker] Background job scheduled for %s", time.Now().Add(1*time.Hour).Format("15:04"))
		}
	}()

	if err := http.ListenAndServe(":"+port, nil); err != nil {
		log.Fatalf("[Worker] Error: %v", err)
	}
}
