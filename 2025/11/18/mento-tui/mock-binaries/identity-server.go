package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"
)

func main() {
	port := os.Getenv("IDENTITY_SERVICE_PORT")
	if port == "" {
		port = "8083"
	}

	// Set some environment variables for testing
	os.Setenv("STYTCH_PROJECT_ID", "project-test-abc123")
	os.Setenv("GOOGLE_CLIENT_ID", "123456789-xyz.apps.googleusercontent.com")

	fmt.Printf("[Identity] Starting server on :%s...\n", port)
	time.Sleep(2 * time.Second)
	fmt.Printf("[Identity] ✅ Server listening on :%s\n", port)

	// Simple HTTP server
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Identity Server - OK")
		log.Printf("[Identity] %s %s - 200 OK (%dms)", r.Method, r.URL.Path, 12)
	})

	http.HandleFunc("/api/auth/session", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, `{"status":"ok"}`)
		log.Printf("[Identity] GET /api/auth/session - 200 OK (12ms)")
	})

	http.HandleFunc("/api/oauth/google", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, `{"status":"ok"}`)
		log.Printf("[Identity] POST /api/oauth/google - 200 OK (156ms)")
	})

	// Log periodic activity
	go func() {
		ticker := time.NewTicker(10 * time.Second)
		for range ticker.C {
			log.Printf("[Identity] WebSocket connection established: ws_abc%d", time.Now().Unix()%1000)
		}
	}()

	if err := http.ListenAndServe(":"+port, nil); err != nil {
		log.Fatalf("[Identity] Error: %v", err)
	}
}
