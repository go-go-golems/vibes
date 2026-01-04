package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"
)

func main() {
	port := os.Getenv("VITE_PORT")
	if port == "" {
		port = "5173"
	}

	fmt.Printf("[Frontend] VITE v5.0.0 starting...\n")
	time.Sleep(1 * time.Second)
	fmt.Printf("[Frontend] VITE v5.0.0 ready in 432 ms\n")
	fmt.Printf("[Frontend] ➜  Local:   http://localhost:%s/\n", port)

	// Simple HTTP server
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "<html><body><h1>Mento Frontend</h1></body></html>")
		log.Printf("[Frontend] GET /dashboard - 200 OK")
	})

	http.HandleFunc("/dashboard", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "<html><body><h1>Dashboard</h1></body></html>")
		log.Printf("[Frontend] GET /dashboard - 200 OK")
	})

	// Simulate HMR updates
	go func() {
		time.Sleep(15 * time.Second)
		ticker := time.NewTicker(20 * time.Second)
		components := []string{"Dashboard.tsx", "ServiceCard.tsx", "LogViewer.tsx", "ConfigPanel.tsx"}
		i := 0
		for range ticker.C {
			log.Printf("[Frontend] [vite] hmr update /src/components/%s", components[i%len(components)])
			i++
			time.Sleep(2 * time.Second)
			log.Printf("[Frontend] Compiled successfully in %dms", 800+i*10)
		}
	}()

	if err := http.ListenAndServe(":"+port, nil); err != nil {
		log.Fatalf("[Frontend] Error: %v", err)
	}
}
