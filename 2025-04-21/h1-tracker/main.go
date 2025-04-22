package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"sync"
	"time"
)

// H1Event represents a heading captured from a webpage
type H1Event struct {
	URL       string    `json:"url"`
	Text      string    `json:"text"`
	Timestamp time.Time `json:"timestamp"`
}

// BatchEvent represents multiple H1 events sent together
type BatchEvent struct {
	Events []H1Event `json:"events"`
}

var (
	port      = flag.Int("port", 8080, "Port to run the server on")
	outputFile = flag.String("output", "h1_events.json", "File to store H1 events")
	mu         sync.Mutex
)

func main() {
	flag.Parse()

	// Create or verify the output file exists
	if _, err := os.Stat(*outputFile); os.IsNotExist(err) {
		// Create the file with an empty JSON array
		if err := os.WriteFile(*outputFile, []byte("[]"), 0644); err != nil {
			log.Fatalf("Failed to create output file: %v", err)
		}
	}

	http.HandleFunc("/h1", handleH1Event)
	http.HandleFunc("/batch", handleBatchEvent)
	http.HandleFunc("/", handleRoot)

	addr := fmt.Sprintf("0.0.0.0:%d", *port)
	log.Printf("Starting H1 tracker server on %s", addr)
	log.Printf("Storing events to %s", *outputFile)
	log.Fatal(http.ListenAndServe(addr, nil))
}

func handleRoot(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(`
		<html>
			<head>
				<title>H1 Tracker Server</title>
				<style>
					body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
					h1 { color: #333; }
					pre { background-color: #f5f5f5; padding: 10px; border-radius: 5px; }
				</style>
			</head>
			<body>
				<h1>H1 Tracker Server</h1>
				<p>This server receives H1 headings from web pages via a browser extension.</p>
				<p>Endpoints:</p>
				<ul>
					<li><code>/h1</code> - Receive a single H1 event</li>
					<li><code>/batch</code> - Receive multiple H1 events in a batch</li>
				</ul>
				<p>Server is running and ready to receive events.</p>
			</body>
		</html>
	`))
}

func handleH1Event(w http.ResponseWriter, r *http.Request) {
	// Set CORS headers to allow requests from extensions
	setCORSHeaders(w)

	// Handle preflight OPTIONS request
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}

	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var event H1Event
	if err := json.NewDecoder(r.Body).Decode(&event); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Set timestamp if not provided
	if event.Timestamp.IsZero() {
		event.Timestamp = time.Now()
	}

	// Log to stdout
	eventJSON, _ := json.Marshal(event)
	fmt.Printf("Received H1 event: %s\n", string(eventJSON))

	// Save to file
	if err := saveEventToFile(event); err != nil {
		log.Printf("Error saving event: %v", err)
		http.Error(w, "Error saving event", http.StatusInternalServerError)
		return
	}

	w.WriteHeader(http.StatusOK)
	w.Write([]byte(`{"status":"success"}`))
}

func handleBatchEvent(w http.ResponseWriter, r *http.Request) {
	// Set CORS headers to allow requests from extensions
	setCORSHeaders(w)

	// Handle preflight OPTIONS request
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}

	if r.Method != "POST" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var batch BatchEvent
	if err := json.NewDecoder(r.Body).Decode(&batch); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}

	// Set timestamp for events if not provided
	for i := range batch.Events {
		if batch.Events[i].Timestamp.IsZero() {
			batch.Events[i].Timestamp = time.Now()
		}
	}

	// Log to stdout
	batchJSON, _ := json.Marshal(batch)
	fmt.Printf("Received batch of %d H1 events: %s\n", len(batch.Events), string(batchJSON))

	// Save each event to file
	for _, event := range batch.Events {
		if err := saveEventToFile(event); err != nil {
			log.Printf("Error saving event: %v", err)
			http.Error(w, "Error saving events", http.StatusInternalServerError)
			return
		}
	}

	w.WriteHeader(http.StatusOK)
	w.Write([]byte(fmt.Sprintf(`{"status":"success","count":%d}`, len(batch.Events))))
}

func saveEventToFile(event H1Event) error {
	mu.Lock()
	defer mu.Unlock()

	// Read existing events
	data, err := os.ReadFile(*outputFile)
	if err != nil {
		return fmt.Errorf("failed to read output file: %v", err)
	}

	var events []H1Event
	if err := json.Unmarshal(data, &events); err != nil {
		return fmt.Errorf("failed to parse existing events: %v", err)
	}

	// Append new event
	events = append(events, event)

	// Write back to file
	updatedData, err := json.MarshalIndent(events, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal events: %v", err)
	}

	if err := os.WriteFile(*outputFile, updatedData, 0644); err != nil {
		return fmt.Errorf("failed to write to output file: %v", err)
	}

	return nil
}

func setCORSHeaders(w http.ResponseWriter) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
}
