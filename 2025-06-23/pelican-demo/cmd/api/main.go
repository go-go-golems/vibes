package main

import (
	"log"
	"net/http"
	"os"

	httpserver "pelican-demo/internal/http"
	"pelican-demo/internal/progress"
)

func main() {
	// Create publisher and subscriber from the same underlying pub/sub
	publisher, err := progress.NewPublisher()
	if err != nil {
		log.Fatalf("Failed to create publisher: %v", err)
	}
	defer publisher.Close()
	
	subscriber, err := progress.NewSubscriber()
	if err != nil {
		log.Fatalf("Failed to create subscriber: %v", err)
	}
	defer subscriber.Close()
	
	// Create progress sink and source
	sink := progress.NewSink(publisher)
	source := progress.NewSource(subscriber)
	
	// Create HTTP server
	server := httpserver.NewServer(sink, source)
	router := server.SetupRoutes()
	
	// Start server
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}
	
	if os.Getenv("REDIS") == "1" {
		log.Printf("Using Redis Streams for pub/sub")
	} else {
		log.Printf("Using in-memory pub/sub")
	}
	
	log.Printf("Starting server on port %s", port)
	log.Fatal(http.ListenAndServe(":"+port, router))
}

