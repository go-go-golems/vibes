package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/scrapybara/ttmp/pkg/server"
)

func main() {
	// Define command-line flags
	var (
		port     int
		basePath string
	)

	flag.IntVar(&port, "port", 8001, "Port to listen on")
	flag.StringVar(&basePath, "path", "example", "Base path to serve documents from")
	flag.Parse()

	// Resolve the base path to an absolute path
	absPath, err := filepath.Abs(basePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error resolving base path: %v\n", err)
		os.Exit(1)
	}

	// Create a new server
	srv, err := server.NewServer(absPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating server: %v\n", err)
		os.Exit(1)
	}

	// Start the server
	addr := fmt.Sprintf(":%d", port)
	fmt.Printf("Starting TTMP web server on %s\n", addr)
	fmt.Printf("Serving documents from %s\n", absPath)
	if err := srv.Start(addr); err != nil {
		fmt.Fprintf(os.Stderr, "Error starting server: %v\n", err)
		os.Exit(1)
	}
}