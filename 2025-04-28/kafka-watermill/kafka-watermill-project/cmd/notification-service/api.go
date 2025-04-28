package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/gorilla/mux"
)

// API holds dependencies for the API handlers
type API struct {
	// Add any dependencies here
}

// NewAPI creates a new API instance
func NewAPI() *API {
	return &API{}
}

// StartServer starts the HTTP server
func (api *API) StartServer() {
	r := mux.NewRouter()

	// Routes
	r.HandleFunc("/health", api.healthHandler).Methods("GET")

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "8004"
	}

	// Start server
	server := &http.Server{
		Addr:         ":" + port,
		Handler:      r,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}

	log.Printf("Starting HTTP server on port %s...", port)
	log.Fatal(server.ListenAndServe())
}

// healthHandler returns 200 OK if the service is healthy
func (api *API) healthHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}