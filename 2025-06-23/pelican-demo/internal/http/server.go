package http

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/go-chi/cors"
	"github.com/google/uuid"
	"github.com/prometheus/client_golang/prometheus/promhttp"

	"pelican-demo/internal/genome"
	"pelican-demo/internal/metrics"
	"pelican-demo/internal/progress"
)

// Server holds the HTTP server dependencies
type Server struct {
	sink   progress.Sink
	source progress.Source
}

// JobRequest represents a job creation request
type JobRequest struct {
	Species string `json:"species"`
}

// JobResponse represents a job creation response
type JobResponse struct {
	JobID string `json:"job_id"`
}

// NewServer creates a new HTTP server
func NewServer(sink progress.Sink, source progress.Source) *Server {
	return &Server{
		sink:   sink,
		source: source,
	}
}

// SetupRoutes configures the HTTP routes
func (s *Server) SetupRoutes() http.Handler {
	r := chi.NewRouter()
	
	// Middleware
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)
	r.Use(middleware.RequestID)
	r.Use(middleware.Timeout(60 * time.Second))
	
	// CORS configuration
	r.Use(cors.Handler(cors.Options{
		AllowedOrigins:   []string{"*"},
		AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders:   []string{"Accept", "Authorization", "Content-Type", "X-CSRF-Token"},
		ExposedHeaders:   []string{"Link"},
		AllowCredentials: false,
		MaxAge:           300,
	}))
	
	// Routes
	r.Post("/jobs", s.CreateJob)
	r.Get("/jobs/{jobID}/events", s.StreamEvents)
	r.Get("/metrics", promhttp.Handler().ServeHTTP)
	r.Get("/", s.IndexPage)
	r.Get("/health", s.HealthCheck)
	
	// Static files
	r.Handle("/static/*", http.StripPrefix("/static/", http.FileServer(http.Dir("web/static/"))))
	
	return r
}

// CreateJob handles job creation requests
func (s *Server) CreateJob(w http.ResponseWriter, r *http.Request) {
	var req JobRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "Invalid JSON", http.StatusBadRequest)
		return
	}
	
	if req.Species == "" {
		req.Species = "brown_pelican" // Default species
	}
	
	// Validate species
	speciesInfo := genome.GetSpeciesInfo()
	if _, exists := speciesInfo[req.Species]; !exists {
		http.Error(w, "Invalid species", http.StatusBadRequest)
		return
	}
	
	// Generate job ID
	jobID := uuid.New().String()
	
	// Start job in background with a small delay to allow SSE connection
	go func() {
		time.Sleep(100 * time.Millisecond) // Give time for SSE to connect
		s.runGenomeJob(jobID, req.Species)
	}()
	
	// Return job ID
	resp := JobResponse{JobID: jobID}
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(resp)
}

// StreamEvents handles Server-Sent Events streaming
func (s *Server) StreamEvents(w http.ResponseWriter, r *http.Request) {
	jobID := chi.URLParam(r, "jobID")
	if jobID == "" {
		http.Error(w, "Missing job ID", http.StatusBadRequest)
		return
	}
	
	// Set SSE headers
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("Access-Control-Allow-Origin", "*")
	
	// Send initial connection message
	fmt.Fprintf(w, "data: %s\n\n", `{"stage":"connected","job_id":"`+jobID+`","message":"Connected to live progress stream"}`)
	if flusher, ok := w.(http.Flusher); ok {
		flusher.Flush()
	}
	
	// Subscribe to events
	events, err := s.source.Subscribe(jobID)
	if err != nil {
		log.Printf("Failed to subscribe to events for job %s: %v", jobID, err)
		http.Error(w, "Failed to subscribe to events", http.StatusInternalServerError)
		return
	}
	
	flusher, ok := w.(http.Flusher)
	if !ok {
		http.Error(w, "Streaming unsupported", http.StatusInternalServerError)
		return
	}
	
	// Stream events
	for {
		select {
		case event, ok := <-events:
			if !ok {
				return // Channel closed
			}
			
			data, err := json.Marshal(event)
			if err != nil {
				log.Printf("Failed to marshal event: %v", err)
				continue
			}
			
			fmt.Fprintf(w, "data: %s\n\n", data)
			flusher.Flush()
			
			// Close connection when job is done
			if event.Stage == "done" || event.Stage == "error" {
				return
			}
			
		case <-r.Context().Done():
			return // Client disconnected
		}
	}
}

// IndexPage serves the main HTML page
func (s *Server) IndexPage(w http.ResponseWriter, r *http.Request) {
	html := `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Pelican Genome Sequencer</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <style>
        .progress-container { min-height: 400px; }
        .log-container { max-height: 300px; overflow-y: auto; }
        .species-info { font-size: 0.9em; color: #666; }
    </style>
</head>
<body>
    <div class="container py-5">
        <div class="row">
            <div class="col-lg-8 mx-auto">
                <h1 class="text-center mb-4">🦆 Pelican Genome Sequencer</h1>
                <p class="text-center text-muted mb-5">Advanced genomic analysis for pelican species worldwide</p>
                
                <div class="card mb-4">
                    <div class="card-body">
                        <h5 class="card-title">Start New Sequencing Job</h5>
                        <form id="seqForm">
                            <div class="row align-items-end">
                                <div class="col-md-8">
                                    <label for="species" class="form-label">Select Pelican Species</label>
                                    <select id="species" class="form-select">
                                        <option value="brown_pelican">Brown Pelican (Pelecanus occidentalis)</option>
                                        <option value="peruvian_pelican">Peruvian Pelican (Pelecanus thagus)</option>
                                        <option value="dalmatian_pelican">Dalmatian Pelican (Pelecanus crispus)</option>
                                        <option value="american_white_pelican">American White Pelican (Pelecanus erythrorhynchos)</option>
                                        <option value="australian_pelican">Australian Pelican (Pelecanus conspicillatus)</option>
                                    </select>
                                </div>
                                <div class="col-md-4">
                                    <button type="submit" class="btn btn-primary w-100">🧬 Sequence Genome</button>
                                </div>
                            </div>
                        </form>
                    </div>
                </div>

                <div id="progress" class="progress-container d-none">
                    <div class="card">
                        <div class="card-body">
                            <h5 class="card-title">Sequencing Progress</h5>
                            <div id="jobInfo" class="mb-3"></div>
                            
                            <div class="mb-3">
                                <label class="form-label">Overall Progress</label>
                                <div class="progress mb-2" style="height: 25px;">
                                    <div id="progressBar" class="progress-bar progress-bar-striped progress-bar-animated" 
                                         role="progressbar" style="width: 0%">0%</div>
                                </div>
                            </div>
                            
                            <div class="row mb-3">
                                <div class="col-md-6">
                                    <div class="card bg-light">
                                        <div class="card-body text-center">
                                            <h6 class="card-title">Records Fetched</h6>
                                            <h4 id="fetchedCount" class="text-primary">0</h4>
                                        </div>
                                    </div>
                                </div>
                                <div class="col-md-6">
                                    <div class="card bg-light">
                                        <div class="card-body text-center">
                                            <h6 class="card-title">Records Indexed</h6>
                                            <h4 id="indexedCount" class="text-success">0</h4>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            
                            <div class="mb-3">
                                <label class="form-label">Live Event Log</label>
                                <div id="log" class="log-container bg-dark text-white p-3 rounded font-monospace small"></div>
                            </div>
                            
                            <div id="rateLimitWarning" class="alert alert-warning d-none">
                                ⚠️ Rate limiting detected - sequencing may be slower than usual
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script src="/static/main.js"></script>
</body>
</html>`
	
	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(html))
}

// HealthCheck provides a simple health check endpoint
func (s *Server) HealthCheck(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{
		"status": "healthy",
		"time":   time.Now().Format(time.RFC3339),
	})
}

// runGenomeJob executes a genome sequencing job
func (s *Server) runGenomeJob(jobID, species string) {
	startTime := time.Now()
	metrics.JobStarted()
	
	defer func() {
		duration := time.Since(startTime).Seconds()
		metrics.JobCompleted(species, "completed", duration)
	}()
	
	cfg := genome.DefaultConfig(species)
	ctx := context.Background()
	
	// Create a metrics-aware sink
	metricsSink := &MetricsSink{
		sink:    s.sink,
		species: species,
	}
	
	if err := genome.Run(ctx, metricsSink, jobID, cfg); err != nil {
		log.Printf("Job %s failed: %v", jobID, err)
		
		// Send error event
		s.sink.Send(progress.Event{
			JobID: jobID,
			Stage: "error",
			Err:   err.Error(),
		})
	}
}

// MetricsSink wraps a progress sink to record metrics
type MetricsSink struct {
	sink    progress.Sink
	species string
}

// Send forwards the event and records metrics
func (m *MetricsSink) Send(event progress.Event) error {
	// Record metrics
	metrics.RecordEvent(event.JobID, event.Stage, event.Fetched, event.Indexed, event.RateLimited, m.species)
	
	// Forward to actual sink
	return m.sink.Send(event)
}

