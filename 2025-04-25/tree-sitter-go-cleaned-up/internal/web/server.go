package web

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/mux"
	"github.com/pkg/errors"

	"github.com/wesen/tree-sitter-go-cleaned-up/internal/analyzer"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/formatter"
	"github.com/wesen/tree-sitter-go-cleaned-up/pkg/models"
)

// Server is the web server for the Go code analyzer.
type Server struct {
	config        Config
	analyzer      *analyzer.Analyzer // Injected
	formatter     *formatter.Formatter // Injected
	analysisCache AnalysisCache
	router        *mux.Router
}

// Config holds the configuration for the web server.
type Config struct {
	Port           int    `json:"port"`
	StaticDir      string `json:"staticDir"` // Path relative to where the binary runs
	DefaultCodeDir string `json:"defaultCodeDir"`
	CacheResults   bool   `json:"cacheResults"`
}

// AnalysisCache is a simple in-memory cache for analysis results.
type AnalysisCache struct {
	mutex    sync.RWMutex
	cache    map[string][]byte // Key: analysis request path, Value: JSON result
	metadata map[string]AnalysisCacheMetadata
}

// AnalysisCacheMetadata contains metadata about a cached analysis.
type AnalysisCacheMetadata struct {
	Path       string    `json:"path"`
	Size       int64     `json:"size"`       // Size of the JSON data
	FileCount  int       `json:"fileCount"`  // From models.AnalysisStats
	FuncCount  int       `json:"funcCount"`  // Sum of functions + methods
	LastUpdate time.Time `json:"lastUpdate"` // When the cache entry was created
}

// AnalysisRequest represents a request from the frontend to analyze a directory.
type AnalysisRequest struct {
	Path           string   `json:"path"`
	ExcludeDirs    []string `json:"excludeDirs,omitempty"`
	ExcludeFiles   []string `json:"excludeFiles,omitempty"`
	// EnhancedOutput bool     `json:"enhancedOutput"` // Output is always enhanced now
}

// NewServer creates and initializes a new web server instance.
func NewServer(cfg Config, anlz *analyzer.Analyzer, frmtr *formatter.Formatter) (*Server, error) {
	if anlz == nil {
		return nil, errors.New("analyzer cannot be nil")
	}
	if frmtr == nil {
		return nil, errors.New("formatter cannot be nil")
	}

	// Ensure static directory exists (relative to CWD where server runs)
	// If StaticDir is empty, default to "internal/web/static"
	if cfg.StaticDir == "" {
		cfg.StaticDir = "internal/web/static" // Default path
	}
	if _, err := os.Stat(cfg.StaticDir); os.IsNotExist(err) {
		log.Printf("Static directory %s does not exist. Attempting to use default files.", cfg.StaticDir)
		// The generateBasicIndexHTML logic will handle creating files if needed.
	} else {
		log.Printf("Serving static files from: %s", cfg.StaticDir)
	}

	srv := &Server{
		config:    cfg,
		analyzer:  anlz,
		formatter: frmtr,
		analysisCache: AnalysisCache{
			cache:    make(map[string][]byte),
			metadata: make(map[string]AnalysisCacheMetadata),
		},
		router: mux.NewRouter(),
	}

	srv.setupRoutes()
	return srv, nil
}

func (s *Server) setupRoutes() {
	// API routes
	apiRouter := s.router.PathPrefix("/api").Subrouter()
	apiRouter.HandleFunc("/analyze", s.handleAnalyzeRequest).Methods("POST")
	apiRouter.HandleFunc("/cache", s.handleCacheList).Methods("GET")
	apiRouter.HandleFunc("/cache/{id}", s.handleCacheGet).Methods("GET")         // id is the base64 encoded path
	apiRouter.HandleFunc("/cache/{id}", s.handleCacheDelete).Methods("DELETE")        // id is the base64 encoded path
	apiRouter.HandleFunc("/status", s.handleServerStatus).Methods("GET")

	// Serve static files - ensure the path is correct relative to the running binary
	s.router.PathPrefix("/static/").Handler(http.StripPrefix("/static/", http.FileServer(http.Dir(s.config.StaticDir))))

	// Serve index.html for the root path
	s.router.HandleFunc("/", s.handleIndex)

	// Handle 404s
	s.router.NotFoundHandler = http.HandlerFunc(handle404)
}

// ListenAndServe starts the web server.
func (s *Server) ListenAndServe() error {
	serverAddr := fmt.Sprintf(":%d", s.config.Port)
	log.Printf("Starting server on %s", serverAddr)
	log.Printf("Default code directory for analysis: %s", s.config.DefaultCodeDir)

	return http.ListenAndServe(serverAddr, s.router)
}

// handleIndex serves the main application page.
func (s *Server) handleIndex(w http.ResponseWriter, r *http.Request) {
	indexPath := filepath.Join(s.config.StaticDir, "index.html")

	// Check if the index file exists
	if _, err := os.Stat(indexPath); os.IsNotExist(err) {
		// If not, generate a basic index.html and supporting files
		log.Printf("index.html not found at %s, generating default files...", indexPath)
		s.generateBasicStaticFiles()
	}

	// Try serving the file again after potential generation
	if _, err := os.Stat(indexPath); err == nil {
		http.ServeFile(w, r, indexPath)
	} else {
		// If still not found, report error
		log.Printf("Error: Could not find or generate index.html at %s: %v", indexPath, err)
		http.Error(w, "Application entry point not found.", http.StatusInternalServerError)
	}
}

// generateBasicStaticFiles generates basic index.html, CSS, and JS files if they are missing.
func (s *Server) generateBasicStaticFiles() {
	indexPath := filepath.Join(s.config.StaticDir, "index.html")
	cssPath := filepath.Join(s.config.StaticDir, "css", "main.css")
	jsPath := filepath.Join(s.config.StaticDir, "js", "main.js")

	// Create directories if they don't exist
	_ = os.MkdirAll(filepath.Dir(indexPath), 0755)
	_ = os.MkdirAll(filepath.Dir(cssPath), 0755)
	_ = os.MkdirAll(filepath.Dir(jsPath), 0755)

	// Generate index.html if missing
	if _, err := os.Stat(indexPath); os.IsNotExist(err) {
		basicHTML := `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Go Code Analyzer</title>
    <link rel="stylesheet" href="/static/css/main.css">
    <script src="https://cdn.jsdelivr.net/npm/d3@7"></script>
    <script src="https://cdn.jsdelivr.net/npm/htmx.org@1.9.10"></script>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet">
</head>
<body class="bg-light">
    <header class="bg-dark text-white p-3 mb-4">
        <div class="container">
            <h1 class="display-4">Go Code Analyzer</h1>
        </div>
    </header>
    
    <main class="container">
        <section id="analysis-form" class="card p-4 mb-4">
            <h2 class="card-title mb-3">Analyze Go Code</h2>
            <form id="analyze-form" hx-post="/api/analyze" hx-target="#results-container" hx-swap="innerHTML" hx-indicator="#loading-indicator">
                <div class="mb-3">
                    <label for="code-path" class="form-label">Code Path:</label>
                    <input type="text" id="code-path" name="path" class="form-control" placeholder="Enter path to Go code or directory" value="` + s.config.DefaultCodeDir + `">
                </div>
                <div class="mb-3">
                    <label for="exclude-dirs" class="form-label">Exclude Directories (comma-separated):</label>
                    <input type="text" id="exclude-dirs" name="excludeDirs" class="form-control" placeholder="vendor,node_modules,dist,.git">
                </div>
                <div class="mb-3">
                    <label for="exclude-files" class="form-label">Exclude File Patterns (comma-separated):</label>
                    <input type="text" id="exclude-files" name="excludeFiles" class="form-control" placeholder="*_test.go,generated_*.go">
                </div>
                <button type="submit" class="btn btn-primary">Analyze</button>
                <span id="loading-indicator" class="htmx-indicator ms-2 spinner-border spinner-border-sm" role="status"></span>
            </form>
        </section>
        
        <section id="results-container" class="card">
            <div class="card-body">
                 <h2 class="card-title">Results</h2>
                 <p>Submit a path above to see analysis results.</p>
                 <!-- Results will be loaded here by HTMX -->
            </div>
        </section>
        
        <!-- Cache section could also use HTMX -->

    </main>
    
    <footer class="bg-dark text-white text-center p-3 mt-4">
        <p>Go Code Analyzer</p>
    </footer>
    
    <script src="/static/js/main.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>`
		if err := ioutil.WriteFile(indexPath, []byte(basicHTML), 0644); err != nil {
			log.Printf("Error writing basic index.html: %v", err)
		}
	}

	// Generate main.css if missing
	if _, err := os.Stat(cssPath); os.IsNotExist(err) {
		basicCSS := `/* Basic Styling */
.htmx-indicator { display: none; }
.htmx-request .htmx-indicator { display: inline-block; }
.htmx-request.htmx-indicator { display: inline-block; }

.error-message {
    color: red;
    font-weight: bold;
}

#json-output {
    max-height: 500px;
    overflow-y: auto;
    background-color: #f8f9fa;
    border: 1px solid #dee2e6;
    padding: 1rem;
    border-radius: 0.25rem;
    font-family: monospace;
    white-space: pre;
}
`
		if err := ioutil.WriteFile(cssPath, []byte(basicCSS), 0644); err != nil {
			log.Printf("Error writing main.css: %v", err)
		}
	}

	// Generate main.js if missing
	if _, err := os.Stat(jsPath); os.IsNotExist(err) {
		basicJS := `document.body.addEventListener('htmx:afterRequest', function(evt) {
    if (evt.detail.failed) {
        console.error("HTMX request failed:", evt.detail.xhr.status, evt.detail.xhr.responseText);
        // Optionally display a user-friendly error message
        const target = document.getElementById(evt.detail.target.id);
        if(target) {
            target.innerHTML = `<div class="alert alert-danger">Analysis failed: ${evt.detail.xhr.responseText || 'Server error'}</div>`;
        }
    } else {
        // You could potentially initialize D3 visualizations here after successful requests
        console.log("HTMX request successful.");
        // Example: Check if visualization data exists and call D3
        const vizDataElement = document.getElementById('visualization-data'); // Assuming you add this
        if (vizDataElement && window.d3) {
             // const vizData = JSON.parse(vizDataElement.textContent);
             // createVisualization(vizData);
             console.log("Placeholder for D3 visualization init");
        }
    }
});

// Example D3 function placeholder
function createVisualization(data) {
    console.log("Creating visualization with:", data);
    const visualization = d3.select("#visualization"); // Assuming an element with id="visualization"
    visualization.html(''); // Clear previous
    // ... your D3 code ...
}
`
		if err := ioutil.WriteFile(jsPath, []byte(basicJS), 0644); err != nil {
			log.Printf("Error writing main.js: %v", err)
		}
	}
}

// handle404 serves the 404 page.
func handle404(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusNotFound)
	fmt.Fprintln(w, "404 - Page Not Found")
	log.Printf("404 Not Found: %s", r.URL.Path)
}

// handleAnalyzeRequest handles requests to analyze a codebase.
func (s *Server) handleAnalyzeRequest(w http.ResponseWriter, r *http.Request) {
	// Decode request body for path etc.
	var req AnalysisRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		log.Printf("Error decoding analysis request: %v", err)
		http.Error(w, fmt.Sprintf("Invalid request format: %v", err), http.StatusBadRequest)
		return
	}

	if req.Path == "" {
		log.Printf("Analysis request missing path")
		http.Error(w, "Missing 'path' in request", http.StatusBadRequest)
		return
	}

	// Use context with timeout for the analysis request
	ctx, cancel := context.WithTimeout(r.Context(), 2*time.Minute) // 2-minute timeout for analysis
	defer cancel()

	log.Printf("Received analysis request for path: %s", req.Path)

	// Check cache first
	cacheKey := req.Path // Simple cache key, consider hashing or encoding
	if s.config.CacheResults {
		s.analysisCache.mutex.RLock()
		cachedData, found := s.analysisCache.cache[cacheKey]
		s.analysisCache.mutex.RUnlock()
		if found {
			log.Printf("Cache hit for: %s", req.Path)
			w.Header().Set("Content-Type", "application/json")
			_, _ = w.Write(cachedData) // Send cached result
			return
		}
		log.Printf("Cache miss for: %s", req.Path)
	}

	// Perform analysis using the injected analyzer
	analysisResult, err := s.analyzer.AnalyzeCodebase(ctx, req.Path)
	if err != nil {
		if errors.Is(err, context.DeadlineExceeded) {
			log.Printf("Analysis timeout for %s: %v", req.Path, err)
			http.Error(w, "Analysis timed out", http.StatusRequestTimeout)
		} else if errors.Is(err, context.Canceled) {
			log.Printf("Analysis canceled for %s: %v", req.Path, err)
			// Don't send error if cancellation was client-side (request aborted)
			// Check r.Context().Err() ?
			// For now, assume internal cancellation or timeout leads to error response.
			http.Error(w, "Analysis canceled", http.StatusInternalServerError) // Or appropriate code
		} else {
			log.Printf("Error analyzing codebase %s: %v", req.Path, err)
			http.Error(w, fmt.Sprintf("Error during analysis: %v", err), http.StatusInternalServerError)
		}
		return
	}

	// Format the result using the injected formatter
	jsonData, err := s.formatter.FormatJSON(analysisResult)
	if err != nil {
		log.Printf("Error formatting analysis result for %s: %v", req.Path, err)
		http.Error(w, "Error formatting result", http.StatusInternalServerError)
		return
	}

	// Cache the result if enabled
	if s.config.CacheResults {
		s.analysisCache.mutex.Lock()
		s.analysisCache.cache[cacheKey] = jsonData
		s.analysisCache.metadata[cacheKey] = AnalysisCacheMetadata{
			Path:       req.Path,
			Size:       int64(len(jsonData)),
			FileCount:  analysisResult.Stats.TotalFiles,
			FuncCount:  analysisResult.Stats.TotalFunctions + analysisResult.Stats.TotalMethods,
			LastUpdate: time.Now().UTC(),
		}
		s.analysisCache.mutex.Unlock()
		log.Printf("Cached result for: %s", req.Path)
	}

	// Send response
	w.Header().Set("Content-Type", "application/json")
	_, _ = w.Write(jsonData)
}

// handleCacheList returns a list of cached analysis results.
func (s *Server) handleCacheList(w http.ResponseWriter, r *http.Request) {
	if !s.config.CacheResults {
		http.Error(w, "Caching is disabled", http.StatusServiceUnavailable)
		return
	}

	s.analysisCache.mutex.RLock()
	defer s.analysisCache.mutex.RUnlock()

	metadataList := make([]AnalysisCacheMetadata, 0, len(s.analysisCache.metadata))
	for _, meta := range s.analysisCache.metadata {
		metadataList = append(metadataList, meta)
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(metadataList); err != nil {
		log.Printf("Error encoding cache list: %v", err)
		http.Error(w, "Error retrieving cache list", http.StatusInternalServerError)
	}
}

// handleCacheGet returns a specific cached analysis result.
func (s *Server) handleCacheGet(w http.ResponseWriter, r *http.Request) {
	if !s.config.CacheResults {
		http.Error(w, "Caching is disabled", http.StatusServiceUnavailable)
		return
	}

	vars := mux.Vars(r)
	cacheID := vars["id"] // Assuming ID is the cache key (path)

	s.analysisCache.mutex.RLock()
	cachedData, found := s.analysisCache.cache[cacheID]
	s.analysisCache.mutex.RUnlock()

	if !found {
		http.Error(w, "Cache entry not found", http.StatusNotFound)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	_, _ = w.Write(cachedData)
}

// handleCacheDelete removes a specific cached analysis result.
func (s *Server) handleCacheDelete(w http.ResponseWriter, r *http.Request) {
	if !s.config.CacheResults {
		http.Error(w, "Caching is disabled", http.StatusServiceUnavailable)
		return
	}

	vars := mux.Vars(r)
	cacheID := vars["id"] // Assuming ID is the cache key (path)

	s.analysisCache.mutex.Lock()
	_, found := s.analysisCache.cache[cacheID]
	if found {
		delete(s.analysisCache.cache, cacheID)
		delete(s.analysisCache.metadata, cacheID)
		log.Printf("Deleted cache entry: %s", cacheID)
	}
	s.analysisCache.mutex.Unlock()

	if !found {
		http.Error(w, "Cache entry not found", http.StatusNotFound)
		return
	}

	w.WriteHeader(http.StatusOK)
	fmt.Fprintln(w, "Cache entry deleted")
}

// handleServerStatus returns the current status of the server.
func (s *Server) handleServerStatus(w http.ResponseWriter, r *http.Request) {
	s.analysisCache.mutex.RLock()
	cacheSize := len(s.analysisCache.cache)
	s.analysisCache.mutex.RUnlock()

	status := map[string]interface{}{
		"status":        "running",
		"port":          s.config.Port,
		"caching":       s.config.CacheResults,
		"cache_items":   cacheSize,
		"default_dir":   s.config.DefaultCodeDir,
		"static_dir":    s.config.StaticDir,
		"server_time":   time.Now().UTC().Format(time.RFC3339),
	}

	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(status); err != nil {
		log.Printf("Error encoding server status: %v", err)
		http.Error(w, "Error retrieving server status", http.StatusInternalServerError)
	}
}