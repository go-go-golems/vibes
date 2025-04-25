package cmd

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/user/go-file-analyzer/parser"
)

// ServerOptions contains options for the server command
type ServerOptions struct {
	Port       int
	AssetsPath string
	CacheTTL   time.Duration
}

// analysisCache is a simple cache for analysis results
type analysisCache struct {
	sync.RWMutex
	results    map[string]*parser.AnalysisResult
	timestamps map[string]time.Time
	ttl        time.Duration
}

// newAnalysisCache creates a new analysis cache with the given TTL
func newAnalysisCache(ttl time.Duration) *analysisCache {
	return &analysisCache{
		results:    make(map[string]*parser.AnalysisResult),
		timestamps: make(map[string]time.Time),
		ttl:        ttl,
	}
}

// get retrieves an analysis result from the cache
func (c *analysisCache) get(path string) (*parser.AnalysisResult, bool) {
	c.RLock()
	defer c.RUnlock()
	
	// Check if result exists
	result, exists := c.results[path]
	if !exists {
		return nil, false
	}
	
	// Check if result is expired
	timestamp, _ := c.timestamps[path]
	if time.Since(timestamp) > c.ttl {
		return nil, false
	}
	
	return result, true
}

// set stores an analysis result in the cache
func (c *analysisCache) set(path string, result *parser.AnalysisResult) {
	c.Lock()
	defer c.Unlock()
	
	c.results[path] = result
	c.timestamps[path] = time.Now()
}

// clear removes all entries from the cache
func (c *analysisCache) clear() {
	c.Lock()
	defer c.Unlock()
	
	c.results = make(map[string]*parser.AnalysisResult)
	c.timestamps = make(map[string]time.Time)
}

// RunServer starts the web server with the given options
func RunServer(options ServerOptions) error {
	// Create a new parser
	goParser := parser.NewGoFileParser()
	
	// Create analysis cache
	cache := newAnalysisCache(options.CacheTTL)
	
	// Determine assets path
	assetsPath := options.AssetsPath
	if assetsPath == "" {
		// Default assets path is relative to the executable
		execPath, err := os.Executable()
		if err != nil {
			return fmt.Errorf("error determining executable path: %w", err)
		}
		assetsPath = filepath.Join(filepath.Dir(execPath), "web", "assets")
	}
	
	// Create web server router
	mux := http.NewServeMux()
	
	// API endpoint: Analyze a path
	mux.HandleFunc("/api/analyze", func(w http.ResponseWriter, r *http.Request) {
		// Only allow GET requests
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}
		
		// Get path parameter
		path := r.URL.Query().Get("path")
		if path == "" {
			http.Error(w, "Path parameter is required", http.StatusBadRequest)
			return
		}
		
		// Check if path exists
		_, err := os.Stat(path)
		if err != nil {
			http.Error(w, fmt.Sprintf("Path error: %v", err), http.StatusBadRequest)
			return
		}
		
		// Check cache
		result, cacheHit := cache.get(path)
		if !cacheHit {
			// Parse the path
			startTime := time.Now()
			functions, err := goParser.ParsePath(path)
			if err != nil {
				http.Error(w, fmt.Sprintf("Analysis error: %v", err), http.StatusInternalServerError)
				return
			}
			
			// Create a simple analysis result
			result = &parser.AnalysisResult{
				Functions: functions,
			}
			
			// Update cache
			cache.set(path, result)
			
			log.Printf("Analyzed path %s in %v", path, time.Since(startTime))
		} else {
			log.Printf("Cache hit for path %s", path)
		}
		
		// Set content type
		w.Header().Set("Content-Type", "application/json")
		
		// Write response
		if err := json.NewEncoder(w).Encode(result); err != nil {
			http.Error(w, fmt.Sprintf("Error encoding response: %v", err), http.StatusInternalServerError)
			return
		}
	})
	
	// API endpoint: Get file content
	mux.HandleFunc("/api/file", func(w http.ResponseWriter, r *http.Request) {
		// Only allow GET requests
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}
		
		// Get path parameter
		path := r.URL.Query().Get("path")
		if path == "" {
			http.Error(w, "Path parameter is required", http.StatusBadRequest)
			return
		}
		
		// Read file
		content, err := os.ReadFile(path)
		if err != nil {
			http.Error(w, fmt.Sprintf("Error reading file: %v", err), http.StatusInternalServerError)
			return
		}
		
		// Set content type based on file extension
		contentType := "text/plain"
		switch strings.ToLower(filepath.Ext(path)) {
		case ".go":
			contentType = "text/x-go"
		case ".json":
			contentType = "application/json"
		case ".html":
			contentType = "text/html"
		case ".js":
			contentType = "application/javascript"
		case ".css":
			contentType = "text/css"
		}
		w.Header().Set("Content-Type", contentType)
		
		// Write response
		w.Write(content)
	})
	
	// API endpoint: List directory
	mux.HandleFunc("/api/list", func(w http.ResponseWriter, r *http.Request) {
		// Only allow GET requests
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}
		
		// Get path parameter
		path := r.URL.Query().Get("path")
		if path == "" {
			path = "."
		}
		
		// Check if path exists and is a directory
		info, err := os.Stat(path)
		if err != nil {
			http.Error(w, fmt.Sprintf("Path error: %v", err), http.StatusBadRequest)
			return
		}
		
		if !info.IsDir() {
			http.Error(w, "Path is not a directory", http.StatusBadRequest)
			return
		}
		
		// Read directory
		entries, err := os.ReadDir(path)
		if err != nil {
			http.Error(w, fmt.Sprintf("Error reading directory: %v", err), http.StatusInternalServerError)
			return
		}
		
		// Create result
		type DirEntry struct {
			Name  string `json:"name"`
			Path  string `json:"path"`
			IsDir bool   `json:"isDir"`
			Size  int64  `json:"size,omitempty"`
		}
		
		result := struct {
			Path    string     `json:"path"`
			Entries []DirEntry `json:"entries"`
		}{
			Path:    path,
			Entries: make([]DirEntry, 0, len(entries)),
		}
		
		// Process entries
		for _, entry := range entries {
			entryPath := filepath.Join(path, entry.Name())
			
			// Get file info
			info, err := entry.Info()
			if err != nil {
				continue
			}
			
			// Create entry
			dirEntry := DirEntry{
				Name:  entry.Name(),
				Path:  entryPath,
				IsDir: entry.IsDir(),
			}
			
			// Add size for files
			if !entry.IsDir() {
				dirEntry.Size = info.Size()
			}
			
			result.Entries = append(result.Entries, dirEntry)
		}
		
		// Set content type
		w.Header().Set("Content-Type", "application/json")
		
		// Write response
		if err := json.NewEncoder(w).Encode(result); err != nil {
			http.Error(w, fmt.Sprintf("Error encoding response: %v", err), http.StatusInternalServerError)
			return
		}
	})
	
	// API endpoint: Get function details
	mux.HandleFunc("/api/function", func(w http.ResponseWriter, r *http.Request) {
		// Only allow GET requests
		if r.Method != http.MethodGet {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}
		
		// Get path and function name parameters
		path := r.URL.Query().Get("path")
		name := r.URL.Query().Get("name")
		if path == "" || name == "" {
			http.Error(w, "Path and name parameters are required", http.StatusBadRequest)
			return
		}
		
		// Check if path exists
		_, err := os.Stat(path)
		if err != nil {
			http.Error(w, fmt.Sprintf("Path error: %v", err), http.StatusBadRequest)
			return
		}
		
		// Parse the file or directory
		functions, err := goParser.ParsePath(path)
		if err != nil {
			http.Error(w, fmt.Sprintf("Analysis error: %v", err), http.StatusInternalServerError)
			return
		}
		
		// Find the requested function
		var targetFunction *parser.FunctionInfo
		for i, f := range functions {
			if f.Name == name {
				targetFunction = &functions[i]
				break
			}
		}
		
		if targetFunction == nil {
			http.Error(w, "Function not found", http.StatusNotFound)
			return
		}
		
		// Set content type
		w.Header().Set("Content-Type", "application/json")
		
		// Write response
		if err := json.NewEncoder(w).Encode(targetFunction); err != nil {
			http.Error(w, fmt.Sprintf("Error encoding response: %v", err), http.StatusInternalServerError)
			return
		}
	})
	
	// Serve static files
	mux.Handle("/", http.FileServer(http.Dir(assetsPath)))
	
	// Start web server
	addr := fmt.Sprintf(":%d", options.Port)
	log.Printf("Server running at http://localhost%s", addr)
	return http.ListenAndServe(addr, mux)
}