package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"time"

	"github.com/gorilla/mux"
)

// ServerConfig holds the configuration for the web server
type ServerConfig struct {
	Port           int    `json:"port"`
	StaticDir      string `json:"staticDir"`
	DefaultCodeDir string `json:"defaultCodeDir"`
	CacheResults   bool   `json:"cacheResults"`
}

// AnalysisCache is a cache for analysis results
type AnalysisCache struct {
	mutex    sync.RWMutex
	cache    map[string][]byte
	metadata map[string]AnalysisCacheMetadata
}

// AnalysisCacheMetadata contains metadata about a cached analysis
type AnalysisCacheMetadata struct {
	Path       string    `json:"path"`
	Size       int64     `json:"size"`
	FileCount  int       `json:"fileCount"`
	FuncCount  int       `json:"funcCount"`
	LastUpdate time.Time `json:"lastUpdate"`
}

// AnalysisRequest represents a request to analyze a directory
type AnalysisRequest struct {
	Path           string   `json:"path"`
	ExcludeDirs    []string `json:"excludeDirs,omitempty"`
	ExcludeFiles   []string `json:"excludeFiles,omitempty"`
	EnhancedOutput bool     `json:"enhancedOutput"`
}

var (
	config       ServerConfig
	analysisCache AnalysisCache
)

func init() {
	// Initialize the analysis cache
	analysisCache = AnalysisCache{
		cache:    make(map[string][]byte),
		metadata: make(map[string]AnalysisCacheMetadata),
	}
}

// InitializeServer initializes the web server with configuration
func InitializeServer(port int, staticDir, defaultCodeDir string, cacheResults bool) {
	config = ServerConfig{
		Port:           port,
		StaticDir:      staticDir,
		DefaultCodeDir: defaultCodeDir,
		CacheResults:   cacheResults,
	}

	// Create static directory if it doesn't exist
	if _, err := os.Stat(staticDir); os.IsNotExist(err) {
		log.Printf("Creating static directory: %s", staticDir)
		if err := os.MkdirAll(staticDir, 0755); err != nil {
			log.Fatalf("Failed to create static directory: %v", err)
		}
	}

	// Check if the default code directory exists
	if _, err := os.Stat(defaultCodeDir); os.IsNotExist(err) {
		log.Printf("Warning: Default code directory does not exist: %s", defaultCodeDir)
	}
}

// StartServer starts the web server
func StartServer() error {
	router := mux.NewRouter()

	// API routes
	apiRouter := router.PathPrefix("/api").Subrouter()
	apiRouter.HandleFunc("/analyze", handleAnalyzeRequest).Methods("POST")
	apiRouter.HandleFunc("/cache", handleCacheList).Methods("GET")
	apiRouter.HandleFunc("/cache/{id}", handleCacheGet).Methods("GET")
	apiRouter.HandleFunc("/cache/{id}", handleCacheDelete).Methods("DELETE")
	apiRouter.HandleFunc("/status", handleServerStatus).Methods("GET")

	// Serve static files
	router.PathPrefix("/static/").Handler(http.StripPrefix("/static/", http.FileServer(http.Dir(config.StaticDir))))
	
	// Serve index.html for the root path
	router.HandleFunc("/", handleIndex)
	
	// Handle 404s
	router.NotFoundHandler = http.HandlerFunc(handle404)

	// Start the server
	serverAddr := fmt.Sprintf(":%d", config.Port)
	log.Printf("Starting server on %s", serverAddr)
	log.Printf("Serving static files from: %s", config.StaticDir)
	log.Printf("Default code directory: %s", config.DefaultCodeDir)

	return http.ListenAndServe(serverAddr, router)
}

// handleIndex serves the main application page
func handleIndex(w http.ResponseWriter, r *http.Request) {
	indexPath := filepath.Join(config.StaticDir, "index.html")
	
	// Check if the index file exists
	if _, err := os.Stat(indexPath); os.IsNotExist(err) {
		// If not, generate a basic index.html
		generateBasicIndexHTML(indexPath)
	}
	
	http.ServeFile(w, r, indexPath)
}

// generateBasicIndexHTML generates a basic index.html file
func generateBasicIndexHTML(path string) {
	basicHTML := `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Go File Analyzer</title>
    <link rel="stylesheet" href="/static/css/main.css">
    <script src="https://cdn.jsdelivr.net/npm/d3@7"></script>
</head>
<body>
    <header>
        <h1>Go File Analyzer</h1>
        <nav>
            <ul>
                <li><a href="#" id="analyze-btn">Analyze Code</a></li>
                <li><a href="#" id="cache-btn">Cached Results</a></li>
            </ul>
        </nav>
    </header>
    
    <main>
        <section id="analysis-form">
            <h2>Analyze Go Code</h2>
            <form id="analyze-form">
                <div class="form-group">
                    <label for="code-path">Code Path:</label>
                    <input type="text" id="code-path" name="path" placeholder="Enter path to Go code">
                </div>
                <div class="form-group">
                    <label for="exclude-dirs">Exclude Directories:</label>
                    <input type="text" id="exclude-dirs" name="excludeDirs" placeholder="vendor,node_modules,dist">
                </div>
                <div class="form-group">
                    <label for="exclude-files">Exclude Files:</label>
                    <input type="text" id="exclude-files" name="excludeFiles" placeholder="*_test.go,generated_*.go">
                </div>
                <div class="form-group">
                    <label>
                        <input type="checkbox" id="enhanced-output" name="enhancedOutput" checked>
                        Enhanced Output
                    </label>
                </div>
                <button type="submit">Analyze</button>
            </form>
        </section>
        
        <section id="results" class="hidden">
            <h2>Analysis Results</h2>
            <div id="summary"></div>
            <div id="visualization"></div>
            <pre id="json-output"></pre>
        </section>
        
        <section id="cache-list" class="hidden">
            <h2>Cached Analyses</h2>
            <table>
                <thead>
                    <tr>
                        <th>Path</th>
                        <th>Files</th>
                        <th>Functions</th>
                        <th>Last Updated</th>
                        <th>Actions</th>
                    </tr>
                </thead>
                <tbody id="cache-items">
                    <!-- Cache items will be added here -->
                </tbody>
            </table>
        </section>
    </main>
    
    <footer>
        <p>Go File Analyzer - A tool for analyzing Go codebases</p>
    </footer>
    
    <script src="/static/js/main.js"></script>
</body>
</html>`

	// Create the directory if it doesn't exist
	dir := filepath.Dir(path)
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		if err := os.MkdirAll(dir, 0755); err != nil {
			log.Printf("Error creating directory %s: %v", dir, err)
			return
		}
	}

	// Write the basic HTML to the file
	if err := ioutil.WriteFile(path, []byte(basicHTML), 0644); err != nil {
		log.Printf("Error writing basic index.html: %v", err)
		return
	}

	log.Printf("Generated basic index.html at %s", path)

	// Create CSS directory and file
	cssDir := filepath.Join(filepath.Dir(path), "css")
	if _, err := os.Stat(cssDir); os.IsNotExist(err) {
		if err := os.MkdirAll(cssDir, 0755); err != nil {
			log.Printf("Error creating CSS directory: %v", err)
			return
		}
	}

	// Create a basic CSS file
	cssPath := filepath.Join(cssDir, "main.css")
	basicCSS := `* {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.6;
    color: #333;
    background-color: #f8f9fa;
}

header {
    background-color: #2c3e50;
    color: #ecf0f1;
    padding: 1rem;
    text-align: center;
}

header h1 {
    margin-bottom: 0.5rem;
}

nav ul {
    display: flex;
    justify-content: center;
    list-style: none;
}

nav ul li {
    margin: 0 1rem;
}

nav ul li a {
    color: #ecf0f1;
    text-decoration: none;
    font-weight: bold;
}

nav ul li a:hover {
    text-decoration: underline;
}

main {
    max-width: 1200px;
    margin: 0 auto;
    padding: 2rem;
}

section {
    background-color: white;
    border-radius: 5px;
    box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
    padding: 2rem;
    margin-bottom: 2rem;
}

h2 {
    color: #2c3e50;
    margin-bottom: 1.5rem;
}

.form-group {
    margin-bottom: 1rem;
}

label {
    display: block;
    margin-bottom: 0.5rem;
    font-weight: bold;
}

input[type="text"] {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 3px;
}

button {
    background-color: #3498db;
    color: white;
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 3px;
    cursor: pointer;
    font-weight: bold;
}

button:hover {
    background-color: #2980b9;
}

table {
    width: 100%;
    border-collapse: collapse;
}

table th, table td {
    padding: 0.75rem;
    text-align: left;
    border-bottom: 1px solid #ddd;
}

table th {
    background-color: #f2f2f2;
}

.hidden {
    display: none;
}

#visualization {
    width: 100%;
    height: 500px;
    margin: 2rem 0;
    border: 1px solid #ddd;
    border-radius: 5px;
}

#json-output {
    background-color: #f4f4f4;
    padding: 1rem;
    border-radius: 5px;
    overflow-x: auto;
    max-height: 400px;
}

footer {
    text-align: center;
    padding: 1rem;
    background-color: #2c3e50;
    color: #ecf0f1;
}`

	if err := ioutil.WriteFile(cssPath, []byte(basicCSS), 0644); err != nil {
		log.Printf("Error writing main.css: %v", err)
		return
	}

	// Create JS directory and file
	jsDir := filepath.Join(filepath.Dir(path), "js")
	if _, err := os.Stat(jsDir); os.IsNotExist(err) {
		if err := os.MkdirAll(jsDir, 0755); err != nil {
			log.Printf("Error creating JS directory: %v", err)
			return
		}
	}

	// Create a basic JavaScript file
	jsPath := filepath.Join(jsDir, "main.js")
	basicJS := `document.addEventListener('DOMContentLoaded', function() {
    // DOM elements
    const analyzeForm = document.getElementById('analyze-form');
    const analyzeBtn = document.getElementById('analyze-btn');
    const cacheBtn = document.getElementById('cache-btn');
    const analysisFormSection = document.getElementById('analysis-form');
    const resultsSection = document.getElementById('results');
    const cacheListSection = document.getElementById('cache-list');
    const summary = document.getElementById('summary');
    const jsonOutput = document.getElementById('json-output');
    const visualization = document.getElementById('visualization');
    const cacheItems = document.getElementById('cache-items');
    
    // Event listeners
    analyzeBtn.addEventListener('click', function(e) {
        e.preventDefault();
        showSection(analysisFormSection);
    });
    
    cacheBtn.addEventListener('click', function(e) {
        e.preventDefault();
        loadCacheList();
        showSection(cacheListSection);
    });
    
    analyzeForm.addEventListener('submit', function(e) {
        e.preventDefault();
        const formData = new FormData(analyzeForm);
        const data = {
            path: formData.get('path'),
            excludeDirs: formData.get('excludeDirs').split(',').filter(Boolean),
            excludeFiles: formData.get('excludeFiles').split(',').filter(Boolean),
            enhancedOutput: formData.get('enhancedOutput') === 'on'
        };
        
        analyzeCode(data);
    });
    
    // Functions
    function showSection(section) {
        // Hide all sections
        analysisFormSection.classList.add('hidden');
        resultsSection.classList.add('hidden');
        cacheListSection.classList.add('hidden');
        
        // Show the specified section
        section.classList.remove('hidden');
    }
    
    function analyzeCode(data) {
        // Show loading state
        summary.innerHTML = '<p>Analyzing code... This may take a few moments.</p>';
        showSection(resultsSection);
        
        // Send API request
        fetch('/api/analyze', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        })
        .then(response => {
            if (!response.ok) {
                throw new Error('Analysis failed');
            }
            return response.json();
        })
        .then(result => {
            displayResults(result);
        })
        .catch(error => {
            summary.innerHTML = '<p class="error">Error: ' + error.message + '</p>';
        });
    }
    
    function displayResults(data) {
        // Display summary
        const stats = data.metadata || (data.stats || {});
        const numFiles = stats.numFiles || stats.fileCount || 0;
        const numFunctions = stats.numFunctions || stats.functionCount || 0;
        const numMethods = stats.numMethods || stats.methodCount || 0;
        
        summary.innerHTML = `
            <h3>Analysis Summary</h3>
            <p>Files analyzed: ${numFiles}</p>
            <p>Functions found: ${numFunctions - numMethods}</p>
            <p>Methods found: ${numMethods}</p>
        `;
        
        // Display JSON
        jsonOutput.textContent = JSON.stringify(data, null, 2);
        
        // Create visualization if D3 is available
        if (window.d3 && data.functions && data.functions.length > 0) {
            createVisualization(data);
        } else {
            visualization.innerHTML = '<p>Visualization is not available for this result.</p>';
        }
    }
    
    function createVisualization(data) {
        // Clear previous visualization
        visualization.innerHTML = '';
        
        // Create a simple bar chart of functions by package
        const packages = {};
        
        // Count functions by package
        data.functions.forEach(func => {
            const pkg = func.package || 'unknown';
            if (!packages[pkg]) {
                packages[pkg] = { name: pkg, functions: 0, methods: 0 };
            }
            if (func.isMethod) {
                packages[pkg].methods++;
            } else {
                packages[pkg].functions++;
            }
        });
        
        // Convert to array for D3
        const packageData = Object.values(packages);
        
        // Set up the SVG
        const margin = { top: 30, right: 30, bottom: 70, left: 60 };
        const width = visualization.clientWidth - margin.left - margin.right;
        const height = 400 - margin.top - margin.bottom;
        
        const svg = d3.select('#visualization')
            .append('svg')
            .attr('width', width + margin.left + margin.right)
            .attr('height', height + margin.top + margin.bottom)
            .append('g')
            .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');
        
        // X axis
        const x = d3.scaleBand()
            .range([0, width])
            .domain(packageData.map(d => d.name))
            .padding(0.2);
            
        svg.append('g')
            .attr('transform', 'translate(0,' + height + ')')
            .call(d3.axisBottom(x))
            .selectAll('text')
            .attr('transform', 'translate(-10,0)rotate(-45)')
            .style('text-anchor', 'end');
        
        // Add Y axis
        const y = d3.scaleLinear()
            .domain([0, d3.max(packageData, d => d.functions + d.methods)])
            .range([height, 0]);
            
        svg.append('g')
            .call(d3.axisLeft(y));
        
        // Add title
        svg.append('text')
            .attr('x', width / 2)
            .attr('y', 0 - (margin.top / 2))
            .attr('text-anchor', 'middle')
            .style('font-size', '16px')
            .text('Functions and Methods by Package');
        
        // Add functions bars
        svg.selectAll('bars')
            .data(packageData)
            .enter()
            .append('rect')
            .attr('x', d => x(d.name))
            .attr('y', d => y(d.functions))
            .attr('width', x.bandwidth() / 2)
            .attr('height', d => height - y(d.functions))
            .attr('fill', '#3498db')
            .on('mouseover', function() {
                d3.select(this).attr('fill', '#2980b9');
            })
            .on('mouseout', function() {
                d3.select(this).attr('fill', '#3498db');
            });
        
        // Add methods bars
        svg.selectAll('bars2')
            .data(packageData)
            .enter()
            .append('rect')
            .attr('x', d => x(d.name) + x.bandwidth() / 2)
            .attr('y', d => y(d.methods))
            .attr('width', x.bandwidth() / 2)
            .attr('height', d => height - y(d.methods))
            .attr('fill', '#e74c3c')
            .on('mouseover', function() {
                d3.select(this).attr('fill', '#c0392b');
            })
            .on('mouseout', function() {
                d3.select(this).attr('fill', '#e74c3c');
            });
        
        // Add legend
        const legend = svg.append('g')
            .attr('transform', 'translate(' + (width - 100) + ',0)');
            
        legend.append('rect')
            .attr('x', 0)
            .attr('y', 0)
            .attr('width', 15)
            .attr('height', 15)
            .attr('fill', '#3498db');
            
        legend.append('text')
            .attr('x', 20)
            .attr('y', 12)
            .text('Functions')
            .style('font-size', '12px');
            
        legend.append('rect')
            .attr('x', 0)
            .attr('y', 20)
            .attr('width', 15)
            .attr('height', 15)
            .attr('fill', '#e74c3c');
            
        legend.append('text')
            .attr('x', 20)
            .attr('y', 32)
            .text('Methods')
            .style('font-size', '12px');
    }
    
    function loadCacheList() {
        fetch('/api/cache')
            .then(response => response.json())
            .then(data => {
                cacheItems.innerHTML = '';
                
                if (data.length === 0) {
                    cacheItems.innerHTML = '<tr><td colspan="5">No cached analyses found</td></tr>';
                    return;
                }
                
                data.forEach(item => {
                    const row = document.createElement('tr');
                    row.innerHTML = `
                        <td>${item.path}</td>
                        <td>${item.fileCount}</td>
                        <td>${item.funcCount}</td>
                        <td>${new Date(item.lastUpdate).toLocaleString()}</td>
                        <td>
                            <button class="view-btn" data-id="${item.path}">View</button>
                            <button class="delete-btn" data-id="${item.path}">Delete</button>
                        </td>
                    `;
                    cacheItems.appendChild(row);
                });
                
                // Add event listeners for buttons
                document.querySelectorAll('.view-btn').forEach(btn => {
                    btn.addEventListener('click', function() {
                        const id = this.getAttribute('data-id');
                        viewCachedAnalysis(id);
                    });
                });
                
                document.querySelectorAll('.delete-btn').forEach(btn => {
                    btn.addEventListener('click', function() {
                        const id = this.getAttribute('data-id');
                        deleteCachedAnalysis(id);
                    });
                });
            })
            .catch(error => {
                cacheItems.innerHTML = '<tr><td colspan="5">Error loading cache: ' + error.message + '</td></tr>';
            });
    }
    
    function viewCachedAnalysis(id) {
        fetch('/api/cache/' + encodeURIComponent(id))
            .then(response => response.json())
            .then(data => {
                displayResults(data);
                showSection(resultsSection);
            })
            .catch(error => {
                alert('Error loading cached analysis: ' + error.message);
            });
    }
    
    function deleteCachedAnalysis(id) {
        if (confirm('Are you sure you want to delete this cached analysis?')) {
            fetch('/api/cache/' + encodeURIComponent(id), { method: 'DELETE' })
                .then(response => {
                    if (response.ok) {
                        loadCacheList();
                    } else {
                        throw new Error('Failed to delete cached analysis');
                    }
                })
                .catch(error => {
                    alert('Error: ' + error.message);
                });
        }
    }
});`

	if err := ioutil.WriteFile(jsPath, []byte(basicJS), 0644); err != nil {
		log.Printf("Error writing main.js: %v", err)
		return
	}
}

// handle404 handles 404 errors
func handle404(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusNotFound)
	fmt.Fprintf(w, "404 - Page not found")
}

// handleAnalyzeRequest handles requests to analyze Go code
func handleAnalyzeRequest(w http.ResponseWriter, r *http.Request) {
	// Parse the request body
	var req AnalysisRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, fmt.Sprintf("Invalid request: %v", err), http.StatusBadRequest)
		return
	}

	// Use the default code directory if path is empty
	if req.Path == "" {
		req.Path = config.DefaultCodeDir
	}

	// Check if the path exists
	if _, err := os.Stat(req.Path); os.IsNotExist(err) {
		http.Error(w, fmt.Sprintf("Path does not exist: %s", req.Path), http.StatusBadRequest)
		return
	}

	// Check the cache if enabled
	if config.CacheResults {
		analysisCache.mutex.RLock()
		cachedResult, exists := analysisCache.cache[req.Path]
		analysisCache.mutex.RUnlock()

		if exists {
			// Return the cached result
			w.Header().Set("Content-Type", "application/json")
			w.Header().Set("X-Cache", "HIT")
			w.Write(cachedResult)
			return
		}
	}

	// Run the analysis
	result, err := analyzeCode(req)
	if err != nil {
		http.Error(w, fmt.Sprintf("Analysis failed: %v", err), http.StatusInternalServerError)
		return
	}

	// Convert result to JSON
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		http.Error(w, fmt.Sprintf("Failed to marshal result: %v", err), http.StatusInternalServerError)
		return
	}

	// Cache the result if enabled
	if config.CacheResults {
		analysisCache.mutex.Lock()
		analysisCache.cache[req.Path] = jsonData
		
		// Extract metadata for the cache
		var metadata map[string]interface{}
		if req.EnhancedOutput {
			metadata = result["metadata"].(map[string]interface{})
		} else {
			metadata = result["stats"].(map[string]interface{})
		}
		
		fileCount := 0
		funcCount := 0
		
		if req.EnhancedOutput {
			fileCount = int(metadata["numFiles"].(float64))
			funcCount = int(metadata["numFunctions"].(float64))
		} else {
			fileCount = len(result["files"].([]interface{}))
			funcCount = len(result["functions"].([]interface{}))
		}
		
		analysisCache.metadata[req.Path] = AnalysisCacheMetadata{
			Path:       req.Path,
			Size:       int64(len(jsonData)),
			FileCount:  fileCount,
			FuncCount:  funcCount,
			LastUpdate: time.Now(),
		}
		analysisCache.mutex.Unlock()
	}

	// Return the result
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Cache", "MISS")
	w.Write(jsonData)
}

// handleCacheList handles requests to list cached analyses
func handleCacheList(w http.ResponseWriter, r *http.Request) {
	analysisCache.mutex.RLock()
	defer analysisCache.mutex.RUnlock()

	// Convert metadata map to slice
	metadataList := make([]AnalysisCacheMetadata, 0, len(analysisCache.metadata))
	for _, metadata := range analysisCache.metadata {
		metadataList = append(metadataList, metadata)
	}

	// Return the metadata list
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(metadataList)
}

// handleCacheGet handles requests to get cached analysis
func handleCacheGet(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	analysisCache.mutex.RLock()
	cachedResult, exists := analysisCache.cache[id]
	analysisCache.mutex.RUnlock()

	if !exists {
		http.Error(w, "Cached analysis not found", http.StatusNotFound)
		return
	}

	// Return the cached result
	w.Header().Set("Content-Type", "application/json")
	w.Write(cachedResult)
}

// handleCacheDelete handles requests to delete cached analysis
func handleCacheDelete(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	analysisCache.mutex.Lock()
	defer analysisCache.mutex.Unlock()

	_, exists := analysisCache.cache[id]
	if !exists {
		http.Error(w, "Cached analysis not found", http.StatusNotFound)
		return
	}

	// Delete the cached result
	delete(analysisCache.cache, id)
	delete(analysisCache.metadata, id)

	w.WriteHeader(http.StatusOK)
}

// handleServerStatus handles requests for server status
func handleServerStatus(w http.ResponseWriter, r *http.Request) {
	status := struct {
		Status       string    `json:"status"`
		ServerTime   time.Time `json:"serverTime"`
		DefaultPath  string    `json:"defaultPath"`
		CacheEnabled bool      `json:"cacheEnabled"`
		CacheSize    int       `json:"cacheSize"`
		CacheItems   int       `json:"cacheItems"`
	}{
		Status:       "running",
		ServerTime:   time.Now(),
		DefaultPath:  config.DefaultCodeDir,
		CacheEnabled: config.CacheResults,
		CacheItems:   len(analysisCache.metadata),
	}

	// Calculate cache size
	analysisCache.mutex.RLock()
	for _, cached := range analysisCache.cache {
		status.CacheSize += len(cached)
	}
	analysisCache.mutex.RUnlock()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(status)
}

// analyzeCode runs the analysis on the specified code path
func analyzeCode(req AnalysisRequest) (map[string]interface{}, error) {
	// This is a placeholder function that would call your actual analyzer
	// For now, we'll just create a simple mock result
	mockResult := map[string]interface{}{
		"basePath": req.Path,
		"files": []interface{}{
			map[string]interface{}{
				"path":    filepath.Join(req.Path, "example.go"),
				"package": "example",
				"functions": []interface{}{
					map[string]interface{}{
						"name":      "ExampleFunction",
						"filepath":  filepath.Join(req.Path, "example.go"),
						"startLine": 10,
						"endLine":   15,
						"isMethod":  false,
					},
				},
			},
		},
		"functions": []interface{}{
			map[string]interface{}{
				"name":      "ExampleFunction",
				"filepath":  filepath.Join(req.Path, "example.go"),
				"startLine": 10,
				"endLine":   15,
				"isMethod":  false,
			},
		},
		"packages": map[string]interface{}{
			"example": req.Path,
		},
		"stats": map[string]interface{}{
			"analysisTimeSeconds": 0.001,
			"timestamp":           time.Now().Format(time.RFC3339),
			"fileCount":           1,
			"functionCount":       1,
			"methodCount":         0,
		},
	}

	// Simulate enhanced output if requested
	if req.EnhancedOutput {
		// Create a more detailed result
		enhancedResult := map[string]interface{}{
			"metadata": map[string]interface{}{
				"name":           filepath.Base(req.Path),
				"basePath":       req.Path,
				"goVersion":      "1.18",
				"moduleName":     "example.com/project",
				"numFiles":       1,
				"numFunctions":   1,
				"numMethods":     0,
				"analysisTimeMs": 1,
				"generated":      time.Now().Format(time.RFC3339),
			},
			"files": []interface{}{
				map[string]interface{}{
					"path":    filepath.Join(req.Path, "example.go"),
					"package": "example",
					"functions": []interface{}{
						map[string]interface{}{
							"id":         "example.ExampleFunction",
							"name":       "ExampleFunction",
							"signature":  "func ExampleFunction() error",
							"filepath":   filepath.Join(req.Path, "example.go"),
							"startLine":  10,
							"endLine":    15,
							"package":    "example",
							"isMethod":   false,
							"isExported": true,
							"parameters": []interface{}{},
							"returnTypes": []interface{}{
								"error",
							},
						},
					},
				},
			},
			"functions": []interface{}{
				map[string]interface{}{
					"id":         "example.ExampleFunction",
					"name":       "ExampleFunction",
					"signature":  "func ExampleFunction() error",
					"filepath":   filepath.Join(req.Path, "example.go"),
					"startLine":  10,
					"endLine":    15,
					"package":    "example",
					"isMethod":   false,
					"isExported": true,
					"parameters": []interface{}{},
					"returnTypes": []interface{}{
						"error",
					},
				},
			},
			"packages": []interface{}{
				map[string]interface{}{
					"name":  "example",
					"path":  req.Path,
					"files": []interface{}{filepath.Join(req.Path, "example.go")},
					"functions": []interface{}{
						"example.ExampleFunction",
					},
				},
			},
			"visualizationData": map[string]interface{}{
				"packageHierarchy": map[string]interface{}{
					"name": "root",
					"children": []interface{}{
						map[string]interface{}{
							"name":     "example",
							"path":     req.Path,
							"value":    1,
							"children": []interface{}{},
						},
					},
				},
			},
		}
		return enhancedResult, nil
	}

	return mockResult, nil
}

func main() {
	// Parse command line arguments
	port := flag.Int("port", 8001, "Port number for the web server")
	staticDir := flag.String("static", "./static", "Directory for static files")
	defaultCodeDir := flag.String("code", ".", "Default directory for code analysis")
	cacheResults := flag.Bool("cache", true, "Cache analysis results")
	flag.Parse()

	// Initialize and start the server
	InitializeServer(*port, *staticDir, *defaultCodeDir, *cacheResults)
	log.Fatal(StartServer())
}