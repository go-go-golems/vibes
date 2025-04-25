package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"html/template"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// Configuration for the server
type Config struct {
	Port       int
	StaticDir  string
	AnalyzerPath string
	FormatterPath string
	TempDir    string
}

// AnalysisResult represents the result of a file analysis
type AnalysisResult struct {
	Files     []FileInfo     `json:"files"`
	Functions []FunctionInfo `json:"functions"`
	Packages  []PackageInfo  `json:"packages,omitempty"`
	Metadata  struct {
		Name         string    `json:"name"`
		BasePath     string    `json:"basePath"`
		NumFiles     int       `json:"numFiles"`
		NumFunctions int       `json:"numFunctions"`
		NumMethods   int       `json:"numMethods"`
		Generated    time.Time `json:"generated"`
	} `json:"metadata,omitempty"`
	Stats struct {
		FileCount     int       `json:"fileCount"`
		FunctionCount int       `json:"functionCount"`
		MethodCount   int       `json:"methodCount"`
		AnalysisTime  float64   `json:"analysisTime"`
		Timestamp     time.Time `json:"timestamp"`
	} `json:"stats"`
}

// FileInfo represents a Go file
type FileInfo struct {
	Path      string         `json:"path"`
	Package   string         `json:"package"`
	Functions []FunctionInfo `json:"functions"`
}

// FunctionInfo represents a function
type FunctionInfo struct {
	ID         string   `json:"id,omitempty"`
	Name       string   `json:"name"`
	IsMethod   bool     `json:"isMethod"`
	IsExported bool     `json:"isExported"`
	Filepath   string   `json:"filepath"`
	StartLine  int      `json:"startLine"`
	EndLine    int      `json:"endLine"`
	ReturnType []string `json:"returnType,omitempty"`
	Package    string   `json:"package"`
}

// PackageInfo represents a Go package
type PackageInfo struct {
	Name      string   `json:"name"`
	Path      string   `json:"path"`
	Files     []string `json:"files"`
	Functions []string `json:"functions"`
}

// AnalyzeRequest represents an analysis request
type AnalyzeRequest struct {
	Path string `json:"path"`
}

// Global configuration
var config Config

// runAnalyzer runs the analyzer and returns the path to the output file
func runAnalyzer(path string) (string, error) {
	// Create a temporary output file
	outputFile := filepath.Join(config.TempDir, fmt.Sprintf("analysis_%d.json", time.Now().UnixNano()))
	
	// Run the analyzer
	cmd := exec.Command(config.AnalyzerPath, "-path", path, "-output", outputFile, "-verbose")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	
	log.Printf("Running analyzer: %s", cmd.String())
	
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("error running analyzer: %v", err)
	}
	
	// Run the formatter
	enhancedOutputFile := filepath.Join(config.TempDir, fmt.Sprintf("enhanced_%d.json", time.Now().UnixNano()))
	
	formatCmd := exec.Command(config.FormatterPath, outputFile, enhancedOutputFile)
	formatCmd.Stdout = os.Stdout
	formatCmd.Stderr = os.Stderr
	
	log.Printf("Running formatter: %s", formatCmd.String())
	
	if err := formatCmd.Run(); err != nil {
		return outputFile, fmt.Errorf("error running formatter (using basic output): %v", err)
	}
	
	return enhancedOutputFile, nil
}

// Handler for analyzing a directory
func analyzeHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	
	// Parse the request
	var req AnalyzeRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, fmt.Sprintf("Error parsing request: %v", err), http.StatusBadRequest)
		return
	}
	
	// Validate the path
	if req.Path == "" {
		http.Error(w, "Path is required", http.StatusBadRequest)
		return
	}
	
	// Make sure the path exists
	if _, err := os.Stat(req.Path); os.IsNotExist(err) {
		http.Error(w, fmt.Sprintf("Path does not exist: %s", req.Path), http.StatusBadRequest)
		return
	}
	
	// Run the analyzer
	outputFile, err := runAnalyzer(req.Path)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error analyzing directory: %v", err), http.StatusInternalServerError)
		return
	}
	
	// Read the output
	data, err := ioutil.ReadFile(outputFile)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error reading output file: %v", err), http.StatusInternalServerError)
		return
	}
	
	// Set the content type
	w.Header().Set("Content-Type", "application/json")
	
	// Write the output
	w.Write(data)
}

// Handler for getting file content
func fileContentHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	
	// Get the file path from the query string
	path := r.URL.Query().Get("path")
	if path == "" {
		http.Error(w, "Path is required", http.StatusBadRequest)
		return
	}
	
	// Make sure the path exists
	if _, err := os.Stat(path); os.IsNotExist(err) {
		http.Error(w, fmt.Sprintf("File does not exist: %s", path), http.StatusNotFound)
		return
	}
	
	// Only allow .go files
	if filepath.Ext(path) != ".go" {
		http.Error(w, "Only .go files are allowed", http.StatusBadRequest)
		return
	}
	
	// Read the file content
	data, err := ioutil.ReadFile(path)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error reading file: %v", err), http.StatusInternalServerError)
		return
	}
	
	// Set the content type
	w.Header().Set("Content-Type", "text/plain")
	
	// Write the output
	w.Write(data)
}

// indexHandler serves the main HTML page
func indexHandler(w http.ResponseWriter, r *http.Request) {
	// If the path is not the root, serve the static file
	if r.URL.Path != "/" {
		http.FileServer(http.Dir(config.StaticDir)).ServeHTTP(w, r)
		return
	}
	
	// Serve the index.html template
	tmpl, err := template.ParseFiles(filepath.Join(config.StaticDir, "index.html"))
	if err != nil {
		http.Error(w, fmt.Sprintf("Error parsing template: %v", err), http.StatusInternalServerError)
		return
	}
	
	// Execute the template
	if err := tmpl.Execute(w, nil); err != nil {
		http.Error(w, fmt.Sprintf("Error executing template: %v", err), http.StatusInternalServerError)
		return
	}
}

func main() {
	// Parse command line flags
	port := flag.Int("port", 8001, "Port to listen on")
	staticDir := flag.String("static", "static", "Directory containing static files")
	analyzerPath := flag.String("analyzer", "../recursive_analyzer/recursive_analyzer", "Path to the analyzer executable")
	formatterPath := flag.String("formatter", "../json_formatter/json_formatter", "Path to the formatter executable")
	tempDir := flag.String("temp", os.TempDir(), "Directory for temporary files")
	flag.Parse()
	
	// Set up configuration
	config = Config{
		Port:          *port,
		StaticDir:     *staticDir,
		AnalyzerPath:  *analyzerPath,
		FormatterPath: *formatterPath,
		TempDir:       *tempDir,
	}
	
	// Make sure the temporary directory exists
	if err := os.MkdirAll(config.TempDir, 0755); err != nil {
		log.Fatalf("Error creating temporary directory: %v", err)
	}
	
	// Make sure the static directory exists
	if _, err := os.Stat(config.StaticDir); os.IsNotExist(err) {
		log.Fatalf("Static directory does not exist: %s", config.StaticDir)
	}
	
	// Set up HTTP handlers
	http.HandleFunc("/", indexHandler)
	http.HandleFunc("/api/analyze", analyzeHandler)
	http.HandleFunc("/api/file", fileContentHandler)
	
	// Serve static files
	fs := http.FileServer(http.Dir(config.StaticDir))
	http.Handle("/static/", http.StripPrefix("/static/", fs))
	
	// Start the server
	addr := fmt.Sprintf(":%d", config.Port)
	log.Printf("Starting server on http://localhost%s", addr)
	if err := http.ListenAndServe(addr, nil); err != nil {
		log.Fatalf("Error starting server: %v", err)
	}
}