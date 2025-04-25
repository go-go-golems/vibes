package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/wesen/tree-sitter-go-cleaned-up/internal/analyzer"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/formatter"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/parser"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/web"
)

func main() {
	// Define command-line flags similar to the original web_server
	port := flag.Int("port", 8080, "Port number for the web server")
	staticDir := flag.String("static-dir", "internal/web/static", "Directory for static web assets (relative to execution path)")
	defaultCodeDir := flag.String("default-code-dir", ".", "Default directory path offered for analysis in the UI")
	cacheResults := flag.Bool("cache", true, "Enable in-memory caching of analysis results")
	// Flags for underlying analyzer configuration (can be passed through)
	excludeFlag := flag.String("exclude-dirs", "vendor,node_modules,dist,build,.git,.idea,.vscode", "Default comma-separated list of directory names to exclude for analysis")
	excludeFilesFlag := flag.String("exclude-files", "", "Default comma-separated list of file name patterns to exclude for analysis")
	concurrency := flag.Int("concurrency", 8, "Number of files to analyze concurrently")

	flag.Parse()

	log.Printf("Starting Go Code Analyzer Web Server...")
	log.Printf("Port: %d", *port)
	log.Printf("Static Directory: %s", *staticDir)
	log.Printf("Default Code Dir: %s", *defaultCodeDir)
	log.Printf("Caching Enabled: %t", *cacheResults)
	log.Printf("Default Exclude Dirs: %s", *excludeFlag)
	log.Printf("Default Exclude Files: %s", *excludeFilesFlag)
	log.Printf("Analysis Concurrency: %d", *concurrency)

	// Initialize components
	goParser, err := parser.NewGoParser()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating parser: %v\n", err)
		os.Exit(1)
	}

	// Parse exclude lists for the analyzer
	var excludeDirs []string
	if *excludeFlag != "" {
		excludeDirs = strings.Split(*excludeFlag, ",")
	}
	var excludeFilePatterns []string
	if *excludeFilesFlag != "" {
		excludeFilePatterns = strings.Split(*excludeFilesFlag, ",")
	}

	analyzerConfig := analyzer.Config{
		Parser:          goParser,
		ExcludeDirs:     excludeDirs,
		ExcludePatterns: excludeFilePatterns,
		Concurrency:     *concurrency,
	}
	analyzerInstance, err := analyzer.NewAnalyzer(analyzerConfig)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating analyzer: %v\n", err)
		os.Exit(1)
	}

	formatterInstance := formatter.NewFormatter()

	// Configure and create the web server
	webConfig := web.Config{
		Port:           *port,
		StaticDir:      *staticDir,
		DefaultCodeDir: *defaultCodeDir,
		CacheResults:   *cacheResults,
	}

	server, err := web.NewServer(webConfig, analyzerInstance, formatterInstance)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating web server: %v\n", err)
		os.Exit(1)
	}

	// Start the server
	log.Fatal(server.ListenAndServe())
}
