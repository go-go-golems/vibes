package analyzer

import (
	"context"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/wesen/tree-sitter-go-cleaned-up/internal/parser"
	"github.com/wesen/tree-sitter-go-cleaned-up/pkg/models"
	"golang.org/x/sync/errgroup"
)

// Analyzer handles the analysis of a Go codebase.
type Analyzer struct {
	parser          *parser.GoParser
	excludeDirs     map[string]bool
	excludePatterns []string // Renamed from excludeFileRegex for clarity
	concurrency     int
}

// Config holds configuration for the Analyzer.
type Config struct {
	Parser          *parser.GoParser
	ExcludeDirs     []string
	ExcludePatterns []string
	Concurrency     int
}

// NewAnalyzer creates a new codebase analyzer.
func NewAnalyzer(cfg Config) (*Analyzer, error) {
	if cfg.Parser == nil {
		return nil, errors.New("parser must be provided in analyzer config")
	}
	if cfg.Concurrency <= 0 {
		cfg.Concurrency = 4 // Default concurrency
	}

	// Convert exclude dirs to map for fast lookup
	excludeDirMap := make(map[string]bool)
	for _, dir := range cfg.ExcludeDirs {
		excludeDirMap[dir] = true
	}

	return &Analyzer{
		parser:          cfg.Parser,
		excludeDirs:     excludeDirMap,
		excludePatterns: cfg.ExcludePatterns,
		concurrency:     cfg.Concurrency,
	}, nil
}

// AnalyzeCodebase analyzes a Go codebase located at basePath.
func (a *Analyzer) AnalyzeCodebase(ctx context.Context, basePath string) (*models.CodebaseAnalysis, error) {
	startTime := time.Now()

	// Convert to absolute path
	absBasePath, err := filepath.Abs(basePath)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to get absolute path for %s", basePath)
	}

	// Find all Go files
	goFiles, err := a.findGoFiles(ctx, absBasePath)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to find Go files in %s", absBasePath)
	}

	log.Printf("Found %d Go files to analyze in %s", len(goFiles), absBasePath)
	if len(goFiles) == 0 {
		return nil, errors.New("no Go files found to analyze")
	}

	// Analyze Go files concurrently
	fileInfos, err := a.analyzeGoFiles(ctx, goFiles)
	if err != nil {
		// Log the error but potentially return partial results if desired?
		// For now, return the error.
		log.Printf("Error during file analysis: %v", err)
		// Depending on requirements, we might return partial fileInfos here along with the error.
		return nil, errors.Wrap(err, "failed to analyze Go files")
	}

	// Process analysis results
	var allFunctions []models.FunctionInfo
	packages := make(map[string]models.PackageInfo)
	var analyzedFileInfos []models.FileInfo // Use models.FileInfo

	for _, fileInfo := range fileInfos {
		if fileInfo == nil { // Skip nil entries which might occur on error
			continue
		}
		analyzedFileInfos = append(analyzedFileInfos, *fileInfo)

		// Add functions
		allFunctions = append(allFunctions, fileInfo.Functions...)

		// Add/update package info
		if fileInfo.Package != "" {
			dir := filepath.Dir(fileInfo.Path)
			relDir, _ := filepath.Rel(absBasePath, dir)
			if relDir == "." {
				relDir = ""
			}
			pkgInfo, exists := packages[fileInfo.Package]
			if !exists {
				pkgInfo = models.PackageInfo{
					Name:  fileInfo.Package,
					Path:  relDir,
					Files: []string{},
				}
			}
			pkgInfo.Files = append(pkgInfo.Files, fileInfo.Path) // Store relative path
			packages[fileInfo.Package] = pkgInfo
		}
	}

	// Extract dependencies
	dependencies := a.extractDependencies(analyzedFileInfos)

	// Calculate stats
	funcCount := 0
	methodCount := 0
	for _, f := range allFunctions {
		if f.IsMethod {
			methodCount++
		} else {
			funcCount++
		}
	}

	analysisDuration := time.Since(startTime)
	stats := models.AnalysisStats{
		TotalFiles:     len(analyzedFileInfos),
		TotalFunctions: funcCount,
		TotalMethods:   methodCount,
		TotalPackages:  len(packages),
		AnalysisTimeMs: analysisDuration.Milliseconds(),
		Timestamp:      time.Now().UTC(),
	}

	// Create analysis result
	analysis := &models.CodebaseAnalysis{
		BasePath:     absBasePath,
		Files:        analyzedFileInfos,
		Functions:    allFunctions,
		Packages:     packages,
		Dependencies: dependencies,
		Stats:        stats,
	}

	return analysis, nil
}

// findGoFiles recursively finds all Go files in a directory, respecting context cancellation.
func (a *Analyzer) findGoFiles(ctx context.Context, basePath string) ([]string, error) {
	var goFiles []string
	var walkErr error

	walkFunc := func(path string, info os.FileInfo, err error) error {
		if err != nil {
			// Check if the error is permission denied, log and continue if possible
			if os.IsPermission(err) {
				log.Printf("Permission denied accessing %s, skipping: %v", path, err)
				if info != nil && info.IsDir() {
					return filepath.SkipDir
				}
				return nil // Skip the single file
			}
			return errors.Wrapf(err, "error accessing path %s during walk", path)
		}

		// Check for context cancellation
		select {
		case <-ctx.Done():
			return ctx.Err() // Stop walking if context is cancelled
		default:
		}

		// Skip excluded directories and hidden directories
		if info.IsDir() {
			dirName := info.Name()
			if a.excludeDirs[dirName] || (dirName != "." && strings.HasPrefix(dirName, ".")) {
				// log.Printf("Skipping directory: %s", path)
				return filepath.SkipDir
			}
			return nil
		}

		// Process only Go files, excluding tests
		if !info.IsDir() && filepath.Ext(path) == ".go" {
			if strings.HasSuffix(path, "_test.go") {
				return nil
			}

			// Check against exclude patterns
			fileName := filepath.Base(path)
			for _, pattern := range a.excludePatterns {
				// Use filepath.Match for more robust pattern matching?
				// For now, stick to simple Contains as in original code.
				if pattern != "" && strings.Contains(fileName, pattern) {
					// log.Printf("Skipping file due to exclude pattern '%s': %s", pattern, path)
					return nil
				}
			}

			goFiles = append(goFiles, path)
		}

		return nil
	}

	walkErr = filepath.Walk(basePath, walkFunc)

	// Don't return context.Canceled as the final error if walk completed partially
	if walkErr == context.Canceled || walkErr == context.DeadlineExceeded {
		log.Printf("File search interrupted: %v", walkErr)
		return goFiles, walkErr // Return found files and the context error
	}

	return goFiles, walkErr // Return files and any other walk error
}

// analyzeGoFiles analyzes a list of Go files in parallel using errgroup.
func (a *Analyzer) analyzeGoFiles(ctx context.Context, filePaths []string) ([]*models.FileInfo, error) {
	fileInfos := make([]*models.FileInfo, len(filePaths))
	eg, childCtx := errgroup.WithContext(ctx)
	eg.SetLimit(a.concurrency)

	for i, filePath := range filePaths {
		index := i       // Capture loop variables for goroutine
		path := filePath // Capture loop variables for goroutine

		eg.Go(func() error {
			// Check context before starting analysis for this file
			select {
			case <-childCtx.Done():
				return childCtx.Err()
			default:
			}

			// log.Printf("Analyzing: %s", path)
			fileInfo, err := a.parser.AnalyzeFile(childCtx, path)
			if err != nil {
				// Log the error but don't immediately fail the group, allow others to continue.
				// We will collect errors at the end.
				log.Printf("Error analyzing %s: %v", path, err)
				// Store nil in the result slice for this index to indicate failure
				fileInfos[index] = nil
				// Don't return the error here, let the group continue
				// return errors.Wrapf(err, "failed to analyze %s", path)
				return nil // Signal successful completion of this goroutine's attempt
			}

			// Update paths to be relative to the original base path if needed?
			// Currently, parser stores the absolute path. Analyzer might need to adjust.

			fileInfos[index] = fileInfo
			return nil
		})
	}

	// Wait for all analyses to complete or for an error/cancellation.
	if err := eg.Wait(); err != nil {
		// This error is likely context cancellation if it happens.
		// Individual file errors were logged within the goroutines.
		// We still return the collected fileInfos, which may contain nils.
		log.Printf("Error group finished with error: %v", err)
		return fileInfos, err // Return potentially partial results and the group error
	}

	// Filter out nil entries from fileInfos if any errors occurred but didn't halt the group
	validFileInfos := make([]*models.FileInfo, 0, len(fileInfos))
	for _, fi := range fileInfos {
		if fi != nil {
			validFileInfos = append(validFileInfos, fi)
		}
	}

	return validFileInfos, nil
}

// extractDependencies extracts external dependencies from the import paths.
func (a *Analyzer) extractDependencies(fileInfos []models.FileInfo) []models.Dependency {
	dependencyMap := make(map[string]*models.Dependency)

	for _, fileInfo := range fileInfos {
		for _, importInfo := range fileInfo.Imports {
			// Basic check to filter standard library packages.
			// More robust checks might involve querying `go list std`.
			if !strings.Contains(importInfo.Path, ".") {
				continue // Likely standard library
			}

			// Potentially filter internal packages if module path is known?
			// For now, treat all non-std packages as potential external dependencies.

			dep, exists := dependencyMap[importInfo.Path]
			if !exists {
				dep = &models.Dependency{
					ImportPath: importInfo.Path,
					// Module can be potentially derived later using `go mod graph` or similar
					UsedBy: []string{},
				}
				dependencyMap[importInfo.Path] = dep
			}
			dep.UsedBy = append(dep.UsedBy, fileInfo.Path) // Store relative path
		}
	}

	// Convert map to slice
	var dependencies []models.Dependency
	for _, dep := range dependencyMap {
		// Deduplicate UsedBy list if necessary (unlikely with file paths)
		dependencies = append(dependencies, *dep)
	}

	// Sort for deterministic output
	sort.Slice(dependencies, func(i, j int) bool {
		return dependencies[i].ImportPath < dependencies[j].ImportPath
	})

	return dependencies
}
