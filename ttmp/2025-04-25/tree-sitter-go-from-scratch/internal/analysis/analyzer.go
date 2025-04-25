package analysis

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/your-org/go-analyzer/internal/parser"
	"golang.org/x/sync/errgroup"
)

// FileAnalysis holds analysis results for a single file.
type FileAnalysis struct {
	Path      string                `json:"path"`      // Relative path to the base directory
	Package   string                `json:"package"`   // Package name declared in the file
	Functions []parser.FunctionInfo `json:"functions"` // Functions found in the file
}

// AnalysisResult holds the aggregated results for a directory.
type AnalysisResult struct {
	BasePath string         `json:"basePath"` // The absolute root path that was analyzed
	Files    []FileAnalysis `json:"files"`
	// TODO: Add stats later (e.g., total files, total functions)
}

// AnalyzeDirectory recursively analyzes Go files in a directory.
// It skips directories listed in excludeDirs and files ending in _test.go.
func AnalyzeDirectory(ctx context.Context, rootDir string, p *parser.Parser, excludeDirs []string) (*AnalysisResult, error) {
	var files []FileAnalysis
	var mu sync.Mutex // Mutex to protect concurrent writes to the files slice
	excludeMap := make(map[string]bool)
	for _, dir := range excludeDirs {
		excludeMap[dir] = true
	}

	absRootDir, err := filepath.Abs(rootDir)
	if err != nil {
		return nil, err // Consider wrapping errors
	}

	// Use errgroup for potentially concurrent file parsing
	eg, childCtx := errgroup.WithContext(ctx)

	err = filepath.Walk(rootDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err // Error accessing path
		}

		// Check context cancellation
		select {
		case <-childCtx.Done():
			return childCtx.Err() // Stop walking if context is cancelled
		default:
		}

		if info.IsDir() {
			if excludeMap[info.Name()] {
				return filepath.SkipDir // Skip excluded directories
			}
			return nil // Continue walking
		}

		// Process only Go files, excluding test files
		if strings.HasSuffix(info.Name(), ".go") && !strings.HasSuffix(info.Name(), "_test.go") {
			// Make path relative to the original rootDir for consistent output
			relPath, err := filepath.Rel(rootDir, path)
			if err != nil {
				// Should generally not happen if path is under rootDir
				relPath = path
			}

			// Run parsing in a separate goroutine using errgroup
			eg.Go(func() error {
				absPath, err := filepath.Abs(path) // parser needs absolute path
				if err != nil {
					return err
				}

				// TODO: Enhance parser to return package name as well
				functions, pkgName, err := p.ParseFileAndPackage(childCtx, absPath)
				if err != nil {
					// Log error but potentially continue? Or fail fast?
					// For now, let's fail fast via errgroup.
					return err
				}

				// If functions were found, add the file analysis result
				// We might want to add files even if they have no functions, depends on requirements
				if len(functions) > 0 || pkgName != "" {
					mu.Lock()
					files = append(files, FileAnalysis{
						Path:      relPath,
						Package:   pkgName,
						Functions: functions,
					})
					mu.Unlock()
				}
				return nil
			})
		}

		return nil // Continue walking
	})

	// Wait for all parsing goroutines to complete and collect potential errors.
	if walkErr := eg.Wait(); walkErr != nil {
		// If the walk itself failed, return that error
		if err == nil {
			err = walkErr
		}
	}

	if err != nil {
		return nil, err
	}

	return &AnalysisResult{
		BasePath: absRootDir,
		Files:    files,
	}, nil
}
