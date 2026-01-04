package util

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// EnsureDirectory ensures that the directory exists and creates it if it doesn't
func EnsureDirectory(dir string) error {
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return os.MkdirAll(dir, 0755)
	}
	return nil
}

// FindMarkdownFiles returns a list of markdown files in the given directory
func FindMarkdownFiles(dir string, recursive bool) ([]string, error) {
	var files []string

	walkFunc := func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip directories unless recursive is true
		if info.IsDir() {
			if path == dir {
				return nil // Don't skip the root directory
			}

			if !recursive {
				return filepath.SkipDir
			}

			return nil
		}

		// Only include markdown files
		if strings.HasSuffix(strings.ToLower(path), ".md") {
			files = append(files, path)
		}

		return nil
	}

	err := filepath.Walk(dir, walkFunc)
	if err != nil {
		return nil, fmt.Errorf("error walking directory: %w", err)
	}

	return files, nil
}

// CreateTimestampedDir creates a timestamped directory
func CreateTimestampedDir(baseDir string) (string, error) {
	timestamp := time.Now().Format("2006-01-02")
	dir := filepath.Join(baseDir, timestamp)

	err := EnsureDirectory(dir)
	if err != nil {
		return "", fmt.Errorf("failed to create timestamped directory: %w", err)
	}

	return dir, nil
}

// FileExists checks if a file exists
func FileExists(filePath string) bool {
	info, err := os.Stat(filePath)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}

// DirExists checks if a directory exists
func DirExists(dirPath string) bool {
	info, err := os.Stat(dirPath)
	if os.IsNotExist(err) {
		return false
	}
	return info.IsDir()
}