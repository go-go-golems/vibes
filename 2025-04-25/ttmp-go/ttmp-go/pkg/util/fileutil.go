package util

import (
	"os"
	"path/filepath"
	"strings"
)

// FindMarkdownFiles finds all markdown files in a directory recursively
func FindMarkdownFiles(rootDir string) ([]string, error) {
	var files []string

	err := filepath.Walk(rootDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() && strings.HasSuffix(strings.ToLower(info.Name()), ".md") {
			files = append(files, path)
		}

		return nil
	})

	if err != nil {
		return nil, err
	}

	return files, nil
}

// EnsureDirectory ensures that a directory exists, creating it if necessary
func EnsureDirectory(dirPath string) error {
	if _, err := os.Stat(dirPath); os.IsNotExist(err) {
		return os.MkdirAll(dirPath, 0755)
	}
	return nil
}

// FileExists checks if a file exists
func FileExists(filePath string) bool {
	_, err := os.Stat(filePath)
	return !os.IsNotExist(err)
}

// ReadFileContents reads a file's contents as a string
func ReadFileContents(filePath string) (string, error) {
	data, err := os.ReadFile(filePath)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

// WriteFileContents writes a string to a file
func WriteFileContents(filePath string, contents string) error {
	// Create the directory if it doesn't exist
	dirPath := filepath.Dir(filePath)
	if err := EnsureDirectory(dirPath); err != nil {
		return err
	}

	// Write the file
	return os.WriteFile(filePath, []byte(contents), 0644)
}