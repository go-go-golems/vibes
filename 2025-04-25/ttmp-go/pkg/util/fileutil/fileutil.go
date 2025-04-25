package fileutil

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"ttmp-go/pkg/errors"
)

// FileExists checks if a file exists
func FileExists(path string) bool {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}

// DirectoryExists checks if a directory exists
func DirectoryExists(path string) bool {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false
	}
	return info.IsDir()
}

// GetFileInfo gets the file info for a path
func GetFileInfo(path string) (os.FileInfo, error) {
	info, err := os.Stat(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, errors.NewIOError(fmt.Sprintf("Path does not exist: %s", path), err)
		}
		return nil, errors.NewIOError(fmt.Sprintf("Error accessing path: %s", path), err)
	}
	return info, nil
}

// EnsureDirectoryExists ensures a directory exists, creating it if necessary
func EnsureDirectoryExists(path string) error {
	if !DirectoryExists(path) {
		err := os.MkdirAll(path, 0755)
		if err != nil {
			return errors.NewIOError(fmt.Sprintf("Failed to create directory: %s", path), err)
		}
	}
	return nil
}

// FindMarkdownFiles finds all markdown files in a directory (non-recursive)
func FindMarkdownFiles(dir string) ([]string, error) {
	files, err := os.ReadDir(dir)
	if err != nil {
		return nil, errors.NewIOError(fmt.Sprintf("Failed to read directory: %s", dir), err)
	}

	result := make([]string, 0)
	for _, file := range files {
		if file.IsDir() {
			continue
		}

		name := file.Name()
		if strings.HasSuffix(strings.ToLower(name), ".md") {
			result = append(result, filepath.Join(dir, name))
		}
	}

	return result, nil
}

// FindMarkdownFilesRecursive finds all markdown files in a directory and its subdirectories
func FindMarkdownFilesRecursive(dir string) ([]string, error) {
	var result []string

	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() && strings.HasSuffix(strings.ToLower(path), ".md") {
			result = append(result, path)
		}

		return nil
	})

	if err != nil {
		return nil, errors.NewIOError(fmt.Sprintf("Failed to walk directory: %s", dir), err)
	}

	return result, nil
}

// ReadFileContent reads the content of a file
func ReadFileContent(path string) (string, error) {
	// Check if the file exists
	if !FileExists(path) {
		return "", errors.NewIOError(fmt.Sprintf("File does not exist: %s", path), nil)
	}

	// Read the file content
	content, err := os.ReadFile(path)
	if err != nil {
		return "", errors.NewIOError(fmt.Sprintf("Failed to read file: %s", path), err)
	}

	return string(content), nil
}

// WriteFileContent writes content to a file
func WriteFileContent(path string, content string) error {
	// Ensure the directory exists
	dir := filepath.Dir(path)
	if err := EnsureDirectoryExists(dir); err != nil {
		return err
	}

	// Write the content to the file
	err := os.WriteFile(path, []byte(content), 0644)
	if err != nil {
		return errors.NewIOError(fmt.Sprintf("Failed to write to file: %s", path), err)
	}

	return nil
}