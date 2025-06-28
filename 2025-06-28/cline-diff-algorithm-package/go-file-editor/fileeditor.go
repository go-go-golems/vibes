package fileeditor

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// FileEditor provides file editing capabilities similar to Cline's tools
type FileEditor struct {
	workingDir string
}

// NewFileEditor creates a new file editor with the specified working directory
func NewFileEditor(workingDir string) *FileEditor {
	return &FileEditor{
		workingDir: workingDir,
	}
}

// resolvePath resolves a relative path to an absolute path within the working directory
func (fe *FileEditor) resolvePath(path string) (string, error) {
	if filepath.IsAbs(path) {
		return "", fmt.Errorf("absolute paths are not allowed: %s", path)
	}
	
	resolved := filepath.Join(fe.workingDir, path)
	
	// Ensure the resolved path is still within the working directory
	absWorkingDir, err := filepath.Abs(fe.workingDir)
	if err != nil {
		return "", fmt.Errorf("failed to resolve working directory: %w", err)
	}
	
	absResolved, err := filepath.Abs(resolved)
	if err != nil {
		return "", fmt.Errorf("failed to resolve path: %w", err)
	}
	
	if !strings.HasPrefix(absResolved, absWorkingDir) {
		return "", fmt.Errorf("path escapes working directory: %s", path)
	}
	
	return absResolved, nil
}

// ReadFile reads the contents of a file at the specified path
func (fe *FileEditor) ReadFile(path string) (string, error) {
	resolvedPath, err := fe.resolvePath(path)
	if err != nil {
		return "", err
	}
	
	content, err := os.ReadFile(resolvedPath)
	if err != nil {
		return "", fmt.Errorf("failed to read file %s: %w", path, err)
	}
	
	return string(content), nil
}

// WriteToFile writes content to a file at the specified path, creating directories as needed
func (fe *FileEditor) WriteToFile(path, content string) error {
	resolvedPath, err := fe.resolvePath(path)
	if err != nil {
		return err
	}
	
	// Create directory if it doesn't exist
	dir := filepath.Dir(resolvedPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create directory for %s: %w", path, err)
	}
	
	// Write the file
	if err := os.WriteFile(resolvedPath, []byte(content), 0644); err != nil {
		return fmt.Errorf("failed to write file %s: %w", path, err)
	}
	
	return nil
}

// ReplaceInFile applies SEARCH/REPLACE blocks to modify an existing file
func (fe *FileEditor) ReplaceInFile(path, diff string) error {
	resolvedPath, err := fe.resolvePath(path)
	if err != nil {
		return err
	}
	
	// Check if file exists
	if _, err := os.Stat(resolvedPath); err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf("file does not exist: %s", path)
		}
		return fmt.Errorf("failed to access file %s: %w", path, err)
	}
	
	// Read current content
	originalContent, err := fe.ReadFile(path)
	if err != nil {
		return err
	}
	
	// Apply the diff
	newContent, err := ConstructNewFileContent(diff, originalContent, true)
	if err != nil {
		return fmt.Errorf("failed to apply diff to %s: %w", path, err)
	}
	
	// Write the new content
	if err := fe.WriteToFile(path, newContent); err != nil {
		return err
	}
	
	return nil
}

// ListFiles lists files and directories within the specified directory
func (fe *FileEditor) ListFiles(path string, recursive bool) ([]string, error) {
	resolvedPath, err := fe.resolvePath(path)
	if err != nil {
		return nil, err
	}
	
	var files []string
	
	if recursive {
		err = filepath.WalkDir(resolvedPath, func(filePath string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			
			// Get relative path from the working directory
			relPath, err := filepath.Rel(fe.workingDir, filePath)
			if err != nil {
				return err
			}
			
			if d.IsDir() {
				files = append(files, relPath+"/")
			} else {
				files = append(files, relPath)
			}
			
			return nil
		})
	} else {
		entries, err := os.ReadDir(resolvedPath)
		if err != nil {
			return nil, fmt.Errorf("failed to read directory %s: %w", path, err)
		}
		
		for _, entry := range entries {
			name := entry.Name()
			if entry.IsDir() {
				name += "/"
			}
			
			if path == "." {
				files = append(files, name)
			} else {
				files = append(files, filepath.Join(path, name))
			}
		}
	}
	
	if err != nil {
		return nil, fmt.Errorf("failed to list files in %s: %w", path, err)
	}
	
	return files, nil
}

// FileExists checks if a file exists at the specified path
func (fe *FileEditor) FileExists(path string) (bool, error) {
	resolvedPath, err := fe.resolvePath(path)
	if err != nil {
		return false, err
	}
	
	_, err = os.Stat(resolvedPath)
	if err != nil {
		if os.IsNotExist(err) {
			return false, nil
		}
		return false, err
	}
	
	return true, nil
}

// CreateDirectory creates a directory at the specified path
func (fe *FileEditor) CreateDirectory(path string) error {
	resolvedPath, err := fe.resolvePath(path)
	if err != nil {
		return err
	}
	
	if err := os.MkdirAll(resolvedPath, 0755); err != nil {
		return fmt.Errorf("failed to create directory %s: %w", path, err)
	}
	
	return nil
}

