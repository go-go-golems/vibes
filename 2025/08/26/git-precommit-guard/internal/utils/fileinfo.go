package utils

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strings"

	"github.com/user/git-precommit-guard/pkg/detector"
)

// GetFileInfo gathers comprehensive information about a file
func GetFileInfo(filePath string) (*detector.FileInfo, error) {
	// Get file stats
	stat, err := os.Stat(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to stat file %s: %w", filePath, err)
	}

	// Read first few bytes for magic number detection
	content, err := readFileHeader(filePath, 512) // Read first 512 bytes
	if err != nil {
		return nil, fmt.Errorf("failed to read file header %s: %w", filePath, err)
	}

	// Detect MIME type
	mimeType := detectMimeType(filePath, content)

	return &detector.FileInfo{
		Path:     filePath,
		Size:     stat.Size(),
		MimeType: mimeType,
		Content:  content,
	}, nil
}

// readFileHeader reads the first n bytes of a file
func readFileHeader(filePath string, n int) ([]byte, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	buffer := make([]byte, n)
	bytesRead, err := file.Read(buffer)
	if err != nil && err != io.EOF {
		return nil, err
	}

	return buffer[:bytesRead], nil
}

// detectMimeType detects MIME type using multiple methods
func detectMimeType(filePath string, content []byte) string {
	// Method 1: Use Go's built-in MIME detection
	if len(content) > 0 {
		mimeType := http.DetectContentType(content)
		if mimeType != "application/octet-stream" {
			return mimeType
		}
	}

	// Method 2: Use the file command
	if mimeType := detectMimeWithFileCommand(filePath); mimeType != "" {
		return mimeType
	}

	// Method 3: Fallback to extension-based detection
	return detectMimeFromExtension(filePath)
}

// detectMimeWithFileCommand uses the system's file command to detect MIME type
func detectMimeWithFileCommand(filePath string) string {
	cmd := exec.Command("file", "--mime-type", "--brief", filePath)
	output, err := cmd.Output()
	if err != nil {
		return ""
	}

	mimeType := strings.TrimSpace(string(output))
	return mimeType
}

// detectMimeFromExtension detects MIME type based on file extension
func detectMimeFromExtension(filePath string) string {
	// This is a simplified version - in a real implementation,
	// you might want to use a more comprehensive MIME type database
	ext := strings.ToLower(filePath)
	
	if strings.HasSuffix(ext, ".txt") {
		return "text/plain"
	}
	if strings.HasSuffix(ext, ".json") {
		return "application/json"
	}
	if strings.HasSuffix(ext, ".xml") {
		return "application/xml"
	}
	if strings.HasSuffix(ext, ".yaml") || strings.HasSuffix(ext, ".yml") {
		return "application/yaml"
	}
	if strings.HasSuffix(ext, ".md") {
		return "text/markdown"
	}
	if strings.HasSuffix(ext, ".html") {
		return "text/html"
	}
	if strings.HasSuffix(ext, ".css") {
		return "text/css"
	}
	if strings.HasSuffix(ext, ".js") {
		return "application/javascript"
	}
	if strings.HasSuffix(ext, ".exe") || strings.HasSuffix(ext, ".dll") {
		return "application/x-msdownload"
	}
	if strings.HasSuffix(ext, ".so") {
		return "application/x-sharedlib"
	}
	if strings.HasSuffix(ext, ".jar") {
		return "application/java-archive"
	}

	return "application/octet-stream"
}

// IsTextFile checks if a file is likely a text file based on its content
func IsTextFile(content []byte) bool {
	if len(content) == 0 {
		return true // Empty files are considered text
	}

	// Check for null bytes (common in binary files)
	for _, b := range content {
		if b == 0 {
			return false
		}
	}

	// Check for high ratio of printable characters
	printableCount := 0
	for _, b := range content {
		if (b >= 32 && b <= 126) || b == 9 || b == 10 || b == 13 {
			printableCount++
		}
	}

	ratio := float64(printableCount) / float64(len(content))
	return ratio > 0.95 // 95% printable characters
}

