package storage

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"
)

// DiskStorage implements Storage using the local filesystem
type DiskStorage struct {
	basePath string
	baseURL  string
}

// NewDiskStorage creates a new disk storage instance
func NewDiskStorage(basePath, baseURL string) (*DiskStorage, error) {
	if err := os.MkdirAll(basePath, 0755); err != nil {
		return nil, fmt.Errorf("failed to create storage directory: %w", err)
	}
	return &DiskStorage{
		basePath: basePath,
		baseURL:  baseURL,
	}, nil
}

// Put stores a file on disk and returns its URL
func (s *DiskStorage) Put(ctx context.Context, relKey string, r io.Reader, contentType string) (string, error) {
	fullPath := filepath.Join(s.basePath, relKey)
	
	// Create directory if needed
	if err := os.MkdirAll(filepath.Dir(fullPath), 0755); err != nil {
		return "", fmt.Errorf("failed to create directory: %w", err)
	}
	
	// Create file
	file, err := os.Create(fullPath)
	if err != nil {
		return "", fmt.Errorf("failed to create file: %w", err)
	}
	defer file.Close()
	
	// Copy data
	if _, err := io.Copy(file, r); err != nil {
		return "", fmt.Errorf("failed to write file: %w", err)
	}
	
	// Return URL
	url := fmt.Sprintf("%s/media/%s", s.baseURL, relKey)
	return url, nil
}

// Open opens a file for reading
func (s *DiskStorage) Open(ctx context.Context, relKey string) (io.ReadCloser, error) {
	fullPath := filepath.Join(s.basePath, relKey)
	file, err := os.Open(fullPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}
	return file, nil
}

// Delete removes a file
func (s *DiskStorage) Delete(ctx context.Context, relKey string) error {
	fullPath := filepath.Join(s.basePath, relKey)
	if err := os.Remove(fullPath); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("failed to delete file: %w", err)
	}
	return nil
}

// GetSignedURL returns a URL (disk storage doesn't need signing, but we return a URL anyway)
func (s *DiskStorage) GetSignedURL(ctx context.Context, relKey string, expiration time.Duration) (string, error) {
	// For disk storage, we just return the regular URL
	// In production with S3, this would generate a pre-signed URL
	return fmt.Sprintf("%s/media/%s", s.baseURL, relKey), nil
}

