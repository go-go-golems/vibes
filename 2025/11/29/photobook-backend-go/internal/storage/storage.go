package storage

import (
	"context"
	"io"
	"time"
)

// Storage defines the interface for blob storage operations
type Storage interface {
	// Put stores a file and returns its URL
	Put(ctx context.Context, relKey string, r io.Reader, contentType string) (url string, err error)
	
	// Open opens a file for reading
	Open(ctx context.Context, relKey string) (io.ReadCloser, error)
	
	// Delete removes a file
	Delete(ctx context.Context, relKey string) error
	
	// GetSignedURL returns a signed URL for temporary access (optional, may return error if not supported)
	GetSignedURL(ctx context.Context, relKey string, expiration time.Duration) (string, error)
}

