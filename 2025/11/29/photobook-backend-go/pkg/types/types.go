package types

import "time"

// User represents a user in the system
type User struct {
	ID          int64     `json:"id"`
	OpenID      string    `json:"openId"`
	Name        string    `json:"name"`
	Email       string    `json:"email"`
	LoginMethod string    `json:"loginMethod"`
	Role        string    `json:"role"` // "user" or "admin"
	CreatedAt   time.Time `json:"createdAt"`
	UpdatedAt   time.Time `json:"updatedAt"`
	LastSignedIn time.Time `json:"lastSignedIn"`
}

// Photo represents a photo uploaded by a user
type Photo struct {
	ID       int64     `json:"id"`
	UserID   int64     `json:"userId"`
	FileKey  string    `json:"fileKey"`
	URL      string    `json:"url"`
	Filename string    `json:"filename"`
	Position int       `json:"position"`
	CreatedAt time.Time `json:"createdAt"`
	UpdatedAt time.Time `json:"updatedAt"`
}

// PdfJob represents a PDF generation job
type PdfJob struct {
	ID        int64     `json:"id"`
	UserID    int64     `json:"userId"`
	Status    string    `json:"status"` // "pending", "processing", "completed", "failed"
	PhotoIDs  []int64   `json:"photoIds"` // JSON array of photo IDs
	FileKey   string    `json:"fileKey,omitempty"`
	URL       string    `json:"url,omitempty"`
	Error     string    `json:"error,omitempty"`
	CreatedAt time.Time `json:"createdAt"`
	UpdatedAt time.Time `json:"updatedAt"`
}

