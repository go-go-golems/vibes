package main

import (
	"time"

	"github.com/go-git/go-git/v5/plumbing"
)

// LLMContextManifest represents the JSON manifest stored in Git Notes
type LLMContextManifest struct {
	AgentID     string                 `json:"agent_id"`
	PromptID    string                 `json:"prompt_id"`
	Timestamp   time.Time              `json:"timestamp"`
	Summary     string                 `json:"summary"`
	Artifacts   []ArtifactReference    `json:"artifacts"`
	Metadata    map[string]interface{} `json:"metadata,omitempty"`
}

// ArtifactReference points to files stored in the meta branch
type ArtifactReference struct {
	Type        string `json:"type"`        // e.g., "chat", "explanation", "prompt"
	Path        string `json:"path"`        // relative path in meta branch
	Description string `json:"description"` // human-readable description
	Size        int64  `json:"size"`        // file size in bytes
}

// ContextAttachment represents the data to be attached to a commit
type ContextAttachment struct {
	AgentID     string
	PromptID    string
	Summary     string
	ChatLog     string
	Explanation string
	Metadata    map[string]interface{}
}

// NotesManager handles Git Notes operations
type NotesManager struct {
	repo     Repository
	notesRef string // e.g., "refs/notes/llm"
}

// MetaBranchManager handles meta branch operations
type MetaBranchManager struct {
	repo       Repository
	branchName string // e.g., "meta"
}

// Repository interface abstracts git operations
type Repository interface {
	// Object operations
	CreateBlob(content []byte) (plumbing.Hash, error)
	ReadBlob(hash plumbing.Hash) ([]byte, error)
	
	// Tree operations
	CreateTree(entries []TreeEntry) (plumbing.Hash, error)
	ReadTree(hash plumbing.Hash) ([]TreeEntry, error)
	
	// Reference operations
	SetReference(name plumbing.ReferenceName, hash plumbing.Hash) error
	GetReference(name plumbing.ReferenceName) (plumbing.Hash, error)
	
	// Commit operations
	GetCommit(hash plumbing.Hash) (CommitInfo, error)
}

// TreeEntry represents an entry in a Git tree
type TreeEntry struct {
	Name string
	Hash plumbing.Hash
	Mode int // file mode
}

// CommitInfo contains basic commit information
type CommitInfo struct {
	Hash    plumbing.Hash
	Message string
	Author  string
	Date    time.Time
}

