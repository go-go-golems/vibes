package main

import (
	"encoding/json"
	"fmt"

	"github.com/go-git/go-git/v5/plumbing"
)

// NewNotesManager creates a new notes manager
func NewNotesManager(repo Repository, notesRef string) *NotesManager {
	if notesRef == "" {
		notesRef = "refs/notes/llm"
	}
	
	return &NotesManager{
		repo:     repo,
		notesRef: notesRef,
	}
}

// AttachNote attaches a manifest to a commit as a Git Note
func (nm *NotesManager) AttachNote(commitHash plumbing.Hash, manifest *LLMContextManifest) error {
	// Serialize manifest to JSON
	manifestJSON, err := json.MarshalIndent(manifest, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal manifest: %w", err)
	}
	
	// Create blob for the manifest
	manifestBlobHash, err := nm.repo.CreateBlob(manifestJSON)
	if err != nil {
		return fmt.Errorf("failed to create manifest blob: %w", err)
	}
	
	// Get or create the notes tree
	notesTreeHash, err := nm.getOrCreateNotesTree()
	if err != nil {
		return fmt.Errorf("failed to get notes tree: %w", err)
	}
	
	// Read existing tree entries
	var entries []TreeEntry
	if notesTreeHash != plumbing.ZeroHash {
		entries, err = nm.repo.ReadTree(notesTreeHash)
		if err != nil {
			return fmt.Errorf("failed to read notes tree: %w", err)
		}
	}
	
	// Add or update the entry for this commit
	commitHashStr := commitHash.String()
	found := false
	for i, entry := range entries {
		if entry.Name == commitHashStr {
			entries[i].Hash = manifestBlobHash
			found = true
			break
		}
	}
	
	if !found {
		entries = append(entries, TreeEntry{
			Name: commitHashStr,
			Hash: manifestBlobHash,
			Mode: 0100644, // regular file
		})
	}
	
	// Create new tree with updated entries
	newTreeHash, err := nm.repo.CreateTree(entries)
	if err != nil {
		return fmt.Errorf("failed to create notes tree: %w", err)
	}
	
	// Update the notes reference
	err = nm.repo.SetReference(plumbing.ReferenceName(nm.notesRef), newTreeHash)
	if err != nil {
		return fmt.Errorf("failed to update notes reference: %w", err)
	}
	
	return nil
}

// GetNote retrieves the manifest for a commit
func (nm *NotesManager) GetNote(commitHash plumbing.Hash) (*LLMContextManifest, error) {
	// Get the notes tree
	notesTreeHash, err := nm.getNotesTreeHash()
	if err != nil {
		return nil, fmt.Errorf("failed to get notes tree: %w", err)
	}
	
	if notesTreeHash == plumbing.ZeroHash {
		return nil, fmt.Errorf("no notes found")
	}
	
	// Read tree entries
	entries, err := nm.repo.ReadTree(notesTreeHash)
	if err != nil {
		return nil, fmt.Errorf("failed to read notes tree: %w", err)
	}
	
	// Find the entry for this commit
	commitHashStr := commitHash.String()
	var manifestBlobHash plumbing.Hash
	for _, entry := range entries {
		if entry.Name == commitHashStr {
			manifestBlobHash = entry.Hash
			break
		}
	}
	
	if manifestBlobHash == plumbing.ZeroHash {
		return nil, fmt.Errorf("no note found for commit %s", commitHashStr)
	}
	
	// Read the manifest blob
	manifestJSON, err := nm.repo.ReadBlob(manifestBlobHash)
	if err != nil {
		return nil, fmt.Errorf("failed to read manifest blob: %w", err)
	}
	
	// Deserialize the manifest
	var manifest LLMContextManifest
	if err := json.Unmarshal(manifestJSON, &manifest); err != nil {
		return nil, fmt.Errorf("failed to unmarshal manifest: %w", err)
	}
	
	return &manifest, nil
}

// ListNotes returns all commit hashes that have notes
func (nm *NotesManager) ListNotes() ([]plumbing.Hash, error) {
	// Get the notes tree
	notesTreeHash, err := nm.getNotesTreeHash()
	if err != nil {
		return nil, fmt.Errorf("failed to get notes tree: %w", err)
	}
	
	if notesTreeHash == plumbing.ZeroHash {
		return []plumbing.Hash{}, nil
	}
	
	// Read tree entries
	entries, err := nm.repo.ReadTree(notesTreeHash)
	if err != nil {
		return nil, fmt.Errorf("failed to read notes tree: %w", err)
	}
	
	var commitHashes []plumbing.Hash
	for _, entry := range entries {
		hash := plumbing.NewHash(entry.Name)
		commitHashes = append(commitHashes, hash)
	}
	
	return commitHashes, nil
}

// getOrCreateNotesTree gets the existing notes tree or creates a new one
func (nm *NotesManager) getOrCreateNotesTree() (plumbing.Hash, error) {
	hash, err := nm.getNotesTreeHash()
	if err != nil {
		// If reference doesn't exist, return zero hash (will create new tree)
		return plumbing.ZeroHash, nil
	}
	return hash, nil
}

// getNotesTreeHash gets the hash of the notes tree
func (nm *NotesManager) getNotesTreeHash() (plumbing.Hash, error) {
	return nm.repo.GetReference(plumbing.ReferenceName(nm.notesRef))
}

