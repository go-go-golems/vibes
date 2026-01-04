package main

import (
	"fmt"
	"time"

	"github.com/go-git/go-git/v5/plumbing"
)

// ContextManager provides high-level API for attaching LLM context to commits
type ContextManager struct {
	repo        Repository
	notes       *NotesManager
	metaBranch  *MetaBranchManager
}

// NewContextManager creates a new context manager
func NewContextManager(repoPath string) (*ContextManager, error) {
	repo, err := NewGoGitRepository(repoPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open repository: %w", err)
	}
	
	notes := NewNotesManager(repo, "refs/notes/llm")
	metaBranch := NewMetaBranchManager(repo, "meta")
	
	return &ContextManager{
		repo:       repo,
		notes:      notes,
		metaBranch: metaBranch,
	}, nil
}

// AttachContext attaches LLM context to a commit
func (cm *ContextManager) AttachContext(commitHash plumbing.Hash, attachment *ContextAttachment) error {
	// Store artifacts in meta branch
	var artifacts []ArtifactReference
	
	// Store chat log if provided
	if attachment.ChatLog != "" {
		chatPath, err := cm.metaBranch.StoreArtifact(commitHash, "chat", "chat.md", []byte(attachment.ChatLog))
		if err != nil {
			return fmt.Errorf("failed to store chat log: %w", err)
		}
		
		artifacts = append(artifacts, ArtifactReference{
			Type:        "chat",
			Path:        chatPath,
			Description: "Chat transcript with LLM",
			Size:        int64(len(attachment.ChatLog)),
		})
	}
	
	// Store explanation if provided
	if attachment.Explanation != "" {
		explanationPath, err := cm.metaBranch.StoreArtifact(commitHash, "explanation", "explanation.json", []byte(attachment.Explanation))
		if err != nil {
			return fmt.Errorf("failed to store explanation: %w", err)
		}
		
		artifacts = append(artifacts, ArtifactReference{
			Type:        "explanation",
			Path:        explanationPath,
			Description: "LLM reasoning and explanation",
			Size:        int64(len(attachment.Explanation)),
		})
	}
	
	// Create manifest
	manifest := &LLMContextManifest{
		AgentID:   attachment.AgentID,
		PromptID:  attachment.PromptID,
		Timestamp: time.Now(),
		Summary:   attachment.Summary,
		Artifacts: artifacts,
		Metadata:  attachment.Metadata,
	}
	
	// Attach note
	if err := cm.notes.AttachNote(commitHash, manifest); err != nil {
		return fmt.Errorf("failed to attach note: %w", err)
	}
	
	return nil
}

// GetContext retrieves LLM context for a commit
func (cm *ContextManager) GetContext(commitHash plumbing.Hash) (*LLMContext, error) {
	// Get the manifest from notes
	manifest, err := cm.notes.GetNote(commitHash)
	if err != nil {
		return nil, fmt.Errorf("failed to get note: %w", err)
	}
	
	// Create context object
	context := &LLMContext{
		Manifest: *manifest,
		Artifacts: make(map[string][]byte),
	}
	
	// Load artifacts
	for _, artifact := range manifest.Artifacts {
		content, err := cm.metaBranch.GetArtifact(artifact.Path)
		if err != nil {
			return nil, fmt.Errorf("failed to get artifact %s: %w", artifact.Path, err)
		}
		context.Artifacts[artifact.Type] = content
	}
	
	return context, nil
}

// ListContexts returns all commits that have LLM context attached
func (cm *ContextManager) ListContexts() ([]CommitContext, error) {
	// Get all commits with notes
	commitHashes, err := cm.notes.ListNotes()
	if err != nil {
		return nil, fmt.Errorf("failed to list notes: %w", err)
	}
	
	var contexts []CommitContext
	for _, hash := range commitHashes {
		// Get commit info
		commitInfo, err := cm.repo.GetCommit(hash)
		if err != nil {
			continue // Skip if commit not found
		}
		
		// Get manifest
		manifest, err := cm.notes.GetNote(hash)
		if err != nil {
			continue // Skip if note not found
		}
		
		contexts = append(contexts, CommitContext{
			CommitHash: hash,
			CommitInfo: commitInfo,
			Manifest:   *manifest,
		})
	}
	
	return contexts, nil
}

// RemoveContext removes LLM context from a commit
func (cm *ContextManager) RemoveContext(commitHash plumbing.Hash) error {
	// Get the manifest to find artifacts to remove
	_, err := cm.notes.GetNote(commitHash)
	if err != nil {
		return fmt.Errorf("failed to get note: %w", err)
	}
	
	// Note: For simplicity, we're not implementing artifact removal from meta branch
	// In a production system, you'd want to implement garbage collection
	
	// Remove the note (this would require implementing note removal in NotesManager)
	// For now, we'll just return an error indicating this is not implemented
	return fmt.Errorf("context removal not yet implemented")
}

// GetHEADContext gets the context for the current HEAD commit
func (cm *ContextManager) GetHEADContext() (*LLMContext, error) {
	headHash, err := cm.repo.(*GoGitRepository).GetHEAD()
	if err != nil {
		return nil, fmt.Errorf("failed to get HEAD: %w", err)
	}
	
	return cm.GetContext(headHash)
}

// LLMContext represents the complete context for a commit
type LLMContext struct {
	Manifest  LLMContextManifest
	Artifacts map[string][]byte // type -> content
}

// CommitContext represents a commit with its LLM context manifest
type CommitContext struct {
	CommitHash plumbing.Hash
	CommitInfo CommitInfo
	Manifest   LLMContextManifest
}

// GetChatLog returns the chat log content if available
func (ctx *LLMContext) GetChatLog() string {
	if content, exists := ctx.Artifacts["chat"]; exists {
		return string(content)
	}
	return ""
}

// GetExplanation returns the explanation content if available
func (ctx *LLMContext) GetExplanation() string {
	if content, exists := ctx.Artifacts["explanation"]; exists {
		return string(content)
	}
	return ""
}

