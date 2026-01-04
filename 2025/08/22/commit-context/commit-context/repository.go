package main

import (
	"fmt"

	"github.com/go-git/go-git/v5"
	"github.com/go-git/go-git/v5/plumbing"
	"github.com/go-git/go-git/v5/plumbing/filemode"
	"github.com/go-git/go-git/v5/plumbing/object"
)

// GoGitRepository implements Repository interface using go-git
type GoGitRepository struct {
	repo *git.Repository
}

// NewGoGitRepository creates a new repository wrapper
func NewGoGitRepository(repoPath string) (*GoGitRepository, error) {
	repo, err := git.PlainOpen(repoPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open repository: %w", err)
	}
	
	return &GoGitRepository{repo: repo}, nil
}

// CreateBlob creates a blob object from content
func (r *GoGitRepository) CreateBlob(content []byte) (plumbing.Hash, error) {
	obj := r.repo.Storer.NewEncodedObject()
	obj.SetType(plumbing.BlobObject)
	obj.SetSize(int64(len(content)))
	
	writer, err := obj.Writer()
	if err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to create blob writer: %w", err)
	}
	defer writer.Close()
	
	if _, err := writer.Write(content); err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to write blob content: %w", err)
	}
	
	hash, err := r.repo.Storer.SetEncodedObject(obj)
	if err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to store blob: %w", err)
	}
	
	return hash, nil
}

// ReadBlob reads a blob object by hash
func (r *GoGitRepository) ReadBlob(hash plumbing.Hash) ([]byte, error) {
	obj, err := r.repo.Storer.EncodedObject(plumbing.BlobObject, hash)
	if err != nil {
		return nil, fmt.Errorf("failed to get blob object: %w", err)
	}
	
	reader, err := obj.Reader()
	if err != nil {
		return nil, fmt.Errorf("failed to create blob reader: %w", err)
	}
	defer reader.Close()
	
	content := make([]byte, obj.Size())
	if _, err := reader.Read(content); err != nil {
		return nil, fmt.Errorf("failed to read blob content: %w", err)
	}
	
	return content, nil
}

// CreateTree creates a tree object from entries
func (r *GoGitRepository) CreateTree(entries []TreeEntry) (plumbing.Hash, error) {
	tree := &object.Tree{}
	
	for _, entry := range entries {
		tree.Entries = append(tree.Entries, object.TreeEntry{
			Name: entry.Name,
			Hash: entry.Hash,
			Mode: filemode.FileMode(entry.Mode),
		})
	}
	
	obj := r.repo.Storer.NewEncodedObject()
	if err := tree.Encode(obj); err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to encode tree: %w", err)
	}
	
	hash, err := r.repo.Storer.SetEncodedObject(obj)
	if err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to store tree: %w", err)
	}
	
	return hash, nil
}

// ReadTree reads a tree object by hash
func (r *GoGitRepository) ReadTree(hash plumbing.Hash) ([]TreeEntry, error) {
	tree, err := object.GetTree(r.repo.Storer, hash)
	if err != nil {
		return nil, fmt.Errorf("failed to get tree object: %w", err)
	}
	
	var entries []TreeEntry
	for _, entry := range tree.Entries {
		entries = append(entries, TreeEntry{
			Name: entry.Name,
			Hash: entry.Hash,
			Mode: int(entry.Mode),
		})
	}
	
	return entries, nil
}

// SetReference sets a reference to point to a hash
func (r *GoGitRepository) SetReference(name plumbing.ReferenceName, hash plumbing.Hash) error {
	ref := plumbing.NewHashReference(name, hash)
	return r.repo.Storer.SetReference(ref)
}

// GetReference gets the hash that a reference points to
func (r *GoGitRepository) GetReference(name plumbing.ReferenceName) (plumbing.Hash, error) {
	ref, err := r.repo.Storer.Reference(name)
	if err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to get reference: %w", err)
	}
	
	return ref.Hash(), nil
}

// GetCommit gets commit information by hash
func (r *GoGitRepository) GetCommit(hash plumbing.Hash) (CommitInfo, error) {
	commit, err := object.GetCommit(r.repo.Storer, hash)
	if err != nil {
		return CommitInfo{}, fmt.Errorf("failed to get commit: %w", err)
	}
	
	return CommitInfo{
		Hash:    commit.Hash,
		Message: commit.Message,
		Author:  commit.Author.Name,
		Date:    commit.Author.When,
	}, nil
}

// GetHEAD returns the current HEAD commit hash
func (r *GoGitRepository) GetHEAD() (plumbing.Hash, error) {
	head, err := r.repo.Head()
	if err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to get HEAD: %w", err)
	}
	
	return head.Hash(), nil
}

