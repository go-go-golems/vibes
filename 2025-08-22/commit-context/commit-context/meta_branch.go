package main

import (
	"fmt"
	"path"
	"strings"

	"github.com/go-git/go-git/v5/plumbing"
)

// NewMetaBranchManager creates a new meta branch manager
func NewMetaBranchManager(repo Repository, branchName string) *MetaBranchManager {
	if branchName == "" {
		branchName = "meta"
	}
	
	return &MetaBranchManager{
		repo:       repo,
		branchName: branchName,
	}
}

// StoreArtifact stores an artifact in the meta branch and returns its path
func (mb *MetaBranchManager) StoreArtifact(commitHash plumbing.Hash, artifactType, filename string, content []byte) (string, error) {
	// Create the artifact path: attachments/<commit-sha>/<filename>
	artifactPath := path.Join("attachments", commitHash.String(), filename)
	
	// Create blob for the artifact content
	blobHash, err := mb.repo.CreateBlob(content)
	if err != nil {
		return "", fmt.Errorf("failed to create artifact blob: %w", err)
	}
	
	// Get or create the meta branch tree
	metaTreeHash, err := mb.getOrCreateMetaTree()
	if err != nil {
		return "", fmt.Errorf("failed to get meta tree: %w", err)
	}
	
	// Update the tree with the new artifact
	newTreeHash, err := mb.addFileToTree(metaTreeHash, artifactPath, blobHash)
	if err != nil {
		return "", fmt.Errorf("failed to add artifact to tree: %w", err)
	}
	
	// Update the meta branch reference
	metaBranchRef := plumbing.ReferenceName(fmt.Sprintf("refs/heads/%s", mb.branchName))
	err = mb.repo.SetReference(metaBranchRef, newTreeHash)
	if err != nil {
		return "", fmt.Errorf("failed to update meta branch: %w", err)
	}
	
	return artifactPath, nil
}

// GetArtifact retrieves an artifact from the meta branch
func (mb *MetaBranchManager) GetArtifact(artifactPath string) ([]byte, error) {
	// Get the meta branch tree
	metaTreeHash, err := mb.getMetaTreeHash()
	if err != nil {
		return nil, fmt.Errorf("failed to get meta tree: %w", err)
	}
	
	if metaTreeHash == plumbing.ZeroHash {
		return nil, fmt.Errorf("meta branch not found")
	}
	
	// Find the artifact in the tree
	blobHash, err := mb.findFileInTree(metaTreeHash, artifactPath)
	if err != nil {
		return nil, fmt.Errorf("failed to find artifact: %w", err)
	}
	
	// Read the blob content
	content, err := mb.repo.ReadBlob(blobHash)
	if err != nil {
		return nil, fmt.Errorf("failed to read artifact blob: %w", err)
	}
	
	return content, nil
}

// ListArtifacts lists all artifacts for a specific commit
func (mb *MetaBranchManager) ListArtifacts(commitHash plumbing.Hash) ([]string, error) {
	// Get the meta branch tree
	metaTreeHash, err := mb.getMetaTreeHash()
	if err != nil {
		return nil, fmt.Errorf("failed to get meta tree: %w", err)
	}
	
	if metaTreeHash == plumbing.ZeroHash {
		return []string{}, nil
	}
	
	// Look for the attachments/<commit-sha>/ directory
	attachmentsPath := path.Join("attachments", commitHash.String())
	return mb.listFilesInDirectory(metaTreeHash, attachmentsPath)
}

// getOrCreateMetaTree gets the existing meta tree or creates a new one
func (mb *MetaBranchManager) getOrCreateMetaTree() (plumbing.Hash, error) {
	hash, err := mb.getMetaTreeHash()
	if err != nil {
		// If branch doesn't exist, return zero hash (will create new tree)
		return plumbing.ZeroHash, nil
	}
	return hash, nil
}

// getMetaTreeHash gets the hash of the meta branch tree
func (mb *MetaBranchManager) getMetaTreeHash() (plumbing.Hash, error) {
	metaBranchRef := plumbing.ReferenceName(fmt.Sprintf("refs/heads/%s", mb.branchName))
	return mb.repo.GetReference(metaBranchRef)
}

// addFileToTree adds a file to a tree at the specified path
func (mb *MetaBranchManager) addFileToTree(treeHash plumbing.Hash, filePath string, blobHash plumbing.Hash) (plumbing.Hash, error) {
	pathParts := strings.Split(filePath, "/")
	return mb.addFileToTreeRecursive(treeHash, pathParts, blobHash)
}

// addFileToTreeRecursive recursively adds a file to a tree structure
func (mb *MetaBranchManager) addFileToTreeRecursive(treeHash plumbing.Hash, pathParts []string, blobHash plumbing.Hash) (plumbing.Hash, error) {
	var entries []TreeEntry
	
	// Read existing tree entries if tree exists
	if treeHash != plumbing.ZeroHash {
		var err error
		entries, err = mb.repo.ReadTree(treeHash)
		if err != nil {
			return plumbing.ZeroHash, fmt.Errorf("failed to read tree: %w", err)
		}
	}
	
	if len(pathParts) == 1 {
		// This is the final file, add or update it
		filename := pathParts[0]
		found := false
		for i, entry := range entries {
			if entry.Name == filename {
				entries[i].Hash = blobHash
				found = true
				break
			}
		}
		
		if !found {
			entries = append(entries, TreeEntry{
				Name: filename,
				Hash: blobHash,
				Mode: 0100644, // regular file
			})
		}
	} else {
		// This is a directory, recurse
		dirName := pathParts[0]
		remainingPath := pathParts[1:]
		
		// Find existing directory entry
		var dirTreeHash plumbing.Hash
		found := false
		for i, entry := range entries {
			if entry.Name == dirName {
				dirTreeHash = entry.Hash
				found = true
				
				// Recursively add to subdirectory
				newDirTreeHash, err := mb.addFileToTreeRecursive(dirTreeHash, remainingPath, blobHash)
				if err != nil {
					return plumbing.ZeroHash, err
				}
				entries[i].Hash = newDirTreeHash
				break
			}
		}
		
		if !found {
			// Create new directory
			newDirTreeHash, err := mb.addFileToTreeRecursive(plumbing.ZeroHash, remainingPath, blobHash)
			if err != nil {
				return plumbing.ZeroHash, err
			}
			
			entries = append(entries, TreeEntry{
				Name: dirName,
				Hash: newDirTreeHash,
				Mode: 0040000, // directory
			})
		}
	}
	
	// Create new tree with updated entries
	return mb.repo.CreateTree(entries)
}

// findFileInTree finds a file in a tree at the specified path
func (mb *MetaBranchManager) findFileInTree(treeHash plumbing.Hash, filePath string) (plumbing.Hash, error) {
	pathParts := strings.Split(filePath, "/")
	return mb.findFileInTreeRecursive(treeHash, pathParts)
}

// findFileInTreeRecursive recursively finds a file in a tree structure
func (mb *MetaBranchManager) findFileInTreeRecursive(treeHash plumbing.Hash, pathParts []string) (plumbing.Hash, error) {
	if treeHash == plumbing.ZeroHash {
		return plumbing.ZeroHash, fmt.Errorf("path not found")
	}
	
	entries, err := mb.repo.ReadTree(treeHash)
	if err != nil {
		return plumbing.ZeroHash, fmt.Errorf("failed to read tree: %w", err)
	}
	
	if len(pathParts) == 1 {
		// This is the final file
		filename := pathParts[0]
		for _, entry := range entries {
			if entry.Name == filename {
				return entry.Hash, nil
			}
		}
		return plumbing.ZeroHash, fmt.Errorf("file not found: %s", filename)
	}
	
	// This is a directory, recurse
	dirName := pathParts[0]
	remainingPath := pathParts[1:]
	
	for _, entry := range entries {
		if entry.Name == dirName {
			return mb.findFileInTreeRecursive(entry.Hash, remainingPath)
		}
	}
	
	return plumbing.ZeroHash, fmt.Errorf("directory not found: %s", dirName)
}

// listFilesInDirectory lists all files in a directory
func (mb *MetaBranchManager) listFilesInDirectory(treeHash plumbing.Hash, dirPath string) ([]string, error) {
	if dirPath == "" {
		return mb.listFilesInDirectoryRecursive(treeHash, "")
	}
	
	pathParts := strings.Split(dirPath, "/")
	dirTreeHash, err := mb.findFileInTreeRecursive(treeHash, pathParts)
	if err != nil {
		return []string{}, nil // Directory doesn't exist, return empty list
	}
	
	return mb.listFilesInDirectoryRecursive(dirTreeHash, dirPath)
}

// listFilesInDirectoryRecursive recursively lists files in a directory
func (mb *MetaBranchManager) listFilesInDirectoryRecursive(treeHash plumbing.Hash, basePath string) ([]string, error) {
	if treeHash == plumbing.ZeroHash {
		return []string{}, nil
	}
	
	entries, err := mb.repo.ReadTree(treeHash)
	if err != nil {
		return nil, fmt.Errorf("failed to read tree: %w", err)
	}
	
	var files []string
	for _, entry := range entries {
		entryPath := entry.Name
		if basePath != "" {
			entryPath = path.Join(basePath, entry.Name)
		}
		
		if entry.Mode == 0040000 { // directory
			subFiles, err := mb.listFilesInDirectoryRecursive(entry.Hash, entryPath)
			if err != nil {
				return nil, err
			}
			files = append(files, subFiles...)
		} else {
			files = append(files, entryPath)
		}
	}
	
	return files, nil
}

