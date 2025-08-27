package git

import (
	"bufio"
	"fmt"
	"os/exec"
	"path/filepath"
	"strings"
)

// StagedFile represents a file staged for commit
type StagedFile struct {
	Path   string
	Status string // A=added, M=modified, D=deleted, R=renamed, C=copied
}

// GetStagedFiles returns a list of files staged for commit
func GetStagedFiles() ([]StagedFile, error) {
	cmd := exec.Command("git", "diff", "--cached", "--name-status")
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to get staged files: %w", err)
	}

	var stagedFiles []StagedFile
	scanner := bufio.NewScanner(strings.NewReader(string(output)))
	
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.Fields(line)
		if len(parts) < 2 {
			continue
		}

		status := parts[0]
		path := parts[1]

		// Handle renamed files (R100 oldname newname)
		if strings.HasPrefix(status, "R") && len(parts) >= 3 {
			path = parts[2] // Use the new name for renamed files
		}

		stagedFiles = append(stagedFiles, StagedFile{
			Path:   path,
			Status: status,
		})
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading git output: %w", err)
	}

	return stagedFiles, nil
}

// GetStagedFilePaths returns just the paths of staged files (excluding deleted files)
func GetStagedFilePaths() ([]string, error) {
	stagedFiles, err := GetStagedFiles()
	if err != nil {
		return nil, err
	}

	var paths []string
	for _, file := range stagedFiles {
		// Skip deleted files as they don't exist to check
		if !strings.HasPrefix(file.Status, "D") {
			paths = append(paths, file.Path)
		}
	}

	return paths, nil
}

// IsGitRepository checks if the current directory is a git repository
func IsGitRepository() bool {
	cmd := exec.Command("git", "rev-parse", "--git-dir")
	err := cmd.Run()
	return err == nil
}

// GetRepositoryRoot returns the root directory of the git repository
func GetRepositoryRoot() (string, error) {
	cmd := exec.Command("git", "rev-parse", "--show-toplevel")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get repository root: %w", err)
	}

	return strings.TrimSpace(string(output)), nil
}

// HasStagedChanges checks if there are any staged changes
func HasStagedChanges() (bool, error) {
	cmd := exec.Command("git", "diff", "--cached", "--quiet")
	err := cmd.Run()
	
	// git diff --quiet returns 0 if no differences, 1 if differences exist
	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			if exitError.ExitCode() == 1 {
				return true, nil // Differences exist
			}
		}
		return false, fmt.Errorf("failed to check staged changes: %w", err)
	}
	
	return false, nil // No differences
}

// GetGitDir returns the absolute path to the repository's git directory.
// This supports standard repos and worktrees (where .git is a file pointing to the gitdir).
func GetGitDir() (string, error) {
	cmd := exec.Command("git", "rev-parse", "--git-dir")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get git dir: %w", err)
	}
	gitDir := strings.TrimSpace(string(output))
	if filepath.IsAbs(gitDir) {
		return gitDir, nil
	}
	root, err := GetRepositoryRoot()
	if err != nil {
		return gitDir, nil // return as-is if we can't resolve; likely fine
	}
	return filepath.Join(root, gitDir), nil
}

