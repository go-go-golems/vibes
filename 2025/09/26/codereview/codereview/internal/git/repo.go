package git

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// Repository represents a Git repository
type Repository struct {
	Path string
}

// NewRepository creates a new repository instance
func NewRepository(path string) (*Repository, error) {
	if path == "" {
		var err error
		path, err = os.Getwd()
		if err != nil {
			return nil, fmt.Errorf("failed to get current directory: %w", err)
		}
	}

	// Check if it's a git repository
	if !IsGitRepository(path) {
		return nil, fmt.Errorf("not a git repository: %s", path)
	}

	return &Repository{Path: path}, nil
}

// IsGitRepository checks if the given path is a git repository
func IsGitRepository(path string) bool {
	gitDir := filepath.Join(path, ".git")
	if stat, err := os.Stat(gitDir); err == nil {
		return stat.IsDir()
	}

	// Check if we're in a subdirectory of a git repo
	cmd := exec.Command("git", "rev-parse", "--git-dir")
	cmd.Dir = path
	return cmd.Run() == nil
}

// GetCurrentBranch returns the current branch name
func (r *Repository) GetCurrentBranch() (string, error) {
	cmd := exec.Command("git", "rev-parse", "--abbrev-ref", "HEAD")
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get current branch: %w", err)
	}
	return strings.TrimSpace(string(output)), nil
}

// GetCurrentCommit returns the current commit hash
func (r *Repository) GetCurrentCommit() (string, error) {
	cmd := exec.Command("git", "rev-parse", "HEAD")
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get current commit: %w", err)
	}
	return strings.TrimSpace(string(output)), nil
}

// GetCommitMessage returns the commit message for a given commit
func (r *Repository) GetCommitMessage(commit string) (string, error) {
	cmd := exec.Command("git", "log", "-1", "--pretty=format:%s", commit)
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get commit message: %w", err)
	}
	return strings.TrimSpace(string(output)), nil
}

// GetBranches returns a list of all branches
func (r *Repository) GetBranches() ([]string, error) {
	cmd := exec.Command("git", "branch", "-a")
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to get branches: %w", err)
	}

	lines := strings.Split(string(output), "\n")
	var branches []string
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		// Remove current branch marker and remote prefixes
		line = strings.TrimPrefix(line, "* ")
		line = strings.TrimPrefix(line, "remotes/origin/")
		if !strings.Contains(line, "->") && line != "" {
			branches = append(branches, line)
		}
	}

	return branches, nil
}

// GetCommits returns a list of commits for a branch
func (r *Repository) GetCommits(branch string, limit int) ([]Commit, error) {
	args := []string{"log", "--pretty=format:%H|%s|%an|%ae|%ad", "--date=iso"}
	if limit > 0 {
		args = append(args, fmt.Sprintf("-%d", limit))
	}
	if branch != "" {
		args = append(args, branch)
	}

	cmd := exec.Command("git", args...)
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to get commits: %w", err)
	}

	lines := strings.Split(string(output), "\n")
	var commits []Commit
	for _, line := range lines {
		if line == "" {
			continue
		}
		parts := strings.Split(line, "|")
		if len(parts) >= 5 {
			commits = append(commits, Commit{
				Hash:      parts[0],
				Message:   parts[1],
				Author:    parts[2],
				Email:     parts[3],
				Date:      parts[4],
			})
		}
	}

	return commits, nil
}

// GetChangedFiles returns a list of files changed between two commits
func (r *Repository) GetChangedFiles(fromCommit, toCommit string) ([]string, error) {
	var cmd *exec.Cmd
	if toCommit == "" {
		// Compare with working directory
		cmd = exec.Command("git", "diff", "--name-only", fromCommit)
	} else {
		cmd = exec.Command("git", "diff", "--name-only", fromCommit+".."+toCommit)
	}
	
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to get changed files: %w", err)
	}

	lines := strings.Split(string(output), "\n")
	var files []string
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line != "" {
			files = append(files, line)
		}
	}

	return files, nil
}

// GetFileContent returns the content of a file at a specific commit
func (r *Repository) GetFileContent(commit, filePath string) (string, error) {
	cmd := exec.Command("git", "show", commit+":"+filePath)
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get file content: %w", err)
	}
	return string(output), nil
}

// Commit represents a git commit
type Commit struct {
	Hash    string `json:"hash"`
	Message string `json:"message"`
	Author  string `json:"author"`
	Email   string `json:"email"`
	Date    string `json:"date"`
}
