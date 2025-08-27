package git

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	gogit "github.com/go-git/go-git/v5"
)

// StagedFile represents a file staged for commit
type StagedFile struct {
	Path   string
	Status string // A=added, M=modified, D=deleted, R=renamed, C=copied
}

// GetStagedFiles returns a list of files staged for commit
func GetStagedFiles() ([]StagedFile, error) {
	root, err := GetRepositoryRoot()
	if err != nil {
		return nil, fmt.Errorf("failed to find repository root: %w", err)
	}

	repo, err := gogit.PlainOpenWithOptions(root, &gogit.PlainOpenOptions{DetectDotGit: true})
	if err != nil {
		return nil, fmt.Errorf("failed to open git repo: %w", err)
	}
	wt, err := repo.Worktree()
	if err != nil {
		return nil, fmt.Errorf("failed to get worktree: %w", err)
	}
	status, err := wt.Status()
	if err != nil {
		return nil, fmt.Errorf("failed to get status: %w", err)
	}

	var stagedFiles []StagedFile
	for path, s := range status {
		if s.Staging == gogit.Unmodified || s.Staging == gogit.Untracked {
			continue
		}
		stagedFiles = append(stagedFiles, StagedFile{
			Path:   path,
			Status: statusCodeToLetter(s.Staging),
		})
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
	_, _, err := findGitDirAndRoot("")
	return err == nil
}

// GetRepositoryRoot returns the root directory of the git repository
func GetRepositoryRoot() (string, error) {
	_, root, err := findGitDirAndRoot("")
	if err != nil {
		return "", err
	}
	return root, nil
}

// HasStagedChanges checks if there are any staged changes
func HasStagedChanges() (bool, error) {
	root, err := GetRepositoryRoot()
	if err != nil {
		return false, fmt.Errorf("failed to find repository root: %w", err)
	}
	repo, err := gogit.PlainOpenWithOptions(root, &gogit.PlainOpenOptions{DetectDotGit: true})
	if err != nil {
		return false, fmt.Errorf("failed to open git repo: %w", err)
	}
	wt, err := repo.Worktree()
	if err != nil {
		return false, fmt.Errorf("failed to get worktree: %w", err)
	}
	status, err := wt.Status()
	if err != nil {
		return false, fmt.Errorf("failed to get status: %w", err)
	}
	for _, s := range status {
		if s.Staging != gogit.Unmodified && s.Staging != gogit.Untracked {
			return true, nil
		}
	}
	return false, nil
}

// GetGitDir returns the absolute path to the repository's git directory.
// This supports standard repos and worktrees (where .git is a file pointing to the gitdir).
func GetGitDir() (string, error) {
	gitDir, _, err := findGitDirAndRoot("")
	return gitDir, err
}

// findGitDirAndRoot locates the .git directory or file and returns (gitDir, repoRoot).
// If .git is a file, it parses the 'gitdir: <path>' pointer.
func findGitDirAndRoot(start string) (string, string, error) {
	startDir := start
	if startDir == "" {
		cwd, err := os.Getwd()
		if err != nil {
			return "", "", fmt.Errorf("failed to get working directory: %w", err)
		}
		startDir = cwd
	}

	d := startDir
	for {
		candidate := filepath.Join(d, ".git")
		fi, err := os.Stat(candidate)
		if err == nil {
			if fi.IsDir() {
				return candidate, d, nil
			}
			// .git is a file containing 'gitdir: <path>'
			data, err := os.ReadFile(candidate)
			if err != nil {
				return "", "", fmt.Errorf("failed to read .git file: %w", err)
			}
			line := strings.TrimSpace(string(data))
			// handle possible multiple lines, pick the one starting with gitdir:
			scanner := bufio.NewScanner(strings.NewReader(line))
			for scanner.Scan() {
				l := strings.TrimSpace(scanner.Text())
				if strings.HasPrefix(strings.ToLower(l), "gitdir:") {
					p := strings.TrimSpace(strings.TrimPrefix(l, "gitdir:"))
					if !filepath.IsAbs(p) {
						p = filepath.Clean(filepath.Join(d, p))
					}
					return p, d, nil
				}
			}
			if err := scanner.Err(); err != nil {
				return "", "", fmt.Errorf("failed to parse .git file: %w", err)
			}
		}

		parent := filepath.Dir(d)
		if parent == d {
			break
		}
		d = parent
	}
	return "", "", fmt.Errorf("not a git repository (or any of the parent directories): .git")
}

func statusCodeToLetter(code gogit.StatusCode) string {
	switch code {
	case gogit.Added:
		return "A"
	case gogit.Modified:
		return "M"
	case gogit.Deleted:
		return "D"
	case gogit.Renamed:
		return "R"
	case gogit.Untracked:
		return "?"
	case gogit.UpdatedButUnmerged:
		return "U"
	default:
		return "?"
	}
}

