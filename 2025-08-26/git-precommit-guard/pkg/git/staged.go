package git

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	gogit "github.com/go-git/go-git/v5"
	"github.com/rs/zerolog/log"
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
			path = parts[2]
		}
		stagedFiles = append(stagedFiles, StagedFile{Path: path, Status: status})
		log.Trace().Str("path", path).Str("status", status).Msg("staged file")
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
		// Skip deleted files
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
	root := strings.TrimSpace(string(output))
	log.Debug().Str("root", root).Msg("repository root")
	return root, nil
}

// HasStagedChanges checks if there are any staged changes
func HasStagedChanges() (bool, error) {
	cmd := exec.Command("git", "diff", "--cached", "--quiet")
	err := cmd.Run()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			if exitErr.ExitCode() == 1 {
				return true, nil // differences exist
			}
		}
		return false, fmt.Errorf("failed to check staged changes: %w", err)
	}
	return false, nil // no differences
}

// GetGitDir returns the absolute path to the repository's git directory.
// This supports standard repos and worktrees as resolved by git itself.
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
		// Return as-is if we can't resolve; likely fine
		return gitDir, nil
	}
	return filepath.Join(root, gitDir), nil
}

// findGitDirAndRoot locates the .git directory or file and returns (gitDir, repoRoot).
// If .git is a file, it parses the 'gitdir: <path>' pointer.
// Kept for compatibility; not used by the CLI-based functions above but useful in non-git environments.
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

