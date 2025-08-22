package git

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/go-git/go-git/v5"
	"github.com/go-git/go-git/v5/plumbing"
	"github.com/go-git/go-git/v5/plumbing/object"
	"github.com/rs/zerolog/log"
)

// Repository wraps go-git repository with additional functionality
type Repository struct {
	repo *git.Repository
	path string
}

// Path returns the filesystem path of the repository root
func (r *Repository) Path() string {
	return r.path
}

// GetCommitByHash returns the commit object for the given hash string
func (r *Repository) GetCommitByHash(hash string) (*object.Commit, error) {
	h := plumbing.NewHash(hash)
	c, err := r.repo.CommitObject(h)
	if err != nil {
		return nil, fmt.Errorf("failed to get commit %s: %w", hash, err)
	}
	return c, nil
}

// OpenRepository opens a git repository at the given path
func OpenRepository(path string) (*Repository, error) {
	absPath, err := filepath.Abs(path)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}

	repo, err := git.PlainOpen(absPath)
	if err == nil {
		log.Debug().Str("path", absPath).Msg("opened git repository")
		return &Repository{repo: repo, path: absPath}, nil
	}

	// If not found, walk up to find .git directory or bare repo
	rootPath, findErr := findGitRoot(absPath)
	if findErr != nil {
		return nil, fmt.Errorf("failed to open git repository at %s: %w", absPath, err)
	}

	repo, err = git.PlainOpen(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to open git repository at %s: %w", rootPath, err)
	}
	log.Debug().Str("requested", absPath).Str("root", rootPath).Msg("resolved repository root")
	return &Repository{repo: repo, path: rootPath}, nil
}

// findGitRoot walks parent directories to locate a git repository root
func findGitRoot(start string) (string, error) {
	current := start
	for {
		// Try opening as a (possibly bare) repo
		if _, err := git.PlainOpen(current); err == nil {
			return current, nil
		}
		// Check for .git directory
		gitDir := filepath.Join(current, ".git")
		if info, err := os.Stat(gitDir); err == nil && info.IsDir() {
			return current, nil
		}
		parent := filepath.Dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return "", fmt.Errorf(".git directory not found from %s upwards", start)
}

// GetCommitsBetween returns commits between two branches/commits
func (r *Repository) GetCommitsBetween(base, head string) ([]*object.Commit, error) {
	log.Debug().Str("base", base).Str("head", head).Msg("resolving revisions for range analysis")
	baseRef, err := r.repo.ResolveRevision(plumbing.Revision(base))
	if err != nil {
		return nil, fmt.Errorf("failed to resolve base revision %s: %w", base, err)
	}

	headRef, err := r.repo.ResolveRevision(plumbing.Revision(head))
	if err != nil {
		return nil, fmt.Errorf("failed to resolve head revision %s: %w", head, err)
	}
	log.Trace().Str("base_hash", baseRef.String()).Str("head_hash", headRef.String()).Msg("resolved revisions")

	// Get commit objects
	baseCommit, err := r.repo.CommitObject(*baseRef)
	if err != nil {
		return nil, fmt.Errorf("failed to get base commit: %w", err)
	}

	headCommit, err := r.repo.CommitObject(*headRef)
	if err != nil {
		return nil, fmt.Errorf("failed to get head commit: %w", err)
	}

	// Get commits reachable from head but not from base
	commits, err := r.getCommitsNotInBase(headCommit, baseCommit)
	if err != nil {
		return nil, fmt.Errorf("failed to get commits between branches: %w", err)
	}
	log.Debug().Int("commits", len(commits)).Msg("computed head-minus-base commit set")

	return commits, nil
}

// GetCommitsFromMerge extracts commits from a merge commit (excluding the merge commit itself)
func (r *Repository) GetCommitsFromMerge(mergeCommitHash string) ([]*object.Commit, error) {
	hash := plumbing.NewHash(mergeCommitHash)
	mergeCommit, err := r.repo.CommitObject(hash)
	if err != nil {
		return nil, fmt.Errorf("failed to get merge commit: %w", err)
	}

	// Check if it's actually a merge commit
	if mergeCommit.NumParents() < 2 {
		return nil, fmt.Errorf("commit %s is not a merge commit", mergeCommitHash)
	}

	// Get the first parent (usually the main branch)
	parents := mergeCommit.Parents()
	firstParent, err := parents.Next()
	if err != nil {
		return nil, fmt.Errorf("failed to get first parent: %w", err)
	}

	// Get the second parent (the feature branch)
	secondParent, err := parents.Next()
	if err != nil {
		return nil, fmt.Errorf("failed to get second parent: %w", err)
	}
	log.Debug().Str("merge", mergeCommitHash).Str("p1", firstParent.Hash.String()).Str("p2", secondParent.Hash.String()).Msg("extracting feature commits from merge")

	// Get commits from the feature branch
	commits, err := r.getCommitsNotInBase(secondParent, firstParent)
	if err != nil {
		return nil, fmt.Errorf("failed to get commits from merge: %w", err)
	}
	log.Debug().Int("commits", len(commits)).Msg("computed feature-only commit set from merge")

	return commits, nil
}

// getCommitsNotInBase returns commits reachable from head but not from base
func (r *Repository) getCommitsNotInBase(head, base *object.Commit) ([]*object.Commit, error) {
	// Get all commits reachable from base
	baseCommits := make(map[plumbing.Hash]bool)
	baseIter, err := r.repo.Log(&git.LogOptions{From: base.Hash})
	if err != nil {
		return nil, fmt.Errorf("failed to get base commits: %w", err)
	}
	defer baseIter.Close()

	baseCount := 0
	err = baseIter.ForEach(func(c *object.Commit) error {
		baseCommits[c.Hash] = true
		baseCount++
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("failed to iterate base commits: %w", err)
	}
	log.Trace().Int("base_reachable", baseCount).Msg("collected base-reachable commits")

	// Get commits reachable from head that are not in base
	var commits []*object.Commit
	headIter, err := r.repo.Log(&git.LogOptions{From: head.Hash})
	if err != nil {
		return nil, fmt.Errorf("failed to get head commits: %w", err)
	}
	defer headIter.Close()

	headVisited := 0
	err = headIter.ForEach(func(c *object.Commit) error {
		headVisited++
		if !baseCommits[c.Hash] {
			// Filter out merge commits from main branch
			if !r.isMergeFromMain(c) {
				commits = append(commits, c)
			}
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("failed to iterate head commits: %w", err)
	}

	// Reverse to get chronological order
	for i, j := 0, len(commits)-1; i < j; i, j = i+1, j-1 {
		commits[i], commits[j] = commits[j], commits[i]
	}

	log.Trace().Int("head_visited", headVisited).Int("selected", len(commits)).Msg("filtered head commits not in base")
	return commits, nil
}

// isMergeFromMain checks if a commit is a merge commit from main/master
func (r *Repository) isMergeFromMain(commit *object.Commit) bool {
	if commit.NumParents() < 2 {
		return false
	}

	// Check if the commit message indicates a merge from main
	message := strings.ToLower(commit.Message)
	return strings.Contains(message, "merge branch 'main'") ||
		strings.Contains(message, "merge branch 'master'") ||
		strings.Contains(message, "merge remote-tracking branch") ||
		strings.Contains(message, "merge pull request")
}

// GetBranches returns all branch names in the repository
func (r *Repository) GetBranches() ([]string, error) {
	refs, err := r.repo.References()
	if err != nil {
		return nil, fmt.Errorf("failed to get references: %w", err)
	}
	defer refs.Close()

	var branches []string
	err = refs.ForEach(func(ref *plumbing.Reference) error {
		if ref.Name().IsBranch() {
			branchName := ref.Name().Short()
			branches = append(branches, branchName)
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("failed to iterate references: %w", err)
	}

	return branches, nil
}

// GetCurrentBranch returns the name of the current branch
func (r *Repository) GetCurrentBranch() (string, error) {
	head, err := r.repo.Head()
	if err != nil {
		return "", fmt.Errorf("failed to get HEAD: %w", err)
	}

	if !head.Name().IsBranch() {
		return "", fmt.Errorf("HEAD is not pointing to a branch")
	}

	return head.Name().Short(), nil
}

// BranchExists checks if a branch exists in the repository
func (r *Repository) BranchExists(branchName string) bool {
	_, err := r.repo.Reference(plumbing.ReferenceName("refs/heads/"+branchName), true)
	return err == nil
}

