package git

import (
	"fmt"
	"strings"

	"github.com/go-git/go-git/v5/plumbing/object"
)

// FileDiff represents the diff information for a single file
type FileDiff struct {
	Path         string
	LinesAdded   int
	LinesDeleted int
	IsNew        bool
	IsDeleted    bool
	IsRenamed    bool
	OldPath      string
}

// CommitDiff represents the diff information for a commit
type CommitDiff struct {
	Commit    *object.Commit
	Files     []FileDiff
	TotalAdded   int
	TotalDeleted int
}

// GetCommitDiff analyzes the diff for a single commit
func (r *Repository) GetCommitDiff(commit *object.Commit) (*CommitDiff, error) {
	var parentCommit *object.Commit
	var err error

	// Get parent commit for comparison
	if commit.NumParents() > 0 {
		parentCommit, err = commit.Parents().Next()
		if err != nil {
			return nil, fmt.Errorf("failed to get parent commit: %w", err)
		}
	}

	// Get the diff
	var patch *object.Patch
	if parentCommit != nil {
		parentTree, err := parentCommit.Tree()
		if err != nil {
			return nil, fmt.Errorf("failed to get parent tree: %w", err)
		}

		commitTree, err := commit.Tree()
		if err != nil {
			return nil, fmt.Errorf("failed to get commit tree: %w", err)
		}

		patch, err = parentTree.Patch(commitTree)
		if err != nil {
			return nil, fmt.Errorf("failed to get patch: %w", err)
		}
	} else {
		// First commit - compare against empty tree
		commitTree, err := commit.Tree()
		if err != nil {
			return nil, fmt.Errorf("failed to get commit tree: %w", err)
		}

		patch, err = commitTree.Patch(&object.Tree{})
		if err != nil {
			return nil, fmt.Errorf("failed to get patch for first commit: %w", err)
		}
	}

	// Parse the patch
	diff := &CommitDiff{
		Commit: commit,
		Files:  []FileDiff{},
	}

	for _, filePatch := range patch.FilePatches() {
		from, to := filePatch.Files()
		
		fileDiff := FileDiff{}
		
		if from == nil && to != nil {
			// New file
			fileDiff.Path = to.Path()
			fileDiff.IsNew = true
		} else if from != nil && to == nil {
			// Deleted file
			fileDiff.Path = from.Path()
			fileDiff.IsDeleted = true
		} else if from != nil && to != nil {
			// Modified or renamed file
			fileDiff.Path = to.Path()
			if from.Path() != to.Path() {
				fileDiff.IsRenamed = true
				fileDiff.OldPath = from.Path()
			}
		}

		// Count lines added and deleted
		chunks := filePatch.Chunks()
		for _, chunk := range chunks {
			lines := strings.Split(chunk.Content(), "\n")
			for _, line := range lines {
				if len(line) == 0 {
					continue
				}
				switch line[0] {
				case '+':
					fileDiff.LinesAdded++
					diff.TotalAdded++
				case '-':
					fileDiff.LinesDeleted++
					diff.TotalDeleted++
				}
			}
		}

		diff.Files = append(diff.Files, fileDiff)
	}

	return diff, nil
}

// GetFileList returns a list of all files changed in a commit
func (r *Repository) GetFileList(commit *object.Commit) ([]string, error) {
	diff, err := r.GetCommitDiff(commit)
	if err != nil {
		return nil, err
	}

	var files []string
	for _, fileDiff := range diff.Files {
		files = append(files, fileDiff.Path)
	}

	return files, nil
}

// GetTotalChanges returns the total lines added and deleted across all commits
func GetTotalChanges(diffs []*CommitDiff) (int, int) {
	totalAdded := 0
	totalDeleted := 0

	for _, diff := range diffs {
		totalAdded += diff.TotalAdded
		totalDeleted += diff.TotalDeleted
	}

	return totalAdded, totalDeleted
}

// GetFileChanges returns a map of file paths to their total line changes
func GetFileChanges(diffs []*CommitDiff) map[string]FileDiff {
	fileChanges := make(map[string]FileDiff)

	for _, diff := range diffs {
		for _, fileDiff := range diff.Files {
			if existing, exists := fileChanges[fileDiff.Path]; exists {
				// Aggregate changes for the same file
				existing.LinesAdded += fileDiff.LinesAdded
				existing.LinesDeleted += fileDiff.LinesDeleted
				fileChanges[fileDiff.Path] = existing
			} else {
				fileChanges[fileDiff.Path] = fileDiff
			}
		}
	}

	return fileChanges
}

