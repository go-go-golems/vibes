package git

import (
	"fmt"
	"strings"

	"github.com/go-git/go-git/v5/plumbing/format/diff"
	"github.com/go-git/go-git/v5/plumbing/object"
	"github.com/rs/zerolog/log"
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
	Commit       *object.Commit
	Files        []FileDiff
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

	log.Debug().Str("commit", commit.Hash.String()).Bool("has_parent", parentCommit != nil).Msg("computing commit diff")

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
		log.Trace().Str("commit", commit.Hash.String()).Str("parent", parentCommit.Hash.String()).Msg("created patch vs parent")
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
		log.Trace().Str("commit", commit.Hash.String()).Msg("created patch vs empty tree (root commit)")
	}

	// Parse the patch
	diff_ := &CommitDiff{
		Commit: commit,
		Files:  []FileDiff{},
	}

	for _, filePatch := range patch.FilePatches() {
		from, to := filePatch.Files()

		fileDiff := FileDiff{}

		if from == nil && to != nil {
			log.Trace().Str("file", to.Path()).Msg("new file")
			// New file
			fileDiff.Path = to.Path()
			fileDiff.IsNew = true
		} else if from != nil && to == nil {
			log.Trace().Str("file", from.Path()).Msg("deleted file")
			// Deleted file
			fileDiff.Path = from.Path()
			fileDiff.IsDeleted = true
		} else if from != nil && to != nil {
			log.Trace().Str("file", to.Path()).Msg("modified file")
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
			log.Trace().Str("file", fileDiff.Path).Int("lines", len(lines)).Msg("processing chunk")
			for _, line := range lines {
				if len(line) == 0 {
					continue
				}
				switch chunk.Type() {
				case diff.Add:
					fileDiff.LinesAdded++
					diff_.TotalAdded++
				case diff.Delete:
					fileDiff.LinesDeleted++
					diff_.TotalDeleted++
				}
				switch line[0] {
				case '+':
					// log.Trace().Str("line", line).Msg("added line")
					fileDiff.LinesAdded++
					diff_.TotalAdded++
				case '-':
					// log.Trace().Str("line", line).Msg("deleted line")
					fileDiff.LinesDeleted++
					diff_.TotalDeleted++
				}
			}
		}

		log.Trace().Str("file", fileDiff.Path).
			Bool("new", fileDiff.IsNew).
			Bool("deleted", fileDiff.IsDeleted).
			Bool("renamed", fileDiff.IsRenamed).
			Int("added", fileDiff.LinesAdded).
			Int("deleted", fileDiff.LinesDeleted).
			Msg("file diff stats")
		diff_.Files = append(diff_.Files, fileDiff)
	}

	log.Debug().Str("commit", commit.Hash.String()).Int("files", len(diff_.Files)).Int("added", diff_.TotalAdded).Int("deleted", diff_.TotalDeleted).Msg("computed commit diff")
	return diff_, nil
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
