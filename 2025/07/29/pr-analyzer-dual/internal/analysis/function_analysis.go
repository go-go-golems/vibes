package analysis

import (
	"strings"

	"github.com/pr-analyzer/pr-analyzer/internal/treesitter"
)

// FileChange represents changes to a file with function analysis
type FileChange struct {
	FilePath      string
	LinesAdded    int
	LinesRemoved  int
	LinesModified int
	ChangedLines  []int
}

// ParseDiffForAnalysis parses diff and returns file changes with line information
func ParseDiffForAnalysis(diffContent string) ([]*FileChange, error) {
	fileDiffs, err := ParseDiff(diffContent)
	if err != nil {
		return nil, err
	}

	var fileChanges []*FileChange
	for _, fileDiff := range fileDiffs {
		added, removed, modified := fileDiff.GetStats()

		fileChange := &FileChange{
			FilePath:      fileDiff.NewFile,
			LinesAdded:    added,
			LinesRemoved:  removed,
			LinesModified: modified,
			ChangedLines:  fileDiff.GetChangedLines(),
		}

		fileChanges = append(fileChanges, fileChange)
	}

	return fileChanges, nil
}

// IsFunctionChanged determines if a function was modified based on changed lines
func IsFunctionChanged(fn *treesitter.Function, changedLines []int) bool {
	for _, lineNo := range changedLines {
		if lineNo >= fn.StartLine && lineNo <= fn.EndLine {
			return true
		}
	}
	return false
}

// GetGoFiles filters file changes to only Go files
func GetGoFiles(fileChanges []*FileChange) []*FileChange {
	var goFiles []*FileChange
	for _, fc := range fileChanges {
		if strings.HasSuffix(fc.FilePath, ".go") {
			goFiles = append(goFiles, fc)
		}
	}
	return goFiles
}
