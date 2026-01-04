package git

import (
	"fmt"
	"os/exec"
	"strconv"
	"strings"
)

// DiffLine represents a single line in a diff
type DiffLine struct {
	Type    string `json:"type"`    // "context", "added", "removed"
	OldLine *int   `json:"old_line,omitempty"`
	NewLine *int   `json:"new_line,omitempty"`
	Content string `json:"content"`
}

// FileDiff represents the diff for a single file
type FileDiff struct {
	File     string     `json:"file"`
	OldFile  string     `json:"old_file,omitempty"`
	NewFile  string     `json:"new_file,omitempty"`
	IsNew    bool       `json:"is_new"`
	IsDeleted bool      `json:"is_deleted"`
	Changes  []DiffLine `json:"changes"`
}

// GetDiff returns the diff between two commits
func (r *Repository) GetDiff(fromCommit, toCommit string) ([]FileDiff, error) {
	var cmd *exec.Cmd
	if toCommit == "" {
		// Compare with working directory
		cmd = exec.Command("git", "diff", "-U3", fromCommit)
	} else {
		cmd = exec.Command("git", "diff", "-U3", fromCommit+".."+toCommit)
	}
	
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to get diff: %w", err)
	}

	return parseDiff(string(output))
}

// GetFileDiff returns the diff for a specific file
func (r *Repository) GetFileDiff(fromCommit, toCommit, filePath string) (*FileDiff, error) {
	var cmd *exec.Cmd
	if toCommit == "" {
		cmd = exec.Command("git", "diff", "-U3", fromCommit, "--", filePath)
	} else {
		cmd = exec.Command("git", "diff", "-U3", fromCommit+".."+toCommit, "--", filePath)
	}
	
	cmd.Dir = r.Path
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to get file diff: %w", err)
	}

	diffs, err := parseDiff(string(output))
	if err != nil {
		return nil, err
	}

	if len(diffs) == 0 {
		return nil, fmt.Errorf("no diff found for file: %s", filePath)
	}

	return &diffs[0], nil
}

// parseDiff parses git diff output into structured data
func parseDiff(diffOutput string) ([]FileDiff, error) {
	lines := strings.Split(diffOutput, "\n")
	var diffs []FileDiff
	var currentDiff *FileDiff
	var oldLineNum, newLineNum int

	for _, line := range lines {
		if strings.HasPrefix(line, "diff --git") {
			// Start of new file diff
			if currentDiff != nil {
				diffs = append(diffs, *currentDiff)
			}
			currentDiff = &FileDiff{}
			
			// Extract file paths
			parts := strings.Fields(line)
			if len(parts) >= 4 {
				oldFile := strings.TrimPrefix(parts[2], "a/")
				newFile := strings.TrimPrefix(parts[3], "b/")
				currentDiff.OldFile = oldFile
				currentDiff.NewFile = newFile
				currentDiff.File = newFile
			}
		} else if strings.HasPrefix(line, "new file mode") {
			if currentDiff != nil {
				currentDiff.IsNew = true
			}
		} else if strings.HasPrefix(line, "deleted file mode") {
			if currentDiff != nil {
				currentDiff.IsDeleted = true
			}
		} else if strings.HasPrefix(line, "@@") {
			// Parse hunk header
			if currentDiff != nil {
				oldLineNum, newLineNum = parseHunkHeader(line)
			}
		} else if currentDiff != nil && (strings.HasPrefix(line, "+") || strings.HasPrefix(line, "-") || strings.HasPrefix(line, " ")) {
			// Parse diff line
			diffLine := parseDiffLine(line, &oldLineNum, &newLineNum)
			currentDiff.Changes = append(currentDiff.Changes, diffLine)
		}
	}

	// Add the last diff
	if currentDiff != nil {
		diffs = append(diffs, *currentDiff)
	}

	return diffs, nil
}

// parseHunkHeader parses a hunk header line like "@@ -1,4 +1,6 @@"
func parseHunkHeader(line string) (int, int) {
	// Extract the line numbers from the hunk header
	parts := strings.Fields(line)
	if len(parts) < 3 {
		return 1, 1
	}

	// Parse old line number
	oldPart := strings.TrimPrefix(parts[1], "-")
	oldLineNum := 1
	if commaIndex := strings.Index(oldPart, ","); commaIndex != -1 {
		if num, err := strconv.Atoi(oldPart[:commaIndex]); err == nil {
			oldLineNum = num
		}
	} else {
		if num, err := strconv.Atoi(oldPart); err == nil {
			oldLineNum = num
		}
	}

	// Parse new line number
	newPart := strings.TrimPrefix(parts[2], "+")
	newLineNum := 1
	if commaIndex := strings.Index(newPart, ","); commaIndex != -1 {
		if num, err := strconv.Atoi(newPart[:commaIndex]); err == nil {
			newLineNum = num
		}
	} else {
		if num, err := strconv.Atoi(newPart); err == nil {
			newLineNum = num
		}
	}

	return oldLineNum, newLineNum
}

// parseDiffLine parses a single diff line and updates line numbers
func parseDiffLine(line string, oldLineNum, newLineNum *int) DiffLine {
	if len(line) == 0 {
		return DiffLine{Type: "context", Content: ""}
	}

	switch line[0] {
	case '+':
		diffLine := DiffLine{
			Type:    "added",
			NewLine: newLineNum,
			Content: line[1:],
		}
		*newLineNum++
		return diffLine
	case '-':
		diffLine := DiffLine{
			Type:    "removed",
			OldLine: oldLineNum,
			Content: line[1:],
		}
		*oldLineNum++
		return diffLine
	case ' ':
		diffLine := DiffLine{
			Type:    "context",
			OldLine: oldLineNum,
			NewLine: newLineNum,
			Content: line[1:],
		}
		*oldLineNum++
		*newLineNum++
		return diffLine
	default:
		// Handle lines that don't start with +, -, or space
		return DiffLine{
			Type:    "context",
			Content: line,
		}
	}
}
