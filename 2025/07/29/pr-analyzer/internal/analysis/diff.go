package analysis

import (
	"bufio"
	"regexp"
	"strconv"
	"strings"
)

// DiffLine represents a line in a diff
type DiffLine struct {
	Type    string // "add", "remove", "context"
	Content string
	LineNo  int // Line number in the new file (for add/context)
	OldLineNo int // Line number in the old file (for remove/context)
}

// FileDiff represents changes to a single file
type FileDiff struct {
	OldFile string
	NewFile string
	Lines   []*DiffLine
}

// ParseDiff parses a unified diff and returns structured information
func ParseDiff(diffContent string) ([]*FileDiff, error) {
	var fileDiffs []*FileDiff
	var currentFile *FileDiff
	
	scanner := bufio.NewScanner(strings.NewReader(diffContent))
	
	// Regex patterns for diff parsing
	fileHeaderRegex := regexp.MustCompile(`^diff --git a/(.*) b/(.*)$`)
	oldFileRegex := regexp.MustCompile(`^--- a/(.*)$`)
	newFileRegex := regexp.MustCompile(`^\+\+\+ b/(.*)$`)
	hunkHeaderRegex := regexp.MustCompile(`^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@`)
	
	var oldLineNo, newLineNo int
	
	for scanner.Scan() {
		line := scanner.Text()
		
		// Check for file header
		if matches := fileHeaderRegex.FindStringSubmatch(line); matches != nil {
			if currentFile != nil {
				fileDiffs = append(fileDiffs, currentFile)
			}
			currentFile = &FileDiff{
				OldFile: matches[1],
				NewFile: matches[2],
				Lines:   []*DiffLine{},
			}
			continue
		}
		
		// Check for old file marker
		if matches := oldFileRegex.FindStringSubmatch(line); matches != nil {
			if currentFile != nil {
				currentFile.OldFile = matches[1]
			}
			continue
		}
		
		// Check for new file marker
		if matches := newFileRegex.FindStringSubmatch(line); matches != nil {
			if currentFile != nil {
				currentFile.NewFile = matches[1]
			}
			continue
		}
		
		// Check for hunk header
		if matches := hunkHeaderRegex.FindStringSubmatch(line); matches != nil {
			oldStart, _ := strconv.Atoi(matches[1])
			newStart, _ := strconv.Atoi(matches[3])
			oldLineNo = oldStart
			newLineNo = newStart
			continue
		}
		
		// Parse diff lines
		if currentFile != nil && len(line) > 0 {
			switch line[0] {
			case '+':
				currentFile.Lines = append(currentFile.Lines, &DiffLine{
					Type:    "add",
					Content: line[1:],
					LineNo:  newLineNo,
				})
				newLineNo++
			case '-':
				currentFile.Lines = append(currentFile.Lines, &DiffLine{
					Type:      "remove",
					Content:   line[1:],
					OldLineNo: oldLineNo,
				})
				oldLineNo++
			case ' ':
				currentFile.Lines = append(currentFile.Lines, &DiffLine{
					Type:      "context",
					Content:   line[1:],
					LineNo:    newLineNo,
					OldLineNo: oldLineNo,
				})
				oldLineNo++
				newLineNo++
			}
		}
	}
	
	// Add the last file
	if currentFile != nil {
		fileDiffs = append(fileDiffs, currentFile)
	}
	
	return fileDiffs, scanner.Err()
}

// GetChangedLines returns the line numbers that were added or modified
func (fd *FileDiff) GetChangedLines() []int {
	var lines []int
	for _, line := range fd.Lines {
		if line.Type == "add" {
			lines = append(lines, line.LineNo)
		}
	}
	return lines
}

// GetAddedLines returns only the lines that were added
func (fd *FileDiff) GetAddedLines() []int {
	var lines []int
	for _, line := range fd.Lines {
		if line.Type == "add" {
			lines = append(lines, line.LineNo)
		}
	}
	return lines
}

// GetRemovedLines returns only the lines that were removed
func (fd *FileDiff) GetRemovedLines() []int {
	var lines []int
	for _, line := range fd.Lines {
		if line.Type == "remove" {
			lines = append(lines, line.OldLineNo)
		}
	}
	return lines
}

// IsGoFile checks if the file is a Go source file
func (fd *FileDiff) IsGoFile() bool {
	return strings.HasSuffix(fd.NewFile, ".go") || strings.HasSuffix(fd.OldFile, ".go")
}

// GetStats returns basic statistics about the diff
func (fd *FileDiff) GetStats() (added, removed, modified int) {
	for _, line := range fd.Lines {
		switch line.Type {
		case "add":
			added++
		case "remove":
			removed++
		}
	}
	// Modified lines are harder to determine from unified diff
	// For now, we'll consider it as min(added, removed)
	if added > 0 && removed > 0 {
		if added < removed {
			modified = added
			added = 0
			removed = removed - modified
		} else {
			modified = removed
			removed = 0
			added = added - modified
		}
	}
	return
}

