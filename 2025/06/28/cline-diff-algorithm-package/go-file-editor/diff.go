package fileeditor

import (
	"fmt"
	"regexp"
	"strings"
)

// ProcessingState represents the current state of diff processing
type ProcessingState int

const (
	StateIdle ProcessingState = iota
	StateSearch
	StateReplace
)

// Marker patterns for SEARCH/REPLACE blocks
var (
	searchBlockStartRegex      = regexp.MustCompile(`^[-]{3,} SEARCH$`)
	legacySearchBlockStartRegex = regexp.MustCompile(`^[<]{3,} SEARCH$`)
	searchBlockEndRegex        = regexp.MustCompile(`^[=]{3,}$`)
	replaceBlockEndRegex       = regexp.MustCompile(`^[+]{3,} REPLACE$`)
	legacyReplaceBlockEndRegex = regexp.MustCompile(`^[>]{3,} REPLACE$`)
)

// Replacement represents a single search/replace operation
type Replacement struct {
	Start   int
	End     int
	Content string
}

// DiffProcessor handles the processing of SEARCH/REPLACE blocks
type DiffProcessor struct {
	originalContent     string
	isFinal            bool
	state              ProcessingState
	result             strings.Builder
	lastProcessedIndex int
	currentSearchContent  strings.Builder
	currentReplaceContent strings.Builder
	searchMatchIndex   int
	searchEndIndex     int
	replacements       []Replacement
}

// NewDiffProcessor creates a new diff processor
func NewDiffProcessor(originalContent string, isFinal bool) *DiffProcessor {
	return &DiffProcessor{
		originalContent:    originalContent,
		isFinal:           isFinal,
		state:             StateIdle,
		searchMatchIndex:  -1,
		searchEndIndex:    -1,
		replacements:      make([]Replacement, 0),
	}
}

// isSearchBlockStart checks if a line is a search block start marker
func isSearchBlockStart(line string) bool {
	return searchBlockStartRegex.MatchString(line) || legacySearchBlockStartRegex.MatchString(line)
}

// isSearchBlockEnd checks if a line is a search block end marker
func isSearchBlockEnd(line string) bool {
	return searchBlockEndRegex.MatchString(line)
}

// isReplaceBlockEnd checks if a line is a replace block end marker
func isReplaceBlockEnd(line string) bool {
	return replaceBlockEndRegex.MatchString(line) || legacyReplaceBlockEndRegex.MatchString(line)
}

// lineTrimmedFallbackMatch attempts to match search content using line-by-line trimmed comparison
func (dp *DiffProcessor) lineTrimmedFallbackMatch(searchContent string, startIndex int) (int, int, bool) {
	originalLines := strings.Split(dp.originalContent, "\n")
	searchLines := strings.Split(searchContent, "\n")
	
	// Remove trailing empty line if exists
	if len(searchLines) > 0 && searchLines[len(searchLines)-1] == "" {
		searchLines = searchLines[:len(searchLines)-1]
	}
	
	// Find the line number where startIndex falls
	startLineNum := 0
	currentIndex := 0
	for currentIndex < startIndex && startLineNum < len(originalLines) {
		currentIndex += len(originalLines[startLineNum]) + 1 // +1 for \n
		startLineNum++
	}
	
	// Try to match from each possible starting position
	for i := startLineNum; i <= len(originalLines)-len(searchLines); i++ {
		matches := true
		
		// Try to match all search lines from this position
		for j := 0; j < len(searchLines); j++ {
			originalTrimmed := strings.TrimSpace(originalLines[i+j])
			searchTrimmed := strings.TrimSpace(searchLines[j])
			
			if originalTrimmed != searchTrimmed {
				matches = false
				break
			}
		}
		
		if matches {
			// Calculate exact character positions
			matchStartIndex := 0
			for k := 0; k < i; k++ {
				matchStartIndex += len(originalLines[k]) + 1 // +1 for \n
			}
			
			matchEndIndex := matchStartIndex
			for k := 0; k < len(searchLines); k++ {
				matchEndIndex += len(originalLines[i+k]) + 1 // +1 for \n
			}
			
			return matchStartIndex, matchEndIndex, true
		}
	}
	
	return 0, 0, false
}

// blockAnchorFallbackMatch attempts to match using first and last lines as anchors
func (dp *DiffProcessor) blockAnchorFallbackMatch(searchContent string, startIndex int) (int, int, bool) {
	originalLines := strings.Split(dp.originalContent, "\n")
	searchLines := strings.Split(searchContent, "\n")
	
	// Only use this approach for blocks of 3+ lines
	if len(searchLines) < 3 {
		return 0, 0, false
	}
	
	// Remove trailing empty line if exists
	if len(searchLines) > 0 && searchLines[len(searchLines)-1] == "" {
		searchLines = searchLines[:len(searchLines)-1]
	}
	
	firstLineSearch := strings.TrimSpace(searchLines[0])
	lastLineSearch := strings.TrimSpace(searchLines[len(searchLines)-1])
	searchBlockSize := len(searchLines)
	
	// Find the line number where startIndex falls
	startLineNum := 0
	currentIndex := 0
	for currentIndex < startIndex && startLineNum < len(originalLines) {
		currentIndex += len(originalLines[startLineNum]) + 1
		startLineNum++
	}
	
	// Look for matching start and end anchors
	for i := startLineNum; i <= len(originalLines)-searchBlockSize; i++ {
		// Check if first line matches
		if strings.TrimSpace(originalLines[i]) != firstLineSearch {
			continue
		}
		
		// Check if last line matches at the expected position
		if strings.TrimSpace(originalLines[i+searchBlockSize-1]) != lastLineSearch {
			continue
		}
		
		// Calculate exact character positions
		matchStartIndex := 0
		for k := 0; k < i; k++ {
			matchStartIndex += len(originalLines[k]) + 1
		}
		
		matchEndIndex := matchStartIndex
		for k := 0; k < searchBlockSize; k++ {
			matchEndIndex += len(originalLines[i+k]) + 1
		}
		
		return matchStartIndex, matchEndIndex, true
	}
	
	return 0, 0, false
}

// findSearchMatch attempts to find the search content using multiple strategies
func (dp *DiffProcessor) findSearchMatch(searchContent string) error {
	if searchContent == "" {
		// Empty search block
		if len(dp.originalContent) == 0 {
			// New file scenario
			dp.searchMatchIndex = 0
			dp.searchEndIndex = 0
			return nil
		} else {
			// Complete file replacement scenario
			dp.searchMatchIndex = 0
			dp.searchEndIndex = len(dp.originalContent)
			return nil
		}
	}
	
	// Strategy 1: Exact match
	exactIndex := strings.Index(dp.originalContent[dp.lastProcessedIndex:], searchContent)
	if exactIndex != -1 {
		dp.searchMatchIndex = dp.lastProcessedIndex + exactIndex
		dp.searchEndIndex = dp.searchMatchIndex + len(searchContent)
		return nil
	}
	
	// Strategy 2: Line-trimmed fallback
	if start, end, found := dp.lineTrimmedFallbackMatch(searchContent, dp.lastProcessedIndex); found {
		dp.searchMatchIndex = start
		dp.searchEndIndex = end
		return nil
	}
	
	// Strategy 3: Block anchor fallback
	if start, end, found := dp.blockAnchorFallbackMatch(searchContent, dp.lastProcessedIndex); found {
		dp.searchMatchIndex = start
		dp.searchEndIndex = end
		return nil
	}
	
	// Strategy 4: Full file search (last resort)
	fullFileIndex := strings.Index(dp.originalContent, searchContent)
	if fullFileIndex != -1 {
		dp.searchMatchIndex = fullFileIndex
		dp.searchEndIndex = fullFileIndex + len(searchContent)
		return nil
	}
	
	return fmt.Errorf("The SEARCH block:\n%s\n...does not match anything in the file", strings.TrimSuffix(searchContent, "\n"))
}

// ProcessLine processes a single line of diff content
func (dp *DiffProcessor) ProcessLine(line string) error {
	if isSearchBlockStart(line) {
		dp.state = StateSearch
		dp.currentSearchContent.Reset()
		dp.currentReplaceContent.Reset()
		return nil
	}
	
	if isSearchBlockEnd(line) {
		dp.state = StateReplace
		
		// Find the search match
		searchContent := dp.currentSearchContent.String()
		if err := dp.findSearchMatch(searchContent); err != nil {
			return err
		}
		
		// For in-order replacements, output everything up to the match location
		if dp.searchMatchIndex >= dp.lastProcessedIndex {
			dp.result.WriteString(dp.originalContent[dp.lastProcessedIndex:dp.searchMatchIndex])
		}
		// Note: For out-of-order replacements, we don't output anything here
		// and will rebuild the entire content in GetResult()
		
		return nil
	}
	
	if isReplaceBlockEnd(line) {
		// Store this replacement
		dp.replacements = append(dp.replacements, Replacement{
			Start:   dp.searchMatchIndex,
			End:     dp.searchEndIndex,
			Content: dp.currentReplaceContent.String(),
		})
		
		// Only advance lastProcessedIndex for in-order replacements
		if dp.searchMatchIndex >= dp.lastProcessedIndex {
			dp.lastProcessedIndex = dp.searchEndIndex
		}
		
		// Reset for next block
		dp.state = StateIdle
		dp.currentSearchContent.Reset()
		dp.currentReplaceContent.Reset()
		dp.searchMatchIndex = -1
		dp.searchEndIndex = -1
		return nil
	}
	
	// Accumulate content for search or replace
	switch dp.state {
	case StateSearch:
		dp.currentSearchContent.WriteString(line + "\n")
	case StateReplace:
		dp.currentReplaceContent.WriteString(line + "\n")
		// Only output replacement lines immediately for in-order replacements
		if dp.searchMatchIndex != -1 && dp.searchMatchIndex >= dp.lastProcessedIndex {
			dp.result.WriteString(line + "\n")
		}
	}
	
	return nil
}

// GetResult returns the final processed content
func (dp *DiffProcessor) GetResult() (string, error) {
	// Handle missing final REPLACE marker when isFinal is true
	if dp.isFinal && dp.state == StateReplace && dp.searchMatchIndex != -1 {
		dp.replacements = append(dp.replacements, Replacement{
			Start:   dp.searchMatchIndex,
			End:     dp.searchEndIndex,
			Content: dp.currentReplaceContent.String(),
		})
		dp.lastProcessedIndex = dp.searchEndIndex
		dp.state = StateIdle // Reset state to idle
	}
	
	if dp.isFinal && dp.state != StateIdle {
		return "", fmt.Errorf("File processing incomplete - SEARCH/REPLACE operations still active during finalization")
	}
	
	// If we have replacements, rebuild the entire content
	if len(dp.replacements) > 0 {
		// Sort replacements by start position
		for i := 0; i < len(dp.replacements); i++ {
			for j := i + 1; j < len(dp.replacements); j++ {
				if dp.replacements[i].Start > dp.replacements[j].Start {
					dp.replacements[i], dp.replacements[j] = dp.replacements[j], dp.replacements[i]
				}
			}
		}
		
		// Rebuild the entire result by applying all replacements
		var result strings.Builder
		currentPos := 0
		
		for _, replacement := range dp.replacements {
			// Add original content up to this replacement
			if replacement.Start > currentPos && replacement.Start <= len(dp.originalContent) {
				result.WriteString(dp.originalContent[currentPos:replacement.Start])
			}
			// Add the replacement content
			result.WriteString(replacement.Content)
			// Move position to after the replaced section
			if replacement.End <= len(dp.originalContent) {
				currentPos = replacement.End
			} else {
				currentPos = len(dp.originalContent)
			}
		}
		
		// Add any remaining original content
		if currentPos < len(dp.originalContent) {
			result.WriteString(dp.originalContent[currentPos:])
		}
		
		return result.String(), nil
	}
	
	// If this is the final chunk and no replacements, append any remaining original content
	if dp.isFinal && dp.lastProcessedIndex < len(dp.originalContent) {
		dp.result.WriteString(dp.originalContent[dp.lastProcessedIndex:])
	}
	
	return dp.result.String(), nil
}

// ConstructNewFileContent is the main function that processes diff content and returns the new file content
func ConstructNewFileContent(diffContent, originalContent string, isFinal bool) (string, error) {
	processor := NewDiffProcessor(originalContent, isFinal)
	
	lines := strings.Split(diffContent, "\n")
	
	// Remove potentially incomplete marker at the end
	if len(lines) > 0 {
		lastLine := lines[len(lines)-1]
		if (strings.HasPrefix(lastLine, "-") || strings.HasPrefix(lastLine, "<") ||
			strings.HasPrefix(lastLine, "=") || strings.HasPrefix(lastLine, "+") ||
			strings.HasPrefix(lastLine, ">")) &&
			!isSearchBlockStart(lastLine) && !isSearchBlockEnd(lastLine) && !isReplaceBlockEnd(lastLine) {
			lines = lines[:len(lines)-1]
		}
	}
	
	// Process each line
	for _, line := range lines {
		if err := processor.ProcessLine(line); err != nil {
			return "", err
		}
	}
	
	return processor.GetResult()
}

