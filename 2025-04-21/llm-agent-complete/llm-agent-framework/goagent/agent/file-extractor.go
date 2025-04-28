package agent

import (
	"regexp"
	"strings"
)

// FileExtractor handles the extraction of file content delimited by XML tags,
// managing state across multiple potentially incomplete LLM responses.
type FileExtractor struct {
	incompleteFileName    string          // Name of the file currently being extracted if incomplete
	incompleteFileContent strings.Builder // Accumulated content of the incomplete file
	startTagRegex         *regexp.Regexp  // Compiled regex for <file name="...">
	endTagRegex           *regexp.Regexp  // Compiled regex for </file>
}

// NewFileExtractor creates a new FileExtractor.
func NewFileExtractor() *FileExtractor {
	return &FileExtractor{
		startTagRegex: regexp.MustCompile(`<file\s+name=["']([^"']+)["']>`),
		endTagRegex:   regexp.MustCompile(`</file>`),
	}
}

// Extract parses the response, extracting complete files and managing incomplete ones.
// It returns a map of filenames to their content for files completed in this response chunk.
func (e *FileExtractor) Extract(response string) map[string]string {
	completeFiles := make(map[string]string)
	currentIndex := 0

	// Process the response chunk by chunk based on current index
	for currentIndex < len(response) {
		remainingTextInChunk := response[currentIndex:]

		if e.incompleteFileName == "" { // State: Looking for a new file start tag
			startMatch := e.startTagRegex.FindStringSubmatchIndex(remainingTextInChunk)
			if startMatch == nil {
				// No more start tags found in the remaining text of this chunk.
				// Any remaining text is outside tags, ignore it.
				break
			}

			// Found a start tag. Extract filename and calculate where content begins.
			fileName := remainingTextInChunk[startMatch[2]:startMatch[3]]
			contentStartIndexInChunk := startMatch[1] // Index in remainingTextInChunk where content starts

			// Look for the end tag *after* this start tag's content begins in the remainder of the *chunk*.
			endMatch := e.endTagRegex.FindStringIndex(remainingTextInChunk[contentStartIndexInChunk:])

			if endMatch == nil {
				// No end tag found in the rest of this response chunk.
				// This file is incomplete. Store its name and the partial content from this chunk.
				e.incompleteFileName = fileName
				// Append the content starting from after the start tag to the end of the chunk
				e.incompleteFileContent.WriteString(remainingTextInChunk[contentStartIndexInChunk:])
				currentIndex = len(response) // Mark the rest of the response chunk as consumed
			} else {
				// Found the end tag. The file is complete within this chunk.
				contentEndIndexInChunk := contentStartIndexInChunk + endMatch[0] // End index in remainingTextInChunk
				content := remainingTextInChunk[contentStartIndexInChunk:contentEndIndexInChunk]
				completeFiles[fileName] = strings.TrimSpace(content)

				// Move the main index past the </file> tag *within this chunk*
				currentIndex += contentStartIndexInChunk + endMatch[1]
				// Reset state as we finished a file (no longer incomplete)
				e.incompleteFileName = ""
				e.incompleteFileContent.Reset()
			}

		} else { // State: Inside an incomplete file, looking for the end tag in the current chunk
			// Look for the end tag in the remaining part of the *current* response chunk
			endMatch := e.endTagRegex.FindStringIndex(remainingTextInChunk)

			if endMatch == nil {
				// Still no end tag found in this chunk. Append the entire remaining chunk to the buffer.
				e.incompleteFileContent.WriteString(remainingTextInChunk)
				currentIndex = len(response) // Mark the rest of this response chunk as consumed
			} else {
				// Found the end tag, completing the previously incomplete file.
				contentEndIndexInChunk := endMatch[0]
				// Append the part of the chunk *before* the end tag to the buffer
				e.incompleteFileContent.WriteString(remainingTextInChunk[:contentEndIndexInChunk])
				// Complete the file with the accumulated content from the buffer
				completeFiles[e.incompleteFileName] = strings.TrimSpace(e.incompleteFileContent.String())

				// Reset state
				e.incompleteFileName = ""
				e.incompleteFileContent.Reset() // Reset the builder *after* using its content

				// Move index past the </file> tag *in the current response chunk*
				// Note: currentIndex is relative to the start of the *original* response string
				currentIndex += endMatch[1]

				// Optional: Log completion
				// log.Printf("Completed file '%s' from buffer.", incompleteFileNameBeforeReset)
			}
		}
	}

	return completeFiles
}
