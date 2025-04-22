// pkg/ts/parser.go
package ts

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strings"
	"time"

	"github.com/kb-project/pkg/model"
)

// Parser represents a mock tree-sitter parser
type Parser struct {
	language string
}

// NewParser creates a new mock tree-sitter parser for the specified language
func NewParser(language string) *Parser {
	return &Parser{
		language: language,
	}
}

// ParseFile mocks the tree-sitter parsing of a file and returns chunks
func (p *Parser) ParseFile(ctx context.Context, filePath string, content string) ([]*model.Chunk, error) {
	// In a real implementation, this would use tree-sitter to parse the file
	// For our mock, we'll simulate finding functions, methods, and classes

	var chunks []*model.Chunk
	
	// Split content into lines for easier processing
	lines := strings.Split(content, "\n")
	
	// Mock detection of symbols based on language
	switch p.language {
	case "go":
		chunks = p.parseGoFile(filePath, lines)
	case "python", "py":
		chunks = p.parsePythonFile(filePath, lines)
	case "typescript", "ts":
		chunks = p.parseTypeScriptFile(filePath, lines)
	case "cpp", "c++":
		chunks = p.parseCppFile(filePath, lines)
	default:
		// Generic parsing for other languages
		chunks = p.parseGenericFile(filePath, lines)
	}
	
	return chunks, nil
}

// parseGoFile mocks parsing a Go file
func (p *Parser) parseGoFile(filePath string, lines []string) []*model.Chunk {
	var chunks []*model.Chunk
	
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		
		// Mock function detection
		if strings.Contains(line, "func ") {
			// Extract function name
			parts := strings.Split(line, "func ")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], "(")
			if len(nameParts) < 2 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			
			// Find function body
			startLine := i
			endLine := findClosingBrace(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractGoDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "function", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of function
			i = endLine
		}
		
		// Mock method detection (similar to function but with receiver)
		if strings.Contains(line, "func (") {
			// Extract method name
			parts := strings.Split(line, ")")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], "(")
			if len(nameParts) < 2 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			
			// Find method body
			startLine := i
			endLine := findClosingBrace(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractGoDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "method", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of method
			i = endLine
		}
		
		// Mock struct detection
		if strings.Contains(line, "type ") && strings.Contains(line, "struct") {
			// Extract struct name
			parts := strings.Split(line, "type ")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], " ")
			if len(nameParts) < 2 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			
			// Find struct body
			startLine := i
			endLine := findClosingBrace(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractGoDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "struct", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of struct
			i = endLine
		}
	}
	
	return chunks
}

// parsePythonFile mocks parsing a Python file
func (p *Parser) parsePythonFile(filePath string, lines []string) []*model.Chunk {
	var chunks []*model.Chunk
	
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		
		// Mock function detection
		if strings.HasPrefix(strings.TrimSpace(line), "def ") {
			// Extract function name
			parts := strings.Split(line, "def ")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], "(")
			if len(nameParts) < 2 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			
			// Find function body
			startLine := i
			endLine := findPythonBlockEnd(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract docstring if present
			doc := extractPythonDocstring(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "function", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of function
			i = endLine
		}
		
		// Mock class detection
		if strings.HasPrefix(strings.TrimSpace(line), "class ") {
			// Extract class name
			parts := strings.Split(line, "class ")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], "(")
			if len(nameParts) < 1 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			if strings.Contains(name, ":") {
				name = strings.Split(name, ":")[0]
			}
			
			// Find class body
			startLine := i
			endLine := findPythonBlockEnd(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract docstring if present
			doc := extractPythonDocstring(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "class", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of class
			i = endLine
		}
	}
	
	return chunks
}

// parseTypeScriptFile mocks parsing a TypeScript file
func (p *Parser) parseTypeScriptFile(filePath string, lines []string) []*model.Chunk {
	var chunks []*model.Chunk
	
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		
		// Mock function detection
		if strings.Contains(line, "function ") {
			// Extract function name
			parts := strings.Split(line, "function ")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], "(")
			if len(nameParts) < 2 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			
			// Find function body
			startLine := i
			endLine := findClosingBrace(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractJSDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "function", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of function
			i = endLine
		}
		
		// Mock class detection
		if strings.Contains(line, "class ") {
			// Extract class name
			parts := strings.Split(line, "class ")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], " ")
			if len(nameParts) < 1 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			
			// Find class body
			startLine := i
			endLine := findClosingBrace(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractJSDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "class", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of class
			i = endLine
		}
		
		// Mock method detection
		if strings.Contains(line, "(") && strings.Contains(line, ")") && 
		   !strings.Contains(line, "function") && !strings.Contains(line, "if") && 
		   !strings.Contains(line, "for") && !strings.Contains(line, "while") {
			
			trimmed := strings.TrimSpace(line)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") {
				continue
			}
			
			// Extract method name
			parts := strings.Split(line, "(")
			if len(parts) < 2 {
				continue
			}
			
			name := strings.TrimSpace(parts[0])
			
			// Find method body
			startLine := i
			endLine := findClosingBrace(lines, i)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractJSDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "method", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of method
			i = endLine
		}
	}
	
	return chunks
}

// parseCppFile mocks parsing a C++ file
func (p *Parser) parseCppFile(filePath string, lines []string) []*model.Chunk {
	var chunks []*model.Chunk
	
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		
		// Mock function detection
		if !strings.Contains(line, "class ") && !strings.Contains(line, "struct ") && 
		   strings.Contains(line, "(") && strings.Contains(line, ")") && 
		   !strings.HasPrefix(strings.TrimSpace(line), "if") && 
		   !strings.HasPrefix(strings.TrimSpace(line), "for") && 
		   !strings.HasPrefix(strings.TrimSpace(line), "while") {
			
			// Find opening brace
			braceIndex := i
			for braceIndex < len(lines) && !strings.Contains(lines[braceIndex], "{") {
				braceIndex++
			}
			
			if braceIndex >= len(lines) {
				continue
			}
			
			// Extract function name
			parts := strings.Split(line, "(")
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[0], " ")
			name := strings.TrimSpace(nameParts[len(nameParts)-1])
			
			// Find function body
			startLine := i
			endLine := findClosingBrace(lines, braceIndex)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractCppDocComment(lines, startLine)
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "function", name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of function
			i = endLine
		}
		
		// Mock class detection
		if strings.Contains(line, "class ") || strings.Contains(line, "struct ") {
			// Extract class name
			keyword := "class "
			if strings.Contains(line, "struct ") {
				keyword = "struct "
			}
			
			parts := strings.Split(line, keyword)
			if len(parts) < 2 {
				continue
			}
			
			nameParts := strings.Split(parts[1], " ")
			if len(nameParts) < 1 {
				continue
			}
			
			name := strings.TrimSpace(nameParts[0])
			if strings.Contains(name, ":") {
				name = strings.Split(name, ":")[0]
			}
			if strings.Contains(name, "{") {
				name = strings.Split(name, "{")[0]
			}
			
			// Find opening brace
			braceIndex := i
			for braceIndex < len(lines) && !strings.Contains(lines[braceIndex], "{") {
				braceIndex++
			}
			
			if braceIndex >= len(lines) {
				continue
			}
			
			// Find class body
			startLine := i
			endLine := findClosingBrace(lines, braceIndex)
			
			// Extract code
			code := strings.Join(lines[startLine:endLine+1], "\n")
			
			// Extract doc comment if present
			doc := extractCppDocComment(lines, startLine)
			
			// Create chunk
			symbolType := "class"
			if strings.Contains(line, "struct ") {
				symbolType = "struct"
			}
			chunk := createChunk(filePath, p.language, symbolType, name, doc, code, startLine, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of class
			i = endLine
		}
	}
	
	return chunks
}

// parseGenericFile provides a fallback parsing for unsupported languages
func (p *Parser) parseGenericFile(filePath string, lines []string) []*model.Chunk {
	var chunks []*model.Chunk
	
	// For generic files, we'll create chunks based on indentation patterns
	// and common programming constructs
	
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		
		// Look for potential function/method declarations
		if (strings.Contains(line, "(") && strings.Contains(line, ")")) ||
		   (strings.Contains(line, "function") || strings.Contains(line, "def ") || 
		    strings.Contains(line, "void ") || strings.Contains(line, "int ") || 
		    strings.Contains(line, "string ") || strings.Contains(line, "bool ")) {
			
			// Skip comments and control structures
			if strings.HasPrefix(line, "//") || strings.HasPrefix(line, "/*") || 
			   strings.HasPrefix(line, "#") || strings.HasPrefix(line, "if") || 
			   strings.HasPrefix(line, "for") || strings.HasPrefix(line, "while") {
				continue
			}
			
			// Find potential block start
			blockStart := i
			for blockStart < len(lines) && !strings.Contains(lines[blockStart], "{") && 
			    !strings.HasSuffix(strings.TrimSpace(lines[blockStart]), ":") {
				blockStart++
				if blockStart >= len(lines) {
					break
				}
			}
			
			if blockStart >= len(lines) {
				continue
			}
			
			// Find block end
			var endLine int
			if strings.Contains(lines[blockStart], "{") {
				endLine = findClosingBrace(lines, blockStart)
			} else {
				endLine = findIndentedBlockEnd(lines, blockStart)
			}
			
			// Extract name (best effort)
			name := "unknown"
			if strings.Contains(line, "(") {
				parts := strings.Split(line, "(")
				nameParts := strings.Split(parts[0], " ")
				name = strings.TrimSpace(nameParts[len(nameParts)-1])
			}
			
			// Extract code
			code := strings.Join(lines[i:endLine+1], "\n")
			
			// Create chunk
			chunk := createChunk(filePath, p.language, "function", name, "", code, i, endLine)
			chunks = append(chunks, chunk)
			
			// Skip to end of block
			i = endLine
		}
	}
	
	return chunks
}

// Helper functions

// findClosingBrace finds the line number of the closing brace that matches the opening brace
func findClosingBrace(lines []string, startLine int) int {
	braceCount := 0
	openingFound := false
	
	for i := startLine; i < len(lines); i++ {
		line := lines[i]
		
		for _, char := range line {
			if char == '{' {
				braceCount++
				openingFound = true
			} else if char == '}' {
				braceCount--
			}
		}
		
		if openingFound && braceCount == 0 {
			return i
		}
	}
	
	// If no matching closing brace is found, return the last line
	return len(lines) - 1
}

// findPythonBlockEnd finds the end of a Python block based on indentation
func findPythonBlockEnd(lines []string, startLine int) int {
	if startLine >= len(lines) {
		return startLine
	}
	
	// Get the indentation of the first line after the definition
	var baseIndent int
	if startLine+1 < len(lines) {
		baseIndent = countLeadingSpaces(lines[startLine+1])
	} else {
		return startLine
	}
	
	// Find the first line with less indentation
	for i := startLine + 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "" {
			continue
		}
		
		currentIndent := countLeadingSpaces(lines[i])
		if currentIndent < baseIndent {
			return i - 1
		}
	}
	
	// If no line with less indentation is found, return the last line
	return len(lines) - 1
}

// findIndentedBlockEnd finds the end of a block based on indentation
func findIndentedBlockEnd(lines []string, startLine int) int {
	if startLine >= len(lines) {
		return startLine
	}
	
	// Get the indentation of the first line after the definition
	var baseIndent int
	if startLine+1 < len(lines) {
		baseIndent = countLeadingSpaces(lines[startLine+1])
	} else {
		return startLine
	}
	
	// Find the first line with less indentation
	for i := startLine + 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "" {
			continue
		}
		
		currentIndent := countLeadingSpaces(lines[i])
		if currentIndent < baseIndent {
			return i - 1
		}
	}
	
	// If no line with less indentation is found, return the last line
	return len(lines) - 1
}

// countLeadingSpaces counts the number of leading spaces in a string
func countLeadingSpaces(s string) int {
	count := 0
	for _, char := range s {
		if char == ' ' {
			count++
		} else if char == '\t' {
			count += 4 // Count a tab as 4 spaces
		} else {
			break
		}
	}
	return count
}

// extractGoDocComment extracts the doc comment for a Go symbol
func extractGoDocComment(lines []string, symbolLine int) string {
	var docLines []string
	
	// Look for comments above the symbol
	for i := symbolLine - 1; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		
		if line == "" {
			break
		}
		
		if strings.HasPrefix(line, "//") {
			// Remove the comment marker and add to doc
			docLine := strings.TrimSpace(line[2:])
			docLines = append([]string{docLine}, docLines...)
		} else {
			break
		}
	}
	
	return strings.Join(docLines, "\n")
}

// extractPythonDocstring extracts the docstring for a Python symbol
func extractPythonDocstring(lines []string, symbolLine int) string {
	// Look for triple-quoted string after the symbol definition
	for i := symbolLine + 1; i < len(lines) && i < symbolLine + 5; i++ {
		line := strings.TrimSpace(lines[i])
		
		if strings.HasPrefix(line, "\"\"\"") || strings.HasPrefix(line, "'''") {
			// Find the end of the docstring
			marker := line[:3]
			startLine := i
			endLine := startLine
			
			// If the docstring is a single line
			if strings.HasSuffix(line, marker) && len(line) > 3 {
				return line[3:len(line)-3]
			}
			
			// Look for the closing marker
			for j := startLine + 1; j < len(lines); j++ {
				if strings.Contains(lines[j], marker) {
					endLine = j
					break
				}
			}
			
			// Extract the docstring
			if endLine > startLine {
				docLines := lines[startLine:endLine+1]
				
				// Remove the markers
				docLines[0] = strings.TrimPrefix(docLines[0], marker)
				docLines[len(docLines)-1] = strings.TrimSuffix(docLines[len(docLines)-1], marker)
				
				return strings.Join(docLines, "\n")
			}
		}
	}
	
	return ""
}

// extractJSDocComment extracts the JSDoc comment for a TypeScript/JavaScript symbol
func extractJSDocComment(lines []string, symbolLine int) string {
	var docLines []string
	
	// Look for comments above the symbol
	inComment := false
	for i := symbolLine - 1; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		
		if line == "" && !inComment {
			break
		}
		
		if strings.HasPrefix(line, "//") {
			// Single-line comment
			docLine := strings.TrimSpace(line[2:])
			docLines = append([]string{docLine}, docLines...)
		} else if strings.HasPrefix(line, "/*") && strings.HasSuffix(line, "*/") {
			// Single-line block comment
			docLine := strings.TrimSpace(line[2:len(line)-2])
			docLines = append([]string{docLine}, docLines...)
			break
		} else if strings.HasSuffix(line, "*/") {
			// End of multi-line comment
			docLine := strings.TrimSpace(strings.TrimSuffix(line, "*/"))
			docLines = append([]string{docLine}, docLines...)
			inComment = true
		} else if inComment {
			// Middle of multi-line comment
			docLines = append([]string{line}, docLines...)
		} else if strings.HasPrefix(line, "/*") {
			// Start of multi-line comment
			docLine := strings.TrimSpace(strings.TrimPrefix(line, "/*"))
			docLines = append([]string{docLine}, docLines...)
			inComment = true
			if strings.HasSuffix(docLine, "*/") {
				break
			}
		} else {
			break
		}
	}
	
	return strings.Join(docLines, "\n")
}

// extractCppDocComment extracts the doc comment for a C++ symbol
func extractCppDocComment(lines []string, symbolLine int) string {
	var docLines []string
	
	// Look for comments above the symbol
	inComment := false
	for i := symbolLine - 1; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		
		if line == "" && !inComment {
			break
		}
		
		if strings.HasPrefix(line, "//") {
			// Single-line comment
			docLine := strings.TrimSpace(line[2:])
			docLines = append([]string{docLine}, docLines...)
		} else if strings.HasPrefix(line, "/*") && strings.HasSuffix(line, "*/") {
			// Single-line block comment
			docLine := strings.TrimSpace(line[2:len(line)-2])
			docLines = append([]string{docLine}, docLines...)
			break
		} else if strings.HasSuffix(line, "*/") {
			// End of multi-line comment
			docLine := strings.TrimSpace(strings.TrimSuffix(line, "*/"))
			docLines = append([]string{docLine}, docLines...)
			inComment = true
		} else if inComment {
			// Middle of multi-line comment
			docLines = append([]string{line}, docLines...)
		} else if strings.HasPrefix(line, "/*") {
			// Start of multi-line comment
			docLine := strings.TrimSpace(strings.TrimPrefix(line, "/*"))
			docLines = append([]string{docLine}, docLines...)
			inComment = true
			if strings.HasSuffix(docLine, "*/") {
				break
			}
		} else {
			break
		}
	}
	
	return strings.Join(docLines, "\n")
}

// createChunk creates a new chunk with the given parameters
func createChunk(filePath, language, symbolType, symbolName, doc, code string, startLine, endLine int) *model.Chunk {
	// Create a hash of the code
	hash := sha256.Sum256([]byte(code))
	hashStr := hex.EncodeToString(hash[:])
	
	// Create a unique ID based on file path and code hash
	fileHash := sha256.Sum256([]byte(filePath))
	fileHashStr := hex.EncodeToString(fileHash[:8])
	id := fmt.Sprintf("%s-%s", fileHashStr, hashStr[:8])
	
	// Count tokens (simplified)
	tokens := countTokens(code)
	
	return &model.Chunk{
		ID:           id,
		FilePath:     filePath,
		Language:     language,
		SymbolType:   symbolType,
		SymbolName:   symbolName,
		Doc:          doc,
		Code:         code,
		StartLine:    startLine,
		EndLine:      endLine,
		Tokens:       tokens,
		Embedding:    nil, // Will be filled by the embedder
		IndexedAtUTC: time.Now().UTC(),
		Hash:         hashStr,
	}
}

// countTokens provides a simplified token count estimation
func countTokens(text string) int {
	// In a real implementation, this would use a proper tokenizer
	// For our mock, we'll use a simple approximation
	words := strings.Fields(text)
	return len(words)
}
