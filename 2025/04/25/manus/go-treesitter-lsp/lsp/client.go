package lsp

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
)

// Reference represents a reference to a function
type Reference struct {
	FilePath  string `json:"file_path"`
	Line      int    `json:"line"`
	Column    int    `json:"column"`
	Context   string `json:"context"`
	Type      string `json:"type"` // "definition" or "usage"
}

// Client represents a custom LSP-like client for finding function references
type Client struct {
	fileSet *token.FileSet
}

// NewClient creates a new LSP client
func NewClient() *Client {
	return &Client{
		fileSet: token.NewFileSet(),
	}
}

// FindReferences finds all references (usages and definitions) of a function in a directory
func (c *Client) FindReferences(funcName string, dirPath string) ([]Reference, error) {
	var references []Reference

	// Walk through all Go files in the directory
	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".go") {
			// Parse the Go file
			fileReferences, err := c.findReferencesInFile(funcName, path)
			if err != nil {
				fmt.Printf("Warning: Error processing file %s: %v\n", path, err)
				return nil // Continue with other files
			}
			references = append(references, fileReferences...)
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to walk directory %s: %v", dirPath, err)
	}

	return references, nil
}

// findReferencesInFile finds all references of a function in a single file
func (c *Client) findReferencesInFile(funcName string, filePath string) ([]Reference, error) {
	var references []Reference

	// Parse the Go file
	node, err := parser.ParseFile(c.fileSet, filePath, nil, parser.AllErrors)
	if err != nil {
		return nil, fmt.Errorf("failed to parse file %s: %v", filePath, err)
	}

	// Read file content for context extraction
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	lines := strings.Split(string(content), "\n")

	// Check for function definitions
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			// Check if this is the function we're looking for
			if x.Name.Name == funcName {
				pos := c.fileSet.Position(x.Pos())
				
				// Extract context (the function signature line)
				lineIndex := pos.Line - 1 // Convert to 0-based index
				context := ""
				if lineIndex >= 0 && lineIndex < len(lines) {
					context = strings.TrimSpace(lines[lineIndex])
				}
				
				references = append(references, Reference{
					FilePath: filePath,
					Line:     pos.Line,
					Column:   pos.Column,
					Context:  context,
					Type:     "definition",
				})
			}
		case *ast.CallExpr:
			// Check for function calls
			if ident, ok := x.Fun.(*ast.Ident); ok && ident.Name == funcName {
				pos := c.fileSet.Position(ident.Pos())
				
				// Extract context (the line containing the function call)
				lineIndex := pos.Line - 1 // Convert to 0-based index
				context := ""
				if lineIndex >= 0 && lineIndex < len(lines) {
					context = strings.TrimSpace(lines[lineIndex])
				}
				
				references = append(references, Reference{
					FilePath: filePath,
					Line:     pos.Line,
					Column:   pos.Column,
					Context:  context,
					Type:     "usage",
				})
			}
		}
		return true
	})

	return references, nil
}

// FindAllFunctionReferences finds references for all functions in a directory
func (c *Client) FindAllFunctionReferences(functions []string, dirPath string) (map[string][]Reference, error) {
	result := make(map[string][]Reference)
	
	for _, funcName := range functions {
		references, err := c.FindReferences(funcName, dirPath)
		if err != nil {
			return nil, fmt.Errorf("failed to find references for function %s: %v", funcName, err)
		}
		result[funcName] = references
	}
	
	return result, nil
}

// FindMethodReferences finds references to a method (receiver.method)
func (c *Client) FindMethodReferences(receiverType, methodName, dirPath string) ([]Reference, error) {
	var references []Reference

	// Walk through all Go files in the directory
	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".go") {
			// Parse the Go file
			fileReferences, err := c.findMethodReferencesInFile(receiverType, methodName, path)
			if err != nil {
				fmt.Printf("Warning: Error processing file %s: %v\n", path, err)
				return nil // Continue with other files
			}
			references = append(references, fileReferences...)
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to walk directory %s: %v", dirPath, err)
	}

	return references, nil
}

// findMethodReferencesInFile finds all references of a method in a single file
func (c *Client) findMethodReferencesInFile(receiverType, methodName, filePath string) ([]Reference, error) {
	var references []Reference

	// Parse the Go file
	node, err := parser.ParseFile(c.fileSet, filePath, nil, parser.AllErrors)
	if err != nil {
		return nil, fmt.Errorf("failed to parse file %s: %v", filePath, err)
	}

	// Read file content for context extraction
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	lines := strings.Split(string(content), "\n")

	// Check for method definitions
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			// Check if this is the method we're looking for
			if x.Recv != nil && x.Name.Name == methodName {
				// Check receiver type
				if len(x.Recv.List) > 0 {
					recvType := ""
					switch rt := x.Recv.List[0].Type.(type) {
					case *ast.StarExpr:
						if ident, ok := rt.X.(*ast.Ident); ok {
							recvType = "*" + ident.Name
						}
					case *ast.Ident:
						recvType = rt.Name
					}
					
					// If receiver type matches
					if recvType == receiverType || strings.TrimPrefix(recvType, "*") == strings.TrimPrefix(receiverType, "*") {
						pos := c.fileSet.Position(x.Pos())
						
						// Extract context
						lineIndex := pos.Line - 1
						context := ""
						if lineIndex >= 0 && lineIndex < len(lines) {
							context = strings.TrimSpace(lines[lineIndex])
						}
						
						references = append(references, Reference{
							FilePath: filePath,
							Line:     pos.Line,
							Column:   pos.Column,
							Context:  context,
							Type:     "definition",
						})
					}
				}
			}
		case *ast.SelectorExpr:
			// Check for method calls
			if x.Sel.Name == methodName {
				// We can't easily determine the exact type of the receiver here
				// without type checking, but we can still record the reference
				pos := c.fileSet.Position(x.Sel.Pos())
				
				// Extract context
				lineIndex := pos.Line - 1
				context := ""
				if lineIndex >= 0 && lineIndex < len(lines) {
					context = strings.TrimSpace(lines[lineIndex])
				}
				
				references = append(references, Reference{
					FilePath: filePath,
					Line:     pos.Line,
					Column:   pos.Column,
					Context:  context,
					Type:     "usage",
				})
			}
		}
		return true
	})

	return references, nil
}
