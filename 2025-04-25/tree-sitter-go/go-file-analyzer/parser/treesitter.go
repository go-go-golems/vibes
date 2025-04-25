package parser

import (
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FunctionInfo represents information about a function
type FunctionInfo struct {
	Name      string `json:"name"`
	Filepath  string `json:"filepath"`
	StartLine int    `json:"startLine"`
	EndLine   int    `json:"endLine"`
	StartCol  int    `json:"startCol"`
	EndCol    int    `json:"endCol"`
}

// GoFileParser represents a parser for Go files using Tree-sitter
type GoFileParser struct {
	parser    *sitter.Parser
	language  *sitter.Language
	queryStmt string
}

// NewGoFileParser creates a new parser for Go files
func NewGoFileParser() *GoFileParser {
	parser := sitter.NewParser()
	
	// Set Go language
	language := golang.GetLanguage()
	parser.SetLanguage(language)
	
	// Define the query to find function declarations and method declarations
	queryStmt := `
		(function_declaration name: (identifier) @func_name) @function
		(method_declaration name: (field_identifier) @method_name) @method
	`
	
	return &GoFileParser{
		parser:    parser,
		language:  language,
		queryStmt: queryStmt,
	}
}

// ParseFile parses a Go file and returns information about all functions
func (p *GoFileParser) ParseFile(filePath string) ([]FunctionInfo, error) {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	
	// Parse file with Tree-sitter
	tree, err := p.parser.ParseCtx(nil, nil, content)
	if err != nil {
		return nil, err
	}
	defer tree.Close()
	
	// Create a query to find function declarations
	query, err := sitter.NewQuery([]byte(p.queryStmt), p.language)
	if err != nil {
		return nil, err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	
	var functions []FunctionInfo
	
	// Process query matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		var functionNode *sitter.Node
		var nameNode *sitter.Node
		
		// Extract function name and node
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // function or method node
				functionNode = capture.Node
			case 1: // function or method name
				nameNode = capture.Node
			}
		}
		
		if functionNode != nil && nameNode != nil {
			startPoint := functionNode.StartPoint()
			endPoint := functionNode.EndPoint()
			
			// Extract function name from source
			name := string(content[nameNode.StartByte():nameNode.EndByte()])
			
			// Create function info
			functionInfo := FunctionInfo{
				Name:      name,
				Filepath:  filePath,
				StartLine: int(startPoint.Row) + 1, // Convert to 1-based line numbering
				EndLine:   int(endPoint.Row) + 1,   // Convert to 1-based line numbering
				StartCol:  int(startPoint.Column),
				EndCol:    int(endPoint.Column),
			}
			
			functions = append(functions, functionInfo)
		}
	}
	
	return functions, nil
}

// ParseDirectory recursively parses all Go files in a directory and its subdirectories
func (p *GoFileParser) ParseDirectory(dirPath string) ([]FunctionInfo, error) {
	var allFunctions []FunctionInfo
	
	// Walk the directory and process all Go files
	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			log.Printf("Error accessing path %s: %v", path, err)
			return nil // Continue walking
		}
		
		// Skip directories
		if info.IsDir() {
			return nil
		}
		
		// Only process Go files
		if filepath.Ext(path) != ".go" {
			return nil
		}
		
		// Parse the file
		functions, err := p.ParseFile(path)
		if err != nil {
			log.Printf("Error parsing file %s: %v", path, err)
			return nil // Continue walking
		}
		
		// Add functions to the result
		allFunctions = append(allFunctions, functions...)
		return nil
	})
	
	if err != nil {
		return nil, err
	}
	
	return allFunctions, nil
}

// ParsePath parses a file or directory path and returns all functions
func (p *GoFileParser) ParsePath(path string) ([]FunctionInfo, error) {
	// Check if path is a directory or a file
	fileInfo, err := os.Stat(path)
	if err != nil {
		return nil, err
	}
	
	if fileInfo.IsDir() {
		return p.ParseDirectory(path)
	} else if filepath.Ext(path) == ".go" {
		return p.ParseFile(path)
	}
	
	// Return empty slice for non-Go files
	return []FunctionInfo{}, nil
}