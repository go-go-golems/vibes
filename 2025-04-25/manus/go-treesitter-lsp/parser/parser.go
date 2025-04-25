package parser

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// FunctionInfo represents information about a Go function
type FunctionInfo struct {
	Name       string `json:"name"`
	FilePath   string `json:"file_path"`
	StartLine  uint32 `json:"start_line"`
	EndLine    uint32 `json:"end_line"`
	StartByte  uint32 `json:"start_byte"`
	EndByte    uint32 `json:"end_byte"`
	Parameters string `json:"parameters"`
	ReturnType string `json:"return_type,omitempty"`
}

// Parser represents a Go file parser using tree-sitter
type Parser struct {
	parser *sitter.Parser
}

// NewParser creates a new Parser instance
func NewParser() *Parser {
	parser := sitter.NewParser()
	
	// Explicitly initialize the Go language
	language := golang.GetLanguage()
	if language == nil {
		fmt.Println("Error: Failed to initialize Go language for tree-sitter")
		// Fallback to avoid nil pointer dereference
		language = &sitter.Language{}
	}
	
	parser.SetLanguage(language)
	return &Parser{
		parser: parser,
	}
}

// ParseFile parses a Go file and returns function information
func (p *Parser) ParseFile(filePath string) ([]FunctionInfo, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %v", filePath, err)
	}

	tree, err := p.parser.ParseCtx(nil, nil, content)
	if err != nil {
		return nil, fmt.Errorf("failed to parse file %s: %v", filePath, err)
	}
	defer tree.Close()

	root := tree.RootNode()
	functions := p.extractFunctions(root, content, filePath)
	return functions, nil
}

// ParseDirectory recursively parses all Go files in a directory
func (p *Parser) ParseDirectory(dirPath string) ([]FunctionInfo, error) {
	var allFunctions []FunctionInfo

	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".go") {
			functions, err := p.ParseFile(path)
			if err != nil {
				return err
			}
			allFunctions = append(allFunctions, functions...)
		}
		return nil
	})

	if err != nil {
		return nil, fmt.Errorf("failed to walk directory %s: %v", dirPath, err)
	}

	return allFunctions, nil
}

// extractFunctions extracts function information from the syntax tree
func (p *Parser) extractFunctions(node *sitter.Node, content []byte, filePath string) []FunctionInfo {
	var functions []FunctionInfo

	// Query to find function declarations
	query, err := sitter.NewQuery([]byte(`
		(function_declaration
			name: (identifier) @func.name
			parameters: (parameter_list) @func.params
			result: [
				(parameter_list) @func.return
				(type_identifier) @func.return
				(qualified_type) @func.return
			]?) @func.declaration
	`), golang.GetLanguage())
	if err != nil {
		fmt.Printf("Error creating query: %v\n", err)
		return functions
	}

	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, node)

	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var funcName, funcParams, funcReturn string
		var funcNode *sitter.Node

		for _, capture := range match.Captures {
			captureNode := capture.Node
			name := query.CaptureNameForId(capture.Index)

			switch name {
			case "func.name":
				funcName = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "func.params":
				funcParams = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "func.return":
				funcReturn = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "func.declaration":
				funcNode = captureNode
			}
		}

		if funcNode != nil {
			function := FunctionInfo{
				Name:       funcName,
				FilePath:   filePath,
				StartLine:  funcNode.StartPoint().Row + 1, // Convert to 1-based line numbers
				EndLine:    funcNode.EndPoint().Row + 1,
				StartByte:  funcNode.StartByte(),
				EndByte:    funcNode.EndByte(),
				Parameters: funcParams,
				ReturnType: funcReturn,
			}
			functions = append(functions, function)
		}
	}

	// Also find method declarations
	methodQuery, err := sitter.NewQuery([]byte(`
		(method_declaration
			receiver: (parameter_list) @method.receiver
			name: (field_identifier) @method.name
			parameters: (parameter_list) @method.params
			result: [
				(parameter_list) @method.return
				(type_identifier) @method.return
				(qualified_type) @method.return
			]?) @method.declaration
	`), golang.GetLanguage())
	if err != nil {
		fmt.Printf("Error creating method query: %v\n", err)
		return functions
	}

	cursor = sitter.NewQueryCursor()
	cursor.Exec(methodQuery, node)

	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}

		var methodName, methodParams, methodReturn, methodReceiver string
		var methodNode *sitter.Node

		for _, capture := range match.Captures {
			captureNode := capture.Node
			name := methodQuery.CaptureNameForId(capture.Index)

			switch name {
			case "method.name":
				methodName = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "method.params":
				methodParams = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "method.return":
				methodReturn = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "method.receiver":
				methodReceiver = string(content[captureNode.StartByte():captureNode.EndByte()])
			case "method.declaration":
				methodNode = captureNode
			}
		}

		if methodNode != nil {
			// Format method name with receiver for clarity
			receiverType := extractReceiverType(methodReceiver)
			fullMethodName := fmt.Sprintf("(%s).%s", receiverType, methodName)
			
			function := FunctionInfo{
				Name:       fullMethodName,
				FilePath:   filePath,
				StartLine:  methodNode.StartPoint().Row + 1,
				EndLine:    methodNode.EndPoint().Row + 1,
				StartByte:  methodNode.StartByte(),
				EndByte:    methodNode.EndByte(),
				Parameters: methodParams,
				ReturnType: methodReturn,
			}
			functions = append(functions, function)
		}
	}

	return functions
}

// extractReceiverType extracts the type from a method receiver parameter list
func extractReceiverType(receiver string) string {
	// Simple extraction - this could be improved for complex receivers
	receiver = strings.TrimSpace(receiver)
	receiver = strings.TrimPrefix(receiver, "(")
	receiver = strings.TrimSuffix(receiver, ")")
	
	parts := strings.Fields(receiver)
	if len(parts) > 1 {
		// Handle pointer receivers
		if strings.HasPrefix(parts[1], "*") {
			return parts[1]
		}
		return parts[1]
	} else if len(parts) == 1 {
		// Handle pointer receivers without variable name
		if strings.HasPrefix(parts[0], "*") {
			return parts[0]
		}
		return parts[0]
	}
	
	return "unknown"
}
