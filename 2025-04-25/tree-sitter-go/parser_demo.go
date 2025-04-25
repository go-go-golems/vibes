package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// EnhancedFunctionInfo represents detailed information about a function
type EnhancedFunctionInfo struct {
	Name       string           `json:"name"`
	Filepath   string           `json:"filepath"`
	StartLine  int              `json:"startLine"`
	EndLine    int              `json:"endLine"`
	StartCol   int              `json:"startCol"`
	EndCol     int              `json:"endCol"`
	IsExported bool             `json:"isExported"`
	IsMethod   bool             `json:"isMethod"`
	Receiver   *ReceiverInfo    `json:"receiver,omitempty"`
	Parameters []ParameterInfo  `json:"parameters"`
	ReturnType []string         `json:"returnType"`
	Comments   string           `json:"comments,omitempty"`
	Package    string           `json:"package"`
}

// ReceiverInfo represents information about a method receiver
type ReceiverInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// ParameterInfo represents a function parameter
type ParameterInfo struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

func main() {
	// Parse command line arguments
	if len(os.Args) < 2 {
		fmt.Println("Usage: parser_demo <go-file-path>")
		os.Exit(1)
	}
	
	filePath := os.Args[1]
	
	// Check if the file exists
	_, err := os.Stat(filePath)
	if err != nil {
		log.Fatalf("Error accessing file %s: %v", filePath, err)
	}
	
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Fatalf("Error reading file: %v", err)
	}
	
	// Create Tree-sitter parser for Go
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())
	
	// Parse file with Tree-sitter
	tree, err := parser.ParseCtx(nil, nil, content)
	if err != nil {
		log.Fatalf("Error parsing file: %v", err)
	}
	defer tree.Close()
	
	// Define the query to find function declarations
	funcQuery := `
		(function_declaration
			name: (identifier) @func_name
			parameters: (parameter_list) @parameters
			result: [
				(type_identifier) @return_type
				(parameter_list) @return_params
			]?
		) @function
	`
	
	// Define the query to find method declarations
	methodQuery := `
		(method_declaration
			receiver: (parameter_list
				(parameter_declaration
					name: (identifier)? @receiver_name
					type: [
						(type_identifier) @receiver_type
						(pointer_type
							(type_identifier) @receiver_pointer_type)
					]
				)
			)
			name: (field_identifier) @method_name
			parameters: (parameter_list) @parameters
			result: [
				(type_identifier) @return_type
				(parameter_list) @return_params
			]?
		) @method
	`
	
	// Create a query to find function declarations
	query, err := sitter.NewQuery([]byte(funcQuery), golang.GetLanguage())
	if err != nil {
		log.Fatalf("Error creating function query: %v", err)
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	
	var functions []EnhancedFunctionInfo
	
	// Process function matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		var funcName string
		var funcNode *sitter.Node
		var paramNode *sitter.Node
		var returnTypeNode *sitter.Node
		
		// Extract function information from captures
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // func_name
				funcName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 1: // parameters
				paramNode = capture.Node
			case 2: // return_type
				returnTypeNode = capture.Node
			case 3: // function
				funcNode = capture.Node
			}
		}
		
		if funcNode != nil && funcName != "" {
			// Get function position
			startPoint := funcNode.StartPoint()
			endPoint := funcNode.EndPoint()
			
			// Determine if function is exported
			isExported := len(funcName) > 0 && funcName[0] >= 'A' && funcName[0] <= 'Z'
			
			// Create function info
			functionInfo := EnhancedFunctionInfo{
				Name:       funcName,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1,
				EndLine:    int(endPoint.Row) + 1,
				StartCol:   int(startPoint.Column),
				EndCol:     int(endPoint.Column),
				IsExported: isExported,
				IsMethod:   false,
			}
			
			// Add return type if available
			if returnTypeNode != nil {
				returnType := string(content[returnTypeNode.StartByte():returnTypeNode.EndByte()])
				functionInfo.ReturnType = []string{returnType}
			}
			
			functions = append(functions, functionInfo)
		}
	}
	
	// Create a query to find method declarations
	methodQueryObj, err := sitter.NewQuery([]byte(methodQuery), golang.GetLanguage())
	if err != nil {
		log.Fatalf("Error creating method query: %v", err)
	}
	
	cursor = sitter.NewQueryCursor()
	cursor.Exec(methodQueryObj, tree.RootNode())
	
	// Process method matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		var methodName string
		var methodNode *sitter.Node
		var receiverName string
		var receiverType string
		var receiverPointerType string
		
		// Extract method information from captures
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // receiver_name
				if capture.Node != nil {
					receiverName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
				}
			case 1: // receiver_type
				receiverType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 2: // receiver_pointer_type
				receiverPointerType = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 3: // method_name
				methodName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			case 7: // method
				methodNode = capture.Node
			}
		}
		
		if methodNode != nil && methodName != "" {
			// Get method position
			startPoint := methodNode.StartPoint()
			endPoint := methodNode.EndPoint()
			
			// Determine if method is exported
			isExported := len(methodName) > 0 && methodName[0] >= 'A' && methodName[0] <= 'Z'
			
			// Use pointer type if available
			finalReceiverType := receiverType
			if receiverPointerType != "" {
				finalReceiverType = "*" + receiverPointerType
			}
			
			// Create method info
			methodInfo := EnhancedFunctionInfo{
				Name:       methodName,
				Filepath:   filePath,
				StartLine:  int(startPoint.Row) + 1,
				EndLine:    int(endPoint.Row) + 1,
				StartCol:   int(startPoint.Column),
				EndCol:     int(endPoint.Column),
				IsExported: isExported,
				IsMethod:   true,
				Receiver: &ReceiverInfo{
					Name: receiverName,
					Type: finalReceiverType,
				},
			}
			
			functions = append(functions, methodInfo)
		}
	}
	
	// Print results
	fmt.Printf("Found %d functions/methods in %s\n", len(functions), filePath)
	
	// Output as JSON
	jsonData, err := json.MarshalIndent(functions, "", "  ")
	if err != nil {
		log.Fatalf("Error marshaling to JSON: %v", err)
	}
	
	fmt.Println(string(jsonData))
	
	// Write to output file
	outputPath := "parser_demo_result.json"
	err = ioutil.WriteFile(outputPath, jsonData, 0644)
	if err != nil {
		log.Fatalf("Error writing output file: %v", err)
	}
	
	fmt.Printf("Results written to %s\n", outputPath)
}