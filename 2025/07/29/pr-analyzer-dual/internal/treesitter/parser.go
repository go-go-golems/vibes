package treesitter

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// Parser wraps tree-sitter functionality for Go code analysis
type Parser struct {
	parser *sitter.Parser
}

// NewParser creates a new tree-sitter parser for Go
func NewParser() *Parser {
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())

	return &Parser{parser: parser}
}

// Function represents a Go function found in the code
type Function struct {
	Name       string
	StartLine  int
	EndLine    int
	StartByte  int
	EndByte    int
	Signature  string
	Body       string
	Receiver   string // For methods
	IsExported bool
}

// ParseCode parses Go source code and returns the AST
func (p *Parser) ParseCode(sourceCode []byte) (*sitter.Tree, error) {
	tree, err := p.parser.ParseCtx(context.Background(), nil, sourceCode)
	if err != nil {
		return nil, fmt.Errorf("failed to parse code: %w", err)
	}
	return tree, nil
}

// ExtractFunctions extracts all function declarations from Go source code
func (p *Parser) ExtractFunctions(sourceCode []byte) ([]*Function, error) {
	tree, err := p.ParseCode(sourceCode)
	if err != nil {
		return nil, err
	}
	defer tree.Close()

	var functions []*Function

	// Use a simpler approach - traverse the tree manually
	p.traverseNode(tree.RootNode(), sourceCode, &functions)

	return functions, nil
}

// traverseNode recursively traverses the AST to find function and method declarations
func (p *Parser) traverseNode(node *sitter.Node, sourceCode []byte, functions *[]*Function) {
	nodeType := node.Type()

	if nodeType == "function_declaration" {
		fn := p.extractFunctionFromNode(node, sourceCode)
		if fn != nil {
			*functions = append(*functions, fn)
		}
	} else if nodeType == "method_declaration" {
		fn := p.extractMethodFromNode(node, sourceCode)
		if fn != nil {
			*functions = append(*functions, fn)
		}
	}

	// Recursively traverse child nodes
	for i := 0; i < int(node.ChildCount()); i++ {
		child := node.Child(i)
		if child != nil {
			p.traverseNode(child, sourceCode, functions)
		}
	}
}

// extractFunctionFromNode extracts function information from a function_declaration node
func (p *Parser) extractFunctionFromNode(node *sitter.Node, sourceCode []byte) *Function {
	if node.Type() != "function_declaration" {
		return nil
	}

	var name string
	nameNode := node.ChildByFieldName("name")
	if nameNode != nil {
		name = nameNode.Content(sourceCode)
	}

	startPoint := node.StartPoint()
	endPoint := node.EndPoint()

	signature := p.extractSignature(node, sourceCode)
	body := p.extractBody(node, sourceCode)

	return &Function{
		Name:       name,
		StartLine:  int(startPoint.Row) + 1, // tree-sitter uses 0-based indexing
		EndLine:    int(endPoint.Row) + 1,
		StartByte:  int(node.StartByte()),
		EndByte:    int(node.EndByte()),
		Signature:  signature,
		Body:       body,
		IsExported: isExported(name),
	}
}

// extractMethodFromNode extracts method information from a method_declaration node
func (p *Parser) extractMethodFromNode(node *sitter.Node, sourceCode []byte) *Function {
	if node.Type() != "method_declaration" {
		return nil
	}

	var name, receiver string
	nameNode := node.ChildByFieldName("name")
	if nameNode != nil {
		name = nameNode.Content(sourceCode)
	}

	receiverNode := node.ChildByFieldName("receiver")
	if receiverNode != nil {
		receiver = receiverNode.Content(sourceCode)
	}

	startPoint := node.StartPoint()
	endPoint := node.EndPoint()

	signature := p.extractSignature(node, sourceCode)
	body := p.extractBody(node, sourceCode)

	return &Function{
		Name:       name,
		StartLine:  int(startPoint.Row) + 1,
		EndLine:    int(endPoint.Row) + 1,
		StartByte:  int(node.StartByte()),
		EndByte:    int(node.EndByte()),
		Signature:  signature,
		Body:       body,
		Receiver:   receiver,
		IsExported: isExported(name),
	}
}

// extractSignature extracts the function signature
func (p *Parser) extractSignature(node *sitter.Node, sourceCode []byte) string {
	// Find the body node and extract everything before it
	bodyNode := node.ChildByFieldName("body")
	if bodyNode == nil {
		return node.Content(sourceCode)
	}

	// Extract from start of function to start of body
	startByte := node.StartByte()
	endByte := bodyNode.StartByte()

	if endByte > startByte {
		return string(sourceCode[startByte:endByte])
	}

	return node.Content(sourceCode)
}

// extractBody extracts the function body
func (p *Parser) extractBody(node *sitter.Node, sourceCode []byte) string {
	bodyNode := node.ChildByFieldName("body")
	if bodyNode == nil {
		return ""
	}
	return bodyNode.Content(sourceCode)
}

// isExported checks if a function name is exported (starts with uppercase)
func isExported(name string) bool {
	if len(name) == 0 {
		return false
	}
	return name[0] >= 'A' && name[0] <= 'Z'
}

// FindFunctionAtLine finds the function that contains the given line number
func (p *Parser) FindFunctionAtLine(functions []*Function, line int) *Function {
	for _, fn := range functions {
		if line >= fn.StartLine && line <= fn.EndLine {
			return fn
		}
	}
	return nil
}

// GetChangedFunctions analyzes a diff and returns functions that were modified
func (p *Parser) GetChangedFunctions(sourceCode []byte, changedLines []int) ([]*Function, error) {
	functions, err := p.ExtractFunctions(sourceCode)
	if err != nil {
		return nil, err
	}

	var changedFunctions []*Function
	functionMap := make(map[string]*Function)

	// Find functions that contain changed lines
	for _, line := range changedLines {
		fn := p.FindFunctionAtLine(functions, line)
		if fn != nil {
			// Use function signature as key to avoid duplicates
			key := fmt.Sprintf("%s:%d", fn.Name, fn.StartLine)
			if _, exists := functionMap[key]; !exists {
				functionMap[key] = fn
				changedFunctions = append(changedFunctions, fn)
			}
		}
	}

	return changedFunctions, nil
}
