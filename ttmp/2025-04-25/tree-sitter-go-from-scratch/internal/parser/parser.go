package parser

import (
	"context"
	"io/ioutil"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

type FunctionInfo struct {
	Name      string `json:"name"`
	Filepath  string `json:"filepath"`
	StartLine int    `json:"startLine"`
	EndLine   int    `json:"endLine"`
}

type Parser struct {
	parser   *sitter.Parser
	language *sitter.Language
	// We can store compiled queries here later for efficiency
}

func NewParser() *Parser {
	p := sitter.NewParser()
	lang := golang.GetLanguage()
	p.SetLanguage(lang)
	return &Parser{parser: p, language: lang}
}

func (p *Parser) ParseFile(ctx context.Context, filePath string) ([]FunctionInfo, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, err // Consider wrapping errors with github.com/pkg/errors later
	}

	tree, err := p.parser.ParseCtx(ctx, nil, content)
	if err != nil {
		return nil, err
	}
	defer tree.Close()

	// Query to find function declarations and capture the name and the whole declaration block
	queryStr := `(function_declaration name: (identifier) @func_name) @function`
	query, err := sitter.NewQuery([]byte(queryStr), p.language)
	if err != nil {
		return nil, err
	}
	defer query.Close()

	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	defer cursor.Close()

	var functions []FunctionInfo
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break // No more matches
		}

		// Apply predicates if any (none in this simple query)
		match = cursor.FilterPredicates(match, content)
		if len(match.Captures) == 0 {
			continue // Should not happen with this query if match was ok
		}

		var funcName string
		var funcNode *sitter.Node

		for _, capture := range match.Captures {
			captureName := query.CaptureNameForId(capture.Index)
			node := capture.Node

			if captureName == "func_name" {
				funcName = node.Content(content)
			} else if captureName == "function" {
				funcNode = node
			}
		}

		if funcNode != nil && funcName != "" {
			start := funcNode.StartPoint()
			end := funcNode.EndPoint()
			functions = append(functions, FunctionInfo{
				Name:      funcName,
				Filepath:  filePath,           // Store the path for context
				StartLine: int(start.Row) + 1, // Tree-sitter rows are 0-based
				EndLine:   int(end.Row) + 1,
			})
		}
	}

	return functions, nil
}
