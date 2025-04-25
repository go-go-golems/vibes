package parser

import (
	"io/ioutil"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

// ExtractImports parses a Go file and extracts all import statements
func (p *GoFileParser) ExtractImports(filePath string) ([]ImportInfo, error) {
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
	
	// Create a query to find import declarations
	query, err := sitter.NewQuery([]byte(`
		(import_declaration 
			(import_spec_list
				(import_spec 
					path: [(interpreted_string_literal) (raw_string_literal)] @import_path 
					name: (package_identifier)? @import_alias
				)
			)
		)
		(import_declaration 
			(import_spec 
				path: [(interpreted_string_literal) (raw_string_literal)] @import_path 
				name: (package_identifier)? @import_alias
			)
		)
	`), p.language)
	if err != nil {
		return nil, err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	
	var imports []ImportInfo
	
	// Process query matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		var importPath string
		var importAlias string
		
		// Extract import path and alias
		for _, capture := range match.Captures {
			switch capture.Index {
			case 0: // import path
				// Remove quotes from import path
				path := string(content[capture.Node.StartByte()+1 : capture.Node.EndByte()-1])
				importPath = path
			case 1: // import alias (optional)
				importAlias = string(content[capture.Node.StartByte():capture.Node.EndByte()])
			}
		}
		
		if importPath != "" {
			importInfo := ImportInfo{
				Path:  importPath,
				Alias: importAlias,
			}
			imports = append(imports, importInfo)
		}
	}
	
	return imports, nil
}

// ExtractPackageName extracts the package name from a Go file
func (p *GoFileParser) ExtractPackageName(filePath string) (string, error) {
	// Read file content
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return "", err
	}
	
	// Parse file with Tree-sitter
	tree, err := p.parser.ParseCtx(nil, nil, content)
	if err != nil {
		return "", err
	}
	defer tree.Close()
	
	// Create a query to find the package declaration
	query, err := sitter.NewQuery([]byte(`
		(package_clause (package_identifier) @package_name)
	`), p.language)
	if err != nil {
		return "", err
	}
	
	cursor := sitter.NewQueryCursor()
	cursor.Exec(query, tree.RootNode())
	
	// Process query matches
	for {
		match, ok := cursor.NextMatch()
		if !ok {
			break
		}
		
		for _, capture := range match.Captures {
			if capture.Index == 0 {
				// Extract package name
				return string(content[capture.Node.StartByte():capture.Node.EndByte()]), nil
			}
		}
	}
	
	return "", nil // No package name found
}

// AnalyzeFileStructure analyzes a Go file and returns its structure
func (p *GoFileParser) AnalyzeFileStructure(filePath string) (*FileStructure, error) {
	// Extract functions
	functions, err := p.ParseFile(filePath)
	if err != nil {
		return nil, err
	}
	
	// Extract imports
	imports, err := p.ExtractImports(filePath)
	if err != nil {
		return nil, err
	}
	
	// Extract package name
	packageName, err := p.ExtractPackageName(filePath)
	if err != nil {
		return nil, err
	}
	
	// Create file structure
	fileStructure := &FileStructure{
		Path:      filePath,
		Functions: functions,
		Imports:   imports,
		Package:   packageName,
	}
	
	return fileStructure, nil
}