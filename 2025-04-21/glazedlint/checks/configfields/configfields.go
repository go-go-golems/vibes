// Package configfields implements a linter to flag deprecated fields in Config struct
package configfields

import (
	"go/ast"

	"golang.org/x/tools/go/analysis"
)

// Analyzer is the main analyzer for the configfields linter
var Analyzer = &analysis.Analyzer{
	Name: "configfields",
	Doc:  "flag deprecated fields in Config struct",
	Run:  run,
}

// Map of deprecated field names to explanations
var unwanted = map[string]string{
	"ConfigPath":   "moved to file-based settings; delete this field",
	"Repositories": "renamed to Repos in v0.3; update struct tags",
}

func run(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			// Look for struct type declarations
			typeSpec, ok := n.(*ast.TypeSpec)
			if !ok {
				return true
			}

			// Check if it's a struct
			structType, ok := typeSpec.Type.(*ast.StructType)
			if !ok {
				return true
			}

			// Check if it's named Config
			if typeSpec.Name.Name == "Config" {
				// Check each field
				for _, field := range structType.Fields.List {
					for _, name := range field.Names {
						if reason, deprecated := unwanted[name.Name]; deprecated {
							pass.Reportf(name.Pos(), 
								"field %s in Config is deprecated: %s", 
								name.Name, reason)
						}
					}
				}
			}
			return true
		})
	}
	return nil, nil
}
