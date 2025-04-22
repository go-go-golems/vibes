// Package parselayers implements a linter to detect mis-imports of cmds.ParsedLayers and cmds.DefaultSlug
package parselayers

import (
	"go/ast"

	"golang.org/x/tools/go/analysis"
)

// Analyzer is the main analyzer for the parselayers linter
var Analyzer = &analysis.Analyzer{
	Name: "parselayers",
	Doc:  "detect use of cmds.ParsedLayers / cmds.DefaultSlug (moved to layers pkg)",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			sel, ok := n.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			if ident, ok := sel.X.(*ast.Ident); ok && ident.Name == "cmds" {
				if sel.Sel.Name == "ParsedLayers" || sel.Sel.Name == "DefaultSlug" {
					pass.Reportf(sel.Pos(),
						"use layers.%s instead of cmds.%s (glazed refactor)",
						sel.Sel.Name, sel.Sel.Name)
				}
			}
			return true
		})
	}
	return nil, nil
}
