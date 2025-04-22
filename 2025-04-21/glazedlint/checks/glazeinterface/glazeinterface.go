// Package glazeinterface implements a linter to ensure GlazeCommand implementors define RunIntoGlazeProcessor
package glazeinterface

import (
	"go/ast"
	"go/token"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// Analyzer is the main analyzer for the glazeinterface linter
var Analyzer = &analysis.Analyzer{
	Name: "glazeinterface",
	Doc:  "verify that XxxCmd types implement both Run and RunIntoGlazeProcessor",
	Run:  run,
}

func run(pass *analysis.Pass) (interface{}, error) {
	methodSets := map[string]map[string]token.Pos{} // type → method → pos

	// Collect methods and receivers
	for _, f := range pass.Files {
		for _, d := range f.Decls {
			fn, ok := d.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || len(fn.Recv.List) == 0 {
				continue
			}
			
			// Get the receiver type
			var recvType string
			if ident, ok := fn.Recv.List[0].Type.(*ast.Ident); ok {
				recvType = ident.Name
			} else if star, ok := fn.Recv.List[0].Type.(*ast.StarExpr); ok {
				if ident, ok := star.X.(*ast.Ident); ok {
					recvType = ident.Name
				}
			}
			
			// Skip if not a Cmd type
			if !strings.HasSuffix(recvType, "Cmd") {
				continue
			}
			
			// Record the method
			name := fn.Name.Name
			if _, ok := methodSets[recvType]; !ok {
				methodSets[recvType] = map[string]token.Pos{}
			}
			methodSets[recvType][name] = fn.Pos()
		}
	}

	// Check for missing RunIntoGlazeProcessor
	for typ, methods := range methodSets {
		if _, hasRun := methods["Run"]; hasRun {
			if _, hasProc := methods["RunIntoGlazeProcessor"]; !hasProc {
				pass.Reportf(methods["Run"], 
					"%s defines Run but misses RunIntoGlazeProcessor (required by cmds.GlazeCommand)", 
					typ)
			}
		}
	}
	
	return nil, nil
}
