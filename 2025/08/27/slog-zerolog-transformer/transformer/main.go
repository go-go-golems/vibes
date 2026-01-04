package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	var inputDir = flag.String("input", "", "Input directory containing Go files")
	var outputDir = flag.String("output", "", "Output directory for transformed files")
	flag.Parse()

	if *inputDir == "" || *outputDir == "" {
		fmt.Println("Usage: transformer -input <input_dir> -output <output_dir>")
		os.Exit(1)
	}

	err := transformDirectory(*inputDir, *outputDir)
	if err != nil {
		log.Fatalf("Error transforming directory: %v", err)
	}

	fmt.Printf("Successfully transformed files from %s to %s\n", *inputDir, *outputDir)
}

func transformDirectory(inputDir, outputDir string) error {
	// Create output directory if it doesn't exist
	err := os.MkdirAll(outputDir, 0755)
	if err != nil {
		return fmt.Errorf("failed to create output directory: %v", err)
	}

	// Walk through all Go files in input directory
	return filepath.Walk(inputDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip directories and non-Go files
		if info.IsDir() || !strings.HasSuffix(path, ".go") {
			return nil
		}

		// Calculate relative path and output path
		relPath, err := filepath.Rel(inputDir, path)
		if err != nil {
			return err
		}
		outputPath := filepath.Join(outputDir, relPath)

		// Create output subdirectories if needed
		outputSubDir := filepath.Dir(outputPath)
		err = os.MkdirAll(outputSubDir, 0755)
		if err != nil {
			return err
		}

		// Transform the file
		return transformFile(path, outputPath)
	})
}

func transformFile(inputPath, outputPath string) error {
	// Read the original file
	content, err := os.ReadFile(inputPath)
	if err != nil {
		return fmt.Errorf("failed to read file %s: %v", inputPath, err)
	}

	// Check if file contains slog usage
	contentStr := string(content)
	if !strings.Contains(contentStr, "log/slog") && !strings.Contains(contentStr, "slog.") {
		// Copy file as-is if no slog usage
		return copyFile(inputPath, outputPath)
	}

	// Parse the Go file
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, inputPath, nil, parser.ParseComments)
	if err != nil {
		return fmt.Errorf("failed to parse file %s: %v", inputPath, err)
	}

	// Transform the AST
	transformer := &SlogToZerologTransformer{
		fset: fset,
	}
	
	ast.Walk(transformer, node)

	// Write the transformed file
	outputFile, err := os.Create(outputPath)
	if err != nil {
		return fmt.Errorf("failed to create output file %s: %v", outputPath, err)
	}
	defer outputFile.Close()

	err = format.Node(outputFile, fset, node)
	if err != nil {
		return fmt.Errorf("failed to format transformed file %s: %v", outputPath, err)
	}

	fmt.Printf("Transformed: %s -> %s\n", inputPath, outputPath)
	return nil
}

func copyFile(src, dst string) error {
	input, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	return os.WriteFile(dst, input, 0644)
}

type SlogToZerologTransformer struct {
	fset *token.FileSet
}

func (t *SlogToZerologTransformer) Visit(node ast.Node) ast.Visitor {
	switch n := node.(type) {
	case *ast.File:
		t.transformImports(n)
		return t
		
	case *ast.CallExpr:
		t.transformCallExpr(n)
		return t
	}
	
	return t
}

func (t *SlogToZerologTransformer) transformImports(file *ast.File) {
	for _, decl := range file.Decls {
		if genDecl, ok := decl.(*ast.GenDecl); ok && genDecl.Tok == token.IMPORT {
			for i, spec := range genDecl.Specs {
				if importSpec, ok := spec.(*ast.ImportSpec); ok {
					if importSpec.Path.Value == `"log/slog"` {
						// Replace with zerolog imports
						genDecl.Specs = append(genDecl.Specs[:i], genDecl.Specs[i+1:]...)
						
						// Add zerolog import
						zerologImport := &ast.ImportSpec{
							Path: &ast.BasicLit{
								Kind:  token.STRING,
								Value: `"github.com/rs/zerolog"`,
							},
						}
						genDecl.Specs = append(genDecl.Specs, zerologImport)
						
						// Add zerolog/log import
						zerologLogImport := &ast.ImportSpec{
							Path: &ast.BasicLit{
								Kind:  token.STRING,
								Value: `"github.com/rs/zerolog/log"`,
							},
						}
						genDecl.Specs = append(genDecl.Specs, zerologLogImport)
						break
					}
				}
			}
		}
	}
}

func (t *SlogToZerologTransformer) transformCallExpr(call *ast.CallExpr) {
	// Handle slog.Method() calls
	if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
		if ident, ok := sel.X.(*ast.Ident); ok && ident.Name == "slog" {
			t.transformSlogMethodCall(call, sel.Sel.Name)
		}
	}
}

func (t *SlogToZerologTransformer) transformSlogMethodCall(call *ast.CallExpr, methodName string) {
	switch methodName {
	case "Info", "Debug", "Warn", "Error":
		t.transformLogCall(call, methodName)
	case "InfoContext", "DebugContext", "WarnContext", "ErrorContext":
		t.transformLogContextCall(call, strings.TrimSuffix(methodName, "Context"))
	case "New":
		t.transformNewCall(call)
	case "NewTextHandler":
		t.transformNewTextHandlerCall(call)
	case "SetDefault":
		t.transformSetDefaultCall(call)
	}
}

func (t *SlogToZerologTransformer) transformLogCall(call *ast.CallExpr, level string) {
	if len(call.Args) == 0 {
		return
	}

	// Change slog.Info to log.Info().Msg
	if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
		if ident, ok := sel.X.(*ast.Ident); ok {
			ident.Name = "log"
		}
	}

	// Get message (first argument)
	message := call.Args[0]
	
	// Build zerolog chain for key-value pairs
	var chainExpr ast.Expr = call.Fun
	
	// Process key-value pairs
	for i := 1; i < len(call.Args); i += 2 {
		if i+1 < len(call.Args) {
			key := call.Args[i]
			value := call.Args[i+1]
			
			method := t.getZerologMethodForValue(value)
			
			chainExpr = &ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   chainExpr,
					Sel: &ast.Ident{Name: method},
				},
				Args: []ast.Expr{key, value},
			}
		}
	}
	
	// Final .Msg() call
	call.Fun = &ast.SelectorExpr{
		X:   chainExpr,
		Sel: &ast.Ident{Name: "Msg"},
	}
	call.Args = []ast.Expr{message}
}

func (t *SlogToZerologTransformer) transformLogContextCall(call *ast.CallExpr, level string) {
	if len(call.Args) < 2 {
		return
	}

	// Remove context parameter (first argument)
	call.Args = call.Args[1:]
	
	// Transform as regular log call
	t.transformLogCall(call, level)
}

func (t *SlogToZerologTransformer) transformNewCall(call *ast.CallExpr) {
	// Transform slog.New(handler) to zerolog.New(os.Stdout)
	if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
		if ident, ok := sel.X.(*ast.Ident); ok {
			ident.Name = "zerolog"
		}
	}
	
	// Replace handler argument with os.Stdout
	call.Args = []ast.Expr{
		&ast.SelectorExpr{
			X:   &ast.Ident{Name: "os"},
			Sel: &ast.Ident{Name: "Stdout"},
		},
	}
}

func (t *SlogToZerologTransformer) transformNewTextHandlerCall(call *ast.CallExpr) {
	// Transform slog.NewTextHandler to zerolog.ConsoleWriter
	if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
		if ident, ok := sel.X.(*ast.Ident); ok {
			ident.Name = "zerolog"
			sel.Sel.Name = "ConsoleWriter"
		}
	}
	
	// Create struct literal for ConsoleWriter
	call.Args = []ast.Expr{
		&ast.CompositeLit{
			Type: &ast.SelectorExpr{
				X:   &ast.Ident{Name: "zerolog"},
				Sel: &ast.Ident{Name: "ConsoleWriter"},
			},
			Elts: []ast.Expr{
				&ast.KeyValueExpr{
					Key: &ast.Ident{Name: "Out"},
					Value: &ast.SelectorExpr{
						X:   &ast.Ident{Name: "os"},
						Sel: &ast.Ident{Name: "Stdout"},
					},
				},
			},
		},
	}
}

func (t *SlogToZerologTransformer) transformSetDefaultCall(call *ast.CallExpr) {
	// Transform slog.SetDefault(logger) to log.Logger = logger
	// This is a simple assignment transformation
	if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
		if ident, ok := sel.X.(*ast.Ident); ok {
			ident.Name = "log"
			sel.Sel.Name = "Logger"
		}
	}
}

func (t *SlogToZerologTransformer) getZerologMethodForValue(value ast.Expr) string {
	switch v := value.(type) {
	case *ast.BasicLit:
		switch v.Kind {
		case token.STRING:
			return "Str"
		case token.INT:
			return "Int"
		case token.FLOAT:
			return "Float64"
		}
	case *ast.Ident:
		if v.Name == "true" || v.Name == "false" {
			return "Bool"
		}
	}
	
	// Default to interface{} method for complex types
	return "Interface"
}

