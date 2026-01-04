package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

// FunctionInfo represents information about a Go function
type FunctionInfo struct {
	Name       string `json:"name"`
	FilePath   string `json:"file_path"`
	StartLine  int    `json:"start_line"`
	EndLine    int    `json:"end_line"`
	StartCol   int    `json:"start_col"`
	Parameters string `json:"parameters"`
	ReturnType string `json:"return_type,omitempty"`
	IsMethod   bool   `json:"is_method"`
	Receiver   string `json:"receiver,omitempty"`
}

// Reference represents a reference to a function
type Reference struct {
	FilePath  string `json:"file_path"`
	Line      int    `json:"line"`
	Column    int    `json:"column"`
	Context   string `json:"context"`
	Type      string `json:"type"` // "definition" or "usage"
}

// FunctionData represents the combined data for a function
type FunctionData struct {
	Function   FunctionInfo `json:"function"`
	References []Reference  `json:"references"`
}

// mainLogCaller adds file and line information to the log
func mainLogCaller() zerolog.Logger {
	_, file, line, ok := runtime.Caller(1)
	if !ok {
		file = "unknown"
		line = 0
	}
	
	// Get just the filename, not the full path
	parts := strings.Split(file, "/")
	file = parts[len(parts)-1]
	
	return log.With().Str("file", file).Int("line", line).Logger()
}

// extractFunctions extracts function information from a Go file
func extractFunctions(filePath string) ([]FunctionInfo, error) {
	logger := mainLogCaller()
	logger.Debug().Str("filePath", filePath).Msg("Extracting functions from file")
	
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, filePath, nil, parser.ParseComments)
	if err != nil {
		logger.Error().Err(err).Str("filePath", filePath).Msg("Failed to parse file")
		return nil, fmt.Errorf("failed to parse file %s: %v", filePath, err)
	}
	
	logger.Debug().Str("filePath", filePath).Msg("File parsed successfully")

	var functions []FunctionInfo

	// Read file content for parameter and return type extraction
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		logger.Error().Err(err).Str("filePath", filePath).Msg("Failed to read file")
		return nil, fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	lines := strings.Split(string(content), "\n")
	
	logger.Debug().Str("filePath", filePath).Int("lineCount", len(lines)).Msg("File content read")

	// Extract functions and methods
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			startPos := fset.Position(x.Pos())
			endPos := fset.Position(x.End())
			
			logger.Debug().
				Str("funcName", x.Name.Name).
				Int("startLine", startPos.Line).
				Int("endLine", endPos.Line).
				Msg("Found function declaration")
			
			// Extract parameters
			params := ""
			if x.Type.Params != nil {
				paramPos := fset.Position(x.Type.Params.Pos())
				paramEndPos := fset.Position(x.Type.Params.End())
				if paramPos.Line <= len(lines) && paramEndPos.Line <= len(lines) {
					// Extract from source to preserve original formatting
					params = string(content[x.Type.Params.Pos()-1:x.Type.Params.End()-1])
					logger.Debug().Str("params", params).Msg("Extracted parameters")
				}
			}
			
			// Extract return type
			returns := ""
			if x.Type.Results != nil {
				resultPos := fset.Position(x.Type.Results.Pos())
				resultEndPos := fset.Position(x.Type.Results.End())
				if resultPos.Line <= len(lines) && resultEndPos.Line <= len(lines) {
					// Extract from source to preserve original formatting
					returns = string(content[x.Type.Results.Pos()-1:x.Type.Results.End()-1])
					logger.Debug().Str("returns", returns).Msg("Extracted return type")
				}
			}
			
			isMethod := x.Recv != nil
			receiver := ""
			
			if isMethod && len(x.Recv.List) > 0 {
				// Extract receiver type
				recvPos := fset.Position(x.Recv.Pos())
				recvEndPos := fset.Position(x.Recv.End())
				
				if recvPos.Line <= len(lines) && recvEndPos.Line <= len(lines) {
					receiver = string(content[x.Recv.Pos()-1:x.Recv.End()-1])
					receiver = strings.TrimPrefix(receiver, "(")
					receiver = strings.TrimSuffix(receiver, ")")
					logger.Debug().Str("receiver", receiver).Msg("Extracted receiver")
				}
			}
			
			// Convert to 0-based for LSP
			zeroBasedStartLine := startPos.Line - 1
			zeroBasedEndLine := endPos.Line - 1
			zeroBasedStartCol := startPos.Column - 1
			
			function := FunctionInfo{
				Name:       x.Name.Name,
				FilePath:   filePath,
				StartLine:  zeroBasedStartLine,
				EndLine:    zeroBasedEndLine,
				StartCol:   zeroBasedStartCol,
				Parameters: params,
				ReturnType: returns,
				IsMethod:   isMethod,
				Receiver:   receiver,
			}
			
			logger.Debug().
				Str("name", function.Name).
				Str("filePath", function.FilePath).
				Int("startLine", function.StartLine).
				Int("startCol", function.StartCol).
				Bool("isMethod", function.IsMethod).
				Msg("Added function to list")
			
			functions = append(functions, function)
		}
		return true
	})
	
	logger.Debug().Str("filePath", filePath).Int("functionCount", len(functions)).Msg("Function extraction complete")
	return functions, nil
}

// findReferencesWithGopls finds all references using gopls
func findReferencesWithGopls(functions []FunctionInfo, rootDir string) (map[string][]Reference, error) {
	logger := mainLogCaller()
	
	// Convert rootDir to URI format
	rootURI := "file://" + filepath.ToSlash(rootDir)
	logger.Debug().Str("rootDir", rootDir).Str("rootURI", rootURI).Msg("Finding references with gopls")
	
	// Create LSP client
	logger.Debug().Msg("Creating LSP client")
	client, err := NewLSPClient()
	if err != nil {
		logger.Error().Err(err).Msg("Failed to create LSP client")
		return nil, fmt.Errorf("failed to create LSP client: %v", err)
	}
	defer func() {
		logger.Debug().Msg("Closing LSP client")
		err := client.Close()
		if err != nil {
			logger.Error().Err(err).Msg("Error closing LSP client")
		}
	}()
	
	// Initialize LSP server
	logger.Debug().Str("rootURI", rootURI).Msg("Initializing LSP server")
	err = client.Initialize(rootURI)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to initialize LSP server")
		return nil, fmt.Errorf("failed to initialize LSP server: %v", err)
	}
	
	// Open all Go files in the directory
	logger.Debug().Str("rootDir", rootDir).Msg("Opening Go files")
	err = openGoFiles(client, rootDir)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to open Go files")
		return nil, fmt.Errorf("failed to open Go files: %v", err)
	}
	
	// Wait a bit for the server to process the files
	logger.Debug().Msg("Waiting for server to process files")
	time.Sleep(2 * time.Second)
	
	// Find references for each function
	result := make(map[string][]Reference)
	
	for _, function := range functions {
		functionLogger := logger.With().
			Str("function", function.Name).
			Str("filePath", function.FilePath).
			Int("line", function.StartLine).
			Int("col", function.StartCol).
			Logger()
		
		functionLogger.Debug().Msg("Finding references for function")
		
		// Convert file path to URI
		fileURI := "file://" + filepath.ToSlash(function.FilePath)
		functionLogger.Debug().Str("fileURI", fileURI).Msg("Converted file path to URI")
		
		// Find references
		functionLogger.Debug().Msg("Calling FindReferences")
		locations, err := client.FindReferences(fileURI, function.StartLine, function.StartCol, true)
		if err != nil {
			functionLogger.Error().Err(err).Msg("Failed to find references")
			fmt.Printf("Warning: Failed to find references for function %s: %v\n", function.Name, err)
			continue
		}
		
		functionLogger.Debug().Int("locationCount", len(locations)).Msg("Found references")
		
		// Convert locations to references
		var references []Reference
		for i, location := range locations {
			locLogger := functionLogger.With().Int("index", i).Str("uri", location.URI).Logger()
			
			// Convert URI to file path
			filePath := strings.TrimPrefix(location.URI, "file://")
			filePath = filepath.FromSlash(filePath)
			locLogger.Debug().Str("filePath", filePath).Msg("Converted URI to file path")
			
			// Get context from file
			context, err := getContextFromFile(filePath, location.Range.Start.Line)
			if err != nil {
				locLogger.Error().Err(err).Msg("Failed to get context")
				fmt.Printf("Warning: Failed to get context for reference: %v\n", err)
				context = ""
			}
			
			// Determine reference type
			refType := "usage"
			if filePath == function.FilePath && 
			   location.Range.Start.Line == function.StartLine && 
			   location.Range.Start.Character == function.StartCol {
				refType = "definition"
			}
			
			// Convert back to 1-based for output
			oneBased := Reference{
				FilePath: filePath,
				Line:     location.Range.Start.Line + 1,
				Column:   location.Range.Start.Character + 1,
				Context:  context,
				Type:     refType,
			}
			
			locLogger.Debug().
				Str("filePath", oneBased.FilePath).
				Int("line", oneBased.Line).
				Int("column", oneBased.Column).
				Str("type", oneBased.Type).
				Msg("Added reference")
			
			references = append(references, oneBased)
		}
		
		functionLogger.Debug().Int("referenceCount", len(references)).Msg("Processed all references")
		result[function.Name] = references
	}
	
	logger.Debug().Int("functionCount", len(result)).Msg("Completed finding references for all functions")
	return result, nil
}

// openGoFiles opens all Go files in the directory
func openGoFiles(client *LSPClient, rootDir string) error {
	logger := mainLogCaller()
	logger.Debug().Str("rootDir", rootDir).Msg("Opening Go files in directory")
	
	fileCount := 0
	err := filepath.Walk(rootDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			logger.Error().Err(err).Str("path", path).Msg("Error accessing path")
			return err
		}
		
		if !info.IsDir() && strings.HasSuffix(path, ".go") {
			fileLogger := logger.With().Str("file", path).Logger()
			fileLogger.Debug().Msg("Found Go file")
			
			// Read file content
			content, err := ioutil.ReadFile(path)
			if err != nil {
				fileLogger.Error().Err(err).Msg("Failed to read file")
				return fmt.Errorf("failed to read file %s: %v", path, err)
			}
			
			// Convert file path to URI
			fileURI := "file://" + filepath.ToSlash(path)
			fileLogger.Debug().Str("fileURI", fileURI).Int("contentLength", len(content)).Msg("Opening file in LSP server")
			
			// Open file in LSP server
			err = client.DidOpenTextDocument(fileURI, "go", string(content))
			if err != nil {
				fileLogger.Error().Err(err).Msg("Failed to open file")
				return fmt.Errorf("failed to open file %s: %v", path, err)
			}
			
			fileLogger.Debug().Msg("File opened successfully")
			fileCount++
		}
		
		return nil
	})
	
	logger.Debug().Int("fileCount", fileCount).Msg("Opened all Go files")
	return err
}

// getContextFromFile gets the context (line of code) from a file
func getContextFromFile(filePath string, line int) (string, error) {
	logger := mainLogCaller()
	logger.Debug().Str("filePath", filePath).Int("line", line).Msg("Getting context from file")
	
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		logger.Error().Err(err).Str("filePath", filePath).Msg("Failed to read file")
		return "", fmt.Errorf("failed to read file %s: %v", filePath, err)
	}
	
	lines := strings.Split(string(content), "\n")
	if line < 0 || line >= len(lines) {
		logger.Error().Int("line", line).Int("lineCount", len(lines)).Msg("Line out of range")
		return "", fmt.Errorf("line %d out of range", line)
	}
	
	context := strings.TrimSpace(lines[line])
	logger.Debug().Str("context", context).Msg("Got context from file")
	return context, nil
}

func main() {
	// Configure zerolog
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnix
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})
	
	// Parse command line arguments
	var (
		filePath    string
		dirPath     string
		outputFile  string
		prettyPrint bool
		useGopls    bool
		logLevel    string
	)

	flag.StringVar(&filePath, "file", "", "Path to a Go file to analyze")
	flag.StringVar(&dirPath, "dir", "", "Path to a directory containing Go files to analyze")
	flag.StringVar(&outputFile, "output", "", "Path to output JSON file (default: stdout)")
	flag.BoolVar(&prettyPrint, "pretty", false, "Pretty print JSON output")
	flag.BoolVar(&useGopls, "gopls", true, "Use gopls for finding references (default: true)")
	flag.StringVar(&logLevel, "log", "debug", "Log level (debug, info, warn, error)")
	flag.Parse()
	
	// Set log level
	switch strings.ToLower(logLevel) {
	case "debug":
		zerolog.SetGlobalLevel(zerolog.DebugLevel)
	case "info":
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
	case "warn":
		zerolog.SetGlobalLevel(zerolog.WarnLevel)
	case "error":
		zerolog.SetGlobalLevel(zerolog.ErrorLevel)
	default:
		fmt.Printf("Invalid log level: %s, using debug\n", logLevel)
		zerolog.SetGlobalLevel(zerolog.DebugLevel)
	}
	
	logger := mainLogCaller()
	logger.Info().
		Str("file", filePath).
		Str("dir", dirPath).
		Str("output", outputFile).
		Bool("pretty", prettyPrint).
		Bool("gopls", useGopls).
		Str("logLevel", logLevel).
		Msg("Starting go-function-parser")

	if filePath == "" && dirPath == "" {
		logger.Error().Msg("Either -file or -dir must be specified")
		fmt.Println("Error: Either -file or -dir must be specified")
		flag.Usage()
		os.Exit(1)
	}

	// Process files and collect function data
	var allFunctions []FunctionInfo
	var searchPath string

	if filePath != "" {
		// Process single file
		logger.Info().Str("filePath", filePath).Msg("Processing single file")
		functions, err := extractFunctions(filePath)
		if err != nil {
			logger.Error().Err(err).Str("filePath", filePath).Msg("Error parsing file")
			fmt.Printf("Error parsing file: %v\n", err)
			os.Exit(1)
		}
		allFunctions = append(allFunctions, functions...)
		searchPath = filepath.Dir(filePath)
		logger.Info().Int("functionCount", len(functions)).Str("searchPath", searchPath).Msg("Processed single file")
	} else {
		// Process directory
		logger.Info().Str("dirPath", dirPath).Msg("Processing directory")
		err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				logger.Error().Err(err).Str("path", path).Msg("Error accessing path")
				return err
			}
			if !info.IsDir() && strings.HasSuffix(path, ".go") {
				pathLogger := logger.With().Str("path", path).Logger()
				pathLogger.Debug().Msg("Processing Go file")
				
				functions, err := extractFunctions(path)
				if err != nil {
					pathLogger.Error().Err(err).Msg("Error processing file")
					fmt.Printf("Warning: Error processing file %s: %v\n", path, err)
					return nil // Continue with other files
				}
				allFunctions = append(allFunctions, functions...)
				pathLogger.Debug().Int("functionCount", len(functions)).Msg("Processed Go file")
			}
			return nil
		})
		if err != nil {
			logger.Error().Err(err).Str("dirPath", dirPath).Msg("Error walking directory")
			fmt.Printf("Error walking directory: %v\n", err)
			os.Exit(1)
		}
		searchPath = dirPath
		logger.Info().Int("totalFunctionCount", len(allFunctions)).Str("searchPath", searchPath).Msg("Processed directory")
	}

	// Find references for each function
	var result []FunctionData

	if useGopls {
		// Use gopls for finding references
		logger.Info().Msg("Using gopls for finding references")
		fmt.Println("Using gopls for finding references...")
		
		// Find references for all functions
		referencesMap, err := findReferencesWithGopls(allFunctions, searchPath)
		if err != nil {
			logger.Error().Err(err).Msg("Error finding references with gopls")
			fmt.Printf("Error finding references with gopls: %v\n", err)
			os.Exit(1)
		}
		
		// Create result
		for _, function := range allFunctions {
			references := referencesMap[function.Name]
			
			result = append(result, FunctionData{
				Function:   function,
				References: references,
			})
		}
		
		logger.Info().Int("functionCount", len(result)).Msg("Created result data")
	} else {
		// Use AST parser for finding references (original implementation)
		logger.Info().Msg("Using AST parser for finding references")
		fmt.Println("Using AST parser for finding references...")
		
		// Find references for each function individually
		for _, function := range allFunctions {
			var references []Reference
			
			// This is a placeholder for the original AST-based reference finding
			// In a real implementation, you would call the original findReferences function here
			
			result = append(result, FunctionData{
				Function:   function,
				References: references,
			})
		}
	}

	// Output JSON
	var jsonData []byte
	var err error

	if prettyPrint {
		logger.Debug().Msg("Pretty printing JSON")
		jsonData, err = json.MarshalIndent(result, "", "  ")
	} else {
		logger.Debug().Msg("Marshaling JSON")
		jsonData, err = json.Marshal(result)
	}

	if err != nil {
		logger.Error().Err(err).Msg("Error generating JSON")
		fmt.Printf("Error generating JSON: %v\n", err)
		os.Exit(1)
	}

	if outputFile != "" {
		logger.Info().Str("outputFile", outputFile).Int("jsonLength", len(jsonData)).Msg("Writing output to file")
		err = os.WriteFile(outputFile, jsonData, 0644)
		if err != nil {
			logger.Error().Err(err).Str("outputFile", outputFile).Msg("Error writing to output file")
			fmt.Printf("Error writing to output file: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Output written to %s\n", outputFile)
	} else {
		logger.Info().Int("jsonLength", len(jsonData)).Msg("Writing output to stdout")
		fmt.Println(string(jsonData))
	}
	
	logger.Info().Msg("Program completed successfully")
}
