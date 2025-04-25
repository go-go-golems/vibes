package models

import "time"

// FileInfo represents information about a Go file
type FileInfo struct {
	Path       string         `json:"path"`                 // Relative path within the project
	AbsPath    string         `json:"absPath"`              // Absolute path on the filesystem
	Package    string         `json:"package"`              // Go package name
	Imports    []ImportInfo   `json:"imports"`              // Imports declared in the file
	Functions  []FunctionInfo `json:"functions"`            // Functions and methods defined in the file
	LastUpdate time.Time      `json:"lastUpdate,omitempty"` // Timestamp of last analysis/update
	Size       int64          `json:"size,omitempty"`       // File size in bytes
	NumLines   int            `json:"numLines,omitempty"`   // Number of lines in the file
}

// ImportInfo represents an import statement
type ImportInfo struct {
	Path  string `json:"path"`            // Import path (e.g., "fmt", "github.com/pkg/errors")
	Alias string `json:"alias,omitempty"` // Import alias (e.g., "p" in `import p "github.com/pkg/errors"`)
}

// FunctionInfo represents information about a function or method
type FunctionInfo struct {
	ID         string          `json:"id,omitempty"`         // Unique ID (e.g., "packageName.FunctionName" or "packageName.(ReceiverType).MethodName")
	Name       string          `json:"name"`                 // Function or method name
	Signature  string          `json:"signature,omitempty"`  // Full function signature
	Filepath   string          `json:"filepath"`             // File path relative to the base path
	StartLine  int             `json:"startLine"`            // Start line number (1-based)
	EndLine    int             `json:"endLine"`              // End line number (1-based)
	StartCol   int             `json:"startCol"`             // Start column number (0-based)
	EndCol     int             `json:"endCol"`               // End column number (0-based)
	IsExported bool            `json:"isExported"`           // Whether the function/method is exported (starts with uppercase)
	IsMethod   bool            `json:"isMethod"`             // Whether this is a method
	Receiver   *ReceiverInfo   `json:"receiver,omitempty"`   // Receiver info if IsMethod is true
	Parameters []ParameterInfo `json:"parameters"`           // List of parameters
	ReturnType []string        `json:"returnType"`           // List of return types as strings
	Comments   string          `json:"comments,omitempty"`   // Associated comments / documentation
	Package    string          `json:"package"`              // Go package name
	Calls      []string        `json:"calls,omitempty"`      // List of function/method IDs called by this function (potentially added later)
	CalledBy   []string        `json:"calledBy,omitempty"`   // List of function/method IDs that call this function (potentially added later)
	Complexity int             `json:"complexity,omitempty"` // Cyclomatic complexity (potentially added later)
}

// ReceiverInfo represents information about a method receiver
type ReceiverInfo struct {
	Name      string `json:"name"`                // Receiver variable name (e.g., "f" in `func (f *Foo)`)
	Type      string `json:"type"`                // Receiver type string (e.g., "*Foo", "Bar")
	IsPointer bool   `json:"isPointer,omitempty"` // Whether the receiver is a pointer
	TypeName  string `json:"typeName,omitempty"`  // Just the type name without pointer indicator (e.g., "Foo", "Bar")
}

// ParameterInfo represents a function or method parameter
type ParameterInfo struct {
	Name string `json:"name"` // Parameter name
	Type string `json:"type"` // Parameter type string
}

// PackageInfo represents information about a single Go package within the analysis
type PackageInfo struct {
	Name      string   `json:"name"`                // Package name
	Path      string   `json:"path"`                // Directory path relative to the project root
	Files     []string `json:"files"`               // List of file paths (relative) belonging to this package
	Functions []string `json:"functions,omitempty"` // List of function/method IDs belonging to this package
}

// CodebaseAnalysis represents the complete result of analyzing a codebase
type CodebaseAnalysis struct {
	BasePath     string                 `json:"basePath"`               // Absolute path of the analyzed directory
	Files        []FileInfo             `json:"files"`                  // Information about each analyzed file
	Functions    []FunctionInfo         `json:"functions"`              // Aggregated list of all functions/methods found
	Packages     map[string]PackageInfo `json:"packages"`               // Map of package name to package info
	Dependencies []Dependency           `json:"dependencies,omitempty"` // List of external dependencies identified
	Stats        AnalysisStats          `json:"stats"`                  // Analysis statistics
}

// AnalysisStats provides statistics about the analysis run
type AnalysisStats struct {
	TotalFiles     int       `json:"totalFiles"`     // Total number of Go files analyzed
	TotalFunctions int       `json:"totalFunctions"` // Total number of functions (excluding methods)
	TotalMethods   int       `json:"totalMethods"`   // Total number of methods
	TotalPackages  int       `json:"totalPackages"`  // Total number of unique packages found
	AnalysisTimeMs int64     `json:"analysisTimeMs"` // Analysis duration in milliseconds
	Timestamp      time.Time `json:"timestamp"`      // Timestamp when the analysis was completed
}

// Dependency represents an external dependency
type Dependency struct {
	ImportPath string   `json:"importPath"`       // Full import path (e.g., "github.com/pkg/errors")
	Module     string   `json:"module,omitempty"` // Go module path it belongs to (if discernible)
	UsedBy     []string `json:"usedBy,omitempty"` // List of file paths (relative) that import this dependency
}
