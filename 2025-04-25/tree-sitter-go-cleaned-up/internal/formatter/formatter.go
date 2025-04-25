package formatter

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/pkg/errors"
	"github.com/wesen/tree-sitter-go-cleaned-up/pkg/models"
)

// Formatter handles formatting analysis results.
// Currently, only JSON formatting is supported.
type Formatter struct {
	// Configuration options for the formatter can be added here if needed.
}

// NewFormatter creates a new formatter instance.
func NewFormatter() *Formatter {
	return &Formatter{}
}

// FormatJSON formats the CodebaseAnalysis result into an enhanced JSON byte slice.
func (f *Formatter) FormatJSON(analysis *models.CodebaseAnalysis) ([]byte, error) {
	if analysis == nil {
		return nil, errors.New("cannot format nil analysis result")
	}

	// Prepare the output structure (simplified initially)
	// We can enhance this later based on the original json_formatter logic if needed
	// For now, it directly maps to the CodebaseAnalysis structure.

	// Add Function IDs and Signatures before marshaling
	generateFunctionDetails(analysis.Functions)

	// Marshal the analysis object directly
	jsonData, err := json.MarshalIndent(analysis, "", "  ")
	if err != nil {
		return nil, errors.Wrap(err, "error marshaling analysis to JSON")
	}

	return jsonData, nil
}

// generateFunctionDetails populates ID and Signature fields for functions.
func generateFunctionDetails(functions []models.FunctionInfo) {
	for i := range functions {
		functions[i].ID = createFunctionID(functions[i])
		functions[i].Signature = buildFunctionSignature(functions[i])
	}
}

// createFunctionID creates a unique ID for a function or method.
func createFunctionID(function models.FunctionInfo) string {
	if function.IsMethod && function.Receiver != nil {
		// Use receiver type name (without pointer) for the ID
		receiverTypeName := function.Receiver.TypeName
		if receiverTypeName == "" { // Fallback if TypeName wasn't populated
			receiverTypeName = strings.TrimPrefix(function.Receiver.Type, "*")
		}
		return fmt.Sprintf("%s.(%s).%s", function.Package, receiverTypeName, function.Name)
	}
	return fmt.Sprintf("%s.%s", function.Package, function.Name)
}

// buildFunctionSignature builds a function signature string.
func buildFunctionSignature(function models.FunctionInfo) string {
	var signature strings.Builder

	signature.WriteString("func ")

	if function.IsMethod && function.Receiver != nil {
		signature.WriteString("(")
		if function.Receiver.Name != "" {
			signature.WriteString(function.Receiver.Name)
			signature.WriteString(" ")
		}
		signature.WriteString(function.Receiver.Type)
		signature.WriteString(") ")
	}

	signature.WriteString(function.Name)

	signature.WriteString("(")
	for i, param := range function.Parameters {
		if i > 0 {
			signature.WriteString(", ")
		}
		if param.Name != "" {
			signature.WriteString(param.Name)
			signature.WriteString(" ")
		}
		signature.WriteString(param.Type)
	}
	signature.WriteString(")")

	if len(function.ReturnType) > 0 {
		if len(function.ReturnType) == 1 && function.ReturnType[0] != "" { // Handle single return type
			signature.WriteString(" ")
			signature.WriteString(function.ReturnType[0])
		} else if len(function.ReturnType) > 1 { // Handle multiple return types
			signature.WriteString(" (")
			for i, returnType := range function.ReturnType {
				if i > 0 {
					signature.WriteString(", ")
				}
				signature.WriteString(returnType)
			}
			signature.WriteString(")")
		}
	}

	return signature.String()
}
