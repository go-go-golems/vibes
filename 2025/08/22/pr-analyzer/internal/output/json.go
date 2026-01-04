package output

import (
	"encoding/json"
	"fmt"

	"pr-analyzer/internal/analysis"
)

// PrintJSON outputs the analysis results in JSON format
func PrintJSON(result *analysis.PRAnalysisResult) error {
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	fmt.Println(string(jsonData))
	return nil
}

