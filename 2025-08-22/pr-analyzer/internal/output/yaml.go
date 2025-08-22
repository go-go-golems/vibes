package output

import (
	"fmt"

	"gopkg.in/yaml.v3"
	"pr-analyzer/internal/analysis"
)

// PrintYAML outputs the analysis results in YAML format
func PrintYAML(result *analysis.PRAnalysisResult) error {
	yamlData, err := yaml.Marshal(result)
	if err != nil {
		return fmt.Errorf("failed to marshal YAML: %w", err)
	}

	fmt.Println(string(yamlData))
	return nil
}

