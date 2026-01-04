package validate

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/scrapybara/ttmp/pkg/parser"
	"github.com/scrapybara/ttmp/pkg/validator"
)

// ResultsOutput defines the output format for validation results
type ResultsOutput struct {
	Valid     bool                      `json:"valid"`
	FilePath  string                    `json:"file_path,omitempty"`
	Error     string                    `json:"error,omitempty"`
	Documents []DocumentValidationResult `json:"documents,omitempty"`
}

// DocumentValidationResult represents validation results for a single document
type DocumentValidationResult struct {
	Valid       bool     `json:"valid"`
	DocumentID  string   `json:"document_id"`
	FilePath    string   `json:"file_path,omitempty"`
	Errors      []string `json:"errors"`
	Warnings    []string `json:"warnings"`
	Suggestions []string `json:"suggestions"`
}

// NewValidateCommand creates a new validate command
func NewValidateCommand(logger *logrus.Logger, parser *parser.TTMPParser) *cobra.Command {
	var (
		recursive    bool
		jsonOutput   bool
		strict       bool
		noSuggestions bool
	)
	
	cmd := &cobra.Command{
		Use:   "validate [path]",
		Short: "Validates TTMP documents",
		Long:  `Validates TTMP documents against the schema and performs additional checks.`,
		Args:  cobra.MinimumNArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			path := args[0]
			validate := validator.NewValidator(logger)
			
			// Set the output format
			if jsonOutput {
				validateWithJSON(logger, parser, validate, path, recursive, strict, noSuggestions)
			} else {
				validateWithText(logger, parser, validate, path, recursive, strict, noSuggestions)
			}
		},
	}
	
	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively validate all files in directory")
	cmd.Flags().BoolVar(&jsonOutput, "json", false, "Output results in JSON format")
	cmd.Flags().BoolVar(&strict, "strict", false, "Treat warnings as errors")
	cmd.Flags().BoolVar(&noSuggestions, "no-suggestions", false, "Don't show suggestions")
	
	return cmd
}

// validateWithJSON validates documents and outputs results in JSON format
func validateWithJSON(logger *logrus.Logger, parser *parser.TTMPParser, validate *validator.Validator, path string, recursive bool, strict bool, noSuggestions bool) {
	output := ResultsOutput{
		Valid:     true,
		Documents: []DocumentValidationResult{},
	}
	
	// Check if path is a file or directory
	fileInfo, err := os.Stat(path)
	if err != nil {
		output.Valid = false
		output.Error = fmt.Sprintf("Error accessing path: %v", err)
		outputJSON(output)
		return
	}
	
	if fileInfo.IsDir() {
		// Validate all files in directory
		var collection *parser.TTMPCollection
		collection, err = parser.LoadCollection(path, recursive)
		if err != nil {
			output.Valid = false
			output.Error = fmt.Sprintf("Error loading documents: %v", err)
			outputJSON(output)
			return
		}
		
		results, err := validate.ValidateCollection(collection)
		if err != nil {
			output.Valid = false
			output.Error = fmt.Sprintf("Error validating documents: %v", err)
			outputJSON(output)
			return
		}
		
		// Process each validation result
		for _, result := range results {
			docResult := DocumentValidationResult{
				Valid:       result.Valid,
				DocumentID:  result.DocumentID,
				FilePath:    result.FilePath,
				Errors:      result.GetErrorStrings(),
				Warnings:    result.GetWarningStrings(),
				Suggestions: result.GetSuggestionStrings(),
			}
			
			// Apply strict mode (treat warnings as errors)
			if strict && len(result.Warnings) > 0 {
				docResult.Valid = false
				output.Valid = false
			}
			
			// Remove suggestions if requested
			if noSuggestions {
				docResult.Suggestions = nil
			}
			
			output.Documents = append(output.Documents, docResult)
			
			// Update overall validity
			if !docResult.Valid {
				output.Valid = false
			}
		}
	} else {
		// Validate single file
		doc, err := parser.ParseFile(path)
		if err != nil {
			output.Valid = false
			output.Error = fmt.Sprintf("Error parsing file: %v", err)
			outputJSON(output)
			return
		}
		
		result, err := validate.ValidateFile(path, doc)
		if err != nil {
			output.Valid = false
			output.Error = fmt.Sprintf("Error validating file: %v", err)
			outputJSON(output)
			return
		}
		
		docResult := DocumentValidationResult{
			Valid:       result.Valid,
			DocumentID:  result.DocumentID,
			FilePath:    result.FilePath,
			Errors:      result.GetErrorStrings(),
			Warnings:    result.GetWarningStrings(),
			Suggestions: result.GetSuggestionStrings(),
		}
		
		// Apply strict mode (treat warnings as errors)
		if strict && len(result.Warnings) > 0 {
			docResult.Valid = false
			output.Valid = false
		}
		
		// Remove suggestions if requested
		if noSuggestions {
			docResult.Suggestions = nil
		}
		
		output.Documents = append(output.Documents, docResult)
		
		// Update overall validity
		if !docResult.Valid {
			output.Valid = false
		}
	}
	
	// Output the results
	outputJSON(output)
}

// validateWithText validates documents and outputs results in text format
func validateWithText(logger *logrus.Logger, parser *parser.TTMPParser, validate *validator.Validator, path string, recursive bool, strict bool, noSuggestions bool) {
	// Check if path is a file or directory
	fileInfo, err := os.Stat(path)
	if err != nil {
		logger.Fatalf("Error accessing path: %v", err)
	}
	
	if fileInfo.IsDir() {
		// Validate all files in directory
		validateDirectory(logger, parser, validate, path, recursive, strict, noSuggestions)
	} else {
		// Validate single file
		validateFile(logger, parser, validate, path, strict, noSuggestions)
	}
}

// validateFile validates a single file and outputs results in text format
func validateFile(logger *logrus.Logger, parser *parser.TTMPParser, validate *validator.Validator, path string, strict bool, noSuggestions bool) {
	// Parse document
	doc, err := parser.ParseFile(path)
	if err != nil {
		logger.Fatalf("Error parsing file: %v", err)
	}
	
	// Validate document
	result, err := validate.ValidateFile(path, doc)
	if err != nil {
		logger.Fatalf("Error validating file: %v", err)
	}
	
	// Apply strict mode (treat warnings as errors)
	if strict && len(result.Warnings) > 0 {
		result.Valid = false
	}
	
	// Display results
	fmt.Printf("Validating: %s\n", path)
	fmt.Printf("ID: %s\n", doc.ID)
	fmt.Printf("Title: %s\n", doc.Title)
	
	if result.Valid {
		fmt.Printf("✅ Document is valid\n")
	} else {
		fmt.Printf("❌ Document is invalid\n")
	}
	
	if len(result.Errors) > 0 {
		fmt.Println("\nErrors:")
		for _, err := range result.Errors {
			fmt.Printf("- %s\n", err)
		}
	}
	
	if len(result.Warnings) > 0 {
		fmt.Println("\nWarnings:")
		for _, warning := range result.Warnings {
			fmt.Printf("- %s\n", warning)
		}
	}
	
	if !noSuggestions && len(result.Suggestions) > 0 {
		fmt.Println("\nSuggestions:")
		for _, suggestion := range result.Suggestions {
			fmt.Printf("- %s\n", suggestion)
		}
	}
}

// validateDirectory validates all files in a directory and outputs results in text format
func validateDirectory(logger *logrus.Logger, parser *parser.TTMPParser, validate *validator.Validator, dirPath string, recursive bool, strict bool, noSuggestions bool) {
	// Load collection
	collection, err := parser.LoadCollection(dirPath, recursive)
	if err != nil {
		logger.Fatalf("Error loading documents: %v", err)
	}
	
	if len(collection.Documents) == 0 {
		fmt.Println("No documents found in the specified directory")
		return
	}
	
	// Validate collection
	results, err := validate.ValidateCollection(collection)
	if err != nil {
		logger.Fatalf("Error validating documents: %v", err)
	}
	
	// Count valid and invalid documents
	valid := 0
	invalid := 0
	
	// Process each validation result
	for _, result := range results {
		// Apply strict mode (treat warnings as errors)
		if strict && len(result.Warnings) > 0 {
			result.Valid = false
		}
		
		if result.Valid {
			valid++
		} else {
			invalid++
			fmt.Printf("\n❌ Invalid document: %s\n", result.FilePath)
			
			if len(result.Errors) > 0 {
				fmt.Println("  Errors:")
				for _, err := range result.Errors {
					fmt.Printf("  - %s\n", err)
				}
			}
			
			if len(result.Warnings) > 0 {
				fmt.Println("  Warnings:")
				for _, warning := range result.Warnings {
					fmt.Printf("  - %s\n", warning)
				}
			}
		}
	}
	
	// Print summary
	fmt.Printf("\nValidation Summary:\n")
	fmt.Printf("Total files: %d\n", len(collection.Documents))
	fmt.Printf("Valid: %d\n", valid)
	fmt.Printf("Invalid: %d\n", invalid)
	
	// Print collection-level issues if any
	for _, result := range results {
		if result.DocumentID == "collection" && result.HasIssues() {
			fmt.Println("\nCollection-level issues:")
			
			if len(result.Warnings) > 0 {
				fmt.Println("  Warnings:")
				for _, warning := range result.Warnings {
					fmt.Printf("  - %s\n", warning)
				}
			}
			
			if !noSuggestions && len(result.Suggestions) > 0 {
				fmt.Println("  Suggestions:")
				for _, suggestion := range result.Suggestions {
					fmt.Printf("  - %s\n", suggestion)
				}
			}
			
			break
		}
	}
}

// outputJSON outputs a struct as formatted JSON
func outputJSON(data interface{}) {
	jsonData, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		fmt.Printf("Error formatting JSON: %v\n", err)
		return
	}
	
	fmt.Println(string(jsonData))
}