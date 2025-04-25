package validate

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/user/ttmp-go/pkg/errors"
	"github.com/user/ttmp-go/pkg/parser"
	"github.com/user/ttmp-go/pkg/util/fileutil"
	"github.com/user/ttmp-go/pkg/validator"
)

// NewValidateCommand creates a new validate command
func NewValidateCommand() *cobra.Command {
	var recursive bool

	cmd := &cobra.Command{
		Use:   "validate [path]",
		Short: "Validate TTMP documents",
		Long:  `Validates TTMP documents against the schema.`,
		Args:  cobra.MinimumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			path := args[0]

			// Create a parser and validator
			p := parser.NewParser()
			v := validator.NewValidator()

			// Check if path is a directory
			fileInfo, err := fileutil.GetFileInfo(path)
			if err != nil {
				return errors.NewIOError(fmt.Sprintf("Failed to access path: %s", path), err)
			}

			if fileInfo.IsDir() {
				// Find all markdown files in the directory
				var files []string
				if recursive {
					files, err = fileutil.FindMarkdownFilesRecursive(path)
				} else {
					files, err = fileutil.FindMarkdownFiles(path)
				}

				if err != nil {
					return errors.NewIOError(fmt.Sprintf("Failed to find markdown files in directory: %s", path), err)
				}

				// Validate each file
				validCount := 0
				invalidCount := 0
				errorCount := 0

				for _, file := range files {
					fmt.Printf("Validating %s...\n", file)
					doc, err := p.ParseFile(file)
					if err != nil {
						fmt.Printf("Error parsing %s: %v\n", file, err)
						errorCount++
						continue
					}

					err = v.ValidateDocument(doc)
					if err != nil {
						fmt.Printf("Validation failed for %s:\n%v\n", file, err)
						invalidCount++
					} else {
						fmt.Printf("Valid: %s\n", file)
						validCount++
					}
				}

				fmt.Printf("\nValidation Summary:\n")
				fmt.Printf("Valid documents: %d\n", validCount)
				fmt.Printf("Invalid documents: %d\n", invalidCount)
				fmt.Printf("Parse errors: %d\n", errorCount)
				fmt.Printf("Total files processed: %d\n", validCount+invalidCount+errorCount)

				return nil
			} else {
				// Process a single file
				fmt.Printf("Validating %s...\n", path)
				doc, err := p.ParseFile(path)
				if err != nil {
					return errors.NewIOError(fmt.Sprintf("Failed to parse file: %s", path), err)
				}

				err = v.ValidateDocument(doc)
				if err != nil {
					fmt.Printf("Validation failed:\n%v\n", err)
					return nil
				}

				fmt.Printf("Document is valid.\n")
				return nil
			}
		},
	}

	cmd.Flags().BoolVarP(&recursive, "recursive", "r", false, "Recursively validate documents in subdirectories")

	return cmd
}