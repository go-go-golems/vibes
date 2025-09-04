package cmd

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"vault-envrc-generator/pkg/envrc"
	"vault-envrc-generator/pkg/vault"
)

var (
	secretPath     string
	templateFile   string
	prefix         string
	excludeKeys    []string
	includeKeys    []string
	transformKeys  bool
	dryRun         bool
	format         string
)

// generateCmd represents the generate command
var generateCmd = &cobra.Command{
	Use:   "generate",
	Short: "Generate .envrc file from Vault secrets",
	Long: `Generate .envrc file from HashiCorp Vault secrets with various customization options.

This command connects to Vault, retrieves secrets from the specified path,
and generates a .envrc file with proper formatting and optional transformations.

Features:
- Support for KV v1 and v2 secret engines
- Custom templating with Go templates
- Key filtering (include/exclude patterns)
- Key transformation (uppercase, prefix addition)
- Multiple output formats (envrc, json, yaml)
- Dry-run mode for testing

Examples:
  # Basic usage
  vault-envrc-generator generate --path secret/myapp

  # With custom prefix and key transformation
  vault-envrc-generator generate --path secret/myapp --prefix MYAPP_ --transform-keys

  # Exclude sensitive keys
  vault-envrc-generator generate --path secret/myapp --exclude password,secret_key

  # Use custom template
  vault-envrc-generator generate --path secret/myapp --template custom.tmpl

  # Dry run to see what would be generated
  vault-envrc-generator generate --path secret/myapp --dry-run`,
	RunE: runGenerate,
}

func init() {
	rootCmd.AddCommand(generateCmd)

	generateCmd.Flags().StringVarP(&secretPath, "path", "p", "", "Vault secret path (required)")
	generateCmd.Flags().StringVarP(&templateFile, "template", "t", "", "Custom template file")
	generateCmd.Flags().StringVar(&prefix, "prefix", "", "Prefix to add to all environment variable names")
	generateCmd.Flags().StringSliceVar(&excludeKeys, "exclude", []string{}, "Keys to exclude (comma-separated)")
	generateCmd.Flags().StringSliceVar(&includeKeys, "include", []string{}, "Keys to include (comma-separated, overrides exclude)")
	generateCmd.Flags().BoolVar(&transformKeys, "transform-keys", false, "Transform keys to uppercase and replace - with _")
	generateCmd.Flags().BoolVar(&dryRun, "dry-run", false, "Show what would be generated without writing file")
	generateCmd.Flags().StringVarP(&format, "format", "f", "envrc", "Output format (envrc, json, yaml)")

	generateCmd.MarkFlagRequired("path")
}

func runGenerate(cmd *cobra.Command, args []string) error {
	if viper.GetBool("verbose") {
		fmt.Fprintf(os.Stderr, "Generating .envrc from Vault path: %s\n", secretPath)
	}

	// Initialize Vault client
	vaultClient, err := vault.NewClient(viper.GetString("vault.addr"), viper.GetString("vault.token"))
	if err != nil {
		return fmt.Errorf("failed to create Vault client: %w", err)
	}

	// Retrieve secrets
	secrets, err := vaultClient.GetSecrets(secretPath)
	if err != nil {
		return fmt.Errorf("failed to retrieve secrets from path %s: %w", secretPath, err)
	}

	if viper.GetBool("verbose") {
		fmt.Fprintf(os.Stderr, "Retrieved %d secrets from Vault\n", len(secrets))
	}

	// Create generator with options
	generator := envrc.NewGenerator(&envrc.Options{
		Prefix:        prefix,
		ExcludeKeys:   excludeKeys,
		IncludeKeys:   includeKeys,
		TransformKeys: transformKeys,
		Format:        format,
		TemplateFile:  templateFile,
		Verbose:       viper.GetBool("verbose"),
	})

	// Generate content
	content, err := generator.Generate(secrets)
	if err != nil {
		return fmt.Errorf("failed to generate content: %w", err)
	}

	// Handle dry run
	if dryRun {
		fmt.Println("=== DRY RUN OUTPUT ===")
		fmt.Println(content)
		fmt.Println("=== END DRY RUN ===")
		return nil
	}

	// Write to file
	outputPath := viper.GetString("output")
	if err := os.WriteFile(outputPath, []byte(content), 0644); err != nil {
		return fmt.Errorf("failed to write output file %s: %w", outputPath, err)
	}

	if viper.GetBool("verbose") {
		fmt.Fprintf(os.Stderr, "Successfully generated %s with %d environment variables\n", 
			outputPath, countEnvVars(content))
	}

	return nil
}

func countEnvVars(content string) int {
	lines := strings.Split(content, "\n")
	count := 0
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line != "" && !strings.HasPrefix(line, "#") && strings.Contains(line, "=") {
			count++
		}
	}
	return count
}

