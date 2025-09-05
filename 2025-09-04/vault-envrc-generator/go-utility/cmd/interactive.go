package cmd

import (
	"bufio"
	"context"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"vault-envrc-generator/pkg/envrc"
	"vault-envrc-generator/pkg/vault"
)

// interactiveCmd represents the interactive command
var interactiveCmd = &cobra.Command{
	Use:   "interactive",
	Short: "Interactive mode for selecting and generating .envrc files",
	Long: `Interactive mode provides a user-friendly interface for:

- Browsing available Vault paths
- Selecting specific secrets to include
- Configuring generation options
- Previewing output before saving
- Testing Vault connectivity

This mode is ideal for exploring Vault contents and fine-tuning
the .envrc generation process.

The interactive session will guide you through:
1. Vault connection testing
2. Path browsing and selection
3. Secret filtering and transformation options
4. Output preview and confirmation
5. File generation`,
	RunE: runInteractive,
}

func init() {
	rootCmd.AddCommand(interactiveCmd)
}

func runInteractive(cmd *cobra.Command, args []string) error {
	fmt.Println("=== Vault .envrc Generator - Interactive Mode ===")
	fmt.Println()

	reader := bufio.NewReader(os.Stdin)

	// Initialize Vault client
	fmt.Println("Connecting to Vault...")
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	resolvedToken, err := vault.ResolveToken(
		ctx,
		viper.GetString("vault.token"),
		vault.TokenSource(viper.GetString("vault.token_source")),
		viper.GetString("vault.token_file"),
		viper.GetBool("verbose"),
	)
	if err != nil {
		return fmt.Errorf("failed to resolve Vault token: %w", err)
	}
	vaultClient, err := vault.NewClient(viper.GetString("vault.addr"), resolvedToken)
	if err != nil {
		return fmt.Errorf("failed to create Vault client: %w", err)
	}

	// Test connection
	if err := vaultClient.TestConnection(); err != nil {
		return fmt.Errorf("Vault connection test failed: %w", err)
	}

	fmt.Printf("✓ Successfully connected to Vault at %s\n\n", viper.GetString("vault.addr"))

	// Get secret path
	secretPath, err := promptForPath(reader, vaultClient)
	if err != nil {
		return err
	}

	// Retrieve secrets
	fmt.Printf("Retrieving secrets from path: %s\n", secretPath)
	secrets, err := vaultClient.GetSecrets(secretPath)
	if err != nil {
		return fmt.Errorf("failed to retrieve secrets: %w", err)
	}

	fmt.Printf("Found %d secrets\n\n", len(secrets))

	// Show available keys
	fmt.Println("Available keys:")
	keys := make([]string, 0, len(secrets))
	for key := range secrets {
		keys = append(keys, key)
	}
	for i, key := range keys {
		fmt.Printf("  %d. %s\n", i+1, key)
	}
	fmt.Println()

	// Configure options
	options, err := promptForOptions(reader, keys)
	if err != nil {
		return err
	}

	// Generate preview
	generator := envrc.NewGenerator(options)
	content, err := generator.Generate(secrets)
	if err != nil {
		return fmt.Errorf("failed to generate content: %w", err)
	}

	// Show preview
	fmt.Println("=== PREVIEW ===")
	fmt.Println(content)
	fmt.Println("=== END PREVIEW ===")
	fmt.Println()

	// Confirm and save
	if confirmed, err := promptConfirmation(reader, "Save this content to file?"); err != nil {
		return err
	} else if !confirmed {
		fmt.Println("Generation cancelled.")
		return nil
	}

	// Get output file
	outputFile := viper.GetString("output")
	fmt.Printf("Output file [%s]: ", outputFile)
	if input, err := reader.ReadString('\n'); err != nil {
		return err
	} else if strings.TrimSpace(input) != "" {
		outputFile = strings.TrimSpace(input)
	}

	// Write file
	if err := os.WriteFile(outputFile, []byte(content), 0644); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	fmt.Printf("✓ Successfully generated %s\n", outputFile)
	return nil
}

func promptForPath(reader *bufio.Reader, vaultClient *vault.Client) (string, error) {
	fmt.Print("Enter Vault secret path: ")
	input, err := reader.ReadString('\n')
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(input), nil
}

func promptForOptions(reader *bufio.Reader, availableKeys []string) (*envrc.Options, error) {
	options := &envrc.Options{}

	// Prefix
	fmt.Print("Enter prefix for environment variables (optional): ")
	if input, err := reader.ReadString('\n'); err != nil {
		return nil, err
	} else {
		options.Prefix = strings.TrimSpace(input)
	}

	// Transform keys
	if confirmed, err := promptConfirmation(reader, "Transform keys to uppercase and replace - with _?"); err != nil {
		return nil, err
	} else {
		options.TransformKeys = confirmed
	}

	// Include/exclude keys
	fmt.Println("\nKey filtering options:")
	fmt.Println("1. Include all keys")
	fmt.Println("2. Select specific keys to include")
	fmt.Println("3. Select specific keys to exclude")

	fmt.Print("Choose option [1]: ")
	if input, err := reader.ReadString('\n'); err != nil {
		return nil, err
	} else {
		choice := strings.TrimSpace(input)
		if choice == "" {
			choice = "1"
		}

		switch choice {
		case "2":
			if keys, err := promptForKeys(reader, availableKeys, "include"); err != nil {
				return nil, err
			} else {
				options.IncludeKeys = keys
			}
		case "3":
			if keys, err := promptForKeys(reader, availableKeys, "exclude"); err != nil {
				return nil, err
			} else {
				options.ExcludeKeys = keys
			}
		}
	}

	// Output format
	fmt.Println("\nOutput format:")
	fmt.Println("1. .envrc (default)")
	fmt.Println("2. JSON")
	fmt.Println("3. YAML")

	fmt.Print("Choose format [1]: ")
	if input, err := reader.ReadString('\n'); err != nil {
		return nil, err
	} else {
		choice := strings.TrimSpace(input)
		switch choice {
		case "2":
			options.Format = "json"
		case "3":
			options.Format = "yaml"
		default:
			options.Format = "envrc"
		}
	}

	return options, nil
}

func promptForKeys(reader *bufio.Reader, availableKeys []string, action string) ([]string, error) {
	fmt.Printf("\nSelect keys to %s (comma-separated numbers or key names):\n", action)
	for i, key := range availableKeys {
		fmt.Printf("  %d. %s\n", i+1, key)
	}

	fmt.Printf("Enter selection: ")
	input, err := reader.ReadString('\n')
	if err != nil {
		return nil, err
	}

	input = strings.TrimSpace(input)
	if input == "" {
		return []string{}, nil
	}

	var selectedKeys []string
	parts := strings.Split(input, ",")

	for _, part := range parts {
		part = strings.TrimSpace(part)

		// Try to parse as number
		if num, err := strconv.Atoi(part); err == nil {
			if num >= 1 && num <= len(availableKeys) {
				selectedKeys = append(selectedKeys, availableKeys[num-1])
			}
		} else {
			// Treat as key name
			selectedKeys = append(selectedKeys, part)
		}
	}

	return selectedKeys, nil
}

func promptConfirmation(reader *bufio.Reader, message string) (bool, error) {
	fmt.Printf("%s [y/N]: ", message)
	input, err := reader.ReadString('\n')
	if err != nil {
		return false, err
	}

	response := strings.ToLower(strings.TrimSpace(input))
	return response == "y" || response == "yes", nil
}
