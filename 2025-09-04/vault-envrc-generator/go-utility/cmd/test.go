package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"vault-envrc-generator/pkg/vault"
)

// testCmd represents the test command
var testCmd = &cobra.Command{
	Use:   "test",
	Short: "Test Vault connectivity and authentication",
	Long: `Test the connection to HashiCorp Vault and verify authentication.

This command performs comprehensive connectivity tests including:
- Network connectivity to Vault server
- Vault server health check
- Authentication token validation
- Basic secret read permissions test

This is useful for troubleshooting connection issues and verifying
that the utility can successfully communicate with Vault before
attempting to generate .envrc files.

Examples:
  # Basic connectivity test
  vault-envrc-generator test

  # Test with specific Vault address
  vault-envrc-generator test --vault-addr https://vault.example.com:8200

  # Test with verbose output
  vault-envrc-generator test --verbose`,
	RunE: runTest,
}

func init() {
	rootCmd.AddCommand(testCmd)
}

func runTest(cmd *cobra.Command, args []string) error {
	fmt.Println("=== Vault Connectivity Test ===")
	fmt.Println()

	vaultAddr := viper.GetString("vault.addr")
	vaultToken := viper.GetString("vault.token")

	if vaultAddr == "" {
		return fmt.Errorf("Vault address not specified. Use --vault-addr or set VAULT_ADDR environment variable")
	}

	if vaultToken == "" {
		return fmt.Errorf("Vault token not specified. Use --vault-token or set VAULT_TOKEN environment variable")
	}

	fmt.Printf("Testing connection to: %s\n", vaultAddr)
	fmt.Printf("Using token: %s...\n", maskToken(vaultToken))
	fmt.Println()

	// Test 1: Create Vault client
	fmt.Print("1. Creating Vault client... ")
	vaultClient, err := vault.NewClient(vaultAddr, vaultToken)
	if err != nil {
		fmt.Printf("❌ FAILED\n")
		return fmt.Errorf("failed to create Vault client: %w", err)
	}
	fmt.Printf("✓ SUCCESS\n")

	// Test 2: Basic connectivity and health
	fmt.Print("2. Testing Vault health... ")
	client := vaultClient.GetClient()
	health, err := client.Sys().Health()
	if err != nil {
		fmt.Printf("❌ FAILED\n")
		return fmt.Errorf("failed to check Vault health: %w", err)
	}
	fmt.Printf("✓ SUCCESS\n")

	if viper.GetBool("verbose") {
		fmt.Printf("   - Initialized: %t\n", health.Initialized)
		fmt.Printf("   - Sealed: %t\n", health.Sealed)
		fmt.Printf("   - Version: %s\n", health.Version)
	}

	// Test 3: Authentication
	fmt.Print("3. Testing authentication... ")
	tokenInfo, err := client.Auth().Token().LookupSelf()
	if err != nil {
		fmt.Printf("❌ FAILED\n")
		return fmt.Errorf("authentication failed: %w", err)
	}
	fmt.Printf("✓ SUCCESS\n")

	if viper.GetBool("verbose") && tokenInfo != nil && tokenInfo.Data != nil {
		if policies, ok := tokenInfo.Data["policies"].([]interface{}); ok {
			fmt.Printf("   - Policies: %v\n", policies)
		}
		if ttl, ok := tokenInfo.Data["ttl"].(float64); ok {
			fmt.Printf("   - TTL: %.0f seconds\n", ttl)
		}
	}

	// Test 4: List mounts (to test read permissions)
	fmt.Print("4. Testing mount access... ")
	mounts, err := client.Sys().ListMounts()
	if err != nil {
		fmt.Printf("❌ FAILED\n")
		fmt.Printf("   Warning: Cannot list mounts (may indicate limited permissions): %v\n", err)
	} else {
		fmt.Printf("✓ SUCCESS\n")
		if viper.GetBool("verbose") {
			fmt.Printf("   - Available mounts: ")
			mountNames := make([]string, 0, len(mounts))
			for mount := range mounts {
				mountNames = append(mountNames, mount)
			}
			fmt.Printf("%v\n", mountNames)
		}
	}

	// Test 5: Comprehensive connection test
	fmt.Print("5. Running comprehensive test... ")
	if err := vaultClient.TestConnection(); err != nil {
		fmt.Printf("❌ FAILED\n")
		return fmt.Errorf("comprehensive test failed: %w", err)
	}
	fmt.Printf("✓ SUCCESS\n")

	fmt.Println()
	fmt.Println("🎉 All tests passed! Vault is ready for use.")
	fmt.Println()

	// Provide helpful information
	fmt.Println("Next steps:")
	fmt.Println("- Use 'vault-envrc-generator generate --path <secret-path>' to generate .envrc files")
	fmt.Println("- Use 'vault-envrc-generator interactive' for guided secret selection")
	fmt.Println("- Use 'vault-envrc-generator batch --config <config-file>' for batch processing")

	return nil
}

func maskToken(token string) string {
	if len(token) <= 8 {
		return "***"
	}
	return token[:4] + "..." + token[len(token)-4:]
}

