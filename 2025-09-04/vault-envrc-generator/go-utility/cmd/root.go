package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	cfgFile     string
	vaultAddr   string
	vaultToken  string
	tokenSource string
	tokenFile   string
	outputFile  string
	verbose     bool
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "vault-envrc-generator",
	Short: "Generate .envrc files from HashiCorp Vault secrets",
	Long: `A comprehensive utility to generate .envrc files from HashiCorp Vault secrets.

This tool provides various features including:
- Multiple secret engine support (KV v1/v2, Database, etc.)
- Template-based generation with custom formatting
- Filtering and transformation capabilities
- Batch processing of multiple paths
- Audit logging integration
- Configuration file support
- Interactive mode for secret selection

Examples:
  # Generate .envrc from a single KV path
  vault-envrc-generator generate --path secret/myapp --output .envrc

  # Generate with custom template
  vault-envrc-generator generate --path secret/myapp --template custom.tmpl

  # Interactive mode
  vault-envrc-generator interactive

  # Batch process multiple paths
  vault-envrc-generator batch --config batch-config.yaml`,
	Version: "1.0.0",
}

// Execute adds all child commands to the root command and sets flags appropriately.
func Execute() error {
	return rootCmd.Execute()
}

func init() {
	cobra.OnInitialize(initConfig)

	// Global flags
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.vault-envrc-generator.yaml)")
	rootCmd.PersistentFlags().StringVar(&vaultAddr, "vault-addr", "", "Vault server address (default: $VAULT_ADDR or http://127.0.0.1:8200)")
	rootCmd.PersistentFlags().StringVar(&vaultToken, "vault-token", "", "Vault authentication token (default: $VAULT_TOKEN)")
	rootCmd.PersistentFlags().StringVar(&tokenSource, "vault-token-source", "auto", "Token source: auto, env, file, lookup")
	rootCmd.PersistentFlags().StringVar(&tokenFile, "vault-token-file", "", "Path to token file (default: ~/.vault-token)")
	rootCmd.PersistentFlags().StringVar(&outputFile, "output", ".envrc", "Output file path")
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "Enable verbose output")

	// Bind flags to viper
	viper.BindPFlag("vault.addr", rootCmd.PersistentFlags().Lookup("vault-addr"))
	viper.BindPFlag("vault.token", rootCmd.PersistentFlags().Lookup("vault-token"))
	viper.BindPFlag("vault.token_source", rootCmd.PersistentFlags().Lookup("vault-token-source"))
	viper.BindPFlag("vault.token_file", rootCmd.PersistentFlags().Lookup("vault-token-file"))
	viper.BindPFlag("output", rootCmd.PersistentFlags().Lookup("output"))
	viper.BindPFlag("verbose", rootCmd.PersistentFlags().Lookup("verbose"))
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	if cfgFile != "" {
		// Use config file from the flag.
		viper.SetConfigFile(cfgFile)
	} else {
		// Find home directory.
		home, err := os.UserHomeDir()
		cobra.CheckErr(err)

		// Search config in home directory with name ".vault-envrc-generator" (without extension).
		viper.AddConfigPath(home)
		viper.AddConfigPath(".")
		viper.SetConfigType("yaml")
		viper.SetConfigName(".vault-envrc-generator")
	}

	// Environment variable support
	viper.SetEnvPrefix("VAULT_ENVRC")
	viper.AutomaticEnv()

	// Set defaults
	viper.SetDefault("vault.addr", getEnvOrDefault("VAULT_ADDR", "http://127.0.0.1:8200"))
	viper.SetDefault("vault.token", os.Getenv("VAULT_TOKEN"))
	viper.SetDefault("vault.token_source", "auto")
	viper.SetDefault("vault.token_file", "")
	viper.SetDefault("output", ".envrc")
	viper.SetDefault("verbose", false)

	// If a config file is found, read it in.
	if err := viper.ReadInConfig(); err == nil && verbose {
		fmt.Fprintln(os.Stderr, "Using config file:", viper.ConfigFileUsed())
	}
}

func getEnvOrDefault(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}
