package cmd

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
	"vault-envrc-generator/pkg/seed"
	"vault-envrc-generator/pkg/vault"
)

type SeedSpec seed.Spec

var (
	seedConfig string
	seedDryRun bool
)

var seedCmd = &cobra.Command{
	Use:   "seed",
	Short: "Seed Vault secrets from a YAML spec and environment",
	Long: `Seed KV secrets into Vault using a YAML specification.

YAML format:

base_path: secrets/environments/personal/102454784610416055110/manuel
sets:
  - path: core
    data:
      VAULT_ADDR: https://vault.mento.co/
    env:
      OP_ACCOUNT: OP_ACCOUNT
      OP_VAULT: OP_VAULT
  - path: google
    env:
      client_email: GOOGLE_EMAIL
    files:
      private_key: /path/to/key.pem

Paths in sets are joined to base_path when not absolute.`,
	RunE: runSeed,
}

func init() {
	rootCmd.AddCommand(seedCmd)
	seedCmd.Flags().StringVarP(&seedConfig, "config", "c", "", "Seed YAML file (required)")
	seedCmd.Flags().BoolVar(&seedDryRun, "dry-run", false, "Show actions without writing to Vault")
	seedCmd.MarkFlagRequired("config")
}

func runSeed(cmd *cobra.Command, args []string) error {
	b, err := os.ReadFile(seedConfig)
	if err != nil {
		return fmt.Errorf("failed to read seed config: %w", err)
	}

	var spec seed.Spec
	if err := yaml.Unmarshal(b, &spec); err != nil {
		return fmt.Errorf("failed to parse YAML: %w", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	token, err := vault.ResolveToken(
		ctx,
		viper.GetString("vault.token"),
		vault.TokenSource(viper.GetString("vault.token_source")),
		viper.GetString("vault.token_file"),
		viper.GetBool("verbose"),
	)
	if err != nil {
		return fmt.Errorf("failed to resolve Vault token: %w", err)
	}
	client, err := vault.NewClient(viper.GetString("vault.addr"), token)
	if err != nil {
		return fmt.Errorf("failed to create Vault client: %w", err)
	}

	return seed.Run(client, &spec, seed.Options{DryRun: seedDryRun}, viper.GetBool("verbose"))
}
