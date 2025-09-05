package vault

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// TokenSource defines where to resolve the Vault token from
type TokenSource string

const (
	TokenSourceAuto   TokenSource = "auto"
	TokenSourceEnv    TokenSource = "env"
	TokenSourceFile   TokenSource = "file"
	TokenSourceLookup TokenSource = "lookup"
)

// ResolveToken attempts to resolve a Vault token using the specified source strategy.
// If explicitToken is provided and source is Auto or Env, it will be used directly.
// For Auto, the order is: explicit/env -> file -> lookup.
func ResolveToken(ctx context.Context, explicitToken string, source TokenSource, tokenFilePath string, verbose bool) (string, error) {
	// Normalize source
	if source == "" {
		source = TokenSourceAuto
	}

	// Expand ~ in tokenFilePath if present
	if tokenFilePath != "" && strings.HasPrefix(tokenFilePath, "~") {
		if home, err := os.UserHomeDir(); err == nil {
			tokenFilePath = filepath.Join(home, strings.TrimPrefix(tokenFilePath, "~"))
		}
	}

	switch source {
	case TokenSourceEnv:
		if explicitToken != "" {
			return explicitToken, nil
		}
		if t := os.Getenv("VAULT_TOKEN"); t != "" {
			return t, nil
		}
		return "", fmt.Errorf("no token found in environment")

	case TokenSourceFile:
		if tokenFilePath == "" {
			// default to ~/.vault-token
			if home, err := os.UserHomeDir(); err == nil {
				tokenFilePath = filepath.Join(home, ".vault-token")
			}
		}
		data, err := os.ReadFile(tokenFilePath)
		if err != nil {
			return "", fmt.Errorf("failed to read token file %s: %w", tokenFilePath, err)
		}
		return strings.TrimSpace(string(data)), nil

	case TokenSourceLookup:
		return lookupTokenViaCLI(ctx, verbose)

	case TokenSourceAuto:
		// 1) explicit or env
		if explicitToken != "" {
			return explicitToken, nil
		}
		if t := os.Getenv("VAULT_TOKEN"); t != "" {
			return t, nil
		}
		// 2) file
		if token, err := ResolveToken(ctx, "", TokenSourceFile, tokenFilePath, verbose); err == nil && token != "" {
			return token, nil
		}
		// 3) lookup
		if token, err := ResolveToken(ctx, "", TokenSourceLookup, tokenFilePath, verbose); err == nil && token != "" {
			return token, nil
		}
		return "", fmt.Errorf("unable to resolve Vault token (tried env, file, lookup)")
	}

	return "", fmt.Errorf("unknown token source: %s", source)
}

// lookupTokenViaCLI runs `vault token lookup -format=json` and extracts .data.id.
func lookupTokenViaCLI(ctx context.Context, verbose bool) (string, error) {
	cmd := exec.CommandContext(ctx, "vault", "token", "lookup", "-format=json")
	// Inherit env so VAULT_TOKEN/VAULT_ADDR/etc. can be used by the CLI
	cmd.Env = os.Environ()
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to execute 'vault token lookup': %w", err)
	}

	// Parse JSON: expect { data: { id: "..." } }
	var payload struct {
		Data map[string]any `json:"data"`
	}
	if err := json.Unmarshal(out, &payload); err != nil {
		return "", fmt.Errorf("failed to parse lookup output: %w", err)
	}
	if payload.Data == nil {
		return "", fmt.Errorf("lookup output missing data")
	}
	if idVal, ok := payload.Data["id"]; ok {
		if idStr, ok := idVal.(string); ok && idStr != "" {
			return idStr, nil
		}
	}
	return "", fmt.Errorf("could not extract token id from lookup output")
}
