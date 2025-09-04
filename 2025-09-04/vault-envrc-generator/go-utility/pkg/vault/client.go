package vault

import (
	"fmt"
	"strings"

	"github.com/hashicorp/vault/api"
)

// Client wraps the Vault API client with additional functionality
type Client struct {
	client *api.Client
}

// NewClient creates a new Vault client with the given address and token
func NewClient(address, token string) (*Client, error) {
	config := api.DefaultConfig()
	config.Address = address

	client, err := api.NewClient(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create Vault client: %w", err)
	}

	client.SetToken(token)

	// Test the connection
	_, err = client.Sys().Health()
	if err != nil {
		return nil, fmt.Errorf("failed to connect to Vault at %s: %w", address, err)
	}

	return &Client{client: client}, nil
}

// GetSecrets retrieves secrets from the given path, handling both KV v1 and v2
func (c *Client) GetSecrets(path string) (map[string]interface{}, error) {
	// First, try to determine if this is a KV v2 mount
	mountPath, secretPath := c.parsePath(path)
	
	// Check mount info to determine KV version
	mounts, err := c.client.Sys().ListMounts()
	if err != nil {
		return nil, fmt.Errorf("failed to list mounts: %w", err)
	}

	var isKVv2 bool
	if mount, exists := mounts[mountPath+"/"]; exists {
		if mount.Type == "kv" && mount.Options != nil {
			if version, ok := mount.Options["version"]; ok && version == "2" {
				isKVv2 = true
			}
		}
	}

	// Try KV v2 first if detected, otherwise try both
	if isKVv2 {
		return c.getKVv2Secrets(mountPath, secretPath)
	}

	// Try KV v1 first, then v2 if that fails
	secrets, err := c.getKVv1Secrets(path)
	if err != nil {
		// If KV v1 fails, try KV v2
		if secrets, err2 := c.getKVv2Secrets(mountPath, secretPath); err2 == nil {
			return secrets, nil
		}
		return nil, err
	}

	return secrets, nil
}

// getKVv1Secrets retrieves secrets from KV v1 engine
func (c *Client) getKVv1Secrets(path string) (map[string]interface{}, error) {
	secret, err := c.client.Logical().Read(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read secret from path %s: %w", path, err)
	}

	if secret == nil {
		return nil, fmt.Errorf("no secret found at path %s", path)
	}

	return secret.Data, nil
}

// getKVv2Secrets retrieves secrets from KV v2 engine
func (c *Client) getKVv2Secrets(mountPath, secretPath string) (map[string]interface{}, error) {
	// KV v2 requires reading from data/ prefix
	fullPath := fmt.Sprintf("%s/data/%s", mountPath, secretPath)
	
	secret, err := c.client.Logical().Read(fullPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read secret from KV v2 path %s: %w", fullPath, err)
	}

	if secret == nil {
		return nil, fmt.Errorf("no secret found at KV v2 path %s", fullPath)
	}

	// KV v2 wraps the actual data in a "data" field
	if data, ok := secret.Data["data"].(map[string]interface{}); ok {
		return data, nil
	}

	return nil, fmt.Errorf("invalid KV v2 secret format at path %s", fullPath)
}

// parsePath splits a path into mount path and secret path
func (c *Client) parsePath(path string) (string, string) {
	parts := strings.SplitN(path, "/", 2)
	if len(parts) == 1 {
		return parts[0], ""
	}
	return parts[0], parts[1]
}

// ListSecrets lists all secrets at the given path (for interactive mode)
func (c *Client) ListSecrets(path string) ([]string, error) {
	// Try to list secrets
	secret, err := c.client.Logical().List(path)
	if err != nil {
		return nil, fmt.Errorf("failed to list secrets at path %s: %w", path, err)
	}

	if secret == nil || secret.Data == nil {
		return []string{}, nil
	}

	keys, ok := secret.Data["keys"].([]interface{})
	if !ok {
		return []string{}, nil
	}

	var result []string
	for _, key := range keys {
		if keyStr, ok := key.(string); ok {
			result = append(result, keyStr)
		}
	}

	return result, nil
}

// GetClient returns the underlying Vault API client
func (c *Client) GetClient() *api.Client {
	return c.client
}

// TestConnection tests the Vault connection and authentication
func (c *Client) TestConnection() error {
	// Test basic connectivity
	health, err := c.client.Sys().Health()
	if err != nil {
		return fmt.Errorf("failed to check Vault health: %w", err)
	}

	if !health.Initialized {
		return fmt.Errorf("Vault is not initialized")
	}

	if health.Sealed {
		return fmt.Errorf("Vault is sealed")
	}

	// Test authentication by trying to read token info
	_, err = c.client.Auth().Token().LookupSelf()
	if err != nil {
		return fmt.Errorf("authentication failed: %w", err)
	}

	return nil
}

