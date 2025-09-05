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
	mountPath, secretPath := c.parsePath(path)

	// Try KV v2 first (common default), then fall back to KV v1 if needed
	if data, err := c.getKVv2Secrets(mountPath, secretPath); err == nil {
		return data, nil
	}

	// Fallback: KV v1 direct read
	if data, err := c.getKVv1Secrets(path); err == nil {
		return data, nil
	} else {
		return nil, err
	}
}

// PutSecrets writes secrets to the given path, handling KV v2 and v1
func (c *Client) PutSecrets(path string, data map[string]interface{}) error {
	mountPath, secretPath := c.parsePath(path)
	// Try KV v2 first
	if err := c.putKVv2Secrets(mountPath, secretPath, data); err == nil {
		return nil
	}
	// Fallback KV v1
	if err := c.putKVv1Secrets(path, data); err == nil {
		return nil
	} else {
		return err
	}
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

// putKVv1Secrets writes secrets to KV v1 engine
func (c *Client) putKVv1Secrets(path string, data map[string]interface{}) error {
	_, err := c.client.Logical().Write(path, data)
	if err != nil {
		return fmt.Errorf("failed to write secret to path %s: %w", path, err)
	}
	return nil
}

// putKVv2Secrets writes secrets to KV v2 engine
func (c *Client) putKVv2Secrets(mountPath, secretPath string, data map[string]interface{}) error {
	fullPath := fmt.Sprintf("%s/data/%s", mountPath, secretPath)
	payload := map[string]interface{}{"data": data}
	_, err := c.client.Logical().Write(fullPath, payload)
	if err != nil {
		return fmt.Errorf("failed to write KV v2 secret to %s: %w", fullPath, err)
	}
	return nil
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
	// First try direct list (works for KV v1 and some KV v2 setups)
	secret, err := c.client.Logical().List(path)
	if err == nil && secret != nil && secret.Data != nil {
		if keys, ok := secret.Data["keys"].([]interface{}); ok {
			var result []string
			for _, key := range keys {
				if keyStr, ok := key.(string); ok {
					result = append(result, keyStr)
				}
			}
			return result, nil
		}
	}

	// If that failed or returned nothing, try KV v2 metadata listing
	mountPath, secretPath := c.parsePath(path)
	var metaPath string
	if secretPath == "" {
		metaPath = fmt.Sprintf("%s/metadata", mountPath)
	} else {
		metaPath = fmt.Sprintf("%s/metadata/%s", mountPath, strings.TrimSuffix(secretPath, "/"))
	}

	metaList, err2 := c.client.Logical().List(metaPath)
	if err2 != nil || metaList == nil || metaList.Data == nil {
		// Return original error if we had one; otherwise, a generic not found
		if err != nil {
			return nil, fmt.Errorf("failed to list secrets at path %s: %w", path, err)
		}
		return []string{}, nil
	}

	keys, ok := metaList.Data["keys"].([]interface{})
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
