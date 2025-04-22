// pkg/config/config.go
package config

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v3"
)

// Config represents the configuration for the KB application
type Config struct {
	IndexPath    string   `yaml:"index_path"`
	ConfigPath   string   `yaml:"config_path"`
	Dimension    int      `yaml:"dimension"`
	Embedder     Embedder `yaml:"embedder"`
	Languages    []string `yaml:"languages"`
	Repositories []string `yaml:"repositories"`
	LLM          LLM      `yaml:"llm"`
}

// Embedder represents the configuration for the embedder
type Embedder struct {
	Provider string `yaml:"provider"`
	Endpoint string `yaml:"endpoint"`
}

// LLM represents the configuration for the LLM
type LLM struct {
	Provider string `yaml:"provider"`
	Model    string `yaml:"model"`
}

// DefaultConfig returns a default configuration
func DefaultConfig() *Config {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		homeDir = "."
	}

	configPath := filepath.Join(homeDir, ".kb", "config.yaml")

	return &Config{
		IndexPath:    filepath.Join(homeDir, ".kb", "index"),
		ConfigPath:   configPath,
		Dimension:    768,
		Embedder: Embedder{
			Provider: "bge-large",
			Endpoint: "http://localhost:8000/encode",
		},
		Languages:    []string{"go", "python", "ts", "cpp"},
		Repositories: []string{},
		LLM: LLM{
			Provider: "openai",
			Model:    "gpt-4o",
		},
	}
}

// LoadConfig loads the configuration from the specified file
func LoadConfig(path string) (*Config, error) {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	config := &Config{}
	if err := yaml.Unmarshal(data, config); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	// Set the config path
	config.ConfigPath = path

	return config, nil
}

// SaveConfig saves the configuration to the specified file
func SaveConfig(config *Config, path string) error {
	// Ensure the directory exists
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	data, err := yaml.Marshal(config)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %w", err)
	}

	if err := ioutil.WriteFile(path, data, 0644); err != nil {
		return fmt.Errorf("failed to write config file: %w", err)
	}

	return nil
}

// AddRepo adds a repository to the configuration
func (c *Config) AddRepo(repo string) {
	// Check if the repo already exists
	for _, r := range c.Repositories {
		if r == repo {
			return
		}
	}

	c.Repositories = append(c.Repositories, repo)
}

// RemoveRepo removes a repository from the configuration
func (c *Config) RemoveRepo(repo string) {
	repos := make([]string, 0, len(c.Repositories))
	for _, r := range c.Repositories {
		if r != repo {
			repos = append(repos, r)
		}
	}
	c.Repositories = repos
}
