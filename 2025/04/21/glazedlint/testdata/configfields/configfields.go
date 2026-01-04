// Package configfields demonstrates the error of using deprecated Config fields
package configfields

import (
	"fmt"
)

// Config is a configuration struct with deprecated fields
type Config struct {
	IndexPath    string   `yaml:"index_path"`
	ConfigPath   string   `yaml:"config_path"`  // Deprecated: moved to file-based settings
	Dimension    int      `yaml:"dimension"`
	Embedder     string   `yaml:"embedder"`
	Languages    []string `yaml:"languages"`
	Repositories []string `yaml:"repositories"` // Deprecated: renamed to Repos in v0.3
	LLM          string   `yaml:"llm"`
}

// UseConfig demonstrates using the deprecated fields
func UseConfig() {
	cfg := &Config{
		IndexPath:    "/path/to/index",
		ConfigPath:   "/path/to/config",  // Should be removed
		Dimension:    128,
		Repositories: []string{"/repo1", "/repo2"}, // Should be Repos
	}

	// Using the deprecated fields
	fmt.Println("Config path:", cfg.ConfigPath)
	for _, repo := range cfg.Repositories {
		fmt.Println("Repository:", repo)
	}
}
