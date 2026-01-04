package configfields

// Config is a configuration struct with deprecated fields
type Config struct {
	IndexPath    string   `yaml:"index_path"`
	ConfigPath   string   `yaml:"config_path"`  // Deprecated field
	Dimension    int      `yaml:"dimension"`
	Embedder     string   `yaml:"embedder"`
	Languages    []string `yaml:"languages"`
	Repositories []string `yaml:"repositories"` // Deprecated field
	LLM          string   `yaml:"llm"`
}

// UseConfig demonstrates using the deprecated fields
func UseConfig() {
	cfg := &Config{
		IndexPath:    "/path/to/index",
		ConfigPath:   "/path/to/config",  // Should trigger linter
		Dimension:    128,
		Repositories: []string{"/repo1", "/repo2"}, // Should trigger linter
	}
	
	// Using the deprecated fields
	_ = cfg
}
