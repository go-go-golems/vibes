package listing

// Entry represents a listed path; type is either "directory" or "secret"
type Entry struct {
	Path     string            `yaml:"path"`
	Type     string            `yaml:"type"`
	Children []string          `yaml:"children,omitempty"`
	Keys     []string          `yaml:"keys,omitempty"`
	Data     map[string]string `yaml:"data,omitempty"`
}
