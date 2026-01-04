package vocabulary

// Vocabulary represents the controlled vocabulary from doc/vocabulary.yaml
type Vocabulary struct {
	Topics   []VocabEntry `yaml:"topics"`
	DocTypes []VocabEntry `yaml:"docTypes"`
	Intent   []VocabEntry `yaml:"intent"`
}

// VocabEntry represents a single vocabulary entry
type VocabEntry struct {
	Slug        string `yaml:"slug"`
	Description string `yaml:"description"`
}

