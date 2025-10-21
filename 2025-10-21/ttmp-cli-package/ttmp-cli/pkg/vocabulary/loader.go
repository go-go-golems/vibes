package vocabulary

import (
	"fmt"
	"os"

	"gopkg.in/yaml.v3"
)

// Load reads and parses the vocabulary file
func Load(filepath string) (*Vocabulary, error) {
	content, err := os.ReadFile(filepath)
	if err != nil {
		return nil, fmt.Errorf("failed to read vocabulary file: %w", err)
	}

	var vocab Vocabulary
	if err := yaml.Unmarshal(content, &vocab); err != nil {
		return nil, fmt.Errorf("failed to parse vocabulary YAML: %w", err)
	}

	return &vocab, nil
}

// Save writes the vocabulary to a file
func Save(filepath string, vocab *Vocabulary) error {
	data, err := yaml.Marshal(vocab)
	if err != nil {
		return fmt.Errorf("failed to marshal vocabulary: %w", err)
	}

	return os.WriteFile(filepath, data, 0644)
}

// AddEntry adds a new entry to the specified category
func AddEntry(vocab *Vocabulary, category, slug, description string) error {
	entry := VocabEntry{
		Slug:        slug,
		Description: description,
	}

	switch category {
	case "topics":
		// Check if already exists
		for _, e := range vocab.Topics {
			if e.Slug == slug {
				return fmt.Errorf("topic '%s' already exists", slug)
			}
		}
		vocab.Topics = append(vocab.Topics, entry)
	case "docTypes":
		for _, e := range vocab.DocTypes {
			if e.Slug == slug {
				return fmt.Errorf("docType '%s' already exists", slug)
			}
		}
		vocab.DocTypes = append(vocab.DocTypes, entry)
	case "intent":
		for _, e := range vocab.Intent {
			if e.Slug == slug {
				return fmt.Errorf("intent '%s' already exists", slug)
			}
		}
		vocab.Intent = append(vocab.Intent, entry)
	default:
		return fmt.Errorf("unknown category: %s", category)
	}

	return nil
}

// GetEntries returns all entries for a category
func GetEntries(vocab *Vocabulary, category string) ([]VocabEntry, error) {
	switch category {
	case "topics":
		return vocab.Topics, nil
	case "docTypes":
		return vocab.DocTypes, nil
	case "intent":
		return vocab.Intent, nil
	default:
		return nil, fmt.Errorf("unknown category: %s", category)
	}
}

