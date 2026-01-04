package vocabulary

import (
	"fmt"
	"strings"
)

// ValidateTopic checks if a topic exists in the vocabulary
func ValidateTopic(vocab *Vocabulary, topic string) error {
	for _, entry := range vocab.Topics {
		if entry.Slug == topic {
			return nil
		}
	}
	return fmt.Errorf("unknown topic: %s", topic)
}

// ValidateDocType checks if a doc type exists in the vocabulary
func ValidateDocType(vocab *Vocabulary, docType string) error {
	for _, entry := range vocab.DocTypes {
		if entry.Slug == docType {
			return nil
		}
	}
	return fmt.Errorf("unknown docType: %s", docType)
}

// ValidateIntent checks if an intent exists in the vocabulary
func ValidateIntent(vocab *Vocabulary, intent string) error {
	for _, entry := range vocab.Intent {
		if entry.Slug == intent {
			return nil
		}
	}
	return fmt.Errorf("unknown intent: %s", intent)
}

// ValidateTopics validates multiple topics
func ValidateTopics(vocab *Vocabulary, topics []string) []error {
	var errors []error
	for _, topic := range topics {
		if err := ValidateTopic(vocab, topic); err != nil {
			errors = append(errors, err)
		}
	}
	return errors
}

// SuggestTopic suggests similar topics based on Levenshtein distance
func SuggestTopic(vocab *Vocabulary, topic string) []string {
	var suggestions []string
	topic = strings.ToLower(topic)
	
	for _, entry := range vocab.Topics {
		slug := strings.ToLower(entry.Slug)
		if strings.Contains(slug, topic) || strings.Contains(topic, slug) {
			suggestions = append(suggestions, entry.Slug)
		}
	}
	
	return suggestions
}

// SuggestDocType suggests similar doc types
func SuggestDocType(vocab *Vocabulary, docType string) []string {
	var suggestions []string
	docType = strings.ToLower(docType)
	
	for _, entry := range vocab.DocTypes {
		slug := strings.ToLower(entry.Slug)
		if strings.Contains(slug, docType) || strings.Contains(docType, slug) {
			suggestions = append(suggestions, entry.Slug)
		}
	}
	
	return suggestions
}

