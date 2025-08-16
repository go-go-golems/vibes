package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
	"os"
)

// Option represents a single option in a multiple choice question
type Option struct {
	ID        string `yaml:"id"`
	Text      string `yaml:"text"`
	Rationale string `yaml:"rationale"`
}

// Question represents a single question in the pretest
type Question struct {
	ID         string   `yaml:"id"`
	Type       string   `yaml:"type"`       // "mcq" or "short"
	Prompt     string   `yaml:"prompt"`
	Options    []Option `yaml:"options,omitempty"`    // Only for MCQ
	Answer     string   `yaml:"answer,omitempty"`     // Only for MCQ - correct option ID
	Hints      []string `yaml:"hints,omitempty"`
	References []string `yaml:"references,omitempty"`
}

// Pretest represents the root structure of a pretest YAML file
type Pretest struct {
	Title     string     `yaml:"title"`
	Questions []Question `yaml:"questions"`
}

// PretestFile represents the complete YAML file structure
type PretestFile struct {
	Pretest Pretest `yaml:"pretest"`
}

// LoadPretestFromFile loads a pretest from a YAML file
func LoadPretestFromFile(filename string) (*PretestFile, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read file %s: %w", filename, err)
	}

	var pretestFile PretestFile
	err = yaml.Unmarshal(data, &pretestFile)
	if err != nil {
		return nil, fmt.Errorf("failed to parse YAML: %w", err)
	}

	// Validate the structure
	if err := validatePretest(&pretestFile.Pretest); err != nil {
		return nil, fmt.Errorf("validation failed: %w", err)
	}

	return &pretestFile, nil
}

// validatePretest validates the pretest structure
func validatePretest(pretest *Pretest) error {
	if pretest.Title == "" {
		return fmt.Errorf("pretest title cannot be empty")
	}

	if len(pretest.Questions) == 0 {
		return fmt.Errorf("pretest must have at least one question")
	}

	for i, question := range pretest.Questions {
		if err := validateQuestion(&question, i); err != nil {
			return err
		}
	}

	return nil
}

// validateQuestion validates a single question
func validateQuestion(question *Question, index int) error {
	if question.ID == "" {
		return fmt.Errorf("question %d: ID cannot be empty", index+1)
	}

	if question.Type != "mcq" && question.Type != "short" {
		return fmt.Errorf("question %d (%s): type must be 'mcq' or 'short'", index+1, question.ID)
	}

	if question.Prompt == "" {
		return fmt.Errorf("question %d (%s): prompt cannot be empty", index+1, question.ID)
	}

	if question.Type == "mcq" {
		if len(question.Options) == 0 {
			return fmt.Errorf("question %d (%s): MCQ must have at least one option", index+1, question.ID)
		}

		if question.Answer == "" {
			return fmt.Errorf("question %d (%s): MCQ must have an answer", index+1, question.ID)
		}

		// Validate that the answer exists in options
		answerFound := false
		for _, option := range question.Options {
			if option.ID == question.Answer {
				answerFound = true
				break
			}
		}
		if !answerFound {
			return fmt.Errorf("question %d (%s): answer '%s' not found in options", index+1, question.ID, question.Answer)
		}

		// Validate options
		for j, option := range question.Options {
			if option.ID == "" {
				return fmt.Errorf("question %d (%s), option %d: ID cannot be empty", index+1, question.ID, j+1)
			}
			if option.Text == "" {
				return fmt.Errorf("question %d (%s), option %d (%s): text cannot be empty", index+1, question.ID, j+1, option.ID)
			}
		}
	}

	return nil
}

