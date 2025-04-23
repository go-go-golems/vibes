package parser

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/usenet-scraper/internal/config"
)

// Parser represents a NNTP post parser
type Parser struct {
	config *config.Config
	transformations map[string]Transformation
}

// Transformation is an interface for all transformation types
type Transformation interface {
	Apply(post *Post) error
}

// Post represents a Usenet post with original and transformed fields
type Post struct {
	ID       string
	Author   string
	Date     string
	Subject  string
	Body     string
	Fields   map[string]interface{}
}

// NewParser creates a new parser with the given configuration
func NewParser(cfg *config.Config) (*Parser, error) {
	p := &Parser{
		config: cfg,
		transformations: make(map[string]Transformation),
	}

	// Initialize transformations
	if err := p.initTransformations(); err != nil {
		return nil, err
	}

	return p, nil
}

// initTransformations initializes transformations based on configuration
func (p *Parser) initTransformations() error {
	for _, t := range p.config.Transformations {
		var transform Transformation
		var err error

		switch {
		case t.Pattern != "" && t.Operations == nil:
			// Extraction transformation
			transform, err = NewExtractionTransformation(t)
		case t.Operations != nil:
			// Cleaning transformation
			transform, err = NewCleaningTransformation(t)
		case t.Model != "":
			// Classification transformation
			transform, err = NewClassificationTransformation(t)
		case t.Replace != nil:
			// Replace transformation
			transform, err = NewReplaceTransformation(t)
		case t.Split != nil:
			// Split transformation
			transform, err = NewSplitTransformation(t)
		case t.Join != nil:
			// Join transformation
			transform, err = NewJoinTransformation(t)
		default:
			return fmt.Errorf("unknown transformation type for ID: %s", t.ID)
		}

		if err != nil {
			return err
		}

		p.transformations[t.ID] = transform
	}

	return nil
}

// ParsePost applies all transformations to a post
func (p *Parser) ParsePost(post *Post) error {
	// Initialize fields map if not already initialized
	if post.Fields == nil {
		post.Fields = make(map[string]interface{})
	}

	// Add base fields to the fields map
	post.Fields["id"] = post.ID
	post.Fields["author"] = post.Author
	post.Fields["date"] = post.Date
	post.Fields["subject"] = post.Subject
	post.Fields["body"] = post.Body

	// Apply each transformation in order
	for _, t := range p.config.Transformations {
		transform, ok := p.transformations[t.ID]
		if !ok {
			return fmt.Errorf("transformation not found: %s", t.ID)
		}

		if err := transform.Apply(post); err != nil {
			return fmt.Errorf("error applying transformation %s: %w", t.ID, err)
		}
	}

	return nil
}

// ExtractionTransformation extracts data using regex patterns
type ExtractionTransformation struct {
	field   string
	pattern *regexp.Regexp
	storeAs string
}

// NewExtractionTransformation creates a new extraction transformation
func NewExtractionTransformation(cfg config.TransformConfig) (*ExtractionTransformation, error) {
	pattern, err := regexp.Compile(cfg.Pattern)
	if err != nil {
		return nil, fmt.Errorf("invalid regex pattern: %w", err)
	}

	return &ExtractionTransformation{
		field:   cfg.Field,
		pattern: pattern,
		storeAs: cfg.StoreAs,
	}, nil
}

// Apply applies the extraction transformation to a post
func (t *ExtractionTransformation) Apply(post *Post) error {
	// Get the field value
	fieldValue, ok := post.Fields[t.field]
	if !ok {
		return fmt.Errorf("field not found: %s", t.field)
	}

	// Convert to string
	fieldStr, ok := fieldValue.(string)
	if !ok {
		return fmt.Errorf("field is not a string: %s", t.field)
	}

	// Extract data using regex
	matches := t.pattern.FindStringSubmatch(fieldStr)
	if len(matches) > 1 {
		// Store the first capture group
		post.Fields[t.storeAs] = matches[1]
	} else if len(matches) == 1 {
		// Store the whole match if no capture groups
		post.Fields[t.storeAs] = matches[0]
	} else {
		// No matches, store empty string
		post.Fields[t.storeAs] = ""
	}

	return nil
}

// CleaningTransformation cleans text using various operations
type CleaningTransformation struct {
	field      string
	operations []string
	storeAs    string
}

// NewCleaningTransformation creates a new cleaning transformation
func NewCleaningTransformation(cfg config.TransformConfig) (*CleaningTransformation, error) {
	return &CleaningTransformation{
		field:      cfg.Field,
		operations: cfg.Operations,
		storeAs:    cfg.StoreAs,
	}, nil
}

// Apply applies the cleaning transformation to a post
func (t *CleaningTransformation) Apply(post *Post) error {
	// Get the field value
	fieldValue, ok := post.Fields[t.field]
	if !ok {
		return fmt.Errorf("field not found: %s", t.field)
	}

	// Convert to string
	fieldStr, ok := fieldValue.(string)
	if !ok {
		return fmt.Errorf("field is not a string: %s", t.field)
	}

	// Apply each operation
	result := fieldStr
	for _, op := range t.operations {
		switch op {
		case "strip_quotes":
			result = stripQuotes(result)
		case "remove_signatures":
			result = removeSignatures(result)
		case "normalize_whitespace":
			result = normalizeWhitespace(result)
		case "strip_html":
			result = stripHTML(result)
		case "lowercase":
			result = strings.ToLower(result)
		case "trim":
			result = strings.TrimSpace(result)
		default:
			return fmt.Errorf("unknown cleaning operation: %s", op)
		}
	}

	// Store the result
	post.Fields[t.storeAs] = result
	return nil
}

// stripQuotes removes quoted text from a message
func stripQuotes(text string) string {
	lines := strings.Split(text, "\n")
	var result []string
	for _, line := range lines {
		if !strings.HasPrefix(line, ">") {
			result = append(result, line)
		}
	}
	return strings.Join(result, "\n")
}

// removeSignatures removes email signatures
func removeSignatures(text string) string {
	parts := strings.Split(text, "\n-- \n")
	if len(parts) > 1 {
		return parts[0]
	}
	return text
}

// normalizeWhitespace normalizes whitespace characters
func normalizeWhitespace(text string) string {
	// Replace multiple spaces with a single space
	re := regexp.MustCompile(`\s+`)
	return re.ReplaceAllString(text, " ")
}

// stripHTML removes HTML tags
func stripHTML(text string) string {
	// Simple HTML tag removal
	re := regexp.MustCompile(`<[^>]*>`)
	return re.ReplaceAllString(text, "")
}

// ClassificationTransformation classifies text using a model
type ClassificationTransformation struct {
	field      string
	model      string
	categories []string
	storeAs    string
}

// NewClassificationTransformation creates a new classification transformation
func NewClassificationTransformation(cfg config.TransformConfig) (*ClassificationTransformation, error) {
	return &ClassificationTransformation{
		field:      cfg.Field,
		model:      cfg.Model,
		categories: cfg.Categories,
		storeAs:    cfg.StoreAs,
	}, nil
}

// Apply applies the classification transformation to a post
func (t *ClassificationTransformation) Apply(post *Post) error {
	// Get the field value
	fieldValue, ok := post.Fields[t.field]
	if !ok {
		return fmt.Errorf("field not found: %s", t.field)
	}

	// Convert to string
	fieldStr, ok := fieldValue.(string)
	if !ok {
		return fmt.Errorf("field is not a string: %s", t.field)
	}

	// Classify the text
	var category string
	switch t.model {
	case "simple_bayes":
		category = simpleClassify(fieldStr, t.categories)
	case "keyword_match":
		category = keywordClassify(fieldStr, t.categories)
	default:
		return fmt.Errorf("unknown classification model: %s", t.model)
	}

	// Store the result
	post.Fields[t.storeAs] = category
	return nil
}

// simpleClassify performs a simple classification
func simpleClassify(text string, categories []string) string {
	// This is a very basic implementation
	// In a real implementation, this would use a proper Bayesian classifier
	text = strings.ToLower(text)
	
	// Simple keyword matching for demonstration
	if strings.Contains(text, "?") {
		return "question"
	} else if strings.Contains(text, "announce") || strings.Contains(text, "release") {
		return "announcement"
	} else {
		return "discussion"
	}
}

// keywordClassify performs keyword-based classification
func keywordClassify(text string, categories []string) string {
	// Simple keyword matching
	text = strings.ToLower(text)
	
	// Count occurrences of each category name in the text
	maxCount := 0
	bestCategory := categories[0]
	
	for _, category := range categories {
		count := strings.Count(text, strings.ToLower(category))
		if count > maxCount {
			maxCount = count
			bestCategory = category
		}
	}
	
	return bestCategory
}

// ReplaceTransformation performs string replacement
type ReplaceTransformation struct {
	field   string
	from    string
	to      string
	storeAs string
}

// NewReplaceTransformation creates a new replace transformation
func NewReplaceTransformation(cfg config.TransformConfig) (*ReplaceTransformation, error) {
	if cfg.Replace == nil {
		return nil, fmt.Errorf("replace configuration is required")
	}
	
	return &ReplaceTransformation{
		field:   cfg.Field,
		from:    cfg.Replace.From,
		to:      cfg.Replace.To,
		storeAs: cfg.StoreAs,
	}, nil
}

// Apply applies the replace transformation to a post
func (t *ReplaceTransformation) Apply(post *Post) error {
	// Get the field value
	fieldValue, ok := post.Fields[t.field]
	if !ok {
		return fmt.Errorf("field not found: %s", t.field)
	}

	// Convert to string
	fieldStr, ok := fieldValue.(string)
	if !ok {
		return fmt.Errorf("field is not a string: %s", t.field)
	}

	// Replace the string
	result := strings.ReplaceAll(fieldStr, t.from, t.to)

	// Store the result
	post.Fields[t.storeAs] = result
	return nil
}

// SplitTransformation splits a string
type SplitTransformation struct {
	field     string
	delimiter string
	storeAs   string
}

// NewSplitTransformation creates a new split transformation
func NewSplitTransformation(cfg config.TransformConfig) (*SplitTransformation, error) {
	if cfg.Split == nil {
		return nil, fmt.Errorf("split configuration is required")
	}
	
	return &SplitTransformation{
		field:     cfg.Field,
		delimiter: cfg.Split.Delimiter,
		storeAs:   cfg.StoreAs,
	}, nil
}

// Apply applies the split transformation to a post
func (t *SplitTransformation) Apply(post *Post) error {
	// Get the field value
	fieldValue, ok := post.Fields[t.field]
	if !ok {
		return fmt.Errorf("field not found: %s", t.field)
	}

	// Convert to string
	fieldStr, ok := fieldValue.(string)
	if !ok {
		return fmt.Errorf("field is not a string: %s", t.field)
	}

	// Split the string
	result := strings.Split(fieldStr, t.delimiter)

	// Store the result
	post.Fields[t.storeAs] = result
	return nil
}

// JoinTransformation joins multiple fields
type JoinTransformation struct {
	fields    []string
	delimiter string
	storeAs   string
}

// NewJoinTransformation creates a new join transformation
func NewJoinTransformation(cfg config.TransformConfig) (*JoinTransformation, error) {
	if cfg.Join == nil {
		return nil, fmt.Errorf("join configuration is required")
	}
	
	return &JoinTransformation{
		fields:    cfg.Join.Fields,
		delimiter: cfg.Join.Delimiter,
		storeAs:   cfg.StoreAs,
	}, nil
}

// Apply applies the join transformation to a post
func (t *JoinTransformation) Apply(post *Post) error {
	var parts []string

	// Get each field value
	for _, field := range t.fields {
		fieldValue, ok := post.Fields[field]
		if !ok {
			return fmt.Errorf("field not found: %s", field)
		}

		// Convert to string
		fieldStr, ok := fieldValue.(string)
		if !ok {
			return fmt.Errorf("field is not a string: %s", field)
		}

		parts = append(parts, fieldStr)
	}

	// Join the strings
	result := strings.Join(parts, t.delimiter)

	// Store the result
	post.Fields[t.storeAs] = result
	return nil
}
