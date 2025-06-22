package models

import (
	"time"

	"github.com/google/uuid"
)

// ViewMode represents the current view mode of the application
type ViewMode int

const (
	ViewModeBrowse ViewMode = iota
	ViewModeSearch
	ViewModeFilter
	ViewModePreview
)

// Focus represents which UI component has focus
type Focus int

const (
	FocusBrowse Focus = iota
	FocusSearch
	FocusFilter
	FocusPreview
)

// FocusState is an alias for Focus for backward compatibility
type FocusState = Focus

// Tag represents a tag with metadata
type Tag struct {
	Name        string `yaml:"name" json:"name"`
	Category    string `yaml:"category" json:"category"`
	Color       string `yaml:"color" json:"color"`
	Description string `yaml:"description,omitempty" json:"description,omitempty"`
}

// TagCategory represents a category of tags
type TagCategory struct {
	Name        string `json:"name"`
	Color       string `json:"color"`
	Icon        string `json:"icon"`
	Description string `json:"description"`
}

// TagFilter represents filtering options for tags
type TagFilter struct {
	Categories []string `json:"categories"`
	Tags       []string `json:"tags"`
	Operator   string   `json:"operator"` // "AND" or "OR"
}

// GetTagCategories returns predefined tag categories
func GetTagCategories() []TagCategory {
	return []TagCategory{
		{
			Name:        "code",
			Color:       "#FF6B6B",
			Icon:        "🔴",
			Description: "Programming and development",
		},
		{
			Name:        "writing",
			Color:       "#4ECDC4",
			Icon:        "🟠",
			Description: "Creative writing and content",
		},
		{
			Name:        "analysis",
			Color:       "#45B7D1",
			Icon:        "🟡",
			Description: "Data analysis and research",
		},
		{
			Name:        "creative",
			Color:       "#96CEB4",
			Icon:        "🟢",
			Description: "Creative projects",
		},
		{
			Name:        "q&a",
			Color:       "#FFEAA7",
			Icon:        "🔵",
			Description: "Questions and answers",
		},
		{
			Name:        "other",
			Color:       "#DDA0DD",
			Icon:        "🟣",
			Description: "Other topics",
		},
	}
}

// GetTagColor returns the color for a given tag based on its category
func GetTagColor(tag string) string {
	categories := GetTagCategories()
	
	// Map common tags to categories
	tagCategoryMap := map[string]string{
		"code":        "code",
		"programming": "code",
		"react":       "code",
		"python":      "code",
		"javascript":  "code",
		"css":         "code",
		"html":        "code",
		"go":          "code",
		"typescript":  "code",
		"nodejs":      "code",
		"api":         "code",
		"debug":       "code",
		"writing":     "writing",
		"story":       "writing",
		"creative":    "creative",
		"fiction":     "creative",
		"analysis":    "analysis",
		"data":        "analysis",
		"ml":          "analysis",
		"ai":          "analysis",
		"question":    "q&a",
		"help":        "q&a",
		"tutorial":    "q&a",
	}
	
	if category, exists := tagCategoryMap[tag]; exists {
		for _, cat := range categories {
			if cat.Name == category {
				return cat.Color
			}
		}
	}
	
	// Default to "other" category
	return "#DDA0DD"
}

// GetTagIcon returns the emoji icon for a given tag
func GetTagIcon(tag string) string {
	categories := GetTagCategories()
	
	// Map common tags to categories
	tagCategoryMap := map[string]string{
		"code":        "code",
		"programming": "code",
		"react":       "code",
		"python":      "code",
		"javascript":  "code",
		"css":         "code",
		"html":        "code",
		"go":          "code",
		"typescript":  "code",
		"nodejs":      "code",
		"api":         "code",
		"debug":       "code",
		"writing":     "writing",
		"story":       "writing",
		"creative":    "creative",
		"fiction":     "creative",
		"analysis":    "analysis",
		"data":        "analysis",
		"ml":          "analysis",
		"ai":          "analysis",
		"question":    "q&a",
		"help":        "q&a",
		"tutorial":    "q&a",
	}
	
	if category, exists := tagCategoryMap[tag]; exists {
		for _, cat := range categories {
			if cat.Name == category {
				return cat.Icon
			}
		}
	}
	
	// Default to "other" category
	return "🟣"
}

// Conversation represents a single conversation with metadata
type Conversation struct {
	ID          string            `yaml:"id" json:"id"`
	Title       string            `yaml:"title" json:"title"`
	CreatedAt   time.Time         `yaml:"created_at" json:"created_at"`
	LastUpdated time.Time         `yaml:"last_updated" json:"last_updated"`
	Tags        []string          `yaml:"tags" json:"tags"`
	Model       string            `yaml:"model" json:"model"`
	Messages    []Message         `yaml:"messages" json:"messages"`
	Metadata    map[string]string `yaml:"metadata,omitempty" json:"metadata,omitempty"`
}

// Message represents a single message in a conversation
type Message struct {
	ID        string    `yaml:"id" json:"id"`
	ParentID  *string   `yaml:"parent_id,omitempty" json:"parent_id,omitempty"`
	Role      string    `yaml:"role" json:"role"` // user, assistant, system
	Content   string    `yaml:"content" json:"content"`
	Timestamp time.Time `yaml:"timestamp" json:"timestamp"`
}

// ConversationSummary represents a lightweight view of a conversation for listing
type ConversationSummary struct {
	ID          string    `json:"id"`
	Title       string    `json:"title"`
	LastMessage string    `json:"last_message"`
	CreatedAt   time.Time `json:"created_at"`
	LastUpdated time.Time `json:"last_updated"`
	Tags        []string  `json:"tags"`
	Model       string    `json:"model"`
	MessageCount int      `json:"message_count"`
}

// GetPrimaryTag returns the primary tag for display purposes
func (c *ConversationSummary) GetPrimaryTag() string {
	if len(c.Tags) > 0 {
		return c.Tags[0]
	}
	return "other"
}

// GetTagsForDisplay returns formatted tags for display
func (c *ConversationSummary) GetTagsForDisplay() []string {
	if len(c.Tags) == 0 {
		return []string{"other"}
	}
	return c.Tags
}

// HasTag checks if the conversation has a specific tag
func (c *ConversationSummary) HasTag(tag string) bool {
	for _, t := range c.Tags {
		if t == tag {
			return true
		}
	}
	return false
}

// HasAnyTag checks if the conversation has any of the specified tags
func (c *ConversationSummary) HasAnyTag(tags []string) bool {
	for _, tag := range tags {
		if c.HasTag(tag) {
			return true
		}
	}
	return false
}

// NewConversation creates a new conversation with a generated ID
func NewConversation(title string, model string) *Conversation {
	now := time.Now()
	return &Conversation{
		ID:          uuid.New().String(),
		Title:       title,
		CreatedAt:   now,
		LastUpdated: now,
		Tags:        []string{},
		Model:       model,
		Messages:    []Message{},
		Metadata:    make(map[string]string),
	}
}

// AddMessage adds a new message to the conversation
func (c *Conversation) AddMessage(role, content string) *Message {
	msg := &Message{
		ID:        uuid.New().String(),
		Role:      role,
		Content:   content,
		Timestamp: time.Now(),
	}
	
	// Set parent ID to the last message if it exists
	if len(c.Messages) > 0 {
		lastMsg := c.Messages[len(c.Messages)-1]
		msg.ParentID = &lastMsg.ID
	}
	
	c.Messages = append(c.Messages, *msg)
	c.LastUpdated = time.Now()
	
	return msg
}

// ToSummary converts a full conversation to a summary
func (c *Conversation) ToSummary() ConversationSummary {
	lastMessage := ""
	if len(c.Messages) > 0 {
		lastMsg := c.Messages[len(c.Messages)-1]
		lastMessage = lastMsg.Content
		if len(lastMessage) > 100 {
			lastMessage = lastMessage[:97] + "..."
		}
	}
	
	return ConversationSummary{
		ID:           c.ID,
		Title:        c.Title,
		LastMessage:  lastMessage,
		CreatedAt:    c.CreatedAt,
		LastUpdated:  c.LastUpdated,
		Tags:         c.Tags,
		Model:        c.Model,
		MessageCount: len(c.Messages),
	}
}

// SearchResult represents a search result with relevance scoring
type SearchResult struct {
	Conversation ConversationSummary `json:"conversation"`
	MatchedText  string              `json:"matched_text"`
	Score        float64             `json:"score"`
	MatchedTags  []string            `json:"matched_tags"`
}

// FilterOptions represents options for filtering conversations
type FilterOptions struct {
	DateRange     string     `json:"date_range"`
	CustomDateRange *DateRange `json:"custom_date_range,omitempty"`
	Tags          []string   `json:"tags"`
	Models        []string   `json:"models"`
	TagFilter     TagFilter  `json:"tag_filter"`
	
	// Date range search terms
	DateRangeTerms []DateRangeSearchTerm `json:"date_range_terms,omitempty"`
}

// HasDateFilter checks if any date filtering is active
func (fo *FilterOptions) HasDateFilter() bool {
	return fo.DateRange != "" && fo.DateRange != "all" ||
		   fo.CustomDateRange != nil ||
		   len(fo.DateRangeTerms) > 0
}

// GetEffectiveDateRange returns the effective date range for filtering
func (fo *FilterOptions) GetEffectiveDateRange() *DateRange {
	// Custom date range takes precedence
	if fo.CustomDateRange != nil {
		return fo.CustomDateRange
	}
	
	// Parse predefined date range
	if fo.DateRange != "" && fo.DateRange != "all" {
		parser := NewDateRangeParser()
		if dr, err := parser.ParseDateRange(fo.DateRange); err == nil {
			return dr
		}
	}
	
	return nil
}

// MatchesDateFilter checks if a timestamp matches the filter's date criteria
func (fo *FilterOptions) MatchesDateFilter(timestamp time.Time) bool {
	// Check effective date range
	if dr := fo.GetEffectiveDateRange(); dr != nil {
		if !dr.IsInRange(timestamp) {
			return false
		}
	}
	
	// Check date range search terms
	for _, term := range fo.DateRangeTerms {
		if term.Range != nil && !term.Range.IsInRange(timestamp) {
			return false
		}
	}
	
	return true
}

