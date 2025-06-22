package models

import (
	"time"

	"github.com/google/uuid"
)

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
func (c *Conversation) AddMessage(role, content string) {
	message := Message{
		ID:        uuid.New().String(),
		Role:      role,
		Content:   content,
		Timestamp: time.Now(),
	}
	
	// Set parent ID to the last message if it exists
	if len(c.Messages) > 0 {
		lastMessage := c.Messages[len(c.Messages)-1]
		message.ParentID = &lastMessage.ID
	}
	
	c.Messages = append(c.Messages, message)
	c.LastUpdated = time.Now()
}

// GetLastMessage returns the last message content for display
func (c *Conversation) GetLastMessage() string {
	if len(c.Messages) == 0 {
		return ""
	}
	
	lastMsg := c.Messages[len(c.Messages)-1]
	if len(lastMsg.Content) > 100 {
		return lastMsg.Content[:97] + "..."
	}
	return lastMsg.Content
}

// ToSummary converts a full conversation to a summary
func (c *Conversation) ToSummary() ConversationSummary {
	return ConversationSummary{
		ID:           c.ID,
		Title:        c.Title,
		LastMessage:  c.GetLastMessage(),
		CreatedAt:    c.CreatedAt,
		LastUpdated:  c.LastUpdated,
		Tags:         c.Tags,
		Model:        c.Model,
		MessageCount: len(c.Messages),
	}
}

// FilterOptions represents the available filter options
type FilterOptions struct {
	DateRange string   // "today", "yesterday", "this_week", "last_30_days", "all"
	Tags      []string
	Models    []string
}

// SearchResult represents a search result with highlighting
type SearchResult struct {
	Conversation ConversationSummary `json:"conversation"`
	MatchedText  string              `json:"matched_text"`
	Score        float64             `json:"score"`
}

