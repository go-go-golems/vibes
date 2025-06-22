package data

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/conversation-manager/pkg/models"
	"gopkg.in/yaml.v3"
)

// Manager handles loading and managing conversation data
type Manager struct {
	dataDir       string
	conversations []models.Conversation
	summaries     []models.ConversationSummary
}

// NewManager creates a new data manager
func NewManager(dataDir string) *Manager {
	return &Manager{
		dataDir:       dataDir,
		conversations: []models.Conversation{},
		summaries:     []models.ConversationSummary{},
	}
}

// LoadConversations loads all conversations from the data directory
func (m *Manager) LoadConversations() error {
	// Ensure data directory exists
	if err := os.MkdirAll(m.dataDir, 0755); err != nil {
		return fmt.Errorf("failed to create data directory: %w", err)
	}

	// Read all YAML files in the data directory
	files, err := ioutil.ReadDir(m.dataDir)
	if err != nil {
		return fmt.Errorf("failed to read data directory: %w", err)
	}

	m.conversations = []models.Conversation{}
	m.summaries = []models.ConversationSummary{}

	for _, file := range files {
		if !strings.HasSuffix(file.Name(), ".yaml") && !strings.HasSuffix(file.Name(), ".yml") {
			continue
		}

		filePath := filepath.Join(m.dataDir, file.Name())
		conversation, err := m.loadConversationFromFile(filePath)
		if err != nil {
			// Log error but continue loading other files
			fmt.Printf("Warning: failed to load conversation from %s: %v\n", file.Name(), err)
			continue
		}

		m.conversations = append(m.conversations, *conversation)
		m.summaries = append(m.summaries, conversation.ToSummary())
	}

	// Sort conversations by last updated time (newest first)
	sort.Slice(m.conversations, func(i, j int) bool {
		return m.conversations[i].LastUpdated.After(m.conversations[j].LastUpdated)
	})

	sort.Slice(m.summaries, func(i, j int) bool {
		return m.summaries[i].LastUpdated.After(m.summaries[j].LastUpdated)
	})

	return nil
}

// loadConversationFromFile loads a single conversation from a YAML file
func (m *Manager) loadConversationFromFile(filePath string) (*models.Conversation, error) {
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read file: %w", err)
	}

	var conversation models.Conversation
	if err := yaml.Unmarshal(data, &conversation); err != nil {
		return nil, fmt.Errorf("failed to unmarshal YAML: %w", err)
	}

	return &conversation, nil
}

// GetConversations returns all loaded conversations
func (m *Manager) GetConversations() []models.Conversation {
	return m.conversations
}

// GetConversationSummaries returns all conversation summaries
func (m *Manager) GetConversationSummaries() []models.ConversationSummary {
	return m.summaries
}

// GetConversationByID returns a specific conversation by ID
func (m *Manager) GetConversationByID(id string) (*models.Conversation, bool) {
	for _, conv := range m.conversations {
		if conv.ID == id {
			return &conv, true
		}
	}
	return nil, false
}

// SearchConversations searches conversations by query
func (m *Manager) SearchConversations(query string) []models.SearchResult {
	// Return empty results for empty or whitespace-only queries
	if strings.TrimSpace(query) == "" {
		return []models.SearchResult{}
	}

	query = strings.ToLower(strings.TrimSpace(query))
	results := []models.SearchResult{}

	for _, conv := range m.conversations {
		score := 0.0
		matchedText := ""

		// Search in title
		if strings.Contains(strings.ToLower(conv.Title), query) {
			score += 10.0
			matchedText = conv.Title
		}

		// Search in messages
		for _, msg := range conv.Messages {
			if strings.Contains(strings.ToLower(msg.Content), query) {
				score += 5.0
				if matchedText == "" {
					// Get context around the match
					content := msg.Content
					if len(content) > 200 {
						content = content[:197] + "..."
					}
					matchedText = content
				}
			}
		}

		// Search in tags
		for _, tag := range conv.Tags {
			if strings.Contains(strings.ToLower(tag), query) {
				score += 3.0
			}
		}

		if score > 0 {
			results = append(results, models.SearchResult{
				Conversation: conv.ToSummary(),
				MatchedText:  matchedText,
				Score:        score,
			})
		}
	}

	// Sort by score (highest first)
	sort.Slice(results, func(i, j int) bool {
		return results[i].Score > results[j].Score
	})

	return results
}

// FilterConversations filters conversations based on the given options
func (m *Manager) FilterConversations(options models.FilterOptions) []models.ConversationSummary {
	filtered := []models.ConversationSummary{}
	now := time.Now()

	for _, summary := range m.summaries {
		// Apply date filter
		if !m.matchesDateFilter(summary.LastUpdated, options.DateRange, now) {
			continue
		}

		// Apply tag filter
		if len(options.Tags) > 0 && !m.matchesTagFilter(summary.Tags, options.Tags) {
			continue
		}

		// Apply model filter
		if len(options.Models) > 0 && !m.matchesModelFilter(summary.Model, options.Models) {
			continue
		}

		filtered = append(filtered, summary)
	}

	return filtered
}

// matchesDateFilter checks if a timestamp matches the date filter
func (m *Manager) matchesDateFilter(timestamp time.Time, dateRange string, now time.Time) bool {
	switch dateRange {
	case "today":
		return timestamp.Format("2006-01-02") == now.Format("2006-01-02")
	case "yesterday":
		yesterday := now.AddDate(0, 0, -1)
		return timestamp.Format("2006-01-02") == yesterday.Format("2006-01-02")
	case "this_week":
		weekStart := now.AddDate(0, 0, -int(now.Weekday()))
		return timestamp.After(weekStart)
	case "last_30_days":
		thirtyDaysAgo := now.AddDate(0, 0, -30)
		return timestamp.After(thirtyDaysAgo)
	case "all":
		return true
	default:
		return true
	}
}

// matchesTagFilter checks if conversation tags match any of the filter tags
func (m *Manager) matchesTagFilter(conversationTags, filterTags []string) bool {
	for _, filterTag := range filterTags {
		for _, convTag := range conversationTags {
			if strings.EqualFold(convTag, filterTag) {
				return true
			}
		}
	}
	return false
}

// matchesModelFilter checks if conversation model matches any of the filter models
func (m *Manager) matchesModelFilter(conversationModel string, filterModels []string) bool {
	for _, filterModel := range filterModels {
		if strings.EqualFold(conversationModel, filterModel) {
			return true
		}
	}
	return false
}

// GetAvailableTags returns all unique tags from loaded conversations
func (m *Manager) GetAvailableTags() []string {
	tagSet := make(map[string]bool)
	for _, conv := range m.conversations {
		for _, tag := range conv.Tags {
			tagSet[tag] = true
		}
	}

	tags := make([]string, 0, len(tagSet))
	for tag := range tagSet {
		tags = append(tags, tag)
	}
	sort.Strings(tags)
	return tags
}

// GetAvailableModels returns all unique models from loaded conversations
func (m *Manager) GetAvailableModels() []string {
	modelSet := make(map[string]bool)
	for _, conv := range m.conversations {
		if conv.Model != "" {
			modelSet[conv.Model] = true
		}
	}

	models := make([]string, 0, len(modelSet))
	for model := range modelSet {
		models = append(models, model)
	}
	sort.Strings(models)
	return models
}

