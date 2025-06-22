package data

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"gopkg.in/yaml.v3"
	"github.com/conversation-manager/pkg/models"
)

// Manager handles loading and filtering conversation data
type Manager struct {
	dataDir       string
	conversations []models.ConversationSummary
	fullConversations map[string]*models.Conversation
}

// NewManager creates a new data manager
func NewManager(dataDir string) *Manager {
	return &Manager{
		dataDir:           dataDir,
		conversations:     []models.ConversationSummary{},
		fullConversations: make(map[string]*models.Conversation),
	}
}

// LoadConversations loads all conversations from the data directory
func (m *Manager) LoadConversations() error {
	m.conversations = []models.ConversationSummary{}
	m.fullConversations = make(map[string]*models.Conversation)

	// Check if data directory exists
	if _, err := os.Stat(m.dataDir); os.IsNotExist(err) {
		return fmt.Errorf("data directory does not exist: %s", m.dataDir)
	}

	// Read all YAML files in the directory
	files, err := filepath.Glob(filepath.Join(m.dataDir, "*.yaml"))
	if err != nil {
		return fmt.Errorf("error reading data directory: %v", err)
	}

	for _, file := range files {
		conv, err := m.loadConversationFromFile(file)
		if err != nil {
			fmt.Printf("Warning: failed to load conversation from %s: %v\n", file, err)
			continue
		}

		summary := conv.ToSummary()
		m.conversations = append(m.conversations, summary)
		m.fullConversations[conv.ID] = conv
	}

	// Sort conversations by last updated (newest first)
	sort.Slice(m.conversations, func(i, j int) bool {
		return m.conversations[i].LastUpdated.After(m.conversations[j].LastUpdated)
	})

	return nil
}

// loadConversationFromFile loads a conversation from a YAML file
func (m *Manager) loadConversationFromFile(filename string) (*models.Conversation, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	var conv models.Conversation
	if err := yaml.Unmarshal(data, &conv); err != nil {
		return nil, err
	}

	return &conv, nil
}

// GetConversations returns all loaded conversations
func (m *Manager) GetConversations() []models.ConversationSummary {
	return m.conversations
}

// GetConversation returns a full conversation by ID
func (m *Manager) GetConversation(id string) (*models.Conversation, bool) {
	conv, exists := m.fullConversations[id]
	return conv, exists
}

// SearchConversations searches conversations with enhanced date range support
func (m *Manager) SearchConversations(query string) []models.SearchResult {
	if strings.TrimSpace(query) == "" {
		// Return all conversations as search results
		var results []models.SearchResult
		for _, conv := range m.conversations {
			results = append(results, models.SearchResult{
				Conversation: conv,
				MatchedText:  conv.LastMessage,
				Score:        1.0,
				MatchedTags:  []string{},
			})
		}
		return results
	}

	var results []models.SearchResult
	
	// Parse date range terms from query
	dateTerms := models.ParseDateRangeSearchTerms(query)
	cleanQuery := models.CleanDateRangeQuery(query)
	
	// Parse tag search terms
	tagTerms := m.parseTagSearchTerms(cleanQuery)
	cleanQuery = m.cleanTagQuery(cleanQuery)
	
	// Parse other search modifiers
	titleTerms := m.parseTitleSearchTerms(cleanQuery)
	contentTerms := m.parseContentSearchTerms(cleanQuery)
	modelTerms := m.parseModelSearchTerms(cleanQuery)
	
	// Clean query of all modifiers
	cleanQuery = m.cleanAllModifiers(cleanQuery)
	
	for _, conv := range m.conversations {
		score := 0.0
		var matchedText string
		var matchedTags []string
		
		// Check date range filters
		if !m.matchesDateFilters(conv, dateTerms) {
			continue
		}
		
		// Check tag filters
		tagScore, tagMatches := m.scoreTagMatch(conv, tagTerms)
		if len(tagTerms) > 0 && tagScore == 0 {
			continue // Required tag not found
		}
		score += tagScore * 2.0 // Weight tag matches higher
		matchedTags = append(matchedTags, tagMatches...)
		
		// Check title filters
		if len(titleTerms) > 0 {
			titleScore, titleMatch := m.scoreTitleMatch(conv, titleTerms)
			if titleScore == 0 {
				continue // Required title term not found
			}
			score += titleScore * 1.5
			if titleMatch != "" {
				matchedText = titleMatch
			}
		}
		
		// Check content filters
		if len(contentTerms) > 0 {
			contentScore, contentMatch := m.scoreContentMatch(conv, contentTerms)
			if contentScore == 0 {
				continue // Required content term not found
			}
			score += contentScore
			if contentMatch != "" && matchedText == "" {
				matchedText = contentMatch
			}
		}
		
		// Check model filters
		if len(modelTerms) > 0 {
			modelScore := m.scoreModelMatch(conv, modelTerms)
			if modelScore == 0 {
				continue // Required model not found
			}
			score += modelScore * 0.5
		}
		
		// General search in title and content
		if cleanQuery != "" {
			generalScore, generalMatch := m.scoreGeneralMatch(conv, cleanQuery)
			score += generalScore
			if generalMatch != "" && matchedText == "" {
				matchedText = generalMatch
			}
		}
		
		// If no specific search terms but we have date/tag filters, include all matching conversations
		if cleanQuery == "" && len(titleTerms) == 0 && len(contentTerms) == 0 && len(modelTerms) == 0 {
			if len(dateTerms) > 0 || len(tagTerms) > 0 {
				score = 1.0
				if matchedText == "" {
					matchedText = conv.LastMessage
				}
			}
		}
		
		if score > 0 {
			if matchedText == "" {
				matchedText = conv.LastMessage
			}
			
			// Truncate matched text
			if len(matchedText) > 100 {
				matchedText = matchedText[:97] + "..."
			}
			
			results = append(results, models.SearchResult{
				Conversation: conv,
				MatchedText:  matchedText,
				Score:        score,
				MatchedTags:  matchedTags,
			})
		}
	}
	
	// Sort by score (highest first)
	sort.Slice(results, func(i, j int) bool {
		if results[i].Score == results[j].Score {
			// If scores are equal, sort by last updated (newest first)
			return results[i].Conversation.LastUpdated.After(results[j].Conversation.LastUpdated)
		}
		return results[i].Score > results[j].Score
	})
	
	return results
}

// matchesDateFilters checks if a conversation matches date range filters
func (m *Manager) matchesDateFilters(conv models.ConversationSummary, dateTerms []models.DateRangeSearchTerm) bool {
	if len(dateTerms) == 0 {
		return true
	}
	
	for _, term := range dateTerms {
		if term.Range != nil {
			if !term.Range.IsInRange(conv.LastUpdated) {
				return false
			}
		}
	}
	
	return true
}

// parseTagSearchTerms parses tag search terms from query
func (m *Manager) parseTagSearchTerms(query string) []string {
	var terms []string
	words := strings.Fields(query)
	
	for _, word := range words {
		if strings.HasPrefix(word, "tag:") {
			tag := strings.TrimPrefix(word, "tag:")
			if tag != "" {
				terms = append(terms, tag)
			}
		}
	}
	
	return terms
}

// parseTitleSearchTerms parses title search terms from query
func (m *Manager) parseTitleSearchTerms(query string) []string {
	var terms []string
	words := strings.Fields(query)
	
	for _, word := range words {
		if strings.HasPrefix(word, "title:") {
			title := strings.TrimPrefix(word, "title:")
			if title != "" {
				terms = append(terms, title)
			}
		}
	}
	
	return terms
}

// parseContentSearchTerms parses content search terms from query
func (m *Manager) parseContentSearchTerms(query string) []string {
	var terms []string
	words := strings.Fields(query)
	
	for _, word := range words {
		if strings.HasPrefix(word, "content:") {
			content := strings.TrimPrefix(word, "content:")
			if content != "" {
				terms = append(terms, content)
			}
		}
	}
	
	return terms
}

// parseModelSearchTerms parses model search terms from query
func (m *Manager) parseModelSearchTerms(query string) []string {
	var terms []string
	words := strings.Fields(query)
	
	for _, word := range words {
		if strings.HasPrefix(word, "model:") {
			model := strings.TrimPrefix(word, "model:")
			if model != "" {
				terms = append(terms, model)
			}
		}
	}
	
	return terms
}

// cleanTagQuery removes tag search terms from query
func (m *Manager) cleanTagQuery(query string) string {
	words := strings.Fields(query)
	var cleanWords []string
	
	for _, word := range words {
		if !strings.HasPrefix(word, "tag:") {
			cleanWords = append(cleanWords, word)
		}
	}
	
	return strings.Join(cleanWords, " ")
}

// cleanAllModifiers removes all search modifiers from query
func (m *Manager) cleanAllModifiers(query string) string {
	words := strings.Fields(query)
	var cleanWords []string
	
	for _, word := range words {
		if !strings.Contains(word, ":") {
			cleanWords = append(cleanWords, word)
		}
	}
	
	return strings.Join(cleanWords, " ")
}

// scoreTagMatch scores tag matches and returns matched tags
func (m *Manager) scoreTagMatch(conv models.ConversationSummary, tagTerms []string) (float64, []string) {
	if len(tagTerms) == 0 {
		return 0, []string{}
	}
	
	var matchedTags []string
	score := 0.0
	
	for _, term := range tagTerms {
		for _, tag := range conv.Tags {
			if strings.Contains(strings.ToLower(tag), strings.ToLower(term)) {
				matchedTags = append(matchedTags, tag)
				score += 1.0
			}
		}
	}
	
	return score, matchedTags
}

// scoreTitleMatch scores title matches
func (m *Manager) scoreTitleMatch(conv models.ConversationSummary, titleTerms []string) (float64, string) {
	if len(titleTerms) == 0 {
		return 0, ""
	}
	
	score := 0.0
	title := strings.ToLower(conv.Title)
	
	for _, term := range titleTerms {
		if strings.Contains(title, strings.ToLower(term)) {
			score += 1.0
		}
	}
	
	if score > 0 {
		return score, conv.Title
	}
	
	return 0, ""
}

// scoreContentMatch scores content matches
func (m *Manager) scoreContentMatch(conv models.ConversationSummary, contentTerms []string) (float64, string) {
	if len(contentTerms) == 0 {
		return 0, ""
	}
	
	score := 0.0
	content := strings.ToLower(conv.LastMessage)
	
	for _, term := range contentTerms {
		if strings.Contains(content, strings.ToLower(term)) {
			score += 1.0
		}
	}
	
	if score > 0 {
		return score, conv.LastMessage
	}
	
	return 0, ""
}

// scoreModelMatch scores model matches
func (m *Manager) scoreModelMatch(conv models.ConversationSummary, modelTerms []string) float64 {
	if len(modelTerms) == 0 {
		return 0
	}
	
	score := 0.0
	model := strings.ToLower(conv.Model)
	
	for _, term := range modelTerms {
		if strings.Contains(model, strings.ToLower(term)) {
			score += 1.0
		}
	}
	
	return score
}

// scoreGeneralMatch scores general text matches
func (m *Manager) scoreGeneralMatch(conv models.ConversationSummary, query string) (float64, string) {
	if query == "" {
		return 0, ""
	}
	
	query = strings.ToLower(query)
	score := 0.0
	var matchedText string
	
	// Search in title (higher weight)
	if strings.Contains(strings.ToLower(conv.Title), query) {
		score += 2.0
		matchedText = conv.Title
	}
	
	// Search in last message
	if strings.Contains(strings.ToLower(conv.LastMessage), query) {
		score += 1.0
		if matchedText == "" {
			matchedText = conv.LastMessage
		}
	}
	
	// Search in tags
	for _, tag := range conv.Tags {
		if strings.Contains(strings.ToLower(tag), query) {
			score += 1.5
			if matchedText == "" {
				matchedText = fmt.Sprintf("Tag: %s", tag)
			}
		}
	}
	
	return score, matchedText
}

// FilterConversations filters conversations based on filter options with date range support
func (m *Manager) FilterConversations(options models.FilterOptions) []models.ConversationSummary {
	var filtered []models.ConversationSummary
	
	for _, conv := range m.conversations {
		// Check date filters
		if !options.MatchesDateFilter(conv.LastUpdated) {
			continue
		}
		
		// Check tag filters
		if len(options.Tags) > 0 {
			hasTag := false
			for _, tag := range options.Tags {
				if conv.HasTag(tag) {
					hasTag = true
					break
				}
			}
			if !hasTag {
				continue
			}
		}
		
		// Check model filters
		if len(options.Models) > 0 {
			hasModel := false
			for _, model := range options.Models {
				if strings.Contains(strings.ToLower(conv.Model), strings.ToLower(model)) {
					hasModel = true
					break
				}
			}
			if !hasModel {
				continue
			}
		}
		
		// Check tag filter (advanced)
		if len(options.TagFilter.Tags) > 0 {
			if options.TagFilter.Operator == "AND" {
				// All tags must be present
				hasAllTags := true
				for _, tag := range options.TagFilter.Tags {
					if !conv.HasTag(tag) {
						hasAllTags = false
						break
					}
				}
				if !hasAllTags {
					continue
				}
			} else {
				// Any tag must be present (OR)
				if !conv.HasAnyTag(options.TagFilter.Tags) {
					continue
				}
			}
		}
		
		filtered = append(filtered, conv)
	}
	
	return filtered
}

// GetAvailableTags returns all unique tags from loaded conversations
func (m *Manager) GetAvailableTags() []string {
	tagSet := make(map[string]bool)
	
	for _, conv := range m.conversations {
		for _, tag := range conv.Tags {
			tagSet[tag] = true
		}
	}
	
	var tags []string
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
		modelSet[conv.Model] = true
	}
	
	var models []string
	for model := range modelSet {
		models = append(models, model)
	}
	
	sort.Strings(models)
	return models
}

// GetConversationsByDateRange returns conversations within a specific date range
func (m *Manager) GetConversationsByDateRange(dateRange *models.DateRange) []models.ConversationSummary {
	if dateRange == nil {
		return m.conversations
	}
	
	var filtered []models.ConversationSummary
	
	for _, conv := range m.conversations {
		if dateRange.IsInRange(conv.LastUpdated) {
			filtered = append(filtered, conv)
		}
	}
	
	return filtered
}

// GetDateRangeStats returns statistics about conversations in different date ranges
func (m *Manager) GetDateRangeStats() map[string]int {
	stats := make(map[string]int)
	now := time.Now()
	
	// Define date ranges
	ranges := map[string]*models.DateRange{
		"today":     m.createDateRange(now, now),
		"yesterday": m.createDateRange(now.AddDate(0, 0, -1), now.AddDate(0, 0, -1)),
		"this-week": m.createWeekRange(now),
		"last-week": m.createWeekRange(now.AddDate(0, 0, -7)),
		"this-month": m.createMonthRange(now),
		"last-month": m.createMonthRange(now.AddDate(0, -1, 0)),
	}
	
	for rangeName, dateRange := range ranges {
		count := 0
		for _, conv := range m.conversations {
			if dateRange.IsInRange(conv.LastUpdated) {
				count++
			}
		}
		stats[rangeName] = count
	}
	
	return stats
}

// createDateRange creates a date range for a specific day
func (m *Manager) createDateRange(start, end time.Time) *models.DateRange {
	startOfDay := time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
	endOfDay := time.Date(end.Year(), end.Month(), end.Day(), 23, 59, 59, 999999999, end.Location())
	
	return &models.DateRange{
		Start: &startOfDay,
		End:   &endOfDay,
	}
}

// createWeekRange creates a date range for a week starting from Monday
func (m *Manager) createWeekRange(date time.Time) *models.DateRange {
	weekday := int(date.Weekday())
	if weekday == 0 { // Sunday
		weekday = 7
	}
	
	start := date.AddDate(0, 0, -(weekday-1))
	start = time.Date(start.Year(), start.Month(), start.Day(), 0, 0, 0, 0, start.Location())
	
	end := start.AddDate(0, 0, 6)
	end = time.Date(end.Year(), end.Month(), end.Day(), 23, 59, 59, 999999999, end.Location())
	
	return &models.DateRange{
		Start: &start,
		End:   &end,
	}
}

// createMonthRange creates a date range for a month
func (m *Manager) createMonthRange(date time.Time) *models.DateRange {
	start := time.Date(date.Year(), date.Month(), 1, 0, 0, 0, 0, date.Location())
	end := start.AddDate(0, 1, 0).Add(-time.Nanosecond)
	
	return &models.DateRange{
		Start: &start,
		End:   &end,
	}
}

