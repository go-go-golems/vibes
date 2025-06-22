package models

import (
	"sort"
	"strings"
)

// TagManager handles tag-related operations
type TagManager struct {
	allTags      []string
	tagCounts    map[string]int
	categories   []TagCategory
}

// NewTagManager creates a new tag manager
func NewTagManager() *TagManager {
	return &TagManager{
		allTags:    []string{},
		tagCounts:  make(map[string]int),
		categories: GetTagCategories(),
	}
}

// UpdateFromConversations updates tag statistics from conversations
func (tm *TagManager) UpdateFromConversations(conversations []ConversationSummary) {
	tm.tagCounts = make(map[string]int)
	tagSet := make(map[string]bool)
	
	for _, conv := range conversations {
		for _, tag := range conv.Tags {
			tag = strings.ToLower(strings.TrimSpace(tag))
			if tag != "" {
				tm.tagCounts[tag]++
				tagSet[tag] = true
			}
		}
	}
	
	// Convert set to sorted slice
	tm.allTags = make([]string, 0, len(tagSet))
	for tag := range tagSet {
		tm.allTags = append(tm.allTags, tag)
	}
	sort.Strings(tm.allTags)
}

// GetAllTags returns all unique tags
func (tm *TagManager) GetAllTags() []string {
	return tm.allTags
}

// GetTagCount returns the count for a specific tag
func (tm *TagManager) GetTagCount(tag string) int {
	return tm.tagCounts[strings.ToLower(tag)]
}

// GetPopularTags returns the most popular tags up to limit
func (tm *TagManager) GetPopularTags(limit int) []string {
	type tagCount struct {
		tag   string
		count int
	}
	
	var tags []tagCount
	for tag, count := range tm.tagCounts {
		tags = append(tags, tagCount{tag, count})
	}
	
	// Sort by count descending
	sort.Slice(tags, func(i, j int) bool {
		return tags[i].count > tags[j].count
	})
	
	result := make([]string, 0, limit)
	for i, tc := range tags {
		if i >= limit {
			break
		}
		result = append(result, tc.tag)
	}
	
	return result
}

// GetTagsByCategory returns tags grouped by category
func (tm *TagManager) GetTagsByCategory() map[string][]string {
	result := make(map[string][]string)
	
	// Initialize categories
	for _, cat := range tm.categories {
		result[cat.Name] = []string{}
	}
	
	// Categorize tags
	for _, tag := range tm.allTags {
		category := tm.getTagCategory(tag)
		result[category] = append(result[category], tag)
	}
	
	return result
}

// getTagCategory determines the category for a tag
func (tm *TagManager) getTagCategory(tag string) string {
	tag = strings.ToLower(tag)
	
	// Programming/Code tags
	codeKeywords := []string{"code", "programming", "react", "python", "javascript", "css", "html", "go", "typescript", "nodejs", "api", "debug", "sql", "database", "git", "docker", "kubernetes", "aws", "cloud"}
	for _, keyword := range codeKeywords {
		if strings.Contains(tag, keyword) {
			return "code"
		}
	}
	
	// Writing tags
	writingKeywords := []string{"writing", "content", "blog", "article", "documentation", "copy"}
	for _, keyword := range writingKeywords {
		if strings.Contains(tag, keyword) {
			return "writing"
		}
	}
	
	// Analysis tags
	analysisKeywords := []string{"analysis", "data", "ml", "ai", "machine", "learning", "statistics", "research", "science"}
	for _, keyword := range analysisKeywords {
		if strings.Contains(tag, keyword) {
			return "analysis"
		}
	}
	
	// Creative tags
	creativeKeywords := []string{"creative", "story", "fiction", "art", "design", "music", "video", "game"}
	for _, keyword := range creativeKeywords {
		if strings.Contains(tag, keyword) {
			return "creative"
		}
	}
	
	// Q&A tags
	qaKeywords := []string{"question", "help", "tutorial", "how", "why", "what", "guide", "tips"}
	for _, keyword := range qaKeywords {
		if strings.Contains(tag, keyword) {
			return "q&a"
		}
	}
	
	return "other"
}

// SearchTags searches for tags matching a query
func (tm *TagManager) SearchTags(query string) []string {
	if query == "" {
		return tm.allTags
	}
	
	query = strings.ToLower(query)
	var matches []string
	
	for _, tag := range tm.allTags {
		if strings.Contains(strings.ToLower(tag), query) {
			matches = append(matches, tag)
		}
	}
	
	// Sort by relevance (exact matches first, then contains)
	sort.Slice(matches, func(i, j int) bool {
		tagI := strings.ToLower(matches[i])
		tagJ := strings.ToLower(matches[j])
		
		// Exact matches first
		if tagI == query && tagJ != query {
			return true
		}
		if tagI != query && tagJ == query {
			return false
		}
		
		// Then by starts with
		startsI := strings.HasPrefix(tagI, query)
		startsJ := strings.HasPrefix(tagJ, query)
		if startsI && !startsJ {
			return true
		}
		if !startsI && startsJ {
			return false
		}
		
		// Finally by count (popularity)
		return tm.tagCounts[matches[i]] > tm.tagCounts[matches[j]]
	})
	
	return matches
}

// GetTagSuggestions returns tag suggestions based on existing tags
func (tm *TagManager) GetTagSuggestions(existingTags []string, limit int) []string {
	// Get tags that commonly appear together
	suggestions := make(map[string]int)
	
	// This is a simplified implementation
	// In a real system, you'd analyze tag co-occurrence patterns
	for _, tag := range tm.GetPopularTags(limit * 2) {
		// Don't suggest tags that are already present
		found := false
		for _, existing := range existingTags {
			if strings.EqualFold(tag, existing) {
				found = true
				break
			}
		}
		if !found {
			suggestions[tag] = tm.tagCounts[tag]
		}
	}
	
	// Convert to sorted slice
	type tagSuggestion struct {
		tag   string
		count int
	}
	
	var suggestionList []tagSuggestion
	for tag, count := range suggestions {
		suggestionList = append(suggestionList, tagSuggestion{tag, count})
	}
	
	sort.Slice(suggestionList, func(i, j int) bool {
		return suggestionList[i].count > suggestionList[j].count
	})
	
	result := make([]string, 0, limit)
	for i, s := range suggestionList {
		if i >= limit {
			break
		}
		result = append(result, s.tag)
	}
	
	return result
}

// ValidateTag checks if a tag is valid
func (tm *TagManager) ValidateTag(tag string) bool {
	tag = strings.TrimSpace(tag)
	if len(tag) == 0 || len(tag) > 50 {
		return false
	}
	
	// Check for invalid characters
	for _, char := range tag {
		if char < 32 || char == 127 { // Control characters
			return false
		}
	}
	
	return true
}

// NormalizeTag normalizes a tag for consistent storage
func (tm *TagManager) NormalizeTag(tag string) string {
	tag = strings.TrimSpace(tag)
	tag = strings.ToLower(tag)
	
	// Replace spaces with hyphens
	tag = strings.ReplaceAll(tag, " ", "-")
	
	// Remove multiple consecutive hyphens
	for strings.Contains(tag, "--") {
		tag = strings.ReplaceAll(tag, "--", "-")
	}
	
	// Remove leading/trailing hyphens
	tag = strings.Trim(tag, "-")
	
	return tag
}

