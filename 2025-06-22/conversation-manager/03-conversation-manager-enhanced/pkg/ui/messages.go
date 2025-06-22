package ui

import (
	"github.com/conversation-manager/pkg/models"
)

// Message types for inter-model communication

// FocusChangedMsg indicates focus has changed between models
type FocusChangedMsg struct {
	Focus models.Focus
}

// ConversationSelectedMsg indicates a conversation was selected
type ConversationSelectedMsg struct {
	ConversationID string
}

// SearchQueryChangedMsg indicates the search query changed
type SearchQueryChangedMsg struct {
	Query string
}

// SearchResultsMsg contains search results
type SearchResultsMsg struct {
	Results []models.SearchResult
}

// FilterChangedMsg indicates filter options changed
type FilterChangedMsg struct {
	Options models.FilterOptions
}

// FilterAppliedMsg indicates filters were applied
type FilterAppliedMsg struct {
	Options models.FilterOptions
}

// TagFilterChangedMsg indicates tag filter selection changed
type TagFilterChangedMsg struct {
	SelectedTags []string
}

// ConversationsLoadedMsg indicates conversations were loaded
type ConversationsLoadedMsg struct {
	Conversations []models.ConversationSummary
}

// PreviewToggleMsg indicates preview should be toggled
type PreviewToggleMsg struct {
	ConversationID string
}

// PreviewRequestMsg requests preview for a conversation
type PreviewRequestMsg struct {
	ConversationID string
}

// StatusUpdateMsg updates the status message
type StatusUpdateMsg struct {
	Message string
}

// WindowResizeMsg indicates the window was resized
type WindowResizeMsg struct {
	Width  int
	Height int
}

// TagSearchMsg indicates tag search query changed
type TagSearchMsg struct {
	Query string
}

// TagCategorySelectedMsg indicates a tag category was selected
type TagCategorySelectedMsg struct {
	Category string
}

// TagSelectedMsg indicates a specific tag was selected/deselected
type TagSelectedMsg struct {
	Tag      string
	Selected bool
}

// TagSuggestionsRequestMsg requests tag suggestions
type TagSuggestionsRequestMsg struct {
	ExistingTags []string
	Limit        int
}

// TagSuggestionsMsg contains tag suggestions
type TagSuggestionsMsg struct {
	Suggestions []string
}

