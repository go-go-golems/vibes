package ui

import (
	"github.com/conversation-manager/pkg/models"
)

// Custom messages for inter-model communication

// Data Loading Messages
type ConversationsLoadedMsg struct {
	Conversations []models.ConversationSummary
}

type ConversationDetailLoadedMsg struct {
	Conversation models.Conversation
}

type SearchResultsMsg struct {
	Results []models.SearchResult
	Query   string
}

type FilterAppliedMsg struct {
	Options models.FilterOptions
	Results []models.ConversationSummary
}

// Navigation Messages
type FocusChangedMsg struct {
	Focus FocusState
}

type ConversationSelectedMsg struct {
	ConversationID string
}

type PreviewRequestMsg struct {
	ConversationID string
}

type PreviewCloseMsg struct{}

// State Change Messages
type SearchQueryChangedMsg struct {
	Query string
}

type FilterToggledMsg struct {
	FilterType string
	Value      string
	Active     bool
}

type ViewModeChangedMsg struct {
	Mode ViewMode
}

type StatusUpdateMsg struct {
	Message string
}

// File Operation Messages
type SaveRequestMsg struct {
	ConversationID string
}

type DeleteRequestMsg struct {
	ConversationID string
}

type ExportRequestMsg struct {
	ConversationID string
}

// Focus states
type FocusState int

const (
	FocusBrowse FocusState = iota
	FocusSearch
	FocusFilter
	FocusPreview
)

// View modes
type ViewMode int

const (
	ViewModeBrowse ViewMode = iota
	ViewModeSearch
	ViewModeFilter
)

// String representations for debugging
func (f FocusState) String() string {
	switch f {
	case FocusBrowse:
		return "Browse"
	case FocusSearch:
		return "Search"
	case FocusFilter:
		return "Filter"
	case FocusPreview:
		return "Preview"
	default:
		return "Unknown"
	}
}

func (v ViewMode) String() string {
	switch v {
	case ViewModeBrowse:
		return "Browse"
	case ViewModeSearch:
		return "Search"
	case ViewModeFilter:
		return "Filter"
	default:
		return "Unknown"
	}
}

