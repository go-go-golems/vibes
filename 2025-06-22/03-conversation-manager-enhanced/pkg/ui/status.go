package ui

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/conversation-manager/pkg/models"
)

// StatusModel handles the status bar display
type StatusModel struct {
	message      string
	currentFocus models.Focus
	currentMode  models.ViewMode
	width        int
	height       int
	styles       *Styles
	visible      bool
}

// NewStatusModel creates a new status model
func NewStatusModel(styles *Styles) *StatusModel {
	return &StatusModel{
		message:      "Ready",
		currentFocus: models.FocusBrowse,
		currentMode:  models.ViewModeBrowse,
		styles:       styles,
		visible:      true,
	}
}

// Init initializes the status model
func (m *StatusModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the status model
func (m *StatusModel) Update(msg tea.Msg) (*StatusModel, tea.Cmd) {
	switch msg := msg.(type) {
	case StatusUpdateMsg:
		m.message = msg.Message

	case FocusChangedMsg:
		m.currentFocus = msg.Focus
		m.updateMessage()

	case ConversationsLoadedMsg:
		m.message = "Loaded conversations"

	case SearchQueryChangedMsg:
		if msg.Query == "" {
			m.message = "Ready"
		} else {
			m.message = "Searching..."
		}

	case SearchResultsMsg:
		if len(msg.Results) == 0 {
			m.message = "No results found"
		} else {
			m.message = "Found results"
		}

	case FilterChangedMsg:
		m.message = "Filters applied"

	case ConversationSelectedMsg:
		m.message = "Conversation selected"
	}

	return m, nil
}

// updateMessage updates the status message based on current state
func (m *StatusModel) updateMessage() {
	switch m.currentFocus {
	case models.FocusBrowse:
		m.message = "Browse conversations"
	case models.FocusSearch:
		m.message = "Search mode"
	case models.FocusFilter:
		m.message = "Filter mode"
	case models.FocusPreview:
		m.message = "Preview mode"
	}
}

// View renders the status model
func (m *StatusModel) View() string {
	if !m.visible {
		return ""
	}

	var sections []string

	// Status message
	sections = append(sections, m.styles.StatusStyle.Render(m.message))

	// Current mode indicator
	var modeText string
	switch m.currentMode {
	case models.ViewModeBrowse:
		modeText = "Browse"
	case models.ViewModeSearch:
		modeText = "Search"
	case models.ViewModeFilter:
		modeText = "Filter"
	case models.ViewModePreview:
		modeText = "Preview"
	}

	if modeText != "" {
		sections = append(sections, m.styles.StatusStyle.Render("Mode: "+modeText))
	}

	// Help text based on current focus
	helpText := m.getHelpText()
	if helpText != "" {
		sections = append(sections, m.styles.HelpStyle.Render(helpText))
	}

	return strings.Join(sections, " | ")
}

// getHelpText returns context-sensitive help text
func (m *StatusModel) getHelpText() string {
	switch m.currentFocus {
	case models.FocusBrowse:
		return "j/k: Navigate • /: Search • f: Filter • Space: Preview • q: Quit"
	case models.FocusSearch:
		return "Type to search • Esc: Exit search • Enter: Select • Tab: Suggestions"
	case models.FocusFilter:
		return "←/→: Categories • ↑/↓: Options • Space: Toggle • Esc: Exit"
	case models.FocusPreview:
		return "j/k: Scroll • Esc: Close preview"
	default:
		return "q: Quit"
	}
}

// SetFocus sets the current focus
func (m *StatusModel) SetFocus(focus models.Focus) {
	m.currentFocus = focus
	m.updateMessage()
}

// SetMode sets the current view mode
func (m *StatusModel) SetMode(mode models.ViewMode) {
	m.currentMode = mode
}

// SetVisible sets the visibility state
func (m *StatusModel) SetVisible(visible bool) {
	m.visible = visible
}

// SetSize sets the model dimensions
func (m *StatusModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

