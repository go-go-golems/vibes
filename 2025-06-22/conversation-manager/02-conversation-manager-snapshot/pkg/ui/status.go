package ui

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
)

// StatusModel displays help text and keyboard shortcuts
type StatusModel struct {
	currentFocus FocusState
	currentMode  ViewMode
	width        int
	height       int
	styles       *Styles
	statusMessage string
}

// NewStatusModel creates a new status model
func NewStatusModel(styles *Styles) *StatusModel {
	return &StatusModel{
		currentFocus: FocusBrowse,
		currentMode:  ViewModeBrowse,
		styles:       styles,
		statusMessage: "Welcome to Conversation Manager",
	}
}

// Init initializes the status model
func (m *StatusModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the status model
func (m *StatusModel) Update(msg tea.Msg) (*StatusModel, tea.Cmd) {
	switch msg := msg.(type) {
	case FocusChangedMsg:
		m.currentFocus = msg.Focus
	case ViewModeChangedMsg:
		m.currentMode = msg.Mode
	case StatusUpdateMsg:
		m.statusMessage = msg.Message
	}

	return m, nil
}

// SetSize sets the dimensions of the model
func (m *StatusModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// SetStatusMessage sets a custom status message
func (m *StatusModel) SetStatusMessage(message string) {
	m.statusMessage = message
}

// View renders the status model
func (m *StatusModel) View() string {
	var content strings.Builder

	// Render status message if any
	if m.statusMessage != "" {
		content.WriteString(m.styles.StatusStyle.Width(m.width).Render(m.statusMessage))
		content.WriteString("\n")
	}

	// Render context-sensitive help
	helpText := m.getContextualHelp()
	content.WriteString(m.styles.HelpStyle.Width(m.width).Render(helpText))

	return content.String()
}

// getContextualHelp returns help text based on current context
func (m *StatusModel) getContextualHelp() string {
	switch m.currentFocus {
	case FocusBrowse:
		return m.getBrowseHelp()
	case FocusSearch:
		return m.getSearchHelp()
	case FocusFilter:
		return m.getFilterHelp()
	case FocusPreview:
		return m.getPreviewHelp()
	default:
		return m.getBrowseHelp()
	}
}

// getBrowseHelp returns help text for browse mode
func (m *StatusModel) getBrowseHelp() string {
	return "↑/↓ Navigate • Enter: Open • Space: Preview • /: Search • f: Filters • q: Quit"
}

// getSearchHelp returns help text for search mode
func (m *StatusModel) getSearchHelp() string {
	return "Type to search • ↑/↓ Navigate • ←/→ Filter nav • Space: Preview • ESC: Back"
}

// getFilterHelp returns help text for filter mode
func (m *StatusModel) getFilterHelp() string {
	return "↑/↓ Navigate • Enter: Open • ←/→ Filter nav • f: Hide filters"
}

// getPreviewHelp returns help text for preview mode
func (m *StatusModel) getPreviewHelp() string {
	return "↑/↓ Navigate • Enter: Open • Space: Close preview • /: Search • f: Filters"
}

// GetCurrentFocus returns the current focus state
func (m *StatusModel) GetCurrentFocus() FocusState {
	return m.currentFocus
}

// GetCurrentMode returns the current view mode
func (m *StatusModel) GetCurrentMode() ViewMode {
	return m.currentMode
}

