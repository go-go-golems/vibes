package ui

import (
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// SearchModel handles search input and results
type SearchModel struct {
	query       string
	cursor      int
	results     []models.SearchResult
	width       int
	height      int
	styles      *Styles
	focused     bool
	resultCursor int
}

// NewSearchModel creates a new search model
func NewSearchModel(styles *Styles) *SearchModel {
	return &SearchModel{
		query:        "",
		cursor:       0,
		results:      []models.SearchResult{},
		styles:       styles,
		focused:      false,
		resultCursor: 0,
	}
}

// Init initializes the search model
func (m *SearchModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the search model
func (m *SearchModel) Update(msg tea.Msg) (*SearchModel, tea.Cmd) {
	switch msg := msg.(type) {
	case SearchResultsMsg:
		m.results = msg.Results
		m.resultCursor = 0

	case tea.KeyMsg:
		if !m.focused {
			return m, nil
		}

		switch msg.String() {
		case "ctrl+u":
			// Clear entire search
			m.query = ""
			m.cursor = 0
			return m, func() tea.Msg {
				return SearchQueryChangedMsg{Query: m.query}
			}

		case "backspace":
			if len(m.query) > 0 && m.cursor > 0 {
				m.query = m.query[:m.cursor-1] + m.query[m.cursor:]
				m.cursor--
				return m, func() tea.Msg {
					return SearchQueryChangedMsg{Query: m.query}
				}
			}

		case "left":
			if m.cursor > 0 {
				m.cursor--
			}

		case "right":
			if m.cursor < len(m.query) {
				m.cursor++
			}

		case "home":
			m.cursor = 0

		case "end":
			m.cursor = len(m.query)

		case "up", "k":
			if len(m.results) > 0 && m.resultCursor > 0 {
				m.resultCursor--
			}

		case "down", "j":
			if len(m.results) > 0 && m.resultCursor < len(m.results)-1 {
				m.resultCursor++
			}

		case "enter":
			if len(m.results) > 0 && m.resultCursor < len(m.results) {
				selectedResult := m.results[m.resultCursor]
				return m, func() tea.Msg {
					return ConversationSelectedMsg{ConversationID: selectedResult.Conversation.ID}
				}
			}

		default:
			// Handle printable characters
			if len(msg.String()) == 1 && msg.String() >= " " && msg.String() <= "~" {
				char := msg.String()
				m.query = m.query[:m.cursor] + char + m.query[m.cursor:]
				m.cursor++
				return m, func() tea.Msg {
					return SearchQueryChangedMsg{Query: m.query}
				}
			}
		}
	}

	return m, nil
}

// SetFocused sets the focus state of the model
func (m *SearchModel) SetFocused(focused bool) {
	m.focused = focused
	if focused {
		m.cursor = len(m.query) // Move cursor to end when focused
	}
}

// SetSize sets the dimensions of the model
func (m *SearchModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// GetQuery returns the current search query
func (m *SearchModel) GetQuery() string {
	return m.query
}

// SetQuery sets the search query
func (m *SearchModel) SetQuery(query string) {
	m.query = query
	m.cursor = len(query)
}

// ClearSearch clears the search query and results
func (m *SearchModel) ClearSearch() {
	m.query = ""
	m.cursor = 0
	m.results = []models.SearchResult{}
	m.resultCursor = 0
}

// GetSelectedResult returns the currently selected search result
func (m *SearchModel) GetSelectedResult() *models.SearchResult {
	if len(m.results) == 0 || m.resultCursor >= len(m.results) {
		return nil
	}
	return &m.results[m.resultCursor]
}

// View renders the search model
func (m *SearchModel) View() string {
	if !m.focused {
		return ""
	}

	var content strings.Builder

	// Render search input
	searchLine := m.renderSearchInput()
	content.WriteString(searchLine)
	content.WriteString("\n")

	// Render search results
	if len(m.results) > 0 {
		content.WriteString("\n")
		resultsLine := m.styles.SearchResultsStyle.Render(
			fmt.Sprintf("%d results found", len(m.results)))
		content.WriteString(resultsLine)
		content.WriteString("\n\n")

		// Render individual results
		visibleHeight := m.height - 5 // Account for search input and headers
		startIdx := 0
		if m.resultCursor >= visibleHeight {
			startIdx = m.resultCursor - visibleHeight + 1
		}

		for i := startIdx; i < len(m.results) && i < startIdx+visibleHeight; i++ {
			result := m.results[i]
			isSelected := i == m.resultCursor

			line := m.renderSearchResult(result, isSelected)
			content.WriteString(line)
			content.WriteString("\n")
		}
	}

	return content.String()
}

// renderSearchInput renders the search input field
func (m *SearchModel) renderSearchInput() string {
	// Create the search prompt
	prompt := "🔍 Search: "
	
	// Create the input field with cursor
	input := m.query
	if m.focused {
		// Add cursor
		if m.cursor <= len(input) {
			input = input[:m.cursor] + "█" + input[m.cursor:]
		}
	}

	searchText := prompt + input
	return m.styles.SearchInputStyle.Width(m.width).Render(searchText)
}

// renderSearchResult renders a single search result
func (m *SearchModel) renderSearchResult(result models.SearchResult, isSelected bool) string {
	conv := result.Conversation

	// Get emoji for the first tag or default
	emoji := "💬"
	if len(conv.Tags) > 0 {
		switch conv.Tags[0] {
		case "code":
			emoji = "🔴"
		case "writing":
			emoji = "🟠"
		case "analysis":
			emoji = "🟡"
		case "creative":
			emoji = "🟢"
		case "q&a":
			emoji = "🔵"
		default:
			emoji = "🟣"
		}
	}

	// Format time
	timeStr := conv.LastUpdated.Format("Jan 2")
	if isToday(conv.LastUpdated) {
		timeStr = "Today"
	} else if isYesterday(conv.LastUpdated) {
		timeStr = "Yesterday"
	}

	// Create the main line
	prefix := "  "
	if isSelected {
		prefix = "> "
	}

	title := conv.Title
	if len(title) > 40 {
		title = title[:37] + "..."
	}

	// Build the line
	line := fmt.Sprintf("%s%s %s", prefix, emoji, title)
	
	// Add time aligned to the right
	availableWidth := m.width - len(line) - len(timeStr) - 2
	if availableWidth > 0 {
		line += strings.Repeat(" ", availableWidth) + timeStr
	}

	// Apply styling
	var style lipgloss.Style
	if isSelected {
		style = m.styles.SelectedItemStyle
	} else {
		style = m.styles.UnselectedItemStyle
	}

	result_line := style.Width(m.width).Render(line)

	// Add matched text preview
	if result.MatchedText != "" {
		matchedText := result.MatchedText
		if len(matchedText) > 80 {
			matchedText = matchedText[:77] + "..."
		}
		previewLine := "  ..." + matchedText + "..."
		result_line += "\n" + m.styles.MessagePreviewStyle.Width(m.width).Render(previewLine)
	}

	return result_line
}

// isYesterday checks if a time is yesterday
func isYesterday(t time.Time) bool {
	now := time.Now()
	yesterday := now.AddDate(0, 0, -1)
	return t.Format("2006-01-02") == yesterday.Format("2006-01-02")
}

