package ui

import (
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// ConversationListModel handles the display and navigation of conversations
type ConversationListModel struct {
	conversations []models.ConversationSummary
	cursor        int
	scrollOffset  int
	width         int
	height        int
	styles        *Styles
	focused       bool
}

// NewConversationListModel creates a new conversation list model
func NewConversationListModel(styles *Styles) *ConversationListModel {
	return &ConversationListModel{
		conversations: []models.ConversationSummary{},
		cursor:        0,
		scrollOffset:  0,
		styles:        styles,
		focused:       true,
	}
}

// Init initializes the conversation list model
func (m *ConversationListModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the conversation list model
func (m *ConversationListModel) Update(msg tea.Msg) (*ConversationListModel, tea.Cmd) {
	switch msg := msg.(type) {
	case ConversationsLoadedMsg:
		m.conversations = msg.Conversations
		m.cursor = 0
		m.scrollOffset = 0

	case SearchResultsMsg:
		// Convert search results to conversation summaries
		summaries := make([]models.ConversationSummary, len(msg.Results))
		for i, result := range msg.Results {
			summaries[i] = result.Conversation
		}
		m.conversations = summaries
		m.cursor = 0
		m.scrollOffset = 0

	case FilterAppliedMsg:
		m.conversations = msg.Results
		m.cursor = 0
		m.scrollOffset = 0

	case tea.KeyMsg:
		if !m.focused {
			return m, nil
		}

		switch msg.String() {
		case "up", "k":
			m.moveCursorUp()
		case "down", "j":
			m.moveCursorDown()
		case "g":
			m.cursor = 0
			m.scrollOffset = 0
		case "G":
			m.cursor = len(m.conversations) - 1
			m.adjustScroll()
		case "pgup":
			m.moveCursorUp()
			for i := 0; i < 10 && m.cursor > 0; i++ {
				m.moveCursorUp()
			}
		case "pgdown":
			m.moveCursorDown()
			for i := 0; i < 10 && m.cursor < len(m.conversations)-1; i++ {
				m.moveCursorDown()
			}
		case "enter":
			if len(m.conversations) > 0 && m.cursor < len(m.conversations) {
				selectedConv := m.conversations[m.cursor]
				return m, func() tea.Msg {
					return ConversationSelectedMsg{ConversationID: selectedConv.ID}
				}
			}
		case " ":
			if len(m.conversations) > 0 && m.cursor < len(m.conversations) {
				selectedConv := m.conversations[m.cursor]
				return m, func() tea.Msg {
					return PreviewRequestMsg{ConversationID: selectedConv.ID}
				}
			}
		}
	}

	return m, nil
}

// SetFocused sets the focus state of the model
func (m *ConversationListModel) SetFocused(focused bool) {
	m.focused = focused
}

// SetSize sets the dimensions of the model
func (m *ConversationListModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// moveCursorUp moves the cursor up and adjusts scroll if needed
func (m *ConversationListModel) moveCursorUp() {
	if m.cursor > 0 {
		m.cursor--
		if m.cursor < m.scrollOffset {
			m.scrollOffset = m.cursor
		}
	}
}

// moveCursorDown moves the cursor down and adjusts scroll if needed
func (m *ConversationListModel) moveCursorDown() {
	if m.cursor < len(m.conversations)-1 {
		m.cursor++
		m.adjustScroll()
	}
}

// adjustScroll adjusts the scroll offset to keep cursor visible
func (m *ConversationListModel) adjustScroll() {
	visibleHeight := m.height - 2 // Account for borders
	if m.cursor >= m.scrollOffset+visibleHeight {
		m.scrollOffset = m.cursor - visibleHeight + 1
	}
}

// GetSelectedConversation returns the currently selected conversation
func (m *ConversationListModel) GetSelectedConversation() *models.ConversationSummary {
	if len(m.conversations) == 0 || m.cursor >= len(m.conversations) {
		return nil
	}
	return &m.conversations[m.cursor]
}

// View renders the conversation list
func (m *ConversationListModel) View() string {
	if len(m.conversations) == 0 {
		return m.styles.UnselectedItemStyle.Render("No conversations found")
	}

	var content strings.Builder
	visibleHeight := m.height - 2 // Account for borders

	// Group conversations by date
	groups := m.groupConversationsByDate()

	currentLine := 0
	for _, group := range groups {
		// Skip if we haven't reached the scroll offset yet
		if currentLine < m.scrollOffset {
			currentLine += 1 + len(group.Conversations) // Header + conversations
			continue
		}

		// Stop if we've filled the visible area
		if currentLine-m.scrollOffset >= visibleHeight {
			break
		}

		// Render group header
		if currentLine >= m.scrollOffset {
			content.WriteString(m.styles.GroupHeaderStyle.Render(group.Title))
			content.WriteString("\n")
		}
		currentLine++

		// Render conversations in this group
		for i, conv := range group.Conversations {
			if currentLine < m.scrollOffset {
				currentLine++
				continue
			}

			if currentLine-m.scrollOffset >= visibleHeight {
				break
			}

			// Calculate the absolute index of this conversation
			absIndex := m.getAbsoluteIndex(group, i)
			isSelected := absIndex == m.cursor

			line := m.renderConversationLine(conv, isSelected)
			content.WriteString(line)
			content.WriteString("\n")
			currentLine++
		}
	}

	return content.String()
}

// ConversationGroup represents a group of conversations by date
type ConversationGroup struct {
	Title         string
	Conversations []models.ConversationSummary
}

// groupConversationsByDate groups conversations by their date
func (m *ConversationListModel) groupConversationsByDate() []ConversationGroup {
	if len(m.conversations) == 0 {
		return []ConversationGroup{}
	}

	now := time.Now()
	today := now.Format("2006-01-02")
	yesterday := now.AddDate(0, 0, -1).Format("2006-01-02")
	thisWeekStart := now.AddDate(0, 0, -int(now.Weekday()))

	groups := []ConversationGroup{
		{Title: "TODAY", Conversations: []models.ConversationSummary{}},
		{Title: "YESTERDAY", Conversations: []models.ConversationSummary{}},
		{Title: "THIS WEEK", Conversations: []models.ConversationSummary{}},
		{Title: "OLDER", Conversations: []models.ConversationSummary{}},
	}

	for _, conv := range m.conversations {
		convDate := conv.LastUpdated.Format("2006-01-02")
		
		if convDate == today {
			groups[0].Conversations = append(groups[0].Conversations, conv)
		} else if convDate == yesterday {
			groups[1].Conversations = append(groups[1].Conversations, conv)
		} else if conv.LastUpdated.After(thisWeekStart) {
			groups[2].Conversations = append(groups[2].Conversations, conv)
		} else {
			groups[3].Conversations = append(groups[3].Conversations, conv)
		}
	}

	// Filter out empty groups
	result := []ConversationGroup{}
	for _, group := range groups {
		if len(group.Conversations) > 0 {
			result = append(result, group)
		}
	}

	return result
}

// getAbsoluteIndex calculates the absolute index of a conversation within a group
func (m *ConversationListModel) getAbsoluteIndex(targetGroup ConversationGroup, indexInGroup int) int {
	groups := m.groupConversationsByDate()
	absoluteIndex := 0

	for _, group := range groups {
		if group.Title == targetGroup.Title {
			return absoluteIndex + indexInGroup
		}
		absoluteIndex += len(group.Conversations)
	}

	return -1 // Not found
}

// renderConversationLine renders a single conversation line
func (m *ConversationListModel) renderConversationLine(conv models.ConversationSummary, isSelected bool) string {
	// Get emoji for the first tag or default
	emoji := "💬"
	if len(conv.Tags) > 0 {
		switch conv.Tags[0] {
		case "code":
			emoji = "🟢"
		case "writing":
			emoji = "🟠"
		case "analysis":
			emoji = "🟡"
		case "creative":
			emoji = "🔵"
		case "q&a":
			emoji = "🟣"
		default:
			emoji = "🔴"
		}
	}

	// Format time
	timeStr := conv.LastUpdated.Format("3:04 PM")
	if !isToday(conv.LastUpdated) {
		timeStr = conv.LastUpdated.Format("Jan 2")
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

	lastMessage := conv.LastMessage
	if len(lastMessage) > 60 {
		lastMessage = lastMessage[:57] + "..."
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

	result := style.Width(m.width).Render(line)

	// Add preview line if not selected (to save space)
	if !isSelected && lastMessage != "" {
		previewLine := "  " + `"` + lastMessage + `"`
		result += "\n" + m.styles.MessagePreviewStyle.Width(m.width).Render(previewLine)
	}

	return result
}

// isToday checks if a time is today
func isToday(t time.Time) bool {
	now := time.Now()
	return t.Format("2006-01-02") == now.Format("2006-01-02")
}

