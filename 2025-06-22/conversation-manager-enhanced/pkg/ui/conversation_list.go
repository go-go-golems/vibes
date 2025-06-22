package ui

import (
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// ConversationListModel handles the display of conversations
type ConversationListModel struct {
	conversations []models.ConversationSummary
	filtered      []models.ConversationSummary
	cursor        int
	width         int
	height        int
	styles        *Styles
	focused       bool
}

// NewConversationListModel creates a new conversation list model
func NewConversationListModel(styles *Styles) *ConversationListModel {
	return &ConversationListModel{
		conversations: []models.ConversationSummary{},
		filtered:      []models.ConversationSummary{},
		cursor:        0,
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
		m.filtered = msg.Conversations
		m.cursor = 0

	case SearchResultsMsg:
		// Update filtered list with search results
		m.filtered = make([]models.ConversationSummary, len(msg.Results))
		for i, result := range msg.Results {
			m.filtered[i] = result.Conversation
		}
		m.cursor = 0

	case FilterAppliedMsg:
		// Filter will be handled by the data manager and result in ConversationsLoadedMsg
		// For now, we don't need to do anything here

	case tea.KeyMsg:
		if !m.focused {
			return m, nil
		}

		switch msg.String() {
		case "up", "k":
			if m.cursor > 0 {
				m.cursor--
			}

		case "down", "j":
			if m.cursor < len(m.filtered)-1 {
				m.cursor++
			}

		case "enter", " ":
			if len(m.filtered) > 0 && m.cursor < len(m.filtered) {
				selectedConv := m.filtered[m.cursor]
				return m, func() tea.Msg {
					return ConversationSelectedMsg{ConversationID: selectedConv.ID}
				}
			}

		case "home":
			m.cursor = 0

		case "end":
			if len(m.filtered) > 0 {
				m.cursor = len(m.filtered) - 1
			}
		}
	}

	return m, nil
}

// View renders the conversation list model
func (m *ConversationListModel) View() string {
	if len(m.filtered) == 0 {
		return m.styles.UnselectedItemStyle.Render("No conversations found")
	}

	var items []string
	
	// Group conversations by date
	groups := m.groupConversationsByDate(m.filtered)
	
	itemIndex := 0
	for _, group := range groups {
		// Add group header
		items = append(items, m.styles.GroupHeaderStyle.Render(group.Date))
		
		// Add conversations in this group
		for _, conv := range group.Conversations {
			style := m.styles.UnselectedItemStyle
			if itemIndex == m.cursor && m.focused {
				style = m.styles.SelectedItemStyle
			}
			
			// Format conversation item
			icon := models.GetTagIcon(conv.GetPrimaryTag())
			title := conv.Title
			timeStr := m.formatTime(conv.LastUpdated)
			
			// Create main line with title and time
			mainLine := fmt.Sprintf("%s %s", icon, title)
			
			// Add tags if any
			if len(conv.Tags) > 0 {
				tagStr := strings.Join(conv.Tags, ", ")
				if len(tagStr) > 30 {
					tagStr = tagStr[:27] + "..."
				}
				mainLine += fmt.Sprintf(" [%s]", tagStr)
			}
			
			// Add time on the right
			mainLine = fmt.Sprintf("%-*s %s", m.width-15, mainLine, timeStr)
			
			// Add preview of last message
			preview := conv.LastMessage
			if len(preview) > m.width-4 {
				preview = preview[:m.width-7] + "..."
			}
			
			item := mainLine
			if preview != "" {
				item += "\n  " + m.styles.MessagePreviewStyle.Render(preview)
			}
			
			items = append(items, style.Render(item))
			itemIndex++
		}
		
		// Add spacing between groups
		items = append(items, "")
	}

	return lipgloss.JoinVertical(lipgloss.Left, items...)
}

// ConversationGroup represents a group of conversations by date
type ConversationGroup struct {
	Date          string
	Conversations []models.ConversationSummary
}

// groupConversationsByDate groups conversations by their last updated date
func (m *ConversationListModel) groupConversationsByDate(conversations []models.ConversationSummary) []ConversationGroup {
	groups := make(map[string][]models.ConversationSummary)
	var dates []string
	
	for _, conv := range conversations {
		dateKey := m.formatDateGroup(conv.LastUpdated)
		
		if _, exists := groups[dateKey]; !exists {
			dates = append(dates, dateKey)
		}
		
		groups[dateKey] = append(groups[dateKey], conv)
	}
	
	var result []ConversationGroup
	for _, date := range dates {
		result = append(result, ConversationGroup{
			Date:          date,
			Conversations: groups[date],
		})
	}
	
	return result
}

// formatDateGroup formats a date for grouping
func (m *ConversationListModel) formatDateGroup(t time.Time) string {
	now := time.Now()
	
	// Check if it's today
	if t.Year() == now.Year() && t.Month() == now.Month() && t.Day() == now.Day() {
		return "Today"
	}
	
	// Check if it's yesterday
	yesterday := now.AddDate(0, 0, -1)
	if t.Year() == yesterday.Year() && t.Month() == yesterday.Month() && t.Day() == yesterday.Day() {
		return "Yesterday"
	}
	
	// Check if it's this week
	weekStart := now.AddDate(0, 0, -int(now.Weekday()))
	if t.After(weekStart) {
		return "This Week"
	}
	
	// Check if it's this month
	if t.Year() == now.Year() && t.Month() == now.Month() {
		return "This Month"
	}
	
	// Check if it's this year
	if t.Year() == now.Year() {
		return t.Format("January")
	}
	
	// Different year
	return t.Format("January 2006")
}

// formatTime formats a time for display
func (m *ConversationListModel) formatTime(t time.Time) string {
	now := time.Now()
	
	// If it's today, show time
	if t.Year() == now.Year() && t.Month() == now.Month() && t.Day() == now.Day() {
		return t.Format("3:04 PM")
	}
	
	// If it's this year, show month and day
	if t.Year() == now.Year() {
		return t.Format("Jan 2")
	}
	
	// Different year, show year
	return t.Format("Jan 2006")
}

// SetFocused sets the focus state
func (m *ConversationListModel) SetFocused(focused bool) {
	m.focused = focused
}

// SetSize sets the model dimensions
func (m *ConversationListModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// GetSelectedConversation returns the currently selected conversation
func (m *ConversationListModel) GetSelectedConversation() *models.ConversationSummary {
	if len(m.filtered) > 0 && m.cursor < len(m.filtered) {
		return &m.filtered[m.cursor]
	}
	return nil
}

