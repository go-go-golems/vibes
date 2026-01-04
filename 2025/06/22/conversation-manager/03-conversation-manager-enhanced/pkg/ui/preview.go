package ui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// PreviewModel handles the conversation preview display
type PreviewModel struct {
	conversation *models.Conversation
	scrollOffset int
	width        int
	height       int
	styles       *Styles
	visible      bool
	focused      bool
}

// NewPreviewModel creates a new preview model
func NewPreviewModel(styles *Styles) *PreviewModel {
	return &PreviewModel{
		conversation: nil,
		scrollOffset: 0,
		styles:       styles,
		visible:      false,
		focused:      false,
	}
}

// Init initializes the preview model
func (m *PreviewModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the preview model
func (m *PreviewModel) Update(msg tea.Msg) (*PreviewModel, tea.Cmd) {
	switch msg := msg.(type) {
	case ConversationSelectedMsg:
		// Conversation will be set via SetConversation method
		m.scrollOffset = 0

	case tea.KeyMsg:
		if !m.focused || !m.visible {
			return m, nil
		}

		switch msg.String() {
		case "up", "k":
			if m.scrollOffset > 0 {
				m.scrollOffset--
			}

		case "down", "j":
			// Calculate max scroll based on content height
			maxScroll := m.getMaxScroll()
			if m.scrollOffset < maxScroll {
				m.scrollOffset++
			}

		case "page_up":
			m.scrollOffset -= m.height / 2
			if m.scrollOffset < 0 {
				m.scrollOffset = 0
			}

		case "page_down":
			maxScroll := m.getMaxScroll()
			m.scrollOffset += m.height / 2
			if m.scrollOffset > maxScroll {
				m.scrollOffset = maxScroll
			}

		case "home":
			m.scrollOffset = 0

		case "end":
			m.scrollOffset = m.getMaxScroll()

		case "escape":
			return m, func() tea.Msg {
				return PreviewToggleMsg{ConversationID: ""}
			}
		}
	}

	return m, nil
}

// getMaxScroll calculates the maximum scroll offset
func (m *PreviewModel) getMaxScroll() int {
	if m.conversation == nil {
		return 0
	}

	// Estimate content height (simplified)
	contentHeight := len(m.conversation.Messages) * 4 // Rough estimate
	if contentHeight <= m.height {
		return 0
	}

	return contentHeight - m.height
}

// View renders the preview model
func (m *PreviewModel) View() string {
	if !m.visible || m.conversation == nil {
		return ""
	}

	var sections []string

	// Header with conversation title and metadata
	header := m.renderHeader()
	sections = append(sections, header)

	// Messages
	messages := m.renderMessages()
	sections = append(sections, messages)

	// Wrap in border
	content := lipgloss.JoinVertical(lipgloss.Left, sections...)
	
	if m.focused {
		return m.styles.FocusedBorderStyle.Width(m.width).Height(m.height).Render(content)
	}
	
	return m.styles.BorderStyle.Width(m.width).Height(m.height).Render(content)
}

// renderHeader renders the conversation header
func (m *PreviewModel) renderHeader() string {
	if m.conversation == nil {
		return ""
	}

	var headerParts []string

	// Title
	title := m.styles.PreviewHeaderStyle.Render(m.conversation.Title)
	headerParts = append(headerParts, title)

	// Metadata
	metadata := fmt.Sprintf("Model: %s | Messages: %d | Created: %s",
		m.conversation.Model,
		len(m.conversation.Messages),
		m.conversation.CreatedAt.Format("Jan 2, 2006"),
	)
	headerParts = append(headerParts, m.styles.StatusStyle.Render(metadata))

	// Tags
	if len(m.conversation.Tags) > 0 {
		var tagItems []string
		for _, tag := range m.conversation.Tags {
			icon := models.GetTagIcon(tag)
			tagItems = append(tagItems, fmt.Sprintf("%s %s", icon, tag))
		}
		tags := "Tags: " + strings.Join(tagItems, ", ")
		headerParts = append(headerParts, m.styles.StatusStyle.Render(tags))
	}

	return lipgloss.JoinVertical(lipgloss.Left, headerParts...)
}

// renderMessages renders the conversation messages
func (m *PreviewModel) renderMessages() string {
	if m.conversation == nil || len(m.conversation.Messages) == 0 {
		return m.styles.PreviewContentStyle.Render("No messages")
	}

	var messageItems []string

	// Apply scroll offset
	startIndex := m.scrollOffset
	if startIndex >= len(m.conversation.Messages) {
		startIndex = len(m.conversation.Messages) - 1
	}
	if startIndex < 0 {
		startIndex = 0
	}

	// Render visible messages
	for i := startIndex; i < len(m.conversation.Messages) && len(messageItems) < m.height-5; i++ {
		msg := m.conversation.Messages[i]
		messageItem := m.renderMessage(msg)
		messageItems = append(messageItems, messageItem)
	}

	if len(messageItems) == 0 {
		return m.styles.PreviewContentStyle.Render("No messages to display")
	}

	return lipgloss.JoinVertical(lipgloss.Left, messageItems...)
}

// renderMessage renders a single message
func (m *PreviewModel) renderMessage(msg models.Message) string {
	var style lipgloss.Style
	var roleIcon string

	switch msg.Role {
	case "user":
		style = m.styles.UserMessageStyle
		roleIcon = "👤"
	case "assistant":
		style = m.styles.AssistantMessageStyle
		roleIcon = "🤖"
	case "system":
		style = m.styles.StatusStyle
		roleIcon = "⚙️"
	default:
		style = m.styles.PreviewContentStyle
		roleIcon = "💬"
	}

	// Format timestamp
	timeStr := msg.Timestamp.Format("3:04 PM")

	// Header with role and timestamp
	header := fmt.Sprintf("%s %s - %s", roleIcon, strings.Title(msg.Role), timeStr)

	// Content (truncate if too long)
	content := msg.Content
	maxContentLength := m.width * 3 // Allow up to 3 lines worth of content
	if len(content) > maxContentLength {
		content = content[:maxContentLength-3] + "..."
	}

	// Combine header and content
	messageText := header + "\n" + content

	return style.Width(m.width - 4).Render(messageText)
}

// SetConversation sets the conversation to preview
func (m *PreviewModel) SetConversation(conversation *models.Conversation) {
	m.conversation = conversation
	m.scrollOffset = 0
}

// SetVisible sets the visibility state
func (m *PreviewModel) SetVisible(visible bool) {
	m.visible = visible
}

// SetFocused sets the focus state
func (m *PreviewModel) SetFocused(focused bool) {
	m.focused = focused
}

// SetSize sets the model dimensions
func (m *PreviewModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

