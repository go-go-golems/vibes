package ui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// PreviewModel shows conversation preview in bottom panel
type PreviewModel struct {
	conversation *models.Conversation
	scrollOffset int
	width        int
	height       int
	styles       *Styles
	focused      bool
	visible      bool
}

// NewPreviewModel creates a new preview model
func NewPreviewModel(styles *Styles) *PreviewModel {
	return &PreviewModel{
		conversation: nil,
		scrollOffset: 0,
		styles:       styles,
		focused:      false,
		visible:      false,
	}
}

// Init initializes the preview model
func (m *PreviewModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the preview model
func (m *PreviewModel) Update(msg tea.Msg) (*PreviewModel, tea.Cmd) {
	switch msg := msg.(type) {
	case ConversationDetailLoadedMsg:
		m.conversation = &msg.Conversation
		m.scrollOffset = 0

	case PreviewCloseMsg:
		m.visible = false
		m.conversation = nil
		m.scrollOffset = 0

	case tea.KeyMsg:
		if !m.focused || !m.visible {
			return m, nil
		}

		switch msg.String() {
		case "up", "k":
			m.scrollUp()
		case "down", "j":
			m.scrollDown()
		case "pgup":
			m.scrollPageUp()
		case "pgdown":
			m.scrollPageDown()
		case "home":
			m.scrollOffset = 0
		case "end":
			m.scrollToEnd()
		case " ":
			return m, func() tea.Msg {
				return PreviewCloseMsg{}
			}
		}
	}

	return m, nil
}

// SetFocused sets the focus state of the model
func (m *PreviewModel) SetFocused(focused bool) {
	m.focused = focused
}

// SetVisible sets the visibility of the preview panel
func (m *PreviewModel) SetVisible(visible bool) {
	m.visible = visible
}

// IsVisible returns whether the preview panel is visible
func (m *PreviewModel) IsVisible() bool {
	return m.visible
}

// SetSize sets the dimensions of the model
func (m *PreviewModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// SetConversation sets the conversation to preview
func (m *PreviewModel) SetConversation(conversation *models.Conversation) {
	m.conversation = conversation
	m.scrollOffset = 0
	m.visible = true
}

// scrollUp scrolls the preview content up
func (m *PreviewModel) scrollUp() {
	if m.scrollOffset > 0 {
		m.scrollOffset--
	}
}

// scrollDown scrolls the preview content down
func (m *PreviewModel) scrollDown() {
	maxScroll := m.getMaxScrollOffset()
	if m.scrollOffset < maxScroll {
		m.scrollOffset++
	}
}

// scrollPageUp scrolls up by a page
func (m *PreviewModel) scrollPageUp() {
	pageSize := m.height - 3 // Account for header and borders
	m.scrollOffset -= pageSize
	if m.scrollOffset < 0 {
		m.scrollOffset = 0
	}
}

// scrollPageDown scrolls down by a page
func (m *PreviewModel) scrollPageDown() {
	pageSize := m.height - 3 // Account for header and borders
	maxScroll := m.getMaxScrollOffset()
	m.scrollOffset += pageSize
	if m.scrollOffset > maxScroll {
		m.scrollOffset = maxScroll
	}
}

// scrollToEnd scrolls to the end of the content
func (m *PreviewModel) scrollToEnd() {
	m.scrollOffset = m.getMaxScrollOffset()
}

// getMaxScrollOffset calculates the maximum scroll offset
func (m *PreviewModel) getMaxScrollOffset() int {
	if m.conversation == nil {
		return 0
	}

	contentHeight := m.getContentHeight()
	visibleHeight := m.height - 3 // Account for header and borders
	
	maxScroll := contentHeight - visibleHeight
	if maxScroll < 0 {
		maxScroll = 0
	}
	
	return maxScroll
}

// getContentHeight calculates the total height of the content
func (m *PreviewModel) getContentHeight() int {
	if m.conversation == nil {
		return 0
	}

	height := 0
	for _, message := range m.conversation.Messages {
		height += m.getMessageHeight(message)
	}
	
	return height
}

// getMessageHeight calculates the height of a single message
func (m *PreviewModel) getMessageHeight(message models.Message) int {
	// Base height for message header
	height := 1

	// Calculate content height based on text wrapping
	contentWidth := m.width - 4 // Account for padding and borders
	if contentWidth <= 0 {
		contentWidth = 40 // Fallback
	}

	lines := strings.Split(message.Content, "\n")
	for _, line := range lines {
		if len(line) == 0 {
			height++
			continue
		}
		
		// Calculate wrapped lines
		wrappedLines := (len(line) + contentWidth - 1) / contentWidth
		if wrappedLines == 0 {
			wrappedLines = 1
		}
		height += wrappedLines
	}

	// Add spacing between messages
	height += 1

	return height
}

// View renders the preview model
func (m *PreviewModel) View() string {
	if !m.visible || m.conversation == nil {
		return ""
	}

	var content strings.Builder

	// Render header
	header := m.renderHeader()
	content.WriteString(header)
	content.WriteString("\n")

	// Render conversation content
	conversationContent := m.renderConversationContent()
	content.WriteString(conversationContent)

	return content.String()
}

// renderHeader renders the preview panel header
func (m *PreviewModel) renderHeader() string {
	if m.conversation == nil {
		return ""
	}

	// Format the header info
	title := m.conversation.Title
	if len(title) > 40 {
		title = title[:37] + "..."
	}

	startedTime := m.conversation.CreatedAt.Format("Jan 2 3:04 PM")
	messageCount := len(m.conversation.Messages)

	headerText := fmt.Sprintf("💬 %s • Started: %s • %d messages",
		title, startedTime, messageCount)

	return m.styles.PreviewHeaderStyle.Width(m.width).Render(headerText)
}

// renderConversationContent renders the conversation messages
func (m *PreviewModel) renderConversationContent() string {
	if m.conversation == nil {
		return ""
	}

	var content strings.Builder
	visibleHeight := m.height - 3 // Account for header and borders
	currentLine := 0

	for _, message := range m.conversation.Messages {
		messageLines := m.renderMessage(message)
		messageHeight := strings.Count(messageLines, "\n") + 1

		// Skip if we haven't reached the scroll offset yet
		if currentLine+messageHeight <= m.scrollOffset {
			currentLine += messageHeight
			continue
		}

		// Stop if we've filled the visible area
		if currentLine-m.scrollOffset >= visibleHeight {
			break
		}

		// Render the message, potentially partially
		lines := strings.Split(messageLines, "\n")
		for i, line := range lines {
			lineIndex := currentLine + i
			
			// Skip lines before scroll offset
			if lineIndex < m.scrollOffset {
				continue
			}
			
			// Stop if we've filled the visible area
			if lineIndex-m.scrollOffset >= visibleHeight {
				break
			}
			
			content.WriteString(line)
			if i < len(lines)-1 { // Don't add newline after last line
				content.WriteString("\n")
			}
		}

		currentLine += messageHeight
		
		// Add spacing between messages if there's room
		if currentLine-m.scrollOffset < visibleHeight {
			content.WriteString("\n")
			currentLine++
		}
	}

	return m.styles.PreviewContentStyle.Width(m.width).Render(content.String())
}

// renderMessage renders a single message
func (m *PreviewModel) renderMessage(message models.Message) string {
	var content strings.Builder

	// Message header with role and timestamp
	emoji := GetEmojiForRole(message.Role)
	timeStr := message.Timestamp.Format("3:04 PM")
	
	var roleStyle lipgloss.Style
	switch message.Role {
	case "user":
		roleStyle = m.styles.UserMessageStyle
	case "assistant":
		roleStyle = m.styles.AssistantMessageStyle
	default:
		roleStyle = m.styles.UnselectedItemStyle
	}

	header := fmt.Sprintf("%s %s • %s", emoji, strings.Title(message.Role), timeStr)
	content.WriteString(roleStyle.Render(header))
	content.WriteString("\n")

	// Message content with word wrapping
	messageContent := m.wrapText(message.Content, m.width-4)
	content.WriteString(messageContent)

	return content.String()
}

// wrapText wraps text to fit within the specified width
func (m *PreviewModel) wrapText(text string, width int) string {
	if width <= 0 {
		return text
	}

	var result strings.Builder
	lines := strings.Split(text, "\n")

	for i, line := range lines {
		if len(line) <= width {
			result.WriteString(line)
		} else {
			// Wrap long lines
			for len(line) > width {
				// Find the best break point (space or punctuation)
				breakPoint := width
				for j := width - 1; j >= width/2; j-- {
					if line[j] == ' ' || line[j] == ',' || line[j] == '.' {
						breakPoint = j
						break
					}
				}
				
				result.WriteString(line[:breakPoint])
				result.WriteString("\n")
				line = line[breakPoint:]
				
				// Skip leading space on continuation lines
				if len(line) > 0 && line[0] == ' ' {
					line = line[1:]
				}
			}
			
			if len(line) > 0 {
				result.WriteString(line)
			}
		}

		// Add newline between original lines
		if i < len(lines)-1 {
			result.WriteString("\n")
		}
	}

	return result.String()
}

