package model

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/pkg/ui/keys"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// ErrorModel handles the error display screen
type ErrorModel struct {
	Common CommonState
	Err    error
	keyMap keys.KeyMap
}

// NewErrorModel creates a new error model
func NewErrorModel(common CommonState) ErrorModel {
	return ErrorModel{
		Common: common,
		keyMap: keys.NewKeyMap(),
	}
}

// Init initializes the error model
func (m ErrorModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the error model
func (m ErrorModel) Update(msg tea.Msg) (ErrorModel, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case msg.String() == "ctrl+c":
			return m, tea.Quit
		case msg.String() == "n", msg.String() == "r":
			return m, func() tea.Msg {
				return ScreenChangeMsg{Screen: ScreenInput}
			}
		case msg.String() == "esc":
			return m, func() tea.Msg {
				return ScreenChangeMsg{Screen: ScreenInput}
			}
		}
	}

	return m, nil
}

// View renders the error screen
func (m ErrorModel) View() string {
	width := view.AdaptWidth(m.Common.Width)
	height := view.AdaptHeight(m.Common.Height)

	// Header
	header := view.RenderHeader("🎬 YouTube Analyzer - Error", width)

	// Error content (used in error message rendering)

	// Error message
	errorMsg := view.RenderError(m.Err)

	// Troubleshooting tips
	troubleshootingTips := []string{
		"• Check your internet connection",
		"• Verify the YouTube URL is correct and accessible",
		"• Ensure your API key is valid and has sufficient quota",
		"• Try a different video URL",
		"• Check if the video is public and not age-restricted",
	}

	troubleshooting := view.RenderSection("🔧 Troubleshooting Tips",
		strings.Join(troubleshootingTips, "\n"), width-4)

	// Help
	helpView := m.Common.Help.View(m.keyMap)

	// Instructions
	instructions := view.Styles.Help.Render(
		"Press N to try again • Esc to go back • Ctrl+C to quit",
	)

	// Footer
	footer := view.RenderFooter("Error occurred during analysis", width)

	// Content
	content := view.Styles.Content.Render(
		fmt.Sprintf("%s\n\n%s\n\n%s\n\n%s",
			errorMsg,
			troubleshooting,
			instructions,
			helpView,
		),
	)

	// Center content vertically
	usedHeight := strings.Count(header+content+footer, "\n") + 3
	remainingHeight := height - usedHeight
	if remainingHeight > 0 {
		padding := strings.Repeat("\n", remainingHeight/2)
		content = padding + content
	}

	return header + "\n" + content + "\n" + footer
}
