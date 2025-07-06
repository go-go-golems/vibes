package model

import (
	"strings"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/pkg/ui/keys"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// InputModel handles the URL input screen
type InputModel struct {
	Common    CommonState
	textInput textinput.Model
	keyMap    keys.InputKeyMap
	err       error
}

// NewInputModel creates a new input model
func NewInputModel(common CommonState) InputModel {
	ti := textinput.New()
	ti.Placeholder = "Enter YouTube URL (e.g., https://www.youtube.com/watch?v=dQw4w9WgXcQ)"
	ti.Focus()
	ti.CharLimit = 256
	ti.Width = 50

	return InputModel{
		Common:    common,
		textInput: ti,
		keyMap:    keys.NewInputKeyMap(),
	}
}

// Init initializes the input model
func (m InputModel) Init() tea.Cmd {
	return textinput.Blink
}

// Update handles messages for the input model
func (m InputModel) Update(msg tea.Msg) (InputModel, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case msg.String() == "ctrl+c":
			return m, tea.Quit
		case msg.String() == "enter":
			url := strings.TrimSpace(m.textInput.Value())
			if url == "" {
				m.err = nil
				return m, nil
			}

			if !isValidYouTubeURL(url) {
				m.err = NewInvalidURLError(url)
				return m, nil
			}

			m.err = nil
			return m, func() tea.Msg {
				return ScreenChangeMsg{
					Screen:   ScreenLoading,
					VideoURL: url,
				}
			}
		case msg.String() == "esc":
			return m, tea.Quit
		case msg.String() == "ctrl+l":
			m.textInput.SetValue("")
			m.err = nil
			return m, nil
		}
	}

	// Update text input
	m.textInput, cmd = m.textInput.Update(msg)
	return m, cmd
}

// View renders the input screen
func (m InputModel) View() string {
	width := view.AdaptWidth(m.Common.Width)
	height := view.AdaptHeight(m.Common.Height)

	// Header
	header := view.RenderHeader("🎬 YouTube Analyzer", width)

	// Title and description
	title := view.Styles.Title.Render("Enter YouTube URL")
	description := view.Styles.Value.Render(
		"Paste a YouTube URL to analyze the video for technical content,\n" +
			"engagement potential, and social media optimization.",
	)

	// Input field
	inputLabel := view.Styles.Label.Render("YouTube URL:")

	// Style the text input based on focus
	var inputField string
	if m.textInput.Focused() {
		inputField = view.Styles.InputFocused.Render(m.textInput.View())
	} else {
		inputField = view.Styles.Input.Render(m.textInput.View())
	}

	// Error message
	var errorMsg string
	if m.err != nil {
		errorMsg = view.RenderError(m.err)
	}

	// Help
	helpView := m.Common.Help.View(m.keyMap)

	// Instructions
	instructions := view.Styles.Help.Render(
		"Press Enter to analyze • Esc to quit • Ctrl+L to clear",
	)

	// Footer
	footer := view.RenderFooter("YouTube Analyzer v1.0", width)

	// Layout
	content := view.Styles.Content.Render(
		view.RenderSection("",
			strings.Join([]string{
				title,
				"",
				description,
				"",
				inputLabel,
				inputField,
				errorMsg,
				"",
				instructions,
				"",
				helpView,
			}, "\n"),
			width,
		),
	)

	// Calculate remaining height for centering
	usedHeight := strings.Count(header+content+footer, "\n") + 3
	remainingHeight := height - usedHeight
	if remainingHeight > 0 {
		padding := strings.Repeat("\n", remainingHeight/2)
		content = padding + content
	}

	return header + "\n" + content + "\n" + footer
}

// isValidYouTubeURL checks if the URL is a valid YouTube URL
func isValidYouTubeURL(url string) bool {
	url = strings.ToLower(url)
	return strings.Contains(url, "youtube.com/watch") ||
		strings.Contains(url, "youtu.be/") ||
		strings.Contains(url, "youtube.com/embed/") ||
		strings.Contains(url, "youtube.com/v/")
}

// InvalidURLError represents an invalid URL error
type InvalidURLError struct {
	URL string
}

func (e InvalidURLError) Error() string {
	return "Invalid YouTube URL: " + e.URL
}

// NewInvalidURLError creates a new invalid URL error
func NewInvalidURLError(url string) InvalidURLError {
	return InvalidURLError{URL: url}
}
