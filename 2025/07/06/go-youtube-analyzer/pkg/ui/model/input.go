package model

import (
	"strings"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"

	"github.com/user/youtube-analyzer-go/pkg/ui/keys"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// InputModel handles the URL/prompt input screen
type InputModel struct {
	Common    CommonState
	textInput textinput.Model
	keyMap    keys.InputKeyMap
	err       error
}

// NewInputModel creates a new input model
func NewInputModel(common CommonState) InputModel {
	ti := textinput.New()
	ti.Placeholder = "Enter YouTube URL or simple text prompt..."
	ti.Focus()
	ti.CharLimit = 512
	ti.Width = 60

	return InputModel{
		Common:    common,
		textInput: ti,
		keyMap:    keys.NewInputKeyMap(),
	}
}

// NewInputModelWithURL creates a new input model with a pre-filled URL
func NewInputModelWithURL(common CommonState, url string) InputModel {
	m := NewInputModel(common)
	m.textInput.SetValue(url)
	return m
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
		if m.Common.Logger != nil {
			m.Common.Logger.Debug().
				Str("component", "input").
				Str("key", msg.String()).
				Msg("Received key message")
		}
		
		switch {
		case msg.String() == "ctrl+c":
			return m, tea.Quit
		case msg.String() == "enter":
			input := strings.TrimSpace(m.textInput.Value())
			
			if m.Common.Logger != nil {
				m.Common.Logger.Debug().
					Str("component", "input").
					Str("input", input).
					Msg("Processing enter key with input")
			}
			
			if input == "" {
				m.err = nil
				if m.Common.Logger != nil {
					m.Common.Logger.Debug().
						Str("component", "input").
						Msg("Empty input - ignoring")
				}
				return m, nil
			}

			// Check if it's a YouTube URL or a simple prompt
			if isValidYouTubeURL(input) {
				// Handle as video URL
				m.err = nil
				
				if m.Common.Logger != nil {
					m.Common.Logger.Info().
						Str("component", "input").
						Str("videoURL", input).
						Msg("Valid YouTube URL detected - transitioning to streaming")
				}
				
				return m, func() tea.Msg {
					return ScreenChangeMsg{
						Screen:   ScreenStreaming,
						VideoURL: input,
					}
				}
			} else {
				// Handle as simple text prompt
				m.err = nil
				
				if m.Common.Logger != nil {
					m.Common.Logger.Info().
						Str("component", "input").
						Str("prompt", input).
						Msg("Text prompt detected - transitioning to streaming")
				}
				
				return m, func() tea.Msg {
					return ScreenChangeMsg{
						Screen: ScreenStreaming,
						Prompt: input,
					}
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
	title := view.Styles.Title.Render("Enter YouTube URL or Text Prompt")
	description := view.Styles.Value.Render(
		"Paste a YouTube URL to analyze video content, or enter a simple text prompt\n" +
			"for AI text generation with streaming output.",
	)

	// Input field
	inputLabel := view.Styles.Label.Render("Input:")

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
