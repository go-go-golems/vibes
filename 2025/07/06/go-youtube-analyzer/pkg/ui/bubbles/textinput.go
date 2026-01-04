package bubbles

import (
	"github.com/charmbracelet/bubbles/textinput"
	"github.com/user/youtube-analyzer-go/pkg/ui/view"
)

// TextInput wraps the standard textinput.Model with custom styling
type TextInput struct {
	textinput.Model
}

// NewTextInput creates a new wrapped text input
func NewTextInput() TextInput {
	ti := textinput.New()

	// Apply custom styling
	ti.TextStyle = view.Styles.Value
	ti.PlaceholderStyle = view.Styles.Value
	ti.Cursor.Style = view.Styles.Value

	return TextInput{
		Model: ti,
	}
}

// NewURLInput creates a text input specifically for URL entry
func NewURLInput() TextInput {
	ti := NewTextInput()
	ti.Placeholder = "Enter YouTube URL..."
	ti.Focus()
	ti.CharLimit = 256
	ti.Width = 60

	return ti
}

// SetFocused applies focused styling to the text input
func (t *TextInput) SetFocused(focused bool) {
	if focused {
		t.TextStyle = view.Styles.Value
		t.PlaceholderStyle = view.Styles.Value
		t.Focus()
	} else {
		t.TextStyle = view.Styles.Value
		t.PlaceholderStyle = view.Styles.Value
		t.Blur()
	}
}
