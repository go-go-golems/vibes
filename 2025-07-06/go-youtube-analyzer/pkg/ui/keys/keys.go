package keys

import (
	"github.com/charmbracelet/bubbles/key"
)

// KeyMap represents the key bindings for the application
type KeyMap struct {
	// Navigation
	Up    key.Binding
	Down  key.Binding
	Left  key.Binding
	Right key.Binding

	// Actions
	Enter key.Binding
	Space key.Binding
	Tab   key.Binding

	// Application
	Quit key.Binding
	Back key.Binding
	Help key.Binding

	// Input
	Clear  key.Binding
	Delete key.Binding

	// Results
	Retry key.Binding
	New   key.Binding
}

// NewKeyMap creates a new key map with default bindings
func NewKeyMap() KeyMap {
	return KeyMap{
		Up: key.NewBinding(
			key.WithKeys("k", "up"),
			key.WithHelp("↑/k", "up"),
		),
		Down: key.NewBinding(
			key.WithKeys("j", "down"),
			key.WithHelp("↓/j", "down"),
		),
		Left: key.NewBinding(
			key.WithKeys("h", "left"),
			key.WithHelp("←/h", "left"),
		),
		Right: key.NewBinding(
			key.WithKeys("l", "right"),
			key.WithHelp("→/l", "right"),
		),
		Enter: key.NewBinding(
			key.WithKeys("enter"),
			key.WithHelp("enter", "confirm"),
		),
		Space: key.NewBinding(
			key.WithKeys(" "),
			key.WithHelp("space", "select"),
		),
		Tab: key.NewBinding(
			key.WithKeys("tab"),
			key.WithHelp("tab", "next"),
		),
		Quit: key.NewBinding(
			key.WithKeys("q", "ctrl+c"),
			key.WithHelp("q", "quit"),
		),
		Back: key.NewBinding(
			key.WithKeys("esc", "ctrl+z"),
			key.WithHelp("esc", "back"),
		),
		Help: key.NewBinding(
			key.WithKeys("?"),
			key.WithHelp("?", "help"),
		),
		Clear: key.NewBinding(
			key.WithKeys("ctrl+l"),
			key.WithHelp("ctrl+l", "clear"),
		),
		Delete: key.NewBinding(
			key.WithKeys("backspace", "delete"),
			key.WithHelp("backspace", "delete"),
		),
		Retry: key.NewBinding(
			key.WithKeys("r"),
			key.WithHelp("r", "retry"),
		),
		New: key.NewBinding(
			key.WithKeys("n"),
			key.WithHelp("n", "new"),
		),
	}
}

// ShortHelp returns the short help for the key map
func (k KeyMap) ShortHelp() []key.Binding {
	return []key.Binding{
		k.Up, k.Down, k.Enter, k.Quit,
	}
}

// FullHelp returns the full help for the key map
func (k KeyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Up, k.Down, k.Left, k.Right},
		{k.Enter, k.Space, k.Tab, k.Back},
		{k.Help, k.Quit, k.Clear, k.Delete},
		{k.Retry, k.New},
	}
}

// InputKeyMap represents key bindings specific to input screens
type InputKeyMap struct {
	KeyMap
	Submit key.Binding
	Cancel key.Binding
}

// NewInputKeyMap creates a new input key map
func NewInputKeyMap() InputKeyMap {
	base := NewKeyMap()
	return InputKeyMap{
		KeyMap: base,
		Submit: key.NewBinding(
			key.WithKeys("enter"),
			key.WithHelp("enter", "submit"),
		),
		Cancel: key.NewBinding(
			key.WithKeys("esc"),
			key.WithHelp("esc", "cancel"),
		),
	}
}

// ShortHelp returns the short help for input key map
func (k InputKeyMap) ShortHelp() []key.Binding {
	return []key.Binding{
		k.Submit, k.Cancel, k.Quit,
	}
}

// FullHelp returns the full help for input key map
func (k InputKeyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Submit, k.Cancel, k.Clear},
		{k.Help, k.Quit},
	}
}

// ResultsKeyMap represents key bindings for the results screen
type ResultsKeyMap struct {
	KeyMap
	ScrollUp   key.Binding
	ScrollDown key.Binding
	Home       key.Binding
	End        key.Binding
}

// NewResultsKeyMap creates a new results key map
func NewResultsKeyMap() ResultsKeyMap {
	base := NewKeyMap()
	return ResultsKeyMap{
		KeyMap: base,
		ScrollUp: key.NewBinding(
			key.WithKeys("u", "pgup"),
			key.WithHelp("u", "scroll up"),
		),
		ScrollDown: key.NewBinding(
			key.WithKeys("d", "pgdown"),
			key.WithHelp("d", "scroll down"),
		),
		Home: key.NewBinding(
			key.WithKeys("home", "g"),
			key.WithHelp("g", "go to top"),
		),
		End: key.NewBinding(
			key.WithKeys("end", "G"),
			key.WithHelp("G", "go to bottom"),
		),
	}
}

// ShortHelp returns the short help for results key map
func (k ResultsKeyMap) ShortHelp() []key.Binding {
	return []key.Binding{
		k.Up, k.Down, k.ScrollUp, k.ScrollDown, k.New, k.Quit,
	}
}

// FullHelp returns the full help for results key map
func (k ResultsKeyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Up, k.Down, k.ScrollUp, k.ScrollDown},
		{k.Home, k.End, k.Left, k.Right},
		{k.New, k.Retry, k.Back, k.Quit},
		{k.Help},
	}
}
