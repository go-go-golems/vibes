package keys

import "github.com/charmbracelet/bubbles/key"

// KeyMap defines key bindings for the application
type KeyMap struct {
	// Navigation
	Up    key.Binding
	Down  key.Binding
	Left  key.Binding
	Right key.Binding
	
	// Actions
	Enter  key.Binding
	Space  key.Binding
	Tab    key.Binding
	Escape key.Binding
	
	// Application
	Quit key.Binding
	Help key.Binding
	
	// Voting specific
	Vote   key.Binding
	Remove key.Binding
	
	// Facilitator specific
	Start    key.Binding
	Stop     key.Binding
	Reset    key.Binding
	Results  key.Binding
	Export   key.Binding
	New      key.Binding
	
	// Editing
	Edit   key.Binding
	Delete key.Binding
	Add    key.Binding
}

// NewKeyMap creates a new key map
func NewKeyMap() KeyMap {
	return KeyMap{
		// Navigation
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
		
		// Actions
		Enter: key.NewBinding(
			key.WithKeys("enter"),
			key.WithHelp("enter", "select/confirm"),
		),
		Space: key.NewBinding(
			key.WithKeys(" "),
			key.WithHelp("space", "toggle/vote"),
		),
		Tab: key.NewBinding(
			key.WithKeys("tab"),
			key.WithHelp("tab", "next field"),
		),
		Escape: key.NewBinding(
			key.WithKeys("esc"),
			key.WithHelp("esc", "back/cancel"),
		),
		
		// Application
		Quit: key.NewBinding(
			key.WithKeys("q", "ctrl+c"),
			key.WithHelp("q", "quit"),
		),
		Help: key.NewBinding(
			key.WithKeys("?"),
			key.WithHelp("?", "help"),
		),
		
		// Voting specific
		Vote: key.NewBinding(
			key.WithKeys("v", " "),
			key.WithHelp("v/space", "vote"),
		),
		Remove: key.NewBinding(
			key.WithKeys("x", "delete"),
			key.WithHelp("x/del", "remove vote"),
		),
		
		// Facilitator specific
		Start: key.NewBinding(
			key.WithKeys("s"),
			key.WithHelp("s", "start voting"),
		),
		Stop: key.NewBinding(
			key.WithKeys("c"),
			key.WithHelp("c", "close voting"),
		),
		Reset: key.NewBinding(
			key.WithKeys("r"),
			key.WithHelp("r", "reset"),
		),
		Results: key.NewBinding(
			key.WithKeys("R"),
			key.WithHelp("R", "show results"),
		),
		Export: key.NewBinding(
			key.WithKeys("e"),
			key.WithHelp("e", "export"),
		),
		New: key.NewBinding(
			key.WithKeys("n"),
			key.WithHelp("n", "new session"),
		),
		
		// Editing
		Edit: key.NewBinding(
			key.WithKeys("e", "enter"),
			key.WithHelp("e/enter", "edit"),
		),
		Delete: key.NewBinding(
			key.WithKeys("d", "x"),
			key.WithHelp("d/x", "delete"),
		),
		Add: key.NewBinding(
			key.WithKeys("a", "+"),
			key.WithHelp("a/+", "add"),
		),
	}
}

// ShortHelp returns key bindings to be shown in the mini help view
func (k KeyMap) ShortHelp() []key.Binding {
	return []key.Binding{k.Up, k.Down, k.Enter, k.Quit}
}

// FullHelp returns key bindings to be shown in the full help view
func (k KeyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Up, k.Down, k.Left, k.Right},
		{k.Enter, k.Space, k.Tab, k.Escape},
		{k.Vote, k.Remove, k.Help, k.Quit},
	}
}

// FacilitatorHelp returns key bindings for facilitators
func (k KeyMap) FacilitatorHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Up, k.Down, k.Enter, k.Tab},
		{k.Start, k.Stop, k.Results, k.Reset},
		{k.Add, k.Edit, k.Delete, k.Export},
		{k.New, k.Help, k.Quit},
	}
}

// ParticipantHelp returns key bindings for participants
func (k KeyMap) ParticipantHelp() [][]key.Binding {
	return [][]key.Binding{
		{k.Up, k.Down, k.Enter, k.Space},
		{k.Vote, k.Remove, k.Help, k.Quit},
	}
}

