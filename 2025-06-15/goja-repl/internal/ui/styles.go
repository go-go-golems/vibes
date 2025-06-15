package ui

import (
	"github.com/charmbracelet/lipgloss"
)

// Styles defines the UI styling for the REPL
type Styles struct {
	Title        lipgloss.Style
	Prompt       lipgloss.Style
	Input        lipgloss.Style
	Result       lipgloss.Style
	Error        lipgloss.Style
	Info         lipgloss.Style
	HistoryEntry lipgloss.Style
	StatusBar    lipgloss.Style
	HelpText     lipgloss.Style
}

// DefaultStyles returns the default styling for the REPL
func DefaultStyles() Styles {
	return Styles{
		Title: lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1),

		Prompt: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#7D56F4")).
			Bold(true),

		Input: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FFFFFF")),

		Result: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#04B575")),

		Error: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FF5F87")),

		Info: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#7D56F4")).
			Italic(true),

		HistoryEntry: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FFFFFF")),

		StatusBar: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FFFFFF")).
			Background(lipgloss.Color("#3C3C3C")).
			Padding(0, 1),

		HelpText: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#888888")).
			Italic(true),
	}
}
