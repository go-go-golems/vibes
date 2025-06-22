package ui

import (
	"github.com/charmbracelet/lipgloss"
)

// Styles contains all the styling for the application
type Styles struct {
	// Header styles
	HeaderStyle       lipgloss.Style
	TitleStyle        lipgloss.Style
	CountStyle        lipgloss.Style
	ActiveFilterStyle lipgloss.Style

	// List styles
	SelectedItemStyle   lipgloss.Style
	UnselectedItemStyle lipgloss.Style
	GroupHeaderStyle    lipgloss.Style
	MessagePreviewStyle lipgloss.Style
	TimeStyle           lipgloss.Style
	ConversationItem    lipgloss.Style
	ConversationItemSelected lipgloss.Style

	// Filter styles
	FilterRowStyle       lipgloss.Style
	ActiveFilterButton   lipgloss.Style
	InactiveFilterButton lipgloss.Style
	FilterButton         lipgloss.Style
	FilterButtonActive   lipgloss.Style
	FilterButtonSelected lipgloss.Style
	FilterHeader         lipgloss.Style

	// Search styles
	SearchInputStyle    lipgloss.Style
	SearchResultsStyle  lipgloss.Style
	SearchBox           lipgloss.Style

	// Preview styles
	PreviewHeaderStyle    lipgloss.Style
	PreviewContentStyle   lipgloss.Style
	UserMessageStyle      lipgloss.Style
	AssistantMessageStyle lipgloss.Style

	// Status styles
	StatusStyle         lipgloss.Style
	HelpStyle           lipgloss.Style
	Help                lipgloss.Style

	// Border styles
	BorderStyle         lipgloss.Style
	FocusedBorderStyle  lipgloss.Style
}

// NewStyles creates a new set of styles
func NewStyles() *Styles {
	// Color palette
	var (
		primaryColor   = lipgloss.Color("#7C3AED")  // Purple
		secondaryColor = lipgloss.Color("#10B981")  // Green
		textColor      = lipgloss.Color("#374151")  // Gray-700
		mutedColor     = lipgloss.Color("#9CA3AF")  // Gray-400
		backgroundColor = lipgloss.Color("#F9FAFB") // Gray-50
		borderColor    = lipgloss.Color("#E5E7EB")  // Gray-200
		selectedColor  = lipgloss.Color("#EDE9FE")  // Purple-100
	)

	return &Styles{
		// Header styles
		HeaderStyle: lipgloss.NewStyle().
			Bold(true).
			Foreground(primaryColor).
			Padding(0, 1),

		TitleStyle: lipgloss.NewStyle().
			Bold(true).
			Foreground(textColor),

		CountStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Italic(true),

		ActiveFilterStyle: lipgloss.NewStyle().
			Background(primaryColor).
			Foreground(lipgloss.Color("#FFFFFF")).
			Padding(0, 1).
			Bold(true),

		// List styles
		SelectedItemStyle: lipgloss.NewStyle().
			Background(selectedColor).
			Foreground(textColor).
			Padding(0, 1).
			Border(lipgloss.NormalBorder(), false, false, false, true).
			BorderForeground(primaryColor),

		UnselectedItemStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Padding(0, 1),

		ConversationItem: lipgloss.NewStyle().
			Foreground(textColor).
			Padding(0, 1),

		ConversationItemSelected: lipgloss.NewStyle().
			Background(selectedColor).
			Foreground(textColor).
			Padding(0, 1).
			Border(lipgloss.NormalBorder(), false, false, false, true).
			BorderForeground(primaryColor),

		GroupHeaderStyle: lipgloss.NewStyle().
			Bold(true).
			Foreground(primaryColor).
			Padding(1, 0, 0, 0),

		MessagePreviewStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Italic(true),

		TimeStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Align(lipgloss.Right),

		// Filter styles
		FilterRowStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder(), true, false, false, false).
			BorderForeground(borderColor).
			Padding(1, 0),

		ActiveFilterButton: lipgloss.NewStyle().
			Background(primaryColor).
			Foreground(lipgloss.Color("#FFFFFF")).
			Padding(0, 2).
			Margin(0, 1).
			Bold(true),

		InactiveFilterButton: lipgloss.NewStyle().
			Background(backgroundColor).
			Foreground(textColor).
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor).
			Padding(0, 2).
			Margin(0, 1),

		FilterButton: lipgloss.NewStyle().
			Background(backgroundColor).
			Foreground(textColor).
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor).
			Padding(0, 2).
			Margin(0, 1),

		FilterButtonActive: lipgloss.NewStyle().
			Background(primaryColor).
			Foreground(lipgloss.Color("#FFFFFF")).
			Padding(0, 2).
			Margin(0, 1).
			Bold(true),

		FilterButtonSelected: lipgloss.NewStyle().
			Background(secondaryColor).
			Foreground(lipgloss.Color("#FFFFFF")).
			Padding(0, 2).
			Margin(0, 1).
			Bold(true),

		FilterHeader: lipgloss.NewStyle().
			Bold(true).
			Foreground(primaryColor).
			Padding(0, 1),

		// Search styles
		SearchInputStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor).
			Padding(0, 1).
			Width(40),

		SearchResultsStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder(), true, false, false, false).
			BorderForeground(borderColor).
			Padding(1, 0),

		SearchBox: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor).
			Padding(0, 1),

		// Preview styles
		PreviewHeaderStyle: lipgloss.NewStyle().
			Bold(true).
			Foreground(primaryColor).
			Border(lipgloss.NormalBorder(), false, false, true, false).
			BorderForeground(borderColor).
			Padding(1),

		PreviewContentStyle: lipgloss.NewStyle().
			Padding(1),

		UserMessageStyle: lipgloss.NewStyle().
			Background(lipgloss.Color("#DBEAFE")).
			Foreground(textColor).
			Padding(1).
			Margin(0, 0, 1, 0),

		AssistantMessageStyle: lipgloss.NewStyle().
			Background(lipgloss.Color("#F3F4F6")).
			Foreground(textColor).
			Padding(1).
			Margin(0, 0, 1, 0),

		// Status styles
		StatusStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Italic(true).
			Padding(0, 1),

		HelpStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Italic(true),

		Help: lipgloss.NewStyle().
			Foreground(mutedColor).
			Italic(true),

		// Border styles
		BorderStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor),

		FocusedBorderStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(primaryColor),
	}
}

