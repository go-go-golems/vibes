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

	// Filter styles
	FilterRowStyle      lipgloss.Style
	ActiveFilterButton  lipgloss.Style
	InactiveFilterButton lipgloss.Style

	// Search styles
	SearchInputStyle    lipgloss.Style
	SearchResultsStyle  lipgloss.Style

	// Preview styles
	PreviewHeaderStyle  lipgloss.Style
	PreviewContentStyle lipgloss.Style
	UserMessageStyle    lipgloss.Style
	AssistantMessageStyle lipgloss.Style

	// Status styles
	StatusStyle         lipgloss.Style
	HelpStyle           lipgloss.Style

	// Border styles
	BorderStyle         lipgloss.Style
	FocusedBorderStyle  lipgloss.Style
}

// NewStyles creates a new set of styles
func NewStyles() *Styles {
	// Color palette
	var (
		primaryColor   = lipgloss.Color("#7C3AED")  // Purple
		accentColor    = lipgloss.Color("#F59E0B")  // Amber
		textColor      = lipgloss.Color("#F3F4F6")  // Light gray
		mutedColor     = lipgloss.Color("#9CA3AF")  // Gray
		borderColor    = lipgloss.Color("#374151")  // Dark gray
		focusColor     = lipgloss.Color("#3B82F6")  // Blue
	)

	return &Styles{
		// Header styles
		HeaderStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(primaryColor).
			Padding(0, 1).
			Bold(true),

		TitleStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Bold(true),

		CountStyle: lipgloss.NewStyle().
			Foreground(mutedColor),

		ActiveFilterStyle: lipgloss.NewStyle().
			Foreground(accentColor).
			Bold(true),

		// List styles
		SelectedItemStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(primaryColor).
			Padding(0, 1).
			Bold(true),

		UnselectedItemStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Padding(0, 1),

		GroupHeaderStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Bold(true).
			MarginTop(1),

		MessagePreviewStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Italic(true),

		TimeStyle: lipgloss.NewStyle().
			Foreground(mutedColor).
			Align(lipgloss.Right),

		// Filter styles
		FilterRowStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor).
			Padding(0, 1),

		ActiveFilterButton: lipgloss.NewStyle().
			Foreground(textColor).
			Background(primaryColor).
			Padding(0, 1).
			Bold(true),

		InactiveFilterButton: lipgloss.NewStyle().
			Foreground(mutedColor).
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor).
			Padding(0, 1),

		// Search styles
		SearchInputStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(lipgloss.Color("#1F2937")).
			Padding(0, 1).
			Border(lipgloss.NormalBorder()).
			BorderForeground(focusColor),

		SearchResultsStyle: lipgloss.NewStyle().
			Foreground(textColor),

		// Preview styles
		PreviewHeaderStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(primaryColor).
			Padding(0, 1).
			Bold(true),

		PreviewContentStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Padding(1),

		UserMessageStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(lipgloss.Color("#1E40AF")).
			Padding(0, 1).
			MarginBottom(1),

		AssistantMessageStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(lipgloss.Color("#059669")).
			Padding(0, 1).
			MarginBottom(1),

		// Status styles
		StatusStyle: lipgloss.NewStyle().
			Foreground(textColor).
			Background(lipgloss.Color("#1F2937")).
			Padding(0, 1),

		HelpStyle: lipgloss.NewStyle().
			Foreground(mutedColor),

		// Border styles
		BorderStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(borderColor),

		FocusedBorderStyle: lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(focusColor),
	}
}

// GetEmojiForRole returns an emoji for the given message role
func GetEmojiForRole(role string) string {
	switch role {
	case "user":
		return "👤"
	case "assistant":
		return "🤖"
	case "system":
		return "⚙️"
	default:
		return "💬"
	}
}

// GetColorForTag returns a color for conversation tags
func GetColorForTag(tag string) lipgloss.Color {
	colors := []lipgloss.Color{
		lipgloss.Color("#10B981"), // Green
		lipgloss.Color("#3B82F6"), // Blue
		lipgloss.Color("#8B5CF6"), // Purple
		lipgloss.Color("#F59E0B"), // Amber
		lipgloss.Color("#EF4444"), // Red
		lipgloss.Color("#06B6D4"), // Cyan
	}
	
	// Simple hash to get consistent color for tag
	hash := 0
	for _, char := range tag {
		hash += int(char)
	}
	
	return colors[hash%len(colors)]
}

