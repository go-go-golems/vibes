package view

import (
	"github.com/charmbracelet/lipgloss"
)

// Color palette
var (
	ColorPrimary   = lipgloss.Color("#5af")
	ColorSecondary = lipgloss.Color("#f5a")
	ColorSuccess   = lipgloss.Color("#5f5")
	ColorWarning   = lipgloss.Color("#fa5")
	ColorError     = lipgloss.Color("#f55")
	ColorMuted     = lipgloss.Color("#888")
	ColorBorder    = lipgloss.Color("#444")
	ColorText      = lipgloss.Color("#fff")
	ColorBackground = lipgloss.Color("#000")
)

// Styles contains all the lipgloss styles used in the application
var Styles = struct {
	// Layout styles
	Container    lipgloss.Style
	Header       lipgloss.Style
	Footer       lipgloss.Style
	Content      lipgloss.Style
	Sidebar      lipgloss.Style
	
	// Text styles
	Title        lipgloss.Style
	Subtitle     lipgloss.Style
	Body         lipgloss.Style
	Muted        lipgloss.Style
	Bold         lipgloss.Style
	
	// Interactive styles
	Button       lipgloss.Style
	ButtonActive lipgloss.Style
	Input        lipgloss.Style
	InputFocused lipgloss.Style
	
	// List styles
	ListItem     lipgloss.Style
	ListSelected lipgloss.Style
	ListTitle    lipgloss.Style
	
	// Status styles
	StatusGood   lipgloss.Style
	StatusWarn   lipgloss.Style
	StatusError  lipgloss.Style
	
	// Voting styles
	VoteBar      lipgloss.Style
	VoteCount    lipgloss.Style
	DotUsed      lipgloss.Style
	DotAvailable lipgloss.Style
	
	// Border styles
	BorderNormal lipgloss.Style
	BorderFocused lipgloss.Style
	BorderError  lipgloss.Style
}{
	// Layout styles
	Container: lipgloss.NewStyle().
		Padding(1, 2).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(ColorBorder),
	
	Header: lipgloss.NewStyle().
		Padding(0, 1).
		Border(lipgloss.NormalBorder(), false, false, true, false).
		BorderForeground(ColorBorder).
		Bold(true),
	
	Footer: lipgloss.NewStyle().
		Padding(0, 1).
		Border(lipgloss.NormalBorder(), true, false, false, false).
		BorderForeground(ColorBorder).
		Foreground(ColorMuted),
	
	Content: lipgloss.NewStyle().
		Padding(1, 2),
	
	Sidebar: lipgloss.NewStyle().
		Padding(1).
		Border(lipgloss.NormalBorder(), false, true, false, false).
		BorderForeground(ColorBorder),
	
	// Text styles
	Title: lipgloss.NewStyle().
		Bold(true).
		Foreground(ColorPrimary).
		Align(lipgloss.Center),
	
	Subtitle: lipgloss.NewStyle().
		Bold(true).
		Foreground(ColorText),
	
	Body: lipgloss.NewStyle().
		Foreground(ColorText),
	
	Muted: lipgloss.NewStyle().
		Foreground(ColorMuted),
	
	Bold: lipgloss.NewStyle().
		Bold(true).
		Foreground(ColorText),
	
	// Interactive styles
	Button: lipgloss.NewStyle().
		Padding(0, 2).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(ColorBorder).
		Foreground(ColorText),
	
	ButtonActive: lipgloss.NewStyle().
		Padding(0, 2).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(ColorPrimary).
		Foreground(ColorPrimary).
		Bold(true),
	
	Input: lipgloss.NewStyle().
		Padding(0, 1).
		Border(lipgloss.NormalBorder()).
		BorderForeground(ColorBorder).
		Foreground(ColorText),
	
	InputFocused: lipgloss.NewStyle().
		Padding(0, 1).
		Border(lipgloss.NormalBorder()).
		BorderForeground(ColorPrimary).
		Foreground(ColorText),
	
	// List styles
	ListItem: lipgloss.NewStyle().
		Padding(0, 2).
		Foreground(ColorText),
	
	ListSelected: lipgloss.NewStyle().
		Padding(0, 2).
		Border(lipgloss.NormalBorder(), false, false, false, true).
		BorderForeground(ColorPrimary).
		Foreground(ColorPrimary).
		Bold(true),
	
	ListTitle: lipgloss.NewStyle().
		Bold(true).
		Foreground(ColorText).
		Padding(0, 2),
	
	// Status styles
	StatusGood: lipgloss.NewStyle().
		Foreground(ColorSuccess).
		Bold(true),
	
	StatusWarn: lipgloss.NewStyle().
		Foreground(ColorWarning).
		Bold(true),
	
	StatusError: lipgloss.NewStyle().
		Foreground(ColorError).
		Bold(true),
	
	// Voting styles
	VoteBar: lipgloss.NewStyle().
		Foreground(ColorPrimary),
	
	VoteCount: lipgloss.NewStyle().
		Bold(true).
		Foreground(ColorText),
	
	DotUsed: lipgloss.NewStyle().
		Foreground(ColorPrimary).
		Bold(true),
	
	DotAvailable: lipgloss.NewStyle().
		Foreground(ColorMuted),
	
	// Border styles
	BorderNormal: lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(ColorBorder),
	
	BorderFocused: lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(ColorPrimary),
	
	BorderError: lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(ColorError),
}

// Helper functions for common UI elements

// RenderTitle renders a centered title
func RenderTitle(title string, width int) string {
	return Styles.Title.Width(width).Render(title)
}

// RenderHeader renders a header with title and status
func RenderHeader(title, status string, width int) string {
	left := Styles.Subtitle.Render(title)
	right := Styles.Muted.Render(status)
	
	padding := width - lipgloss.Width(left) - lipgloss.Width(right)
	if padding < 0 {
		padding = 0
	}
	
	return left + lipgloss.NewStyle().Width(padding).Render("") + right
}

// RenderFooter renders a footer with help text
func RenderFooter(help string, width int) string {
	return Styles.Footer.Width(width).Render(help)
}

// RenderButton renders a button with optional active state
func RenderButton(text string, active bool) string {
	if active {
		return Styles.ButtonActive.Render(text)
	}
	return Styles.Button.Render(text)
}

// RenderProgressBar renders a progress bar for vote counts
func RenderProgressBar(current, max int, width int) string {
	if max == 0 {
		return ""
	}
	
	filled := int(float64(current) / float64(max) * float64(width))
	if filled > width {
		filled = width
	}
	
	bar := ""
	for i := 0; i < filled; i++ {
		bar += "█"
	}
	for i := filled; i < width; i++ {
		bar += "▌"
	}
	
	return Styles.VoteBar.Render(bar)
}

// RenderDots renders voting dots (used/available)
func RenderDots(used, total int) string {
	dots := ""
	
	// Used dots
	for i := 0; i < used; i++ {
		dots += Styles.DotUsed.Render("💙")
	}
	
	// Available dots
	for i := used; i < total; i++ {
		dots += Styles.DotAvailable.Render("💙")
	}
	
	return dots
}

