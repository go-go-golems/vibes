package ui

import "github.com/charmbracelet/lipgloss"

var (
	// Colors
	ColorPrimary   = lipgloss.Color("#7D56F4")
	ColorSecondary = lipgloss.Color("#00BFFF")
	ColorSuccess   = lipgloss.Color("#00FF00")
	ColorWarning   = lipgloss.Color("#FFA500")
	ColorError     = lipgloss.Color("#FF0000")
	ColorMuted     = lipgloss.Color("#666666")
	ColorBorder    = lipgloss.Color("#383838")

	// Base styles
	BaseStyle = lipgloss.NewStyle().
			Padding(0, 1)

	TitleStyle = lipgloss.NewStyle().
			Bold(true).
			Foreground(ColorPrimary).
			Padding(0, 1)

	HeaderStyle = lipgloss.NewStyle().
			Bold(true).
			Foreground(ColorSecondary).
			BorderStyle(lipgloss.NormalBorder()).
			BorderForeground(ColorBorder).
			BorderBottom(true).
			Padding(0, 1)

	FooterStyle = lipgloss.NewStyle().
			Foreground(ColorMuted).
			BorderStyle(lipgloss.NormalBorder()).
			BorderForeground(ColorBorder).
			BorderTop(true).
			Padding(0, 1)

	// Service card styles
	ServiceCardStyle = lipgloss.NewStyle().
				BorderStyle(lipgloss.RoundedBorder()).
				BorderForeground(ColorBorder).
				Padding(1, 2).
				Margin(0, 2, 1, 2)

	ServiceCardSelectedStyle = lipgloss.NewStyle().
					BorderStyle(lipgloss.DoubleBorder()).
					BorderForeground(ColorPrimary).
					Padding(1, 2).
					Margin(0, 2, 1, 2)

	ServiceNameStyle = lipgloss.NewStyle().
				Bold(true).
				Foreground(ColorSecondary)

	ServiceStatusRunningStyle = lipgloss.NewStyle().
					Foreground(ColorSuccess).
					Bold(true)

	ServiceStatusStoppedStyle = lipgloss.NewStyle().
					Foreground(ColorMuted)

	ServiceStatusFailedStyle = lipgloss.NewStyle().
					Foreground(ColorError).
					Bold(true)

	// Log viewer styles
	LogLineStyle = lipgloss.NewStyle().
			Padding(0, 1)

	LogTimestampStyle = lipgloss.NewStyle().
				Foreground(ColorMuted)

	LogServiceStyle = lipgloss.NewStyle().
			Bold(true).
			Foreground(ColorSecondary)

	// Button styles
	ButtonStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FFF")).
			Background(ColorPrimary).
			Padding(0, 2).
			Margin(0, 1)

	ButtonActiveStyle = lipgloss.NewStyle().
				Foreground(lipgloss.Color("#FFF")).
				Background(ColorSecondary).
				Padding(0, 2).
				Margin(0, 1).
				Bold(true)

	// Help styles
	HelpKeyStyle = lipgloss.NewStyle().
			Foreground(ColorPrimary).
			Bold(true)

	HelpDescStyle = lipgloss.NewStyle().
			Foreground(ColorMuted)

	// Error/Warning styles
	ErrorBoxStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(ColorError).
			Padding(1, 2).
			Margin(0, 2, 1, 2)

	WarningBoxStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(ColorWarning).
			Padding(1, 2).
			Margin(0, 2, 1, 2)

	// Config styles
	ConfigSectionStyle = lipgloss.NewStyle().
				Bold(true).
				Foreground(ColorSecondary).
				Padding(1, 0, 0, 0)

	ConfigBoxStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(ColorBorder).
			Padding(1, 2).
			Margin(0, 2, 1, 2)

	ConfigKeyStyle = lipgloss.NewStyle().
			Width(30).
			Foreground(ColorPrimary)

	ConfigValueStyle = lipgloss.NewStyle().
				Foreground(lipgloss.Color("#FFF"))
)

func StatusStyle(status string) lipgloss.Style {
	switch status {
	case "RUNNING":
		return ServiceStatusRunningStyle
	case "FAILED":
		return ServiceStatusFailedStyle
	default:
		return ServiceStatusStoppedStyle
	}
}
