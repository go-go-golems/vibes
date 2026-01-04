package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss"
)

var (
	// Color palette
	primaryColor   = lipgloss.Color("#7C3AED") // Purple
	secondaryColor = lipgloss.Color("#10B981") // Green
	errorColor     = lipgloss.Color("#EF4444") // Red
	warningColor   = lipgloss.Color("#F59E0B") // Yellow
	mutedColor     = lipgloss.Color("#6B7280") // Gray
	backgroundColor = lipgloss.Color("#1F2937") // Dark gray
	textColor      = lipgloss.Color("#F9FAFB") // Light gray

	// Base styles
	baseStyle = lipgloss.NewStyle().
		Foreground(textColor).
		Background(backgroundColor)

	// Title styles
	titleStyle = lipgloss.NewStyle().
		Foreground(primaryColor).
		Bold(true).
		Align(lipgloss.Center).
		MarginBottom(1)

	subtitleStyle = lipgloss.NewStyle().
		Foreground(mutedColor).
		Italic(true).
		Align(lipgloss.Center).
		MarginBottom(2)

	// Question styles
	questionStyle = lipgloss.NewStyle().
		Foreground(textColor).
		Bold(true).
		MarginBottom(1).
		Padding(0, 1)

	promptStyle = lipgloss.NewStyle().
		Foreground(textColor).
		MarginBottom(1).
		Padding(0, 1)

	// Option styles
	optionStyle = lipgloss.NewStyle().
		Foreground(textColor).
		Padding(0, 2).
		MarginLeft(2)

	selectedOptionStyle = lipgloss.NewStyle().
		Foreground(backgroundColor).
		Background(primaryColor).
		Bold(true).
		Padding(0, 2).
		MarginLeft(2)

	correctOptionStyle = lipgloss.NewStyle().
		Foreground(backgroundColor).
		Background(secondaryColor).
		Bold(true).
		Padding(0, 2).
		MarginLeft(2)

	incorrectOptionStyle = lipgloss.NewStyle().
		Foreground(backgroundColor).
		Background(errorColor).
		Bold(true).
		Padding(0, 2).
		MarginLeft(2)

	// Input styles
	inputStyle = lipgloss.NewStyle().
		Foreground(textColor).
		Background(lipgloss.Color("#374151")).
		Padding(0, 1).
		MarginLeft(2).
		MarginBottom(1)

	// Info box styles
	hintBoxStyle = lipgloss.NewStyle().
		Foreground(warningColor).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(warningColor).
		Padding(1, 2).
		MarginTop(1).
		MarginBottom(1)

	rationaleBoxStyle = lipgloss.NewStyle().
		Foreground(secondaryColor).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(secondaryColor).
		Padding(1, 2).
		MarginTop(1).
		MarginBottom(1)

	referenceBoxStyle = lipgloss.NewStyle().
		Foreground(mutedColor).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(mutedColor).
		Padding(1, 2).
		MarginTop(1).
		MarginBottom(1)

	// Progress styles
	progressBarStyle = lipgloss.NewStyle().
		Foreground(primaryColor).
		MarginBottom(1)

	// Help styles
	helpStyle = lipgloss.NewStyle().
		Foreground(mutedColor).
		MarginTop(2).
		Align(lipgloss.Center)

	// Summary styles
	summaryStyle = lipgloss.NewStyle().
		Foreground(textColor).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(primaryColor).
		Padding(2, 4).
		MarginTop(2).
		MarginBottom(2)

	scoreStyle = lipgloss.NewStyle().
		Foreground(secondaryColor).
		Bold(true)

	// Button styles
	buttonStyle = lipgloss.NewStyle().
		Foreground(backgroundColor).
		Background(primaryColor).
		Bold(true).
		Padding(0, 2).
		MarginTop(1).
		Align(lipgloss.Center)

	selectedButtonStyle = lipgloss.NewStyle().
		Foreground(backgroundColor).
		Background(secondaryColor).
		Bold(true).
		Padding(0, 2).
		MarginTop(1).
		Align(lipgloss.Center)
)

// renderProgressBar creates a visual progress bar
func renderProgressBar(progress float64, width int) string {
	if width <= 0 {
		width = 40
	}
	
	filled := int(progress / 100.0 * float64(width))
	if filled > width {
		filled = width
	}
	
	bar := ""
	for i := 0; i < width; i++ {
		if i < filled {
			bar += "█"
		} else {
			bar += "░"
		}
	}
	
	return progressBarStyle.Render(bar + " " + lipgloss.NewStyle().
		Foreground(primaryColor).
		Bold(true).
		Render(fmt.Sprintf("%.1f%%", progress)))
}

