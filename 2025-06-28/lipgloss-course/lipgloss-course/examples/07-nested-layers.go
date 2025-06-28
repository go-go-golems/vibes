package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create styles for nested components
	containerStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#89b4fa")).
		Background(lipgloss.Color("#313244")).
		Foreground(lipgloss.Color("#cdd6f4")).
		Padding(1, 2)

	headerStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#89b4fa")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(0, 1).
		Bold(true)

	contentStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#45475a")).
		Foreground(lipgloss.Color("#cdd6f4")).
		Padding(1, 2)

	buttonStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#a6e3a1")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(0, 2).
		Bold(true)

	// Create nested content
	button1 := buttonStyle.Render("OK")
	button2 := buttonStyle.Render("Cancel")
	buttons := lipgloss.JoinHorizontal(lipgloss.Center, button1, "  ", button2)

	dialogContent := lipgloss.JoinVertical(lipgloss.Left,
		headerStyle.Render("Confirmation Dialog"),
		"",
		contentStyle.Render("Are you sure you want to proceed?\nThis action cannot be undone."),
		"",
		buttons,
	)

	// Create the main dialog layer
	dialog := lipgloss.NewLayer(containerStyle.Render(dialogContent)).
		X(20).Y(5).Z(5).ID("dialog")

	// Create a parent window
	windowContent := "File Edit View Tools Help\n\nDocument.txt - Modified\n\nLorem ipsum dolor sit amet,\nconsectetur adipiscing elit.\nSed do eiusmod tempor incididunt\nut labore et dolore magna aliqua.\n\nUt enim ad minim veniam, quis\nnostrud exercitation ullamco."

	window := lipgloss.NewLayer(containerStyle.Render(windowContent)).
		X(5).Y(2).Z(1).ID("window")

	// Create an overlay background
	overlayStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#1e1e2e")).
		Foreground(lipgloss.Color("#6c7086")).
		Width(70).
		Height(20)

	overlay := lipgloss.NewLayer(overlayStyle.Render("")).
		X(0).Y(0).Z(3).ID("overlay")

	// Create canvas with nested layers
	canvas := lipgloss.NewCanvas(window, overlay, dialog)

	fmt.Println("Nested Layers Demo")
	fmt.Println("==================")
	fmt.Println("Demonstrating modal dialogs over application windows")
	fmt.Println()
	fmt.Println(canvas.Render())
}

