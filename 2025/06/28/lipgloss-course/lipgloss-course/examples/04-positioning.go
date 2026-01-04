package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a style for positioning demo
	boxStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#89b4fa")).
		Padding(1, 2).
		Background(lipgloss.Color("#313244")).
		Foreground(lipgloss.Color("#cdd6f4"))

	// Create layers at different positions
	topLeft := lipgloss.NewLayer(boxStyle.Render("Top Left")).
		X(0).Y(0).ID("topLeft")

	topRight := lipgloss.NewLayer(boxStyle.Render("Top Right")).
		X(40).Y(0).ID("topRight")

	center := lipgloss.NewLayer(boxStyle.Render("Center")).
		X(20).Y(5).ID("center")

	bottomLeft := lipgloss.NewLayer(boxStyle.Render("Bottom Left")).
		X(0).Y(10).ID("bottomLeft")

	bottomRight := lipgloss.NewLayer(boxStyle.Render("Bottom Right")).
		X(40).Y(10).ID("bottomRight")

	// Create a floating overlay
	overlayStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#f38ba8")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Bold(true)

	overlay := lipgloss.NewLayer(overlayStyle.Render("Floating")).
		X(25).Y(7).Z(10).ID("overlay") // Higher Z-index

	// Create canvas
	canvas := lipgloss.NewCanvas(topLeft, topRight, center, bottomLeft, bottomRight, overlay)

	fmt.Println("Positioning Demo")
	fmt.Println("===============")
	fmt.Println()
	fmt.Println(canvas.Render())
}

