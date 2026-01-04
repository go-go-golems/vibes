package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create styles for different layers
	boxStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#89b4fa")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Bold(true)

	overlayStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#f38ba8")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Bold(true)

	greenStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#a6e3a1")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Bold(true)

	// Create layers with content and positioning
	layer1 := lipgloss.NewLayer(boxStyle.Render("Layer 1")).
		X(5).Y(3).ID("layer1")

	layer2 := lipgloss.NewLayer(overlayStyle.Render("Layer 2")).
		X(15).Y(6).ID("layer2")

	layer3 := lipgloss.NewLayer(greenStyle.Render("Layer 3")).
		X(25).Y(9).ID("layer3")

	// Create canvas with all layers
	canvas := lipgloss.NewCanvas(layer1, layer2, layer3)

	fmt.Println("Simple Layering Demo")
	fmt.Println("===================")
	fmt.Println()
	fmt.Println(canvas.Render())
}

