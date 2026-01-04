package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create different colored layers to demonstrate Z-index
	redStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#f38ba8")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(2, 4).
		Bold(true)

	blueStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#89b4fa")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(2, 4).
		Bold(true)

	greenStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#a6e3a1")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(2, 4).
		Bold(true)

	yellowStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#f9e2af")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(2, 4).
		Bold(true)

	// Create layers with different Z-indexes
	// Lower Z-index (background)
	redLayer := lipgloss.NewLayer(redStyle.Render("Red Layer\nZ-index: 1")).
		X(5).Y(3).Z(1).ID("red")

	blueLayer := lipgloss.NewLayer(blueStyle.Render("Blue Layer\nZ-index: 3")).
		X(15).Y(5).Z(3).ID("blue")

	greenLayer := lipgloss.NewLayer(greenStyle.Render("Green Layer\nZ-index: 2")).
		X(25).Y(7).Z(2).ID("green")

	// Higher Z-index (foreground)
	yellowLayer := lipgloss.NewLayer(yellowStyle.Render("Yellow Layer\nZ-index: 4")).
		X(20).Y(4).Z(4).ID("yellow")

	// Create canvas
	canvas := lipgloss.NewCanvas(redLayer, blueLayer, greenLayer, yellowLayer)

	fmt.Println("Z-Index Demonstration")
	fmt.Println("====================")
	fmt.Println("Layers are rendered in Z-index order (higher numbers appear on top)")
	fmt.Println()
	fmt.Println(canvas.Render())
	fmt.Println()
	fmt.Println("Layer stacking order (bottom to top):")
	fmt.Println("1. Red Layer (Z-index: 1)")
	fmt.Println("2. Green Layer (Z-index: 2)")
	fmt.Println("3. Blue Layer (Z-index: 3)")
	fmt.Println("4. Yellow Layer (Z-index: 4)")
}

