package main

import (
	"fmt"
	"time"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a bouncing ball animation
	ballStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#f38ba8")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(0, 1).
		Bold(true)

	// Create boundary
	boundaryStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#89b4fa")).
		Width(50).
		Height(15)

	boundary := lipgloss.NewLayer(boundaryStyle.Render("")).
		X(10).Y(2).Z(0).ID("boundary")

	// Simulate animation frames
	positions := []struct{ x, y int }{
		{15, 5},  // Frame 1
		{25, 8},  // Frame 2
		{35, 11}, // Frame 3
		{45, 14}, // Frame 4
		{40, 10}, // Frame 5
		{30, 7},  // Frame 6
		{20, 4},  // Frame 7
	}

	fmt.Println("Dynamic Positioning Demo")
	fmt.Println("========================")
	fmt.Println("Simulating animated movement (showing multiple frames)")
	fmt.Println()

	for i, pos := range positions {
		// Create ball at current position
		ball := lipgloss.NewLayer(ballStyle.Render("●")).
			X(pos.x).Y(pos.y).Z(1).ID("ball")

		// Create canvas with boundary and ball
		canvas := lipgloss.NewCanvas(boundary, ball)

		fmt.Printf("Frame %d:\n", i+1)
		fmt.Println(canvas.Render())
		fmt.Println()

		// Simulate animation delay
		time.Sleep(500 * time.Millisecond)
	}

	fmt.Println("Animation complete! In a real application, you would:")
	fmt.Println("- Update layer positions in a loop")
	fmt.Println("- Clear and re-render the canvas")
	fmt.Println("- Handle user input for interactive movement")
}

