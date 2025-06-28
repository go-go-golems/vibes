package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Test basic lipgloss v2 functionality
	style := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("205")).
		Background(lipgloss.Color("235")).
		Padding(1, 2).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("63"))

	fmt.Println(style.Render("🎉 Lipgloss v2 is working!"))
	
	// Test basic canvas functionality
	box := lipgloss.NewStyle().
		Width(20).
		Height(3).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("99"))

	layer1 := lipgloss.NewLayer(box.Render("Layer 1"))
	layer2 := lipgloss.NewLayer(box.Render("Layer 2"))

	canvas := lipgloss.NewCanvas(
		layer1.X(0).Y(0),
		layer2.X(10).Y(2),
	)

	fmt.Println("\n🎨 Canvas with overlapping layers:")
	fmt.Println(canvas.Render())
}

