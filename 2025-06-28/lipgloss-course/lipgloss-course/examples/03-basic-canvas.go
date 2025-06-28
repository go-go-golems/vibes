package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a background layer
	backgroundStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#1e1e2e")).
		Foreground(lipgloss.Color("#cdd6f4")).
		Width(50).
		Height(15)

	background := lipgloss.NewLayer(backgroundStyle.Render("")).
		X(0).Y(0).ID("background")

	// Create a title
	titleStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#89b4fa")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Bold(true)

	title := lipgloss.NewLayer(titleStyle.Render("Canvas Demo")).
		X(15).Y(2).ID("title")

	// Create some content boxes
	contentStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#f38ba8")).
		Padding(1, 2).
		Background(lipgloss.Color("#313244")).
		Foreground(lipgloss.Color("#cdd6f4"))

	content1 := lipgloss.NewLayer(contentStyle.Render("Box 1\nContent")).
		X(5).Y(6).ID("content1")

	content2 := lipgloss.NewLayer(contentStyle.Render("Box 2\nMore content")).
		X(25).Y(8).ID("content2")

	// Create canvas
	canvas := lipgloss.NewCanvas(background, title, content1, content2)

	fmt.Println("Basic Canvas Example")
	fmt.Println("===================")
	fmt.Println()
	fmt.Println(canvas.Render())
}

