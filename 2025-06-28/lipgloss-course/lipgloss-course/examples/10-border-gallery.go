package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create different border styles
	borders := []struct {
		name   string
		border lipgloss.Border
		color  string
	}{
		{"Normal", lipgloss.NormalBorder(), "#89b4fa"},
		{"Rounded", lipgloss.RoundedBorder(), "#a6e3a1"},
		{"Thick", lipgloss.ThickBorder(), "#f38ba8"},
		{"Double", lipgloss.DoubleBorder(), "#f9e2af"},
		{"ASCII", lipgloss.ASCIIBorder(), "#cba6f7"},
		{"Block", lipgloss.BlockBorder(), "#fab387"},
	}

	// Create title
	titleStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#cdd6f4")).
		Background(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Width(70).
		Align(lipgloss.Center)

	title := lipgloss.NewLayer(titleStyle.Render("Lipgloss v2 Border Gallery")).
		X(5).Y(1).Z(1).ID("title")

	// Create border examples
	var layers []*lipgloss.Layer
	layers = append(layers, title)

	for i, borderInfo := range borders {
		borderStyle := lipgloss.NewStyle().
			Border(borderInfo.border).
			BorderForeground(lipgloss.Color(borderInfo.color)).
			Background(lipgloss.Color("#313244")).
			Foreground(lipgloss.Color("#cdd6f4")).
			Padding(1, 2).
			Width(15).
			Height(4).
			Align(lipgloss.Center)

		content := fmt.Sprintf("%s\nBorder", borderInfo.name)
		
		x := 10 + (i%3)*20
		y := 5 + (i/3)*7

		layer := lipgloss.NewLayer(borderStyle.Render(content)).
			X(x).Y(y).Z(1).ID(fmt.Sprintf("border_%d", i))

		layers = append(layers, layer)
	}

	// Create background
	bgStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#1e1e2e")).
		Width(80).
		Height(20)

	background := lipgloss.NewLayer(bgStyle.Render("")).
		X(0).Y(0).Z(0).ID("background")

	layers = append([]*lipgloss.Layer{background}, layers...)

	// Create canvas
	canvas := lipgloss.NewCanvas(layers...)

	fmt.Println("Border Gallery Demo")
	fmt.Println("==================")
	fmt.Println()
	fmt.Println(canvas.Render())
}

