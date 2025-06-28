package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a color palette showcase
	colors := []struct {
		name  string
		color string
	}{
		{"Red", "#f38ba8"},
		{"Blue", "#89b4fa"},
		{"Green", "#a6e3a1"},
		{"Yellow", "#f9e2af"},
		{"Purple", "#cba6f7"},
		{"Orange", "#fab387"},
		{"Pink", "#f5c2e7"},
		{"Cyan", "#94e2d5"},
	}

	// Create title
	titleStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#cdd6f4")).
		Background(lipgloss.Color("#1e1e2e")).
		Padding(1, 2).
		Width(60).
		Align(lipgloss.Center)

	title := lipgloss.NewLayer(titleStyle.Render("Lipgloss v2 Color Showcase")).
		X(10).Y(1).Z(1).ID("title")

	// Create color swatches
	var layers []*lipgloss.Layer
	layers = append(layers, title)

	for i, colorInfo := range colors {
		swatchStyle := lipgloss.NewStyle().
			Background(lipgloss.Color(colorInfo.color)).
			Foreground(lipgloss.Color("#1e1e2e")).
			Padding(1, 2).
			Bold(true).
			Width(12)

		labelStyle := lipgloss.NewStyle().
			Foreground(lipgloss.Color("#cdd6f4")).
			Padding(0, 1)

		swatch := lipgloss.JoinHorizontal(lipgloss.Center,
			swatchStyle.Render("████"),
			labelStyle.Render(colorInfo.name),
		)

		x := 15 + (i%4)*15
		y := 4 + (i/4)*3

		layer := lipgloss.NewLayer(swatch).
			X(x).Y(y).Z(1).ID(fmt.Sprintf("swatch_%d", i))

		layers = append(layers, layer)
	}

	// Create background
	bgStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#1e1e2e")).
		Width(80).
		Height(15)

	background := lipgloss.NewLayer(bgStyle.Render("")).
		X(0).Y(0).Z(0).ID("background")

	layers = append([]*lipgloss.Layer{background}, layers...)

	// Create canvas
	canvas := lipgloss.NewCanvas(layers...)

	fmt.Println("Color Showcase Demo")
	fmt.Println("==================")
	fmt.Println()
	fmt.Println(canvas.Render())
}

