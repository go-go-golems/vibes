package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Lipgloss v2 Border Style Gallery ===\n")

	// Base style for all border examples
	baseStyle := lipgloss.NewStyle().
		Width(20).
		Height(5).
		Align(lipgloss.Center).
		Padding(1)

	// Example 1: Normal borders
	fmt.Println("1. Normal Borders:")
	
	normalBorders := []struct {
		name   string
		border lipgloss.Border
		color  string
		x, y   int
	}{
		{"Normal", lipgloss.NormalBorder(), "99", 0, 0},
		{"Rounded", lipgloss.RoundedBorder(), "205", 25, 0},
		{"Thick", lipgloss.ThickBorder(), "82", 50, 0},
		{"Double", lipgloss.DoubleBorder(), "228", 0, 7},
	}

	var normalLayers []*lipgloss.Layer
	for _, border := range normalBorders {
		layer := lipgloss.NewLayer(
			baseStyle.
				Border(border.border).
				BorderForeground(lipgloss.Color(border.color)).
				Render(border.name + "\nBorder"),
		)
		normalLayers = append(normalLayers, layer.X(border.x).Y(border.y))
	}

	normalCanvas := lipgloss.NewCanvas(normalLayers...)
	fmt.Println(normalCanvas.Render())
	fmt.Println()

	// Example 2: Hidden and partial borders
	fmt.Println("2. Hidden and Partial Borders:")
	
	hiddenStyle := lipgloss.NewStyle().
		Width(18).
		Height(4).
		Align(lipgloss.Center).
		Padding(1)

	hiddenBorders := []struct {
		name   string
		border lipgloss.Border
		color  string
		x, y   int
	}{
		{"Hidden", lipgloss.HiddenBorder(), "245", 0, 0},
		{"Top Only", lipgloss.Border{Top: "─", TopLeft: "┌", TopRight: "┐"}, "99", 22, 0},
		{"Bottom Only", lipgloss.Border{Bottom: "─", BottomLeft: "└", BottomRight: "┘"}, "205", 44, 0},
		{"Sides Only", lipgloss.Border{Left: "│", Right: "│"}, "82", 0, 6},
	}

	var hiddenLayers []*lipgloss.Layer
	for _, border := range hiddenBorders {
		layer := lipgloss.NewLayer(
			hiddenStyle.
				Border(border.border).
				BorderForeground(lipgloss.Color(border.color)).
				Render(border.name),
		)
		hiddenLayers = append(hiddenLayers, layer.X(border.x).Y(border.y))
	}

	hiddenCanvas := lipgloss.NewCanvas(hiddenLayers...)
	fmt.Println(hiddenCanvas.Render())
	fmt.Println()

	// Example 3: Custom borders with different characters
	fmt.Println("3. Custom Border Characters:")
	
	customBorders := []struct {
		name   string
		border lipgloss.Border
		color  string
		x, y   int
	}{
		{"Stars", lipgloss.Border{
			Top: "*", Bottom: "*", Left: "*", Right: "*",
			TopLeft: "*", TopRight: "*", BottomLeft: "*", BottomRight: "*",
		}, "228", 0, 0},
		{"Equals", lipgloss.Border{
			Top: "=", Bottom: "=", Left: "‖", Right: "‖",
			TopLeft: "╔", TopRight: "╗", BottomLeft: "╚", BottomRight: "╝",
		}, "99", 25, 0},
		{"Dots", lipgloss.Border{
			Top: "·", Bottom: "·", Left: ":", Right: ":",
			TopLeft: "·", TopRight: "·", BottomLeft: "·", BottomRight: "·",
		}, "205", 50, 0},
		{"Mixed", lipgloss.Border{
			Top: "▀", Bottom: "▄", Left: "▌", Right: "▐",
			TopLeft: "▛", TopRight: "▜", BottomLeft: "▙", BottomRight: "▟",
		}, "82", 0, 7},
	}

	var customLayers []*lipgloss.Layer
	for _, border := range customBorders {
		layer := lipgloss.NewLayer(
			baseStyle.
				Border(border.border).
				BorderForeground(lipgloss.Color(border.color)).
				Render(border.name + "\nCustom"),
		)
		customLayers = append(customLayers, layer.X(border.x).Y(border.y))
	}

	customCanvas := lipgloss.NewCanvas(customLayers...)
	fmt.Println(customCanvas.Render())
	fmt.Println()

	// Example 4: Colored borders with backgrounds
	fmt.Println("4. Colored Borders with Backgrounds:")
	
	coloredStyle := lipgloss.NewStyle().
		Width(18).
		Height(4).
		Align(lipgloss.Center).
		Padding(1)

	coloredBorders := []struct {
		name       string
		border     lipgloss.Border
		borderColor string
		bgColor    string
		fgColor    string
		x, y       int
	}{
		{"Fire", lipgloss.ThickBorder(), "196", "52", "255", 0, 0},
		{"Ocean", lipgloss.RoundedBorder(), "51", "17", "255", 22, 0},
		{"Forest", lipgloss.DoubleBorder(), "46", "22", "255", 44, 0},
		{"Sunset", lipgloss.NormalBorder(), "208", "88", "255", 0, 6},
		{"Purple", lipgloss.ThickBorder(), "129", "54", "255", 22, 6},
		{"Gold", lipgloss.RoundedBorder(), "220", "94", "0", 44, 6},
	}

	var coloredLayers []*lipgloss.Layer
	for _, border := range coloredBorders {
		layer := lipgloss.NewLayer(
			coloredStyle.
				Border(border.border).
				BorderForeground(lipgloss.Color(border.borderColor)).
				Background(lipgloss.Color(border.bgColor)).
				Foreground(lipgloss.Color(border.fgColor)).
				Render(border.name),
		)
		coloredLayers = append(coloredLayers, layer.X(border.x).Y(border.y))
	}

	coloredCanvas := lipgloss.NewCanvas(coloredLayers...)
	fmt.Println(coloredCanvas.Render())
	fmt.Println()

	// Example 5: Nested borders
	fmt.Println("5. Nested Borders:")
	
	// Outer border
	outerStyle := lipgloss.NewStyle().
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(2)

	// Middle border
	middleStyle := lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(1)

	// Inner border
	innerStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("82")).
		Padding(1).
		Align(lipgloss.Center).
		Width(20).
		Height(3)

	innerContent := innerStyle.Render("Nested\nBorders")
	middleContent := middleStyle.Render(innerContent)
	outerContent := outerStyle.Render(middleContent)

	nestedLayer := lipgloss.NewLayer(outerContent)
	nestedCanvas := lipgloss.NewCanvas(nestedLayer.X(0).Y(0))
	fmt.Println(nestedCanvas.Render())
	fmt.Println()

	// Example 6: Border showcase in a complex layout
	fmt.Println("6. Border Showcase Layout:")
	
	// Title
	titleStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("255")).
		Bold(true).
		Width(70).
		Align(lipgloss.Center).
		Padding(0, 1)

	title := lipgloss.NewLayer(titleStyle.Render("Border Style Showcase"))

	// Create a grid of different border styles
	showcaseItems := []struct {
		name   string
		border lipgloss.Border
		color  string
		x, y   int
	}{
		{"Normal", lipgloss.NormalBorder(), "245", 2, 3},
		{"Rounded", lipgloss.RoundedBorder(), "99", 18, 3},
		{"Thick", lipgloss.ThickBorder(), "205", 34, 3},
		{"Double", lipgloss.DoubleBorder(), "82", 50, 3},
		{"Hidden", lipgloss.HiddenBorder(), "245", 2, 9},
		{"Custom", lipgloss.Border{
			Top: "▀", Bottom: "▄", Left: "▌", Right: "▐",
			TopLeft: "▛", TopRight: "▜", BottomLeft: "▙", BottomRight: "▟",
		}, "228", 18, 9},
	}

	showcaseStyle := lipgloss.NewStyle().
		Width(14).
		Height(4).
		Align(lipgloss.Center).
		Padding(1)

	var showcaseLayers []*lipgloss.Layer
	showcaseLayers = append(showcaseLayers, title.X(0).Y(0))

	for _, item := range showcaseItems {
		layer := lipgloss.NewLayer(
			showcaseStyle.
				Border(item.border).
				BorderForeground(lipgloss.Color(item.color)).
				Render(item.name),
		)
		showcaseLayers = append(showcaseLayers, layer.X(item.x).Y(item.y))
	}

	showcaseCanvas := lipgloss.NewCanvas(showcaseLayers...)
	
	// Add an outer border to the whole showcase
	finalStyle := lipgloss.NewStyle().
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	finalShowcase := lipgloss.NewLayer(finalStyle.Render(showcaseCanvas.Render()))
	finalCanvas := lipgloss.NewCanvas(finalShowcase.X(0).Y(0))
	
	fmt.Println(finalCanvas.Render())
	fmt.Println()

	fmt.Println("Border Gallery Complete!")
	fmt.Println("This demonstrates all border styles available in Lipgloss v2")
}

