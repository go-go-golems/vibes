package main

import (
	"fmt"
	"math"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Dynamic Positioning Examples ===\n")

	// Example 1: Responsive layout adaptation
	fmt.Println("1. Responsive Layout Adaptation:")

	createResponsiveLayout := func(width, height int) *lipgloss.Canvas {
		// Header always spans full width
		headerStyle := lipgloss.NewStyle().
			Background(lipgloss.Color("99")).
			Foreground(lipgloss.Color("255")).
			Bold(true).
			Width(width).
			Padding(0, 1).
			Align(lipgloss.Center)

		header := lipgloss.NewLayer(headerStyle.Render("Responsive Header"))

		// Sidebar and main content adapt based on width
		if width >= 60 {
			// Wide layout: sidebar + main content side by side
			sidebarStyle := lipgloss.NewStyle().
				Width(15).
				Height(height-4).
				Border(lipgloss.NormalBorder()).
				BorderForeground(lipgloss.Color("205")).
				Padding(1)

			mainStyle := lipgloss.NewStyle().
				Width(width-20).
				Height(height-4).
				Border(lipgloss.NormalBorder()).
				BorderForeground(lipgloss.Color("82")).
				Padding(1)

			sidebar := lipgloss.NewLayer(sidebarStyle.Render("Sidebar\n• Menu 1\n• Menu 2\n• Menu 3"))
			main := lipgloss.NewLayer(mainStyle.Render("Main Content\nWide layout with\nsidebar and main\ncontent side by side"))

			return lipgloss.NewCanvas(
				header.X(0).Y(0),
				sidebar.X(0).Y(2),
				main.X(18).Y(2),
			)
		} else {
			// Narrow layout: stacked
			mainStyle := lipgloss.NewStyle().
				Width(width).
				Height(height-6).
				Border(lipgloss.NormalBorder()).
				BorderForeground(lipgloss.Color("82")).
				Padding(1)

			sidebarStyle := lipgloss.NewStyle().
				Width(width).
				Height(3).
				Border(lipgloss.NormalBorder()).
				BorderForeground(lipgloss.Color("205")).
				Padding(0, 1)

			main := lipgloss.NewLayer(mainStyle.Render("Main Content\nNarrow layout\nwith stacked\nelements"))
			sidebar := lipgloss.NewLayer(sidebarStyle.Render("Menu: Home | About | Contact"))

			return lipgloss.NewCanvas(
				header.X(0).Y(0),
				main.X(0).Y(2),
				sidebar.X(0).Y(height-3),
			)
		}
	}

	fmt.Println("Wide layout (70x12):")
	wideLayout := createResponsiveLayout(70, 12)
	fmt.Println(wideLayout.Render())

	fmt.Println("\nNarrow layout (30x12):")
	narrowLayout := createResponsiveLayout(30, 12)
	fmt.Println(narrowLayout.Render())
	fmt.Println()

	// Example 2: Animated positioning simulation
	fmt.Println("2. Animated Positioning Simulation:")

	boxStyle := lipgloss.NewStyle().
		Width(8).
		Height(3).
		Background(lipgloss.Color("205")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(0, 1).
		Bold(true).
		Align(lipgloss.Center)

	movingBox := lipgloss.NewLayer(boxStyle.Render("Box"))

	// Simulate movement along a path
	positions := []struct {
		x, y int
		desc string
	}{
		{0, 0, "Start position"},
		{10, 0, "Move right"},
		{10, 5, "Move down"},
		{20, 5, "Move right again"},
		{20, 0, "Move up"},
		{30, 0, "Final position"},
	}

	for i, pos := range positions {
		fmt.Printf("Frame %d - %s:\n", i+1, pos.desc)
		canvas := lipgloss.NewCanvas(
			movingBox.X(pos.x).Y(pos.y),
		)
		fmt.Println(canvas.Render())
		fmt.Println()
	}

	// Example 3: Dynamic overlay positioning based on content
	fmt.Println("3. Dynamic Overlay Positioning Based on Content:")

	baseStyle := lipgloss.NewStyle().
		Width(50).
		Height(10).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	base := lipgloss.NewLayer(
		baseStyle.Render("Base Content Area\n\nThis area contains the main\napplication content that\nremains static while overlays\nare positioned dynamically."),
	)

	// Function to create overlay at optimal position
	createOverlay := func(content string, preferredX, preferredY int) (*lipgloss.Layer, int, int) {
		overlayStyle := lipgloss.NewStyle().
			Width(20).
			Height(4).
			Background(lipgloss.Color("99")).
			Foreground(lipgloss.Color("255")).
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("99")).
			Padding(0, 1)

		overlay := lipgloss.NewLayer(overlayStyle.Render(content))

		// Adjust position to keep overlay within bounds
		maxX, maxY := 30, 6 // Available space considering overlay size
		x := preferredX
		y := preferredY

		if x > maxX {
			x = maxX
		}
		if y > maxY {
			y = maxY
		}

		return overlay, x, y
	}

	overlayContents := []struct {
		content     string
		preferredX  int
		preferredY  int
	}{
		{"Tooltip\nfor button", 40, 1},  // Would go outside, gets adjusted
		{"Context menu\nwith options", 5, 8},   // Would go outside, gets adjusted
		{"Status popup\nshowing info", 15, 3},  // Fits perfectly
	}

	for i, overlay := range overlayContents {
		overlayLayer, x, y := createOverlay(overlay.content, overlay.preferredX, overlay.preferredY)
		
		fmt.Printf("Overlay %d - Preferred: (%d,%d), Actual: (%d,%d)\n", 
			i+1, overlay.preferredX, overlay.preferredY, x, y)
		
		canvas := lipgloss.NewCanvas(
			base.X(0).Y(0).Z(1),
			overlayLayer.X(x).Y(y).Z(2),
		)
		fmt.Println(canvas.Render())
		fmt.Println()
	}

	// Example 4: Circular positioning
	fmt.Println("4. Circular Positioning:")

	centerX, centerY := 15, 5
	radius := 8.0

	// Create elements positioned in a circle
	elementStyle := lipgloss.NewStyle().
		Width(6).
		Height(2).
		Background(lipgloss.Color("82")).
		Foreground(lipgloss.Color("235")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("82")).
		Padding(0, 1).
		Bold(true).
		Align(lipgloss.Center)

	var circleElements []*lipgloss.Layer
	numElements := 6

	for i := 0; i < numElements; i++ {
		angle := float64(i) * 2 * math.Pi / float64(numElements)
		x := int(float64(centerX) + radius*math.Cos(angle))
		y := int(float64(centerY) + radius*math.Sin(angle)/2) // Adjust for character aspect ratio

		element := lipgloss.NewLayer(elementStyle.Render(fmt.Sprintf("E%d", i+1)))
		circleElements = append(circleElements, element.X(x).Y(y))
	}

	// Center element
	centerStyle := lipgloss.NewStyle().
		Width(8).
		Height(3).
		Background(lipgloss.Color("205")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(0, 1).
		Bold(true).
		Align(lipgloss.Center)

	centerElement := lipgloss.NewLayer(centerStyle.Render("Center"))
	circleElements = append(circleElements, centerElement.X(centerX-4).Y(centerY-1))

	circleCanvas := lipgloss.NewCanvas(circleElements...)
	fmt.Println(circleCanvas.Render())
	fmt.Println()

	// Example 5: Grid-based dynamic positioning
	fmt.Println("5. Grid-based Dynamic Positioning:")

	// Create a grid system
	cellWidth, cellHeight := 8, 3

	createGridElement := func(content, color string) *lipgloss.Layer {
		style := lipgloss.NewStyle().
			Width(cellWidth).
			Height(cellHeight).
			Background(lipgloss.Color(color)).
			Foreground(lipgloss.Color("255")).
			Border(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color(color)).
			Padding(0, 1).
			Bold(true).
			Align(lipgloss.Center)

		return lipgloss.NewLayer(style.Render(content))
	}

	// Place elements in grid positions
	gridElements := []struct {
		content string
		color   string
		gridX   int
		gridY   int
	}{
		{"A1", "99", 0, 0},
		{"B2", "205", 1, 1},
		{"C3", "82", 2, 2},
		{"D1", "228", 3, 0},
		{"E4", "201", 4, 3},
	}

	var gridLayers []*lipgloss.Layer
	for _, elem := range gridElements {
		element := createGridElement(elem.content, elem.color)
		x := elem.gridX * (cellWidth + 1)
		y := elem.gridY * (cellHeight + 1)
		gridLayers = append(gridLayers, element.X(x).Y(y))
	}

	gridCanvas := lipgloss.NewCanvas(gridLayers...)
	fmt.Println(gridCanvas.Render())
	fmt.Println()

	// Example 6: Magnetic positioning (snap to grid)
	fmt.Println("6. Magnetic Positioning (Snap to Grid):")

	snapToGrid := func(x, y, gridSize int) (int, int) {
		snappedX := (x + gridSize/2) / gridSize * gridSize
		snappedY := (y + gridSize/2) / gridSize * gridSize
		return snappedX, snappedY
	}

	magneticStyle := lipgloss.NewStyle().
		Width(6).
		Height(2).
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(0, 1).
		Bold(true).
		Align(lipgloss.Center)

	// Original positions and their snapped equivalents
	magneticPositions := []struct {
		originalX, originalY int
		content              string
	}{
		{7, 3, "A"},
		{15, 7, "B"},
		{23, 2, "C"},
		{31, 9, "D"},
	}

	gridSize := 8

	fmt.Println("Original positions vs Snapped positions:")
	for i, pos := range magneticPositions {
		snappedX, snappedY := snapToGrid(pos.originalX, pos.originalY, gridSize)
		
		element := lipgloss.NewLayer(magneticStyle.Render(pos.content))
		
		fmt.Printf("Element %s: (%d,%d) -> (%d,%d)\n", 
			pos.content, pos.originalX, pos.originalY, snappedX, snappedY)
		
		// Show both original and snapped positions
		originalElement := lipgloss.NewLayer(
			lipgloss.NewStyle().
				Width(1).
				Height(1).
				Background(lipgloss.Color("245")).
				Render("·"),
		)
		
		canvas := lipgloss.NewCanvas(
			originalElement.X(pos.originalX).Y(pos.originalY).Z(1),
			element.X(snappedX).Y(snappedY).Z(2),
		)
		
		if i == 0 {
			fmt.Println(canvas.Render())
		}
	}

	// Show all snapped elements together
	fmt.Println("\nAll elements snapped to grid:")
	var snappedLayers []*lipgloss.Layer
	for _, pos := range magneticPositions {
		snappedX, snappedY := snapToGrid(pos.originalX, pos.originalY, gridSize)
		element := lipgloss.NewLayer(magneticStyle.Render(pos.content))
		snappedLayers = append(snappedLayers, element.X(snappedX).Y(snappedY))
	}

	finalCanvas := lipgloss.NewCanvas(snappedLayers...)
	fmt.Println(finalCanvas.Render())
}

