package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Z-index Demonstration ===\n")

	// Helper function to create colored boxes
	createBox := func(content, color string, width, height int) *lipgloss.Layer {
		style := lipgloss.NewStyle().
			Width(width).
			Height(height).
			Background(lipgloss.Color(color)).
			Foreground(lipgloss.Color("255")).
			Border(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color(color)).
			Padding(1).
			Bold(true).
			Align(lipgloss.Center)

		return lipgloss.NewLayer(style.Render(content))
	}

	// Example 1: Basic Z-index ordering
	fmt.Println("1. Basic Z-index Ordering:")

	redBox := createBox("Red\nZ=1", "196", 15, 4)
	greenBox := createBox("Green\nZ=2", "82", 15, 4)
	blueBox := createBox("Blue\nZ=3", "99", 15, 4)

	fmt.Println("All boxes at same position with different Z-index values:")
	canvas1 := lipgloss.NewCanvas(
		redBox.X(0).Y(0).Z(1),    // Bottom
		greenBox.X(0).Y(0).Z(2),  // Middle
		blueBox.X(0).Y(0).Z(3),   // Top
	)
	fmt.Println(canvas1.Render())
	fmt.Println()

	// Example 2: Z-index with partial overlap
	fmt.Println("2. Z-index with Partial Overlap:")

	canvas2 := lipgloss.NewCanvas(
		redBox.X(0).Y(0).Z(1),
		greenBox.X(5).Y(1).Z(2),
		blueBox.X(10).Y(2).Z(3),
	)
	fmt.Println(canvas2.Render())
	fmt.Println()

	// Example 3: Negative Z-index values
	fmt.Println("3. Negative Z-index Values:")

	backgroundBox := createBox("Background\nZ=-1", "235", 25, 6)
	foregroundBox := createBox("Foreground\nZ=1", "205", 15, 4)

	canvas3 := lipgloss.NewCanvas(
		foregroundBox.X(5).Y(1).Z(1),     // Positive Z
		backgroundBox.X(0).Y(0).Z(-1),    // Negative Z (behind)
	)
	fmt.Println(canvas3.Render())
	fmt.Println()

	// Example 4: Complex Z-index hierarchy
	fmt.Println("4. Complex Z-index Hierarchy:")

	// Create multiple layers with various Z-index values
	layers := []struct {
		content string
		color   string
		x, y, z int
		w, h    int
	}{
		{"Base\nZ=0", "245", 0, 0, 0, 30, 8},
		{"Layer 1\nZ=2", "99", 5, 1, 2, 12, 4},
		{"Layer 2\nZ=1", "205", 10, 2, 1, 12, 4},
		{"Layer 3\nZ=5", "82", 15, 3, 5, 12, 4},
		{"Layer 4\nZ=3", "228", 8, 4, 3, 12, 4},
		{"Top\nZ=10", "201", 12, 0, 10, 10, 3},
	}

	var canvasLayers []*lipgloss.Layer
	for _, layer := range layers {
		box := createBox(layer.content, layer.color, layer.w, layer.h)
		canvasLayers = append(canvasLayers, box.X(layer.x).Y(layer.y).Z(layer.z))
	}

	canvas4 := lipgloss.NewCanvas(canvasLayers...)
	fmt.Println(canvas4.Render())
	fmt.Println()

	// Example 5: Dynamic Z-index changes
	fmt.Println("5. Dynamic Z-index Changes:")

	windowA := createBox("Window A", "99", 18, 5)
	windowB := createBox("Window B", "205", 18, 5)
	windowC := createBox("Window C", "86", 18, 5)

	fmt.Println("Initial state (A=1, B=2, C=3):")
	canvas5a := lipgloss.NewCanvas(
		windowA.X(0).Y(0).Z(1),
		windowB.X(6).Y(2).Z(2),
		windowC.X(12).Y(4).Z(3),
	)
	fmt.Println(canvas5a.Render())

	fmt.Println("\nAfter bringing Window A to front (A=10, B=2, C=3):")
	canvas5b := lipgloss.NewCanvas(
		windowA.X(0).Y(0).Z(10),  // Brought to front
		windowB.X(6).Y(2).Z(2),
		windowC.X(12).Y(4).Z(3),
	)
	fmt.Println(canvas5b.Render())

	fmt.Println("\nAfter sending Window C to back (A=10, B=2, C=-1):")
	canvas5c := lipgloss.NewCanvas(
		windowA.X(0).Y(0).Z(10),
		windowB.X(6).Y(2).Z(2),
		windowC.X(12).Y(4).Z(-1),  // Sent to back
	)
	fmt.Println(canvas5c.Render())
	fmt.Println()

	// Example 6: Z-index with different content types
	fmt.Println("6. Z-index with Different Content Types:")

	// Text content
	textStyle := lipgloss.NewStyle().
		Width(40).
		Height(8).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	textLayer := lipgloss.NewLayer(
		textStyle.Render("Text Content Layer\n\nThis layer contains regular\ntext content that serves as\nthe base for other overlays.\n\nZ-index: 1"),
	)

	// Image placeholder
	imageStyle := lipgloss.NewStyle().
		Width(20).
		Height(6).
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1).
		Align(lipgloss.Center)

	imageLayer := lipgloss.NewLayer(
		imageStyle.Render("📷 Image\nPlaceholder\n\nZ-index: 3"),
	)

	// Button overlay
	buttonStyle := lipgloss.NewStyle().
		Width(12).
		Height(3).
		Background(lipgloss.Color("82")).
		Foreground(lipgloss.Color("235")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("82")).
		Padding(0, 1).
		Bold(true).
		Align(lipgloss.Center)

	buttonLayer := lipgloss.NewLayer(
		buttonStyle.Render("[ Click Me ]\nZ-index: 5"),
	)

	canvas6 := lipgloss.NewCanvas(
		textLayer.X(0).Y(0).Z(1),
		imageLayer.X(15).Y(1).Z(3),
		buttonLayer.X(20).Y(6).Z(5),
	)
	fmt.Println(canvas6.Render())
	fmt.Println()

	// Example 7: Z-index collision handling
	fmt.Println("7. Z-index Collision Handling:")

	// Multiple elements with same Z-index
	box1 := createBox("Box 1\nZ=5", "196", 12, 3)
	box2 := createBox("Box 2\nZ=5", "82", 12, 3)
	box3 := createBox("Box 3\nZ=5", "99", 12, 3)

	fmt.Println("Multiple elements with same Z-index (order matters):")
	canvas7 := lipgloss.NewCanvas(
		box1.X(0).Y(0).Z(5),   // Added first
		box2.X(4).Y(1).Z(5),   // Added second (appears on top of box1)
		box3.X(8).Y(2).Z(5),   // Added third (appears on top of both)
	)
	fmt.Println(canvas7.Render())
	fmt.Println()

	// Example 8: Practical Z-index use case - Modal system
	fmt.Println("8. Practical Use Case - Modal System:")

	// Application background
	appStyle := lipgloss.NewStyle().
		Width(50).
		Height(12).
		Background(lipgloss.Color("235")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	app := lipgloss.NewLayer(
		appStyle.Render("Main Application\n\nThis is the main application\ninterface with various\ncontrols and content.\n\nUser can interact with\nthis interface normally."),
	)

	// Modal backdrop
	backdropStyle := lipgloss.NewStyle().
		Width(50).
		Height(12).
		Background(lipgloss.Color("237")).
		Foreground(lipgloss.Color("245"))

	backdrop := lipgloss.NewLayer(
		backdropStyle.Render(""),
	)

	// Modal dialog
	modalDialogStyle := lipgloss.NewStyle().
		Width(30).
		Height(8).
		Background(lipgloss.Color("255")).
		Foreground(lipgloss.Color("235")).
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1, 2)

	modalDialog := lipgloss.NewLayer(
		modalDialogStyle.Render("Confirmation Dialog\n\nAre you sure you want to\ndelete this item?\n\n[Yes]    [Cancel]"),
	)

	canvas8 := lipgloss.NewCanvas(
		app.X(0).Y(0).Z(1),           // Application base
		backdrop.X(0).Y(0).Z(100),    // Modal backdrop
		modalDialog.X(10).Y(2).Z(101), // Modal dialog on top
	)

	fmt.Println(canvas8.Render())
}

