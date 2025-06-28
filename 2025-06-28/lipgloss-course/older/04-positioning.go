package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Positioning Examples ===\n")

	// Helper function to create a styled box
	createBox := func(content, color string, width, height int) *lipgloss.Layer {
		style := lipgloss.NewStyle().
			Border(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color(color)).
			Foreground(lipgloss.Color(color)).
			Width(width).
			Height(height).
			Padding(1).
			Align(lipgloss.Center)
		
		return lipgloss.NewLayer(style.Render(content))
	}

	// Example 1: Absolute positioning
	fmt.Println("1. Absolute Positioning:")
	
	box1 := createBox("Top-Left", "99", 12, 3)
	box2 := createBox("Center", "205", 12, 3)
	box3 := createBox("Bottom-Right", "86", 12, 3)

	canvas1 := lipgloss.NewCanvas(
		box1.X(0).Y(0),      // Top-left corner
		box2.X(20).Y(5),     // Center area
		box3.X(40).Y(10),    // Bottom-right area
	)
	
	fmt.Println(canvas1.Render())
	fmt.Println()

	// Example 2: Grid-like positioning
	fmt.Println("2. Grid-like Positioning:")
	
	gridBoxes := make([]*lipgloss.Layer, 9)
	colors := []string{"99", "205", "86", "228", "201", "82", "63", "245", "99"}
	
	for i := 0; i < 9; i++ {
		content := fmt.Sprintf("Box %d", i+1)
		gridBoxes[i] = createBox(content, colors[i], 10, 2)
	}

	canvas2 := lipgloss.NewCanvas(
		// Row 1
		gridBoxes[0].X(0).Y(0),
		gridBoxes[1].X(15).Y(0),
		gridBoxes[2].X(30).Y(0),
		// Row 2
		gridBoxes[3].X(0).Y(4),
		gridBoxes[4].X(15).Y(4),
		gridBoxes[5].X(30).Y(4),
		// Row 3
		gridBoxes[6].X(0).Y(8),
		gridBoxes[7].X(15).Y(8),
		gridBoxes[8].X(30).Y(8),
	)
	
	fmt.Println(canvas2.Render())
	fmt.Println()

	// Example 3: Overlapping with Z-index control
	fmt.Println("3. Z-index Layering:")
	
	backBox := createBox("Background\nLayer", "245", 20, 5)
	middleBox := createBox("Middle\nLayer", "99", 18, 4)
	frontBox := createBox("Front\nLayer", "205", 16, 3)

	canvas3 := lipgloss.NewCanvas(
		backBox.X(0).Y(0).Z(1),     // Back layer
		middleBox.X(5).Y(2).Z(2),   // Middle layer
		frontBox.X(10).Y(4).Z(3),   // Front layer
	)
	
	fmt.Println(canvas3.Render())
	fmt.Println()

	// Example 4: Dynamic positioning based on content
	fmt.Println("4. Dynamic Content-Based Positioning:")
	
	titleBox := createBox("Dynamic Title", "228", 25, 2)
	
	// Create content boxes with varying sizes
	shortContent := createBox("Short", "99", 10, 2)
	mediumContent := createBox("Medium Content\nTwo Lines", "205", 15, 3)
	longContent := createBox("Very Long Content\nSpanning Multiple\nLines of Text", "86", 20, 4)

	canvas4 := lipgloss.NewCanvas(
		titleBox.X(0).Y(0),
		shortContent.X(0).Y(4),
		mediumContent.X(15).Y(4),
		longContent.X(35).Y(4),
	)
	
	fmt.Println(canvas4.Render())
	fmt.Println()

	// Example 5: Responsive-like positioning
	fmt.Println("5. Responsive-like Layout:")
	
	// Simulate different "screen sizes"
	createResponsiveLayout := func(width int) *lipgloss.Canvas {
		header := createBox("Header", "228", width-2, 2)
		sidebar := createBox("Sidebar\nMenu", "99", 12, 6)
		
		// Adjust main content width based on available space
		mainWidth := width - 16
		if mainWidth < 10 {
			mainWidth = 10
		}
		
		main := createBox(fmt.Sprintf("Main Content\nWidth: %d", mainWidth), "205", mainWidth, 6)
		footer := createBox("Footer", "245", width-2, 2)

		if width > 30 {
			// Wide layout: sidebar + main side by side
			return lipgloss.NewCanvas(
				header.X(0).Y(0),
				sidebar.X(0).Y(3),
				main.X(15).Y(3),
				footer.X(0).Y(11),
			)
		} else {
			// Narrow layout: stacked
			return lipgloss.NewCanvas(
				header.X(0).Y(0),
				main.X(0).Y(3),
				sidebar.X(0).Y(11),
				footer.X(0).Y(19),
			)
		}
	}

	fmt.Println("Wide layout (50 chars):")
	wideCanvas := createResponsiveLayout(50)
	fmt.Println(wideCanvas.Render())
	
	fmt.Println("\nNarrow layout (25 chars):")
	narrowCanvas := createResponsiveLayout(25)
	fmt.Println(narrowCanvas.Render())
	fmt.Println()

	// Example 6: Floating elements
	fmt.Println("6. Floating Elements:")
	
	backgroundBox := createBox("Main Content Area\nwith background content\nthat extends across\nthe full width", "245", 40, 8)
	
	floatingNotification := lipgloss.NewLayer(
		lipgloss.NewStyle().
			Background(lipgloss.Color("201")).
			Foreground(lipgloss.Color("255")).
			Bold(true).
			Padding(0, 1).
			Render("🔔 Notification"),
	)
	
	floatingButton := lipgloss.NewLayer(
		lipgloss.NewStyle().
			Background(lipgloss.Color("82")).
			Foreground(lipgloss.Color("235")).
			Bold(true).
			Padding(0, 2).
			Render("[ Action ]"),
	)

	canvas6 := lipgloss.NewCanvas(
		backgroundBox.X(0).Y(0).Z(1),
		floatingNotification.X(30).Y(1).Z(10),  // Top-right notification
		floatingButton.X(25).Y(7).Z(10),        // Bottom-right button
	)
	
	fmt.Println(canvas6.Render())
	fmt.Println()

	// Example 7: Animated-style positioning (simulating movement)
	fmt.Println("7. Simulated Animation Frames:")
	
	movingBox := createBox("Moving", "99", 8, 2)
	staticBox := createBox("Static Reference", "245", 15, 2)

	// Frame 1
	fmt.Println("Frame 1:")
	frame1 := lipgloss.NewCanvas(
		staticBox.X(0).Y(0),
		movingBox.X(0).Y(3),
	)
	fmt.Println(frame1.Render())

	// Frame 2
	fmt.Println("Frame 2:")
	frame2 := lipgloss.NewCanvas(
		staticBox.X(0).Y(0),
		movingBox.X(5).Y(3),
	)
	fmt.Println(frame2.Render())

	// Frame 3
	fmt.Println("Frame 3:")
	frame3 := lipgloss.NewCanvas(
		staticBox.X(0).Y(0),
		movingBox.X(10).Y(3),
	)
	fmt.Println(frame3.Render())
	fmt.Println()

	// Example 8: Complex positioning with nested elements
	fmt.Println("8. Complex Nested Positioning:")
	
	// Create a window-like structure
	windowStyle := lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("99")).
		Width(35).
		Height(12)

	titleBarStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("235")).
		Bold(true).
		Width(33).
		Padding(0, 1)

	contentAreaStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Width(30).
		Height(6).
		Padding(1)

	// Create nested canvas for window content
	titleBar := lipgloss.NewLayer(titleBarStyle.Render("Window Title"))
	contentArea := lipgloss.NewLayer(contentAreaStyle.Render("Window content\nwith multiple lines\nof information"))
	
	windowContent := lipgloss.NewCanvas(
		titleBar.X(1).Y(1),
		contentArea.X(2).Y(3),
	)

	windowLayer := lipgloss.NewLayer(windowStyle.Render(windowContent.Render()))

	// Position the window on a larger canvas
	canvas8 := lipgloss.NewCanvas(
		windowLayer.X(10).Y(2),
	)
	
	fmt.Println(canvas8.Render())
}

