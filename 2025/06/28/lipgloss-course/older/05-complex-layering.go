package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Complex Layering Examples ===\n")

	// Example 1: Multi-level overlay system
	fmt.Println("1. Multi-level Overlay System:")
	
	// Background layer
	backgroundStyle := lipgloss.NewStyle().
		Width(50).
		Height(15).
		Background(lipgloss.Color("235")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	background := lipgloss.NewLayer(
		backgroundStyle.Render("Background Content\n\nThis is the main application\nbackground with some content\nthat spans multiple lines\nand provides context for\nthe overlays above."),
	)

	// Modal overlay
	modalStyle := lipgloss.NewStyle().
		Width(30).
		Height(8).
		Background(lipgloss.Color("235")).
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1, 2)

	modal := lipgloss.NewLayer(
		modalStyle.Render("Modal Dialog\n\nThis is a modal overlay\nthat appears on top of\nthe background content."),
	)

	// Notification overlay
	notificationStyle := lipgloss.NewStyle().
		Width(25).
		Height(3).
		Background(lipgloss.Color("201")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("201")).
		Padding(0, 1).
		Bold(true)

	notification := lipgloss.NewLayer(
		notificationStyle.Render("🔔 New Message\nYou have 3 unread items"),
	)

	// Tooltip overlay
	tooltipStyle := lipgloss.NewStyle().
		Width(20).
		Height(2).
		Background(lipgloss.Color("228")).
		Foreground(lipgloss.Color("235")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("228")).
		Padding(0, 1)

	tooltip := lipgloss.NewLayer(
		tooltipStyle.Render("Tooltip: Click to\nperform action"),
	)

	canvas1 := lipgloss.NewCanvas(
		background.X(0).Y(0).Z(1),      // Background layer
		modal.X(10).Y(4).Z(5),          // Modal in middle
		notification.X(25).Y(1).Z(10),  // Notification on top
		tooltip.X(5).Y(12).Z(8),        // Tooltip above modal
	)

	fmt.Println(canvas1.Render())
	fmt.Println()

	// Example 2: Cascading windows effect
	fmt.Println("2. Cascading Windows Effect:")

	windowStyle := lipgloss.NewStyle().
		Width(20).
		Height(8).
		Border(lipgloss.DoubleBorder()).
		Padding(1)

	createWindow := func(title, content, color string) *lipgloss.Layer {
		titleBar := lipgloss.NewStyle().
			Background(lipgloss.Color(color)).
			Foreground(lipgloss.Color("255")).
			Bold(true).
			Width(18).
			Align(lipgloss.Center).
			Render(title)

		windowContent := lipgloss.NewStyle().
			Width(18).
			Height(4).
			Render(content)

		fullWindow := titleBar + "\n" + windowContent

		return lipgloss.NewLayer(
			windowStyle.
				BorderForeground(lipgloss.Color(color)).
				Render(fullWindow),
		)
	}

	window1 := createWindow("Window 1", "First window\ncontent with\nsome text", "99")
	window2 := createWindow("Window 2", "Second window\nwith different\ncontent", "205")
	window3 := createWindow("Window 3", "Third window\nstacked on top", "86")

	canvas2 := lipgloss.NewCanvas(
		window1.X(0).Y(0).Z(1),
		window2.X(5).Y(2).Z(2),
		window3.X(10).Y(4).Z(3),
	)

	fmt.Println(canvas2.Render())
	fmt.Println()

	// Example 3: Complex dashboard with multiple overlays
	fmt.Println("3. Complex Dashboard with Multiple Overlays:")

	// Main dashboard
	dashboardStyle := lipgloss.NewStyle().
		Width(60).
		Height(20).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	dashboard := lipgloss.NewLayer(
		dashboardStyle.Render("Main Dashboard\n\nMetrics and data visualization\nwould be displayed here with\ncharts, graphs, and real-time\ninformation updates.\n\nThis content serves as the\nbase layer for all overlays."),
	)

	// Status panel overlay
	statusStyle := lipgloss.NewStyle().
		Width(25).
		Height(6).
		Background(lipgloss.Color("235")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("82")).
		Padding(1)

	statusPanel := lipgloss.NewLayer(
		statusStyle.Render("System Status\n\n✅ All systems operational\n🔄 Updates available\n📊 Performance: Good"),
	)

	// Alert overlay
	alertStyle := lipgloss.NewStyle().
		Width(30).
		Height(4).
		Background(lipgloss.Color("196")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("196")).
		Padding(0, 1).
		Bold(true)

	alert := lipgloss.NewLayer(
		alertStyle.Render("⚠️  CRITICAL ALERT\nDatabase connection lost\nImmediate attention required"),
	)

	// Menu overlay
	menuStyle := lipgloss.NewStyle().
		Width(15).
		Height(10).
		Background(lipgloss.Color("235")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1)

	menu := lipgloss.NewLayer(
		menuStyle.Render("Quick Menu\n\n• Dashboard\n• Reports\n• Settings\n• Help\n• Logout"),
	)

	canvas3 := lipgloss.NewCanvas(
		dashboard.X(0).Y(0).Z(1),       // Base dashboard
		statusPanel.X(35).Y(2).Z(3),    // Status panel
		menu.X(2).Y(8).Z(4),            // Menu
		alert.X(15).Y(12).Z(10),        // Critical alert on top
	)

	fmt.Println(canvas3.Render())
	fmt.Println()

	// Example 4: Layered content with transparency effects
	fmt.Println("4. Layered Content with Visual Depth:")

	// Create layers with different visual weights
	layer1Style := lipgloss.NewStyle().
		Width(40).
		Height(8).
		Background(lipgloss.Color("235")).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	layer2Style := lipgloss.NewStyle().
		Width(30).
		Height(6).
		Background(lipgloss.Color("237")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1)

	layer3Style := lipgloss.NewStyle().
		Width(20).
		Height(4).
		Background(lipgloss.Color("239")).
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(1)

	layer1 := lipgloss.NewLayer(
		layer1Style.Render("Bottom Layer (Z=1)\nThis layer provides the\nfoundation for the\ncomposition above."),
	)

	layer2 := lipgloss.NewLayer(
		layer2Style.Render("Middle Layer (Z=2)\nIntermediate content\nthat bridges layers."),
	)

	layer3 := lipgloss.NewLayer(
		layer3Style.Render("Top Layer (Z=3)\nFocused content\non top."),
	)

	canvas4 := lipgloss.NewCanvas(
		layer1.X(0).Y(0).Z(1),
		layer2.X(8).Y(2).Z(2),
		layer3.X(16).Y(4).Z(3),
	)

	fmt.Println(canvas4.Render())
	fmt.Println()

	// Example 5: Dynamic overlay positioning
	fmt.Println("5. Dynamic Overlay Positioning:")

	baseStyle := lipgloss.NewStyle().
		Width(50).
		Height(12).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	base := lipgloss.NewLayer(
		baseStyle.Render("Base Application Window\n\nThis represents the main\napplication interface that\nremains static while overlays\nare positioned dynamically."),
	)

	overlayStyle := lipgloss.NewStyle().
		Width(20).
		Height(4).
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("255")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(0, 1).
		Bold(true)

	// Show overlay in different positions
	positions := []struct {
		name string
		x, y int
	}{
		{"Top-Left", 2, 1},
		{"Top-Right", 28, 1},
		{"Center", 15, 4},
		{"Bottom-Left", 2, 7},
		{"Bottom-Right", 28, 7},
	}

	for i, pos := range positions {
		overlay := lipgloss.NewLayer(
			overlayStyle.Render(fmt.Sprintf("Overlay %d\n%s", i+1, pos.name)),
		)

		canvas := lipgloss.NewCanvas(
			base.X(0).Y(0).Z(1),
			overlay.X(pos.x).Y(pos.y).Z(2),
		)

		fmt.Printf("Position %d - %s:\n", i+1, pos.name)
		fmt.Println(canvas.Render())
		fmt.Println()
	}

	// Example 6: Nested overlay hierarchies
	fmt.Println("6. Nested Overlay Hierarchies:")

	// Parent container
	containerStyle := lipgloss.NewStyle().
		Width(45).
		Height(15).
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1)

	// Child overlays within the container
	childStyle := lipgloss.NewStyle().
		Width(15).
		Height(4).
		Border(lipgloss.NormalBorder()).
		Padding(0, 1)

	child1 := lipgloss.NewLayer(
		childStyle.
			BorderForeground(lipgloss.Color("205")).
			Render("Child 1\nNested overlay\nwithin parent"),
	)

	child2 := lipgloss.NewLayer(
		childStyle.
			BorderForeground(lipgloss.Color("86")).
			Render("Child 2\nAnother nested\noverlay"),
	)

	child3 := lipgloss.NewLayer(
		childStyle.
			BorderForeground(lipgloss.Color("228")).
			Render("Child 3\nThird nested\noverlay"),
	)

	// Create nested canvas
	nestedCanvas := lipgloss.NewCanvas(
		child1.X(2).Y(2).Z(1),
		child2.X(20).Y(2).Z(2),
		child3.X(11).Y(7).Z(3),
	)

	container := lipgloss.NewLayer(
		containerStyle.Render(nestedCanvas.Render()),
	)

	canvas6 := lipgloss.NewCanvas(
		container.X(0).Y(0).Z(1),
	)

	fmt.Println(canvas6.Render())
}

