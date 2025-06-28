package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Basic Canvas Examples ===\n")

	// Example 1: Simple canvas with two layers
	boxStyle := lipgloss.NewStyle().
		Width(15).
		Height(3).
		Border(lipgloss.NormalBorder()).
		Padding(1).
		Align(lipgloss.Center)

	layer1 := lipgloss.NewLayer(
		boxStyle.
			BorderForeground(lipgloss.Color("99")).
			Foreground(lipgloss.Color("99")).
			Render("Box 1"),
	)

	layer2 := lipgloss.NewLayer(
		boxStyle.
			BorderForeground(lipgloss.Color("205")).
			Foreground(lipgloss.Color("205")).
			Render("Box 2"),
	)

	fmt.Println("1. Simple Canvas Composition:")
	canvas1 := lipgloss.NewCanvas(
		layer1.X(0).Y(0),
		layer2.X(20).Y(0),
	)
	fmt.Println(canvas1.Render())
	fmt.Println()

	// Example 2: Overlapping layers
	fmt.Println("2. Overlapping Layers:")
	canvas2 := lipgloss.NewCanvas(
		layer1.X(0).Y(0),
		layer2.X(10).Y(2),
	)
	fmt.Println(canvas2.Render())
	fmt.Println()

	// Example 3: Multiple layers with different Z-indexes
	layer3 := lipgloss.NewLayer(
		boxStyle.
			BorderForeground(lipgloss.Color("86")).
			Foreground(lipgloss.Color("86")).
			Render("Box 3"),
	)

	fmt.Println("3. Z-index Layering:")
	canvas3 := lipgloss.NewCanvas(
		layer1.X(0).Y(0).Z(1),    // Bottom layer
		layer2.X(8).Y(1).Z(3),    // Top layer
		layer3.X(4).Y(2).Z(2),    // Middle layer
	)
	fmt.Println(canvas3.Render())
	fmt.Println()

	// Example 4: Canvas with different content types
	titleStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("228")).
		Background(lipgloss.Color("235")).
		Padding(0, 2)

	contentStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1, 2).
		Width(30)

	statusStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("82")).
		Background(lipgloss.Color("235")).
		Padding(0, 1)

	titleLayer := lipgloss.NewLayer(titleStyle.Render("Dashboard"))
	contentLayer := lipgloss.NewLayer(contentStyle.Render("Main content area\nwith multiple lines\nof information"))
	statusLayer := lipgloss.NewLayer(statusStyle.Render("Status: Online"))

	fmt.Println("4. Mixed Content Canvas:")
	canvas4 := lipgloss.NewCanvas(
		titleLayer.X(0).Y(0),
		contentLayer.X(0).Y(2),
		statusLayer.X(35).Y(0),
	)
	fmt.Println(canvas4.Render())
	fmt.Println()

	// Example 5: Dynamic canvas updates
	fmt.Println("5. Dynamic Canvas Updates:")
	
	// Initial state
	progressStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("99")).
		Width(20).
		Height(1)

	progressLayer := lipgloss.NewLayer(progressStyle.Render("Progress: 25%"))
	
	canvas5 := lipgloss.NewCanvas(
		titleLayer.X(0).Y(0),
		progressLayer.X(0).Y(2),
	)
	
	fmt.Println("Initial state:")
	fmt.Println(canvas5.Render())
	
	// Updated state
	updatedProgressLayer := lipgloss.NewLayer(
		progressStyle.
			BorderForeground(lipgloss.Color("82")).
			Render("Progress: 75%"),
	)
	
	canvas5Updated := lipgloss.NewCanvas(
		titleLayer.X(0).Y(0),
		updatedProgressLayer.X(0).Y(2),
	)
	
	fmt.Println("\nUpdated state:")
	fmt.Println(canvas5Updated.Render())
	fmt.Println()

	// Example 6: Complex layout with multiple sections
	headerStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("228")).
		Background(lipgloss.Color("235")).
		Width(60).
		Align(lipgloss.Center).
		Padding(1)

	sidebarStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("99")).
		Width(15).
		Height(8).
		Padding(1)

	mainContentStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("205")).
		Width(40).
		Height(8).
		Padding(1)

	footerStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("245")).
		Background(lipgloss.Color("235")).
		Width(60).
		Align(lipgloss.Center).
		Padding(0, 1)

	headerLayer := lipgloss.NewLayer(headerStyle.Render("Application Header"))
	sidebarLayer := lipgloss.NewLayer(sidebarStyle.Render("Sidebar\n• Menu 1\n• Menu 2\n• Menu 3"))
	mainLayer := lipgloss.NewLayer(mainContentStyle.Render("Main Content Area\n\nThis is where the primary\ncontent would be displayed\nwith multiple lines of text."))
	footerLayer := lipgloss.NewLayer(footerStyle.Render("Footer - Status: Ready"))

	fmt.Println("6. Complex Layout:")
	canvas6 := lipgloss.NewCanvas(
		headerLayer.X(0).Y(0),
		sidebarLayer.X(0).Y(3),
		mainLayer.X(18).Y(3),
		footerLayer.X(0).Y(13),
	)
	fmt.Println(canvas6.Render())
	fmt.Println()

	// Example 7: Nested composition
	fmt.Println("7. Nested Composition:")
	
	// Create a sub-canvas for a dialog
	dialogStyle := lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("201")).
		Background(lipgloss.Color("235")).
		Padding(1, 2).
		Width(25)

	dialogTitleLayer := lipgloss.NewLayer(
		lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("201")).Render("Confirmation"),
	)
	
	dialogContentLayer := lipgloss.NewLayer("Are you sure you want\nto proceed?")
	
	dialogButtonsLayer := lipgloss.NewLayer(
		lipgloss.NewStyle().Foreground(lipgloss.Color("82")).Render("[Yes]") + "  " +
		lipgloss.NewStyle().Foreground(lipgloss.Color("205")).Render("[No]"),
	)

	// Create dialog as a sub-canvas
	dialogCanvas := lipgloss.NewCanvas(
		dialogTitleLayer.X(0).Y(0),
		dialogContentLayer.X(0).Y(2),
		dialogButtonsLayer.X(0).Y(5),
	)

	dialogLayer := lipgloss.NewLayer(
		dialogStyle.Render(dialogCanvas.Render()),
	)

	// Compose with main interface
	canvas7 := lipgloss.NewCanvas(
		mainLayer.X(0).Y(0),
		dialogLayer.X(10).Y(3).Z(10), // High Z-index to appear on top
	)
	
	fmt.Println(canvas7.Render())
}

