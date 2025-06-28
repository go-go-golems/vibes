package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Nested Layer Examples ===\n")

	// Example 1: Simple nested composition
	fmt.Println("1. Simple Nested Composition:")

	// Create inner components
	headerStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("255")).
		Bold(true).
		Width(25).
		Padding(0, 1).
		Align(lipgloss.Center)

	contentStyle := lipgloss.NewStyle().
		Width(25).
		Height(4).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	footerStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("245")).
		Foreground(lipgloss.Color("255")).
		Width(25).
		Padding(0, 1).
		Align(lipgloss.Center)

	// Create inner canvas
	innerHeader := lipgloss.NewLayer(headerStyle.Render("Header"))
	innerContent := lipgloss.NewLayer(contentStyle.Render("Content area\nwith text"))
	innerFooter := lipgloss.NewLayer(footerStyle.Render("Footer"))

	innerCanvas := lipgloss.NewCanvas(
		innerHeader.X(0).Y(0),
		innerContent.X(0).Y(1),
		innerFooter.X(0).Y(6),
	)

	// Wrap in outer container
	outerStyle := lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(1)

	outerLayer := lipgloss.NewLayer(
		outerStyle.Render(innerCanvas.Render()),
	)

	canvas1 := lipgloss.NewCanvas(outerLayer.X(0).Y(0))
	fmt.Println(canvas1.Render())
	fmt.Println()

	// Example 2: Multi-level nesting with different components
	fmt.Println("2. Multi-level Nesting with Components:")

	// Level 3: Individual buttons
	buttonStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("82")).
		Foreground(lipgloss.Color("235")).
		Padding(0, 2).
		Bold(true)

	button1 := lipgloss.NewLayer(buttonStyle.Render("Save"))
	button2 := lipgloss.NewLayer(buttonStyle.Render("Cancel"))
	button3 := lipgloss.NewLayer(buttonStyle.Render("Help"))

	// Level 2: Button group
	buttonGroupCanvas := lipgloss.NewCanvas(
		button1.X(0).Y(0),
		button2.X(8).Y(0),
		button3.X(18).Y(0),
	)

	buttonGroupStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	buttonGroup := lipgloss.NewLayer(
		buttonGroupStyle.Render(buttonGroupCanvas.Render()),
	)

	// Level 2: Form fields
	fieldStyle := lipgloss.NewStyle().
		Width(25).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(0, 1)

	field1 := lipgloss.NewLayer(fieldStyle.Render("Name: [John Doe      ]"))
	field2 := lipgloss.NewLayer(fieldStyle.Render("Email: [john@example ]"))

	formCanvas := lipgloss.NewCanvas(
		field1.X(0).Y(0),
		field2.X(0).Y(2),
	)

	formStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1)

	formGroup := lipgloss.NewLayer(
		formStyle.Render(formCanvas.Render()),
	)

	// Level 1: Complete dialog
	dialogCanvas := lipgloss.NewCanvas(
		formGroup.X(1).Y(1),
		buttonGroup.X(1).Y(7),
	)

	dialogStyle := lipgloss.NewStyle().
		Width(35).
		Height(12).
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(1)

	dialogTitle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("205")).
		Width(33).
		Align(lipgloss.Center).
		Render("User Information Dialog")

	fullDialog := dialogTitle + "\n" + dialogCanvas.Render()

	dialog := lipgloss.NewLayer(dialogStyle.Render(fullDialog))

	canvas2 := lipgloss.NewCanvas(dialog.X(0).Y(0))
	fmt.Println(canvas2.Render())
	fmt.Println()

	// Example 3: Nested windows with independent content
	fmt.Println("3. Nested Windows with Independent Content:")

	// Create a window factory
	createWindow := func(title, content, color string, width, height int) *lipgloss.Layer {
		titleBarStyle := lipgloss.NewStyle().
			Background(lipgloss.Color(color)).
			Foreground(lipgloss.Color("255")).
			Bold(true).
			Width(width-2).
			Padding(0, 1)

		contentAreaStyle := lipgloss.NewStyle().
			Width(width-2).
			Height(height-3).
			Padding(1)

		windowStyle := lipgloss.NewStyle().
			Width(width).
			Height(height).
			Border(lipgloss.NormalBorder()).
			BorderForeground(lipgloss.Color(color))

		titleBar := titleBarStyle.Render(title)
		contentArea := contentAreaStyle.Render(content)
		fullWindow := titleBar + "\n" + contentArea

		return lipgloss.NewLayer(windowStyle.Render(fullWindow))
	}

	// Parent window
	parentWindow := createWindow(
		"Parent Window",
		"This is the parent window\nthat contains child windows\nwithin its content area.",
		"99",
		40,
		15,
	)

	// Child windows
	child1 := createWindow(
		"Child 1",
		"First child\nwindow content",
		"205",
		15,
		6,
	)

	child2 := createWindow(
		"Child 2",
		"Second child\nwith different\ncontent",
		"86",
		15,
		7,
	)

	// Create nested composition
	// Combine parent with children
	parentContent := parentWindow.X(0).Y(0)
	
	canvas3 := lipgloss.NewCanvas(
		parentContent.Z(1),
		child1.X(2).Y(3).Z(2),
		child2.X(20).Y(5).Z(2),
	)

	fmt.Println(canvas3.Render())
	fmt.Println()

	// Example 4: Hierarchical menu system
	fmt.Println("4. Hierarchical Menu System:")

	// Menu item style
	menuItemStyle := lipgloss.NewStyle().
		Width(15).
		Padding(0, 1).
		Foreground(lipgloss.Color("255"))

	// Create menu items
	createMenuItem := func(text, color string, selected bool) *lipgloss.Layer {
		style := menuItemStyle.Background(lipgloss.Color(color))
		if selected {
			style = style.Bold(true).Foreground(lipgloss.Color("228"))
		}
		return lipgloss.NewLayer(style.Render(text))
	}

	// Main menu
	mainMenu1 := createMenuItem("File", "99", true)
	mainMenu2 := createMenuItem("Edit", "99", false)
	mainMenu3 := createMenuItem("View", "99", false)

	mainMenuCanvas := lipgloss.NewCanvas(
		mainMenu1.X(0).Y(0),
		mainMenu2.X(17).Y(0),
		mainMenu3.X(34).Y(0),
	)

	// Submenu for "File"
	subMenuItem1 := createMenuItem("New", "205", false)
	subMenuItem2 := createMenuItem("Open", "205", true)
	subMenuItem3 := createMenuItem("Save", "205", false)
	subMenuItem4 := createMenuItem("Exit", "205", false)

	subMenuCanvas := lipgloss.NewCanvas(
		subMenuItem1.X(0).Y(0),
		subMenuItem2.X(0).Y(1),
		subMenuItem3.X(0).Y(2),
		subMenuItem4.X(0).Y(3),
	)

	subMenuStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("205"))

	subMenu := lipgloss.NewLayer(
		subMenuStyle.Render(subMenuCanvas.Render()),
	)

	// Sub-submenu for "Open"
	subSubMenuItem1 := createMenuItem("Recent Files", "86", false)
	subSubMenuItem2 := createMenuItem("Browse...", "86", false)

	subSubMenuCanvas := lipgloss.NewCanvas(
		subSubMenuItem1.X(0).Y(0),
		subSubMenuItem2.X(0).Y(1),
	)

	subSubMenuStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("86"))

	subSubMenu := lipgloss.NewLayer(
		subSubMenuStyle.Render(subSubMenuCanvas.Render()),
	)

	// Complete menu system
	menuBarStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("235")).
		Width(52).
		Padding(0, 1)

	menuBar := lipgloss.NewLayer(
		menuBarStyle.Render(mainMenuCanvas.Render()),
	)

	canvas4 := lipgloss.NewCanvas(
		menuBar.X(0).Y(0).Z(1),
		subMenu.X(0).Y(1).Z(2),
		subSubMenu.X(17).Y(2).Z(3),
	)

	fmt.Println(canvas4.Render())
	fmt.Println()

	// Example 5: Complex nested dashboard
	fmt.Println("5. Complex Nested Dashboard:")

	// Widget factory
	createWidget := func(title, content, color string) *lipgloss.Layer {
		titleStyle := lipgloss.NewStyle().
			Background(lipgloss.Color(color)).
			Foreground(lipgloss.Color("255")).
			Bold(true).
			Width(18).
			Padding(0, 1).
			Align(lipgloss.Center)

		contentStyle := lipgloss.NewStyle().
			Width(18).
			Height(4).
			Padding(1)

		widgetStyle := lipgloss.NewStyle().
			Width(20).
			Height(7).
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color(color))

		titleBar := titleStyle.Render(title)
		contentArea := contentStyle.Render(content)
		fullWidget := titleBar + "\n" + contentArea

		return lipgloss.NewLayer(widgetStyle.Render(fullWidget))
	}

	// Create widgets
	widget1 := createWidget("CPU Usage", "▓▓▓▓▓░░░░░ 45%\nCores: 8\nTemp: 65°C", "99")
	widget2 := createWidget("Memory", "▓▓▓▓▓▓▓░░░ 72%\nUsed: 5.8GB\nFree: 2.2GB", "205")
	widget3 := createWidget("Network", "↑ 1.2 MB/s\n↓ 3.4 MB/s\nPing: 12ms", "82")
	widget4 := createWidget("Storage", "▓▓▓░░░░░░░ 34%\nUsed: 340GB\nFree: 660GB", "228")

	// Create widget grid
	widgetCanvas := lipgloss.NewCanvas(
		widget1.X(0).Y(0),
		widget2.X(22).Y(0),
		widget3.X(0).Y(8),
		widget4.X(22).Y(8),
	)

	// Dashboard container
	dashboardStyle := lipgloss.NewStyle().
		Width(50).
		Height(20).
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	dashboardTitle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("228")).
		Width(48).
		Align(lipgloss.Center).
		Render("System Monitor Dashboard")

	fullDashboard := dashboardTitle + "\n" + widgetCanvas.Render()

	dashboard := lipgloss.NewLayer(dashboardStyle.Render(fullDashboard))

	canvas5 := lipgloss.NewCanvas(dashboard.X(0).Y(0))
	fmt.Println(canvas5.Render())
}

