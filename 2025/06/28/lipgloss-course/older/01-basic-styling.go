package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Basic Lipgloss v2 Styling Examples ===\n")

	// Example 1: Basic text styling
	basicStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("205")).
		Background(lipgloss.Color("235"))

	fmt.Println("1. Basic Text Styling:")
	fmt.Println(basicStyle.Render("Bold pink text on dark background"))
	fmt.Println()

	// Example 2: Borders and padding
	boxStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("63")).
		Padding(1, 2).
		Margin(1).
		Width(40).
		Align(lipgloss.Center)

	fmt.Println("2. Borders and Padding:")
	fmt.Println(boxStyle.Render("Centered text in a bordered box"))
	fmt.Println()

	// Example 3: Multiple styling attributes
	fancyStyle := lipgloss.NewStyle().
		Bold(true).
		Italic(true).
		Underline(true).
		Foreground(lipgloss.Color("86")).
		Background(lipgloss.Color("235")).
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(1, 3).
		Margin(1, 0)

	fmt.Println("3. Multiple Styling Attributes:")
	fmt.Println(fancyStyle.Render("Bold, italic, underlined text\nwith rounded border"))
	fmt.Println()

	// Example 4: Different color formats
	gradientStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("252")).
		Background(lipgloss.Color("235")).
		Padding(1, 2).
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("99"))

	fmt.Println("4. Different Color Formats:")
	fmt.Println(gradientStyle.Render("Text with different color formats"))
	fmt.Println()

	// Example 5: Width and height constraints
	constrainedStyle := lipgloss.NewStyle().
		Width(30).
		Height(5).
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("201")).
		Padding(1).
		Align(lipgloss.Center).
		Foreground(lipgloss.Color("228"))

	fmt.Println("5. Width and Height Constraints:")
	fmt.Println(constrainedStyle.Render("This text is constrained\nto a specific width\nand height"))
	fmt.Println()

	// Example 6: Inheritance and style composition
	baseStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("252")).
		Padding(1)

	headerStyle := baseStyle.
		Bold(true).
		Foreground(lipgloss.Color("99")).
		Border(lipgloss.NormalBorder(), false, false, true, false).
		BorderForeground(lipgloss.Color("99"))

	contentStyle := baseStyle.
		Margin(0, 2)

	fmt.Println("6. Style Inheritance and Composition:")
	fmt.Println(headerStyle.Render("Header Text"))
	fmt.Println(contentStyle.Render("Content text that inherits base styling"))
	fmt.Println()
}

