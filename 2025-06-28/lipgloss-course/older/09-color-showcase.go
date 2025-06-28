package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Lipgloss v2 Color Showcase ===\n")

	// Example 1: 256-color palette demonstration
	fmt.Println("1. 256-Color Palette:")
	
	colorStyle := lipgloss.NewStyle().
		Width(4).
		Height(2).
		Align(lipgloss.Center).
		Bold(true)

	// Show a selection of 256 colors
	colors := []string{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
		"16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
		"32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47",
		"196", "197", "198", "199", "200", "201", "202", "203", "204", "205", "206", "207", "208", "209",
		"82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97",
		"99", "100", "101", "102", "103", "104", "105", "106", "107", "108", "109", "110", "111", "112"}

	var colorLayers []*lipgloss.Layer
	for i, color := range colors {
		if i >= 32 { // Limit to first 32 colors for display
			break
		}
		
		x := (i % 8) * 5
		y := (i / 8) * 3
		
		layer := lipgloss.NewLayer(
			colorStyle.
				Background(lipgloss.Color(color)).
				Foreground(lipgloss.Color("255")).
				Render(color),
		)
		colorLayers = append(colorLayers, layer.X(x).Y(y))
	}

	colorCanvas := lipgloss.NewCanvas(colorLayers...)
	fmt.Println(colorCanvas.Render())
	fmt.Println()

	// Example 2: RGB color demonstration
	fmt.Println("2. RGB Color Support:")
	
	rgbStyle := lipgloss.NewStyle().
		Width(12).
		Height(3).
		Align(lipgloss.Center).
		Bold(true).
		Padding(0, 1)

	rgbColors := []struct {
		name string
		hex  string
		x, y int
	}{
		{"Red", "#FF0000", 0, 0},
		{"Green", "#00FF00", 15, 0},
		{"Blue", "#0000FF", 30, 0},
		{"Purple", "#8A2BE2", 0, 4},
		{"Orange", "#FFA500", 15, 4},
		{"Cyan", "#00FFFF", 30, 4},
	}

	var rgbLayers []*lipgloss.Layer
	for _, color := range rgbColors {
		layer := lipgloss.NewLayer(
			rgbStyle.
				Background(lipgloss.Color(color.hex)).
				Foreground(lipgloss.Color("#FFFFFF")).
				Render(color.name + "\n" + color.hex),
		)
		rgbLayers = append(rgbLayers, layer.X(color.x).Y(color.y))
	}

	rgbCanvas := lipgloss.NewCanvas(rgbLayers...)
	fmt.Println(rgbCanvas.Render())
	fmt.Println()

	// Example 3: Gradient-like effects using multiple colors
	fmt.Println("3. Gradient-like Effects:")
	
	gradientColors := []string{"196", "197", "198", "199", "200", "201", "202", "203", "204", "205"}
	
	gradientStyle := lipgloss.NewStyle().
		Width(6).
		Height(2).
		Align(lipgloss.Center).
		Bold(true)

	var gradientLayers []*lipgloss.Layer
	for i, color := range gradientColors {
		layer := lipgloss.NewLayer(
			gradientStyle.
				Background(lipgloss.Color(color)).
				Foreground(lipgloss.Color("255")).
				Render("█████"),
		)
		gradientLayers = append(gradientLayers, layer.X(i*7).Y(0))
	}

	gradientCanvas := lipgloss.NewCanvas(gradientLayers...)
	fmt.Println(gradientCanvas.Render())
	fmt.Println()

	// Example 4: Foreground and background combinations
	fmt.Println("4. Foreground and Background Combinations:")
	
	comboStyle := lipgloss.NewStyle().
		Width(20).
		Height(3).
		Align(lipgloss.Center).
		Bold(true).
		Padding(0, 1)

	combinations := []struct {
		name string
		fg   string
		bg   string
		x, y int
	}{
		{"High Contrast", "255", "0", 0, 0},
		{"Warm Theme", "228", "52", 25, 0},
		{"Cool Theme", "159", "17", 50, 0},
		{"Nature Theme", "46", "22", 0, 4},
		{"Ocean Theme", "51", "18", 25, 4},
		{"Sunset Theme", "208", "88", 50, 4},
	}

	var comboLayers []*lipgloss.Layer
	for _, combo := range combinations {
		layer := lipgloss.NewLayer(
			comboStyle.
				Background(lipgloss.Color(combo.bg)).
				Foreground(lipgloss.Color(combo.fg)).
				Render(combo.name),
		)
		comboLayers = append(comboLayers, layer.X(combo.x).Y(combo.y))
	}

	comboCanvas := lipgloss.NewCanvas(comboLayers...)
	fmt.Println(comboCanvas.Render())
	fmt.Println()

	// Example 5: Color in complex layouts
	fmt.Println("5. Colors in Complex Layouts:")
	
	// Create a colorful dashboard
	headerStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("99")).
		Foreground(lipgloss.Color("255")).
		Bold(true).
		Width(60).
		Padding(0, 2).
		Align(lipgloss.Center)

	header := lipgloss.NewLayer(headerStyle.Render("Colorful Dashboard"))

	// Status indicators
	statusStyle := lipgloss.NewStyle().
		Width(12).
		Height(3).
		Align(lipgloss.Center).
		Bold(true).
		Padding(0, 1)

	statusItems := []struct {
		text  string
		color string
		x, y  int
	}{
		{"✅ Online", "82", 2, 3},
		{"⚠️ Warning", "228", 16, 3},
		{"❌ Error", "196", 30, 3},
		{"🔄 Loading", "99", 44, 3},
	}

	var statusLayers []*lipgloss.Layer
	statusLayers = append(statusLayers, header.X(0).Y(0))

	for _, status := range statusItems {
		layer := lipgloss.NewLayer(
			statusStyle.
				Background(lipgloss.Color(status.color)).
				Foreground(lipgloss.Color("255")).
				Render(status.text),
		)
		statusLayers = append(statusLayers, layer.X(status.x).Y(status.y))
	}

	// Add a border around the whole dashboard
	dashboardCanvas := lipgloss.NewCanvas(statusLayers...)
	
	dashboardStyle := lipgloss.NewStyle().
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("245")).
		Padding(1)

	dashboard := lipgloss.NewLayer(dashboardStyle.Render(dashboardCanvas.Render()))
	
	finalCanvas := lipgloss.NewCanvas(dashboard.X(0).Y(0))
	fmt.Println(finalCanvas.Render())
	fmt.Println()

	// Example 6: Color accessibility considerations
	fmt.Println("6. Color Accessibility Examples:")
	
	accessibilityStyle := lipgloss.NewStyle().
		Width(25).
		Height(4).
		Align(lipgloss.Center).
		Bold(true).
		Padding(1)

	accessibilityExamples := []struct {
		name string
		fg   string
		bg   string
		desc string
		x, y int
	}{
		{"High Contrast", "255", "0", "White on Black\nWCAG AAA", 0, 0},
		{"Good Contrast", "255", "18", "White on Dark Blue\nWCAG AA", 30, 0},
		{"Moderate", "0", "228", "Black on Yellow\nWCAG AA", 0, 6},
		{"Colorblind Safe", "255", "22", "White on Dark Green\nSafe for most", 30, 6},
	}

	var accessibilityLayers []*lipgloss.Layer
	for _, example := range accessibilityExamples {
		layer := lipgloss.NewLayer(
			accessibilityStyle.
				Background(lipgloss.Color(example.bg)).
				Foreground(lipgloss.Color(example.fg)).
				Render(example.name + "\n" + example.desc),
		)
		accessibilityLayers = append(accessibilityLayers, layer.X(example.x).Y(example.y))
	}

	accessibilityCanvas := lipgloss.NewCanvas(accessibilityLayers...)
	fmt.Println(accessibilityCanvas.Render())
	fmt.Println()

	fmt.Println("Color Showcase Complete!")
	fmt.Println("This demonstrates the full color capabilities of Lipgloss v2")
}

