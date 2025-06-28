package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Simple Layer Examples ===\n")

	// Example 1: Creating basic layers
	boxStyle := lipgloss.NewStyle().
		Width(20).
		Height(3).
		Border(lipgloss.NormalBorder()).
		Padding(1).
		Align(lipgloss.Center)

	layer1 := lipgloss.NewLayer(
		boxStyle.
			BorderForeground(lipgloss.Color("99")).
			Foreground(lipgloss.Color("99")).
			Render("Layer 1"),
	)

	layer2 := lipgloss.NewLayer(
		boxStyle.
			BorderForeground(lipgloss.Color("205")).
			Foreground(lipgloss.Color("205")).
			Render("Layer 2"),
	)

	fmt.Println("1. Basic Layer Creation:")
	fmt.Println("Layer 1 content:")
	canvas1a := lipgloss.NewCanvas(layer1.X(0).Y(0))
	fmt.Println(canvas1a.Render())
	fmt.Println("\nLayer 2 content:")
	canvas1b := lipgloss.NewCanvas(layer2.X(0).Y(0))
	fmt.Println(canvas1b.Render())
	fmt.Println()

	// Example 2: Layer positioning
	fmt.Println("2. Layer Positioning:")
	positionedLayer1 := layer1.X(0).Y(0)
	positionedLayer2 := layer2.X(5).Y(2)

	fmt.Printf("Layer 1 positioned at X=0, Y=0\n")
	fmt.Printf("Layer 2 positioned at X=5, Y=2\n")
	
	// Show them in a canvas to demonstrate positioning
	canvas2 := lipgloss.NewCanvas(
		positionedLayer1,
		positionedLayer2,
	)
	fmt.Println(canvas2.Render())
	fmt.Println()

	// Example 3: Layer with different content types
	textLayer := lipgloss.NewLayer("Simple text layer")
	
	multilineContent := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("86")).
		Padding(1).
		Render("Multi-line\ncontent\nlayer")
	
	multilineLayer := lipgloss.NewLayer(multilineContent)

	fmt.Println("3. Different Content Types:")
	fmt.Println("Text layer:")
	canvas3a := lipgloss.NewCanvas(textLayer.X(0).Y(0))
	fmt.Println(canvas3a.Render())
	fmt.Println("\nMulti-line layer:")
	canvas3b := lipgloss.NewCanvas(multilineLayer.X(0).Y(0))
	fmt.Println(canvas3b.Render())
	fmt.Println()

	// Example 4: Layer with Z-index
	fmt.Println("4. Layer Z-index:")
	frontLayer := layer1.X(0).Y(0).Z(2)
	backLayer := layer2.X(0).Y(0).Z(1)

	fmt.Printf("Front layer (Z=2): %s\n", "Will appear on top")
	fmt.Printf("Back layer (Z=1): %s\n", "Will appear behind")
	
	// Demonstrate overlapping with Z-index
	canvas4 := lipgloss.NewCanvas(
		backLayer,   // This will be behind
		frontLayer,  // This will be on top
	)
	fmt.Println("Overlapping layers with Z-index:")
	fmt.Println(canvas4.Render())
	fmt.Println()

	// Example 5: Dynamic layer content
	dynamicStyle := lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("201")).
		Padding(1, 2).
		Width(25)

	statusLayer := lipgloss.NewLayer(
		dynamicStyle.Render("Status: Ready"),
	)

	fmt.Println("5. Dynamic Layer Content:")
	fmt.Println("Initial status:")
	canvas5a := lipgloss.NewCanvas(statusLayer.X(0).Y(0))
	fmt.Println(canvas5a.Render())

	// Update the layer content
	updatedStatusLayer := lipgloss.NewLayer(
		dynamicStyle.
			BorderForeground(lipgloss.Color("82")).
			Render("Status: Processing..."),
	)

	fmt.Println("\nUpdated status:")
	canvas5b := lipgloss.NewCanvas(updatedStatusLayer.X(0).Y(0))
	fmt.Println(canvas5b.Render())
	fmt.Println()

	// Example 6: Layer with complex styling
	complexStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("228")).
		Background(lipgloss.Color("235")).
		Border(lipgloss.DoubleBorder()).
		BorderForeground(lipgloss.Color("99")).
		Padding(2, 4).
		Margin(1).
		Width(35).
		Align(lipgloss.Center)

	complexLayer := lipgloss.NewLayer(
		complexStyle.Render("Complex Styled Layer\nwith multiple attributes"),
	)

	fmt.Println("6. Complex Styled Layer:")
	canvas6 := lipgloss.NewCanvas(complexLayer.X(0).Y(0))
	fmt.Println(canvas6.Render())
	fmt.Println()

	// Example 7: Layer reusability
	cardStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		Padding(1, 2).
		Margin(1, 0).
		Width(30)

	createCard := func(title, content, color string) *lipgloss.Layer {
		return lipgloss.NewLayer(
			cardStyle.
				BorderForeground(lipgloss.Color(color)).
				Render(fmt.Sprintf("%s\n%s", 
					lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color(color)).Render(title),
					content,
				)),
		)
	}

	fmt.Println("7. Reusable Layer Patterns:")
	card1 := createCard("Info", "This is an information card", "99")
	card2 := createCard("Warning", "This is a warning card", "228")
	card3 := createCard("Error", "This is an error card", "205")

	canvas7a := lipgloss.NewCanvas(card1.X(0).Y(0))
	fmt.Println(canvas7a.Render())
	canvas7b := lipgloss.NewCanvas(card2.X(0).Y(0))
	fmt.Println(canvas7b.Render())
	canvas7c := lipgloss.NewCanvas(card3.X(0).Y(0))
	fmt.Println(canvas7c.Render())
}

