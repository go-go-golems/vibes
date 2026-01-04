package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Test basic layer creation and usage
	layer := lipgloss.NewLayer("Hello World")
	
	// Try to use it in a canvas
	canvas := lipgloss.NewCanvas(
		layer.X(0).Y(0),
	)
	
	fmt.Println(canvas.Render())
}

