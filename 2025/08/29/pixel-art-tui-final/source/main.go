package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// PixelImage represents a single pixel art image
type PixelImage struct {
	Filename   string     `json:"filename"`
	Width      int        `json:"width"`
	Height     int        `json:"height"`
	Pixels     [][]int    `json:"pixels"`
	Palette    []string   `json:"palette"`
	ColorCount int        `json:"color_count"`
}

// Model represents the TUI application state
type Model struct {
	images       []PixelImage
	currentImage int
	width        int
	height       int
}

// Init initializes the model
func (m Model) Init() tea.Cmd {
	return nil
}

// Update handles messages and updates the model
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit
		case "right", "l":
			if m.currentImage < len(m.images)-1 {
				m.currentImage++
			}
		case "left", "h":
			if m.currentImage > 0 {
				m.currentImage--
			}
		case "1":
			if len(m.images) > 0 {
				m.currentImage = 0
			}
		case "2":
			if len(m.images) > 1 {
				m.currentImage = 1
			}
		case "3":
			if len(m.images) > 2 {
				m.currentImage = 2
			}
		case "4":
			if len(m.images) > 3 {
				m.currentImage = 3
			}
		}
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
	}
	return m, nil
}

// renderPixelImage renders a pixel image using lipgloss
func renderPixelImage(img PixelImage) string {
	var result strings.Builder
	
	// Create a style for each color in the palette
	styles := make([]lipgloss.Style, len(img.Palette))
	for i, color := range img.Palette {
		styles[i] = lipgloss.NewStyle().Background(lipgloss.Color(color))
	}
	
	// Render each pixel as a colored block
	for _, row := range img.Pixels {
		for _, colorIndex := range row {
			if colorIndex < len(styles) {
				// Use double space for better aspect ratio
				result.WriteString(styles[colorIndex].Render("  "))
			} else {
				result.WriteString("  ")
			}
		}
		result.WriteString("\n")
	}
	
	return result.String()
}

// View renders the current view
func (m Model) View() string {
	if len(m.images) == 0 {
		return "No images loaded.\n"
	}
	
	img := m.images[m.currentImage]
	
	// Header style
	headerStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FAFAFA")).
		Background(lipgloss.Color("#7D56F4")).
		Padding(0, 1).
		MarginBottom(1)
	
	// Info style
	infoStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#626262")).
		MarginBottom(1)
	
	// Controls style
	controlsStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#04B575")).
		MarginTop(1)
	
	// Build the view
	var view strings.Builder
	
	// Header
	header := fmt.Sprintf("Pixel Art Viewer - Image %d/%d", m.currentImage+1, len(m.images))
	view.WriteString(headerStyle.Render(header))
	view.WriteString("\n")
	
	// Image info
	info := fmt.Sprintf("File: %s | Size: %dx%d | Colors: %d", 
		img.Filename, img.Width, img.Height, img.ColorCount)
	view.WriteString(infoStyle.Render(info))
	view.WriteString("\n")
	
	// Render the pixel image
	view.WriteString(renderPixelImage(img))
	
	// Color palette
	view.WriteString("\nColor Palette:\n")
	paletteStyle := lipgloss.NewStyle().MarginBottom(1)
	var palette strings.Builder
	for i, color := range img.Palette {
		colorStyle := lipgloss.NewStyle().
			Background(lipgloss.Color(color)).
			Foreground(lipgloss.Color("#000000"))
		if i > 0 && i%8 == 0 {
			palette.WriteString("\n")
		}
		palette.WriteString(colorStyle.Render(fmt.Sprintf(" %02d ", i)))
		palette.WriteString(" ")
	}
	view.WriteString(paletteStyle.Render(palette.String()))
	view.WriteString("\n")
	
	// Controls
	controls := "Controls: ← → (navigate) | 1-4 (direct select) | q (quit)"
	view.WriteString(controlsStyle.Render(controls))
	
	return view.String()
}

func main() {
	// Load pixel data
	data, err := ioutil.ReadFile("pixel_data.json")
	if err != nil {
		log.Fatal("Error reading pixel_data.json:", err)
	}
	
	var images []PixelImage
	err = json.Unmarshal(data, &images)
	if err != nil {
		log.Fatal("Error parsing pixel_data.json:", err)
	}
	
	if len(images) == 0 {
		log.Fatal("No images found in pixel_data.json")
	}
	
	// Initialize model
	m := Model{
		images:       images,
		currentImage: 0,
	}
	
	// Start the TUI
	p := tea.NewProgram(m, tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v", err)
		os.Exit(1)
	}
}

