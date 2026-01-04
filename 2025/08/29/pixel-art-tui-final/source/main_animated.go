package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"time"

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

// AnimFrame represents a single frame in an animation
type AnimFrame struct {
	FrameNumber int        `json:"frame_number"`
	Width       int        `json:"width"`
	Height      int        `json:"height"`
	Pixels      [][]int    `json:"pixels"`
	Palette     []string   `json:"palette"`
	ColorCount  int        `json:"color_count"`
}

// AnimatedGIF represents an animated GIF
type AnimatedGIF struct {
	Filename       string      `json:"filename"`
	OriginalWidth  int         `json:"original_width"`
	OriginalHeight int         `json:"original_height"`
	IsAnimated     bool        `json:"is_animated"`
	FrameCount     int         `json:"frame_count"`
	Frames         []AnimFrame `json:"frames"`
}

// Model represents the TUI application state
type Model struct {
	images         []PixelImage
	animatedGIF    *AnimatedGIF
	currentImage   int
	currentFrame   int
	width          int
	height         int
	animationMode  bool
	animationSpeed time.Duration
}

// Animation tick message
type tickMsg time.Time

// doTick returns a command that sends a tick message after the specified duration
func doTick(d time.Duration) tea.Cmd {
	return tea.Tick(d, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

// Init initializes the model
func (m Model) Init() tea.Cmd {
	if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
		return doTick(m.animationSpeed)
	}
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
			if !m.animationMode && m.currentImage < len(m.images)-1 {
				m.currentImage++
			}
		case "left", "h":
			if !m.animationMode && m.currentImage > 0 {
				m.currentImage--
			}
		case "1":
			if len(m.images) > 0 {
				m.currentImage = 0
				m.animationMode = false
			}
		case "2":
			if len(m.images) > 1 {
				m.currentImage = 1
				m.animationMode = false
			}
		case "3":
			if len(m.images) > 2 {
				m.currentImage = 2
				m.animationMode = false
			}
		case "4":
			if len(m.images) > 3 {
				m.currentImage = 3
				m.animationMode = false
			}
		case "a":
			// Toggle animation mode
			if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
				m.animationMode = !m.animationMode
				if m.animationMode {
					return m, doTick(m.animationSpeed)
				}
			}
		case "=", "+":
			// Speed up animation
			if m.animationSpeed > 50*time.Millisecond {
				m.animationSpeed -= 50 * time.Millisecond
			}
		case "-", "_":
			// Slow down animation
			if m.animationSpeed < 2*time.Second {
				m.animationSpeed += 50 * time.Millisecond
			}
		case " ":
			// Pause/resume animation
			m.animationMode = !m.animationMode
			if m.animationMode && m.animatedGIF != nil {
				return m, doTick(m.animationSpeed)
			}
		}
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
	case tickMsg:
		if m.animationMode && m.animatedGIF != nil && m.animatedGIF.IsAnimated {
			m.currentFrame = (m.currentFrame + 1) % m.animatedGIF.FrameCount
			return m, doTick(m.animationSpeed)
		}
	}
	return m, nil
}

// renderPixelImage renders a pixel image using lipgloss
func renderPixelImage(pixels [][]int, palette []string) string {
	var result strings.Builder
	
	// Create a style for each color in the palette
	styles := make([]lipgloss.Style, len(palette))
	for i, color := range palette {
		styles[i] = lipgloss.NewStyle().Background(lipgloss.Color(color))
	}
	
	// Render each pixel as a colored block
	for _, row := range pixels {
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
	
	if m.animationMode && m.animatedGIF != nil {
		// Animation mode
		header := fmt.Sprintf("Pixel Art Viewer - Animation Mode - Frame %d/%d", 
			m.currentFrame+1, m.animatedGIF.FrameCount)
		view.WriteString(headerStyle.Render(header))
		view.WriteString("\n")
		
		// Animation info
		info := fmt.Sprintf("File: %s | Size: %dx%d | Speed: %v | Frames: %d", 
			m.animatedGIF.Filename, 
			m.animatedGIF.OriginalWidth, 
			m.animatedGIF.OriginalHeight,
			m.animationSpeed,
			m.animatedGIF.FrameCount)
		view.WriteString(infoStyle.Render(info))
		view.WriteString("\n")
		
		// Render current frame
		if m.currentFrame < len(m.animatedGIF.Frames) {
			frame := m.animatedGIF.Frames[m.currentFrame]
			view.WriteString(renderPixelImage(frame.Pixels, frame.Palette))
			
			// Frame palette
			view.WriteString("\nFrame Palette:\n")
			paletteStyle := lipgloss.NewStyle().MarginBottom(1)
			var palette strings.Builder
			for i, color := range frame.Palette {
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
		}
		
		// Animation controls
		controls := "Controls: SPACE (pause/play) | +/- (speed) | a (exit anim) | q (quit)"
		view.WriteString(controlsStyle.Render(controls))
		
	} else {
		// Static image mode
		if len(m.images) == 0 {
			return "No images loaded.\n"
		}
		
		img := m.images[m.currentImage]
		
		header := fmt.Sprintf("Pixel Art Viewer - Image %d/%d", m.currentImage+1, len(m.images))
		view.WriteString(headerStyle.Render(header))
		view.WriteString("\n")
		
		// Image info
		info := fmt.Sprintf("File: %s | Size: %dx%d | Colors: %d", 
			img.Filename, img.Width, img.Height, img.ColorCount)
		view.WriteString(infoStyle.Render(info))
		view.WriteString("\n")
		
		// Render the pixel image
		view.WriteString(renderPixelImage(img.Pixels, img.Palette))
		
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
		animText := ""
		if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
			animText = " | a (animation)"
		}
		controls := fmt.Sprintf("Controls: ← → (navigate) | 1-4 (direct select)%s | q (quit)", animText)
		view.WriteString(controlsStyle.Render(controls))
	}
	
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
	
	// Try to load animated GIF data
	var animatedGIF *AnimatedGIF
	gifData, err := ioutil.ReadFile("../gif_analysis.json")
	if err == nil {
		var gif AnimatedGIF
		err = json.Unmarshal(gifData, &gif)
		if err == nil {
			animatedGIF = &gif
		}
	}
	
	if len(images) == 0 && animatedGIF == nil {
		log.Fatal("No images or animations found")
	}
	
	// Initialize model
	m := Model{
		images:         images,
		animatedGIF:    animatedGIF,
		currentImage:   0,
		currentFrame:   0,
		animationMode:  false,
		animationSpeed: 200 * time.Millisecond,
	}
	
	// Start the TUI
	p := tea.NewProgram(m, tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v", err)
		os.Exit(1)
	}
}

