package main

import (
	"flag"
	"fmt"
	"image"
	"image/color"
	"image/gif"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"gopkg.in/yaml.v3"
)

// Animation represents the full animation configuration
type Animation struct {
	Title      string    `yaml:"title"`
	Duration   float64   `yaml:"duration"`
	Size       Size      `yaml:"size"`
	Background string    `yaml:"background"`
	Frames     []Frame   `yaml:"frames"`
}

// Size represents the dimensions of the animation canvas
type Size struct {
	Width  int `yaml:"width"`
	Height int `yaml:"height"`
}

// Position represents the x,y coordinates on screen
type Position struct {
	X int `yaml:"x"`
	Y int `yaml:"y"`
}

// Frame represents a single frame in the animation
type Frame struct {
	Text     string   `yaml:"text"`
	Position Position `yaml:"position"`
	Color    string   `yaml:"color"`
	Duration float64  `yaml:"duration"`
}

// Model represents the application state
type Model struct {
	animation Animation
	frames    []Frame
	width     int
	height    int
	frame     int
	done      bool
}

// Init initializes the Bubble Tea model
func (m Model) Init() tea.Cmd {
	return tea.Batch(
		tea.ClearScreen,
		nextFrame(time.Duration(m.frames[0].Duration*1000) * time.Millisecond),
	)
}

// Update handles events and updates the model
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		if msg.String() == "q" || msg.String() == "ctrl+c" {
			return m, tea.Quit
		}
	case frameMsg:
		m.frame++
		if m.frame >= len(m.frames) {
			m.done = true
			return m, tea.Quit
		}
		return m, nextFrame(time.Duration(m.frames[m.frame].Duration*1000) * time.Millisecond)
	}
	return m, nil
}

// View renders the current state of the model
func (m Model) View() string {
	if m.done || m.frame >= len(m.frames) {
		return ""
	}

	frame := m.frames[m.frame]
	style := lipgloss.NewStyle().
		Foreground(lipgloss.Color(frame.Color)).
		Background(lipgloss.Color(m.animation.Background))

	// Create a blank canvas
	canvas := make([][]string, m.height)
	for i := range canvas {
		canvas[i] = make([]string, m.width)
		for j := range canvas[i] {
			canvas[i][j] = " "
		}
	}

	// Place the text at the specified position
	x, y := frame.Position.X, frame.Position.Y
	for i, char := range frame.Text {
		if x+i < m.width && y < m.height {
			canvas[y][x+i] = string(char)
		}
	}

	// Render the canvas
	var view string
	for _, row := range canvas {
		line := ""
		for _, cell := range row {
			line += cell
		}
		view += style.Render(line) + "\n"
	}

	return view
}

// frameMsg is sent when it's time to show the next frame
type frameMsg time.Time

func nextFrame(d time.Duration) tea.Cmd {
	return tea.Tick(d, func(t time.Time) tea.Msg {
		return frameMsg(t)
	})
}

// LoadAnimation loads an animation from a YAML file
func LoadAnimation(filename string) (Animation, error) {
	var animation Animation

	data, err := ioutil.ReadFile(filename)
	if err != nil {
		return animation, err
	}

	err = yaml.Unmarshal(data, &animation)
	if err != nil {
		return animation, err
	}

	return animation, nil
}

// hexToRGBA converts a hex color string to RGBA
func hexToRGBA(hex string) color.RGBA {
	hex = strings.TrimPrefix(hex, "#")
	
	var r, g, b uint8
	fmt.Sscanf(hex, "%02x%02x%02x", &r, &g, &b)
	
	return color.RGBA{r, g, b, 255}
}

// ExportGIF exports the animation as a GIF file
func ExportGIF(animation Animation, outputFile string) error {
	// Create a new GIF
	anim := &gif.GIF{}
	
	// Handle the case of a single frame animation
	if len(animation.Frames) == 1 {
		// For single frame animations, we'll create two identical frames
		// with a small delay to ensure the GIF is properly displayed
		animation.Frames = append(animation.Frames, animation.Frames[0])
	}
	
	// Calculate delay for each frame (in 100ths of a second)
	delays := make([]int, len(animation.Frames))
	for i, frame := range animation.Frames {
		delays[i] = int(frame.Duration * 100)
		// Ensure minimum delay to prevent issues
		if delays[i] < 10 {
			delays[i] = 10
		}
	}
	
	// Create each frame
	for i, frame := range animation.Frames {
		// Create a new image for this frame
		img := image.NewRGBA(image.Rect(0, 0, animation.Size.Width*8, animation.Size.Height*16))
		
		// Fill with background color
		bgColor := hexToRGBA(animation.Background)
		for y := 0; y < img.Bounds().Dy(); y++ {
			for x := 0; x < img.Bounds().Dx(); x++ {
				img.Set(x, y, bgColor)
			}
		}
		
		// Draw the text
		textColor := hexToRGBA(frame.Color)
		
		// Draw each character as a better representation
		drawText(img, frame.Text, frame.Position.X, frame.Position.Y, textColor)
		
		// Convert to paletted image with more colors for better quality
		bounds := img.Bounds()
		palette := []color.Color{
			bgColor,
			textColor,
			color.RGBA{0, 0, 0, 255},       // Black
			color.RGBA{255, 255, 255, 255}, // White
		}
		
		palettedImg := image.NewPaletted(bounds, palette)
		
		for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
			for x := bounds.Min.X; x < bounds.Max.X; x++ {
				palettedImg.Set(x, y, img.At(x, y))
			}
		}
		
		// Add the frame to the GIF
		anim.Image = append(anim.Image, palettedImg)
		anim.Delay = append(anim.Delay, delays[i])
		anim.Disposal = append(anim.Disposal, gif.DisposalBackground)
	}
	
	// Save the GIF
	f, err := os.Create(outputFile)
	if err != nil {
		return err
	}
	defer f.Close()
	
	return gif.EncodeAll(f, anim)
}

// drawText draws text on the image with better character representation
func drawText(img *image.RGBA, text string, posX, posY int, textColor color.RGBA) {
	fontWidth, fontHeight := 8, 16 // Simple fixed-width font size
	
	for i, char := range text {
		x := posX * fontWidth + i * fontWidth
		y := posY * fontHeight
		
		// Draw a better representation of each character
		drawChar(img, char, x, y, textColor)
	}
}

// drawChar draws a single character on the image
func drawChar(img *image.RGBA, char rune, x, y int, textColor color.RGBA) {
	
	// Simple character patterns for common characters
	// This is a very basic implementation - in a real app you'd use a proper font renderer
	switch char {
	case 'A', 'a':
		drawPattern(img, x, y, []string{
			"  ██  ",
			" ████ ",
			"██  ██",
			"██████",
			"██  ██",
			"██  ██",
		}, textColor)
	case 'B', 'b':
		drawPattern(img, x, y, []string{
			"█████ ",
			"██  ██",
			"█████ ",
			"██  ██",
			"██  ██",
			"█████ ",
		}, textColor)
	case 'C', 'c':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██    ",
			"██    ",
			"██  ██",
			" ████ ",
		}, textColor)
	case 'D', 'd':
		drawPattern(img, x, y, []string{
			"█████ ",
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			"█████ ",
		}, textColor)
	case 'E', 'e':
		drawPattern(img, x, y, []string{
			"██████",
			"██    ",
			"█████ ",
			"██    ",
			"██    ",
			"██████",
		}, textColor)
	case 'F', 'f':
		drawPattern(img, x, y, []string{
			"██████",
			"██    ",
			"█████ ",
			"██    ",
			"██    ",
			"██    ",
		}, textColor)
	case 'G', 'g':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██    ",
			"██ ███",
			"██  ██",
			" ████ ",
		}, textColor)
	case 'H', 'h':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			"██████",
			"██  ██",
			"██  ██",
			"██  ██",
		}, textColor)
	case 'I', 'i':
		drawPattern(img, x, y, []string{
			"██████",
			"  ██  ",
			"  ██  ",
			"  ██  ",
			"  ██  ",
			"██████",
		}, textColor)
	case 'J', 'j':
		drawPattern(img, x, y, []string{
			"██████",
			"    ██",
			"    ██",
			"    ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case 'K', 'k':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██ ██ ",
			"████  ",
			"████  ",
			"██ ██ ",
			"██  ██",
		}, textColor)
	case 'L', 'l':
		drawPattern(img, x, y, []string{
			"██    ",
			"██    ",
			"██    ",
			"██    ",
			"██    ",
			"██████",
		}, textColor)
	case 'M', 'm':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██████",
			"██████",
			"██  ██",
			"██  ██",
			"██  ██",
		}, textColor)
	case 'N', 'n':
		drawPattern(img, x, y, []string{
			"██  ██",
			"███ ██",
			"██████",
			"██ ███",
			"██  ██",
			"██  ██",
		}, textColor)
	case 'O', 'o':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case 'P', 'p':
		drawPattern(img, x, y, []string{
			"█████ ",
			"██  ██",
			"██  ██",
			"█████ ",
			"██    ",
			"██    ",
		}, textColor)
	case 'Q', 'q':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██  ██",
			"██  ██",
			"██ ██ ",
			" ██ ██",
		}, textColor)
	case 'R', 'r':
		drawPattern(img, x, y, []string{
			"█████ ",
			"██  ██",
			"██  ██",
			"█████ ",
			"██ ██ ",
			"██  ██",
		}, textColor)
	case 'S', 's':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██    ",
			" ████ ",
			"    ██",
			"█████ ",
		}, textColor)
	case 'T', 't':
		drawPattern(img, x, y, []string{
			"██████",
			"  ██  ",
			"  ██  ",
			"  ██  ",
			"  ██  ",
			"  ██  ",
		}, textColor)
	case 'U', 'u':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case 'V', 'v':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			" ████ ",
			"  ██  ",
		}, textColor)
	case 'W', 'w':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			"██  ██",
			"██████",
			"██████",
			"██  ██",
		}, textColor)
	case 'X', 'x':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			" ████ ",
			" ████ ",
			"██  ██",
			"██  ██",
		}, textColor)
	case 'Y', 'y':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			" ████ ",
			"  ██  ",
			"  ██  ",
			"  ██  ",
		}, textColor)
	case 'Z', 'z':
		drawPattern(img, x, y, []string{
			"██████",
			"    ██",
			"  ██  ",
			" ██   ",
			"██    ",
			"██████",
		}, textColor)
	case '0':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case '1':
		drawPattern(img, x, y, []string{
			"  ██  ",
			" ███  ",
			"  ██  ",
			"  ██  ",
			"  ██  ",
			"██████",
		}, textColor)
	case '2':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"   ██ ",
			" ██   ",
			"██    ",
			"██████",
		}, textColor)
	case '3':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"   ██ ",
			"   ██ ",
			"██  ██",
			" ████ ",
		}, textColor)
	case '4':
		drawPattern(img, x, y, []string{
			"██  ██",
			"██  ██",
			"██████",
			"    ██",
			"    ██",
			"    ██",
		}, textColor)
	case '5':
		drawPattern(img, x, y, []string{
			"██████",
			"██    ",
			"█████ ",
			"    ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case '6':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██    ",
			"█████ ",
			"██  ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case '7':
		drawPattern(img, x, y, []string{
			"██████",
			"    ██",
			"   ██ ",
			"  ██  ",
			" ██   ",
			"██    ",
		}, textColor)
	case '8':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			" ████ ",
			"██  ██",
			"██  ██",
			" ████ ",
		}, textColor)
	case '9':
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██  ██",
			" █████",
			"    ██",
			" ████ ",
		}, textColor)
	case ' ':
		// Space - do nothing
	case '_':
		drawPattern(img, x, y, []string{
			"      ",
			"      ",
			"      ",
			"      ",
			"      ",
			"██████",
		}, textColor)
	case '-':
		drawPattern(img, x, y, []string{
			"      ",
			"      ",
			"      ",
			"██████",
			"      ",
			"      ",
		}, textColor)
	default:
		// For any other character, draw a simple block
		drawPattern(img, x, y, []string{
			" ████ ",
			"██  ██",
			"██  ██",
			"██  ██",
			"██  ██",
			" ████ ",
		}, textColor)
	}
}

// drawPattern draws a pattern on the image
func drawPattern(img *image.RGBA, x, y int, pattern []string, color color.RGBA) {
	for dy, row := range pattern {
		for dx, pixel := range row {
			if pixel == '█' {
				img.Set(x+dx, y+dy+5, color) // +5 to center vertically
			}
		}
	}
}

func main() {
	// Define command-line flags
	var (
		playCmd   = flag.NewFlagSet("play", flag.ExitOnError)
		exportCmd = flag.NewFlagSet("export", flag.ExitOnError)
		
		playFile = playCmd.String("file", "", "Path to the YAML animation file")
		
		exportFile   = exportCmd.String("file", "", "Path to the YAML animation file")
		exportOutput = exportCmd.String("output", "output.gif", "Output GIF file path")
	)
	
	// Check if a command was provided
	if len(os.Args) < 2 {
		fmt.Println("Usage: title-animator [command] [flags]")
		fmt.Println("Commands:")
		fmt.Println("  play    Play the animation in the terminal")
		fmt.Println("  export  Export the animation as a GIF")
		os.Exit(1)
	}
	
	// Parse the command
	switch os.Args[1] {
	case "play":
		playCmd.Parse(os.Args[2:])
		if *playFile == "" {
			fmt.Println("Error: --file flag is required")
			os.Exit(1)
		}
		
		animation, err := LoadAnimation(*playFile)
		if err != nil {
			fmt.Printf("Error loading animation: %v\n", err)
			os.Exit(1)
		}
		
		model := Model{
			animation: animation,
			frames:    animation.Frames,
			width:     animation.Size.Width,
			height:    animation.Size.Height,
			frame:     0,
			done:      false,
		}
		
		p := tea.NewProgram(model, tea.WithAltScreen())
		if _, err := p.Run(); err != nil {
			fmt.Printf("Error running program: %v\n", err)
			os.Exit(1)
		}
		
	case "export":
		exportCmd.Parse(os.Args[2:])
		if *exportFile == "" {
			fmt.Println("Error: --file flag is required")
			os.Exit(1)
		}
		
		animation, err := LoadAnimation(*exportFile)
		if err != nil {
			fmt.Printf("Error loading animation: %v\n", err)
			os.Exit(1)
		}
		
		fmt.Printf("Exporting animation to %s...\n", *exportOutput)
		if err := ExportGIF(animation, *exportOutput); err != nil {
			fmt.Printf("Error exporting GIF: %v\n", err)
			os.Exit(1)
		}
		fmt.Println("Export complete!")
		
	default:
		fmt.Printf("Unknown command: %s\n", os.Args[1])
		os.Exit(1)
	}
}
