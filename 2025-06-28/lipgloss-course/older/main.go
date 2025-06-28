package main

import (
	"fmt"
	"time"
	"github.com/charmbracelet/lipgloss/v2"
)

// Window represents a single window in the window manager
type Window struct {
	ID       int
	Title    string
	Content  string
	X, Y     int
	Width    int
	Height   int
	ZIndex   int
	Focused  bool
	Visible  bool
	Color    string
}

// WindowManager manages multiple windows
type WindowManager struct {
	Windows       []*Window
	ScreenWidth   int
	ScreenHeight  int
	NextID        int
	FocusedWindow *Window
}

// NewWindowManager creates a new window manager
func NewWindowManager(width, height int) *WindowManager {
	return &WindowManager{
		Windows:      make([]*Window, 0),
		ScreenWidth:  width,
		ScreenHeight: height,
		NextID:       1,
	}
}

// CreateWindow creates a new window
func (wm *WindowManager) CreateWindow(title, content string, x, y, width, height int, color string) *Window {
	window := &Window{
		ID:      wm.NextID,
		Title:   title,
		Content: content,
		X:       x,
		Y:       y,
		Width:   width,
		Height:  height,
		ZIndex:  wm.NextID, // Higher ID = higher Z-index by default
		Focused: false,
		Visible: true,
		Color:   color,
	}
	
	wm.NextID++
	wm.Windows = append(wm.Windows, window)
	wm.FocusWindow(window)
	
	return window
}

// FocusWindow brings a window to the front and focuses it
func (wm *WindowManager) FocusWindow(window *Window) {
	// Unfocus all windows
	for _, w := range wm.Windows {
		w.Focused = false
	}
	
	// Focus the selected window and bring to front
	window.Focused = true
	wm.FocusedWindow = window
	
	// Find the highest Z-index
	maxZ := 0
	for _, w := range wm.Windows {
		if w.ZIndex > maxZ {
			maxZ = w.ZIndex
		}
	}
	
	// Set focused window to highest Z-index + 1
	window.ZIndex = maxZ + 1
}

// MoveWindow moves a window to a new position
func (wm *WindowManager) MoveWindow(window *Window, newX, newY int) {
	// Keep window within screen bounds
	if newX < 0 {
		newX = 0
	}
	if newY < 0 {
		newY = 0
	}
	if newX+window.Width > wm.ScreenWidth {
		newX = wm.ScreenWidth - window.Width
	}
	if newY+window.Height > wm.ScreenHeight {
		newY = wm.ScreenHeight - window.Height
	}
	
	window.X = newX
	window.Y = newY
}

// ResizeWindow resizes a window
func (wm *WindowManager) ResizeWindow(window *Window, newWidth, newHeight int) {
	// Minimum size constraints
	if newWidth < 10 {
		newWidth = 10
	}
	if newHeight < 5 {
		newHeight = 5
	}
	
	// Keep window within screen bounds
	if window.X+newWidth > wm.ScreenWidth {
		newWidth = wm.ScreenWidth - window.X
	}
	if window.Y+newHeight > wm.ScreenHeight {
		newHeight = wm.ScreenHeight - window.Y
	}
	
	window.Width = newWidth
	window.Height = newHeight
}

// CloseWindow removes a window from the manager
func (wm *WindowManager) CloseWindow(window *Window) {
	for i, w := range wm.Windows {
		if w.ID == window.ID {
			wm.Windows = append(wm.Windows[:i], wm.Windows[i+1:]...)
			break
		}
	}
	
	// Focus another window if the closed window was focused
	if wm.FocusedWindow == window {
		wm.FocusedWindow = nil
		if len(wm.Windows) > 0 {
			wm.FocusWindow(wm.Windows[len(wm.Windows)-1])
		}
	}
}

// RenderWindow renders a single window as a lipgloss layer
func (wm *WindowManager) RenderWindow(window *Window) *lipgloss.Layer {
	// Title bar style
	titleBarColor := window.Color
	if window.Focused {
		titleBarColor = "99" // Bright blue for focused windows
	}
	
	titleBarStyle := lipgloss.NewStyle().
		Background(lipgloss.Color(titleBarColor)).
		Foreground(lipgloss.Color("255")).
		Bold(true).
		Width(window.Width-2).
		Padding(0, 1)
	
	// Window controls (close button)
	controls := "✕"
	if window.Width > 15 {
		controls = " [✕]"
	}
	
	titleText := window.Title
	if len(titleText) > window.Width-8 {
		titleText = titleText[:window.Width-11] + "..."
	}
	
	// Create title bar with controls
	titleBar := titleBarStyle.Render(titleText + controls)
	
	// Content area style
	contentStyle := lipgloss.NewStyle().
		Width(window.Width-2).
		Height(window.Height-3).
		Padding(1)
	
	contentArea := contentStyle.Render(window.Content)
	
	// Window border style
	borderStyle := lipgloss.NewStyle().
		Width(window.Width).
		Height(window.Height).
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color(titleBarColor))
	
	// Combine title bar and content
	fullWindow := titleBar + "\n" + contentArea
	
	// Create the complete window
	windowLayer := lipgloss.NewLayer(
		borderStyle.Render(fullWindow),
	)
	
	return windowLayer.X(window.X).Y(window.Y).Z(window.ZIndex)
}

// Render renders the complete window manager scene
func (wm *WindowManager) Render() string {
	// Create desktop background
	desktopStyle := lipgloss.NewStyle().
		Width(wm.ScreenWidth).
		Height(wm.ScreenHeight).
		Background(lipgloss.Color("235")).
		Foreground(lipgloss.Color("245"))
	
	desktopContent := "Desktop Environment\n\nWindow Manager Demo\nLipgloss v2 Compositing\n\nWindows can be:\n• Moved\n• Resized\n• Focused\n• Overlapped\n• Closed"
	
	desktop := lipgloss.NewLayer(
		desktopStyle.Render(desktopContent),
	)
	
	// Create layers for all windows
	var layers []*lipgloss.Layer
	layers = append(layers, desktop.X(0).Y(0).Z(0)) // Desktop at bottom
	
	// Add all visible windows
	for _, window := range wm.Windows {
		if window.Visible {
			layers = append(layers, wm.RenderWindow(window))
		}
	}
	
	// Create and render the canvas
	canvas := lipgloss.NewCanvas(layers...)
	return canvas.Render()
}

// GetWindowInfo returns information about all windows
func (wm *WindowManager) GetWindowInfo() string {
	info := fmt.Sprintf("Window Manager Status:\n")
	info += fmt.Sprintf("Screen: %dx%d\n", wm.ScreenWidth, wm.ScreenHeight)
	info += fmt.Sprintf("Windows: %d\n\n", len(wm.Windows))
	
	for _, window := range wm.Windows {
		focusedStr := ""
		if window.Focused {
			focusedStr = " [FOCUSED]"
		}
		info += fmt.Sprintf("Window %d: %s%s\n", window.ID, window.Title, focusedStr)
		info += fmt.Sprintf("  Position: (%d, %d)\n", window.X, window.Y)
		info += fmt.Sprintf("  Size: %dx%d\n", window.Width, window.Height)
		info += fmt.Sprintf("  Z-Index: %d\n\n", window.ZIndex)
	}
	
	return info
}

func main() {
	fmt.Println("=== Window Manager Demo ===\n")
	
	// Create window manager
	wm := NewWindowManager(80, 25)
	
	// Demo 1: Basic window creation
	fmt.Println("1. Basic Window Creation:")
	
	window1 := wm.CreateWindow(
		"Text Editor",
		"This is a text editor window\nwith multiple lines of content.\n\nYou can edit text here\nand save your work.",
		5, 3, 25, 8,
		"99",
	)
	
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Demo 2: Multiple overlapping windows
	fmt.Println("2. Multiple Overlapping Windows:")
	
	window2 := wm.CreateWindow(
		"File Browser",
		"📁 Documents/\n📁 Pictures/\n📁 Downloads/\n📄 readme.txt\n📄 config.json",
		15, 6, 22, 7,
		"205",
	)
	
	window3 := wm.CreateWindow(
		"Terminal",
		"$ ls -la\ntotal 42\ndrwxr-xr-x  5 user user 4096\n$ cd projects/\n$ make build\nBuilding...",
		25, 9, 28, 6,
		"82",
	)
	
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Demo 3: Window management operations
	fmt.Println("3. Window Management Operations:")
	
	// Focus different window
	wm.FocusWindow(window1)
	fmt.Println("Focused Text Editor:")
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Move window
	fmt.Println("Moved File Browser:")
	wm.MoveWindow(window2, 35, 2)
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Resize window
	fmt.Println("Resized Terminal:")
	wm.ResizeWindow(window3, 35, 8)
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Demo 4: Complex window arrangement
	fmt.Println("4. Complex Window Arrangement:")
	
	// Create more windows
	window4 := wm.CreateWindow(
		"Calculator",
		"┌─────────────┐\n│    123.45   │\n├─────────────┤\n│ 7 8 9  /    │\n│ 4 5 6  *    │\n│ 1 2 3  -    │\n│ 0 . =  +    │\n└─────────────┘",
		50, 12, 17, 10,
		"228",
	)
	
	window5 := wm.CreateWindow(
		"Notes",
		"📝 Meeting Notes\n\n• Discuss project timeline\n• Review budget\n• Plan next sprint\n• Update documentation",
		2, 15, 24, 8,
		"201",
	)
	
	// Focus the notes window to show it's interactive
	wm.FocusWindow(window5)
	
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Demo 5: Window information
	fmt.Println("5. Window Manager Information:")
	fmt.Println(wm.GetWindowInfo())
	
	// Demo 6: Closing windows
	fmt.Println("6. Closing Windows:")
	
	fmt.Println("Before closing Terminal:")
	fmt.Printf("Total windows: %d\n\n", len(wm.Windows))
	
	wm.CloseWindow(window3)
	
	fmt.Println("After closing Terminal:")
	fmt.Printf("Total windows: %d\n", len(wm.Windows))
	fmt.Println(wm.Render())
	fmt.Println()
	
	// Demo 7: Animated window movement simulation
	fmt.Println("7. Animated Window Movement Simulation:")
	
	originalX, originalY := window4.X, window4.Y
	
	// Simulate moving calculator window
	positions := []struct {
		x, y int
		desc string
	}{
		{50, 12, "Original position"},
		{45, 10, "Moving up-left"},
		{40, 8, "Continuing movement"},
		{35, 6, "Near center"},
		{30, 4, "Final position"},
	}
	
	for i, pos := range positions {
		fmt.Printf("Frame %d - %s:\n", i+1, pos.desc)
		wm.MoveWindow(window4, pos.x, pos.y)
		fmt.Println(wm.Render())
		fmt.Println()
		
		// Simulate delay
		time.Sleep(100 * time.Millisecond)
	}
	
	// Reset position
	wm.MoveWindow(window4, originalX, originalY)
	
	// Demo 8: Z-index management
	fmt.Println("8. Z-index Management:")
	
	fmt.Println("Current Z-order (highest to lowest):")
	for _, w := range wm.Windows {
		fmt.Printf("  %s (Z=%d)\n", w.Title, w.ZIndex)
	}
	fmt.Println()
	
	fmt.Println("Bringing Text Editor to front:")
	wm.FocusWindow(window1)
	fmt.Println(wm.Render())
	fmt.Println()
	
	fmt.Println("Final window arrangement:")
	fmt.Println(wm.GetWindowInfo())
	
	fmt.Println("Window Manager Demo Complete!")
	fmt.Println("This demonstrates the power of Lipgloss v2's compositing system")
	fmt.Println("for creating complex, layered terminal interfaces.")
}

