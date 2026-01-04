package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"github.com/charmbracelet/lipgloss/v2"
)

// InteractiveWindowManager extends WindowManager with interactive capabilities
type InteractiveWindowManager struct {
	*WindowManager
	Running bool
}

// NewInteractiveWindowManager creates a new interactive window manager
func NewInteractiveWindowManager(width, height int) *InteractiveWindowManager {
	return &InteractiveWindowManager{
		WindowManager: NewWindowManager(width, height),
		Running:       true,
	}
}

// ShowHelp displays available commands
func (iwm *InteractiveWindowManager) ShowHelp() {
	fmt.Println("\n=== Interactive Window Manager Commands ===")
	fmt.Println("create <title> <x> <y> <width> <height> <color> - Create new window")
	fmt.Println("move <id> <x> <y>                               - Move window")
	fmt.Println("resize <id> <width> <height>                    - Resize window")
	fmt.Println("focus <id>                                      - Focus window")
	fmt.Println("close <id>                                      - Close window")
	fmt.Println("list                                            - List all windows")
	fmt.Println("render                                          - Render current state")
	fmt.Println("demo                                            - Run demo sequence")
	fmt.Println("clear                                           - Clear screen")
	fmt.Println("help                                            - Show this help")
	fmt.Println("quit                                            - Exit program")
	fmt.Println("============================================\n")
}

// ProcessCommand processes a user command
func (iwm *InteractiveWindowManager) ProcessCommand(command string) {
	parts := strings.Fields(command)
	if len(parts) == 0 {
		return
	}
	
	cmd := strings.ToLower(parts[0])
	
	switch cmd {
	case "create":
		if len(parts) < 7 {
			fmt.Println("Usage: create <title> <x> <y> <width> <height> <color>")
			return
		}
		
		title := parts[1]
		x, _ := strconv.Atoi(parts[2])
		y, _ := strconv.Atoi(parts[3])
		width, _ := strconv.Atoi(parts[4])
		height, _ := strconv.Atoi(parts[5])
		color := parts[6]
		
		content := fmt.Sprintf("Window: %s\nCreated at (%d, %d)\nSize: %dx%d\nColor: %s", 
			title, x, y, width, height, color)
		
		window := iwm.CreateWindow(title, content, x, y, width, height, color)
		fmt.Printf("Created window %d: %s\n", window.ID, title)
		
	case "move":
		if len(parts) < 4 {
			fmt.Println("Usage: move <id> <x> <y>")
			return
		}
		
		id, _ := strconv.Atoi(parts[1])
		x, _ := strconv.Atoi(parts[2])
		y, _ := strconv.Atoi(parts[3])
		
		window := iwm.findWindow(id)
		if window == nil {
			fmt.Printf("Window %d not found\n", id)
			return
		}
		
		iwm.MoveWindow(window, x, y)
		fmt.Printf("Moved window %d to (%d, %d)\n", id, window.X, window.Y)
		
	case "resize":
		if len(parts) < 4 {
			fmt.Println("Usage: resize <id> <width> <height>")
			return
		}
		
		id, _ := strconv.Atoi(parts[1])
		width, _ := strconv.Atoi(parts[2])
		height, _ := strconv.Atoi(parts[3])
		
		window := iwm.findWindow(id)
		if window == nil {
			fmt.Printf("Window %d not found\n", id)
			return
		}
		
		iwm.ResizeWindow(window, width, height)
		fmt.Printf("Resized window %d to %dx%d\n", id, window.Width, window.Height)
		
	case "focus":
		if len(parts) < 2 {
			fmt.Println("Usage: focus <id>")
			return
		}
		
		id, _ := strconv.Atoi(parts[1])
		window := iwm.findWindow(id)
		if window == nil {
			fmt.Printf("Window %d not found\n", id)
			return
		}
		
		iwm.FocusWindow(window)
		fmt.Printf("Focused window %d: %s\n", id, window.Title)
		
	case "close":
		if len(parts) < 2 {
			fmt.Println("Usage: close <id>")
			return
		}
		
		id, _ := strconv.Atoi(parts[1])
		window := iwm.findWindow(id)
		if window == nil {
			fmt.Printf("Window %d not found\n", id)
			return
		}
		
		title := window.Title
		iwm.CloseWindow(window)
		fmt.Printf("Closed window %d: %s\n", id, title)
		
	case "list":
		fmt.Println(iwm.GetWindowInfo())
		
	case "render":
		fmt.Println(iwm.Render())
		
	case "demo":
		iwm.runDemo()
		
	case "clear":
		fmt.Print("\033[2J\033[H") // Clear screen and move cursor to top
		
	case "help":
		iwm.ShowHelp()
		
	case "quit", "exit":
		iwm.Running = false
		fmt.Println("Goodbye!")
		
	default:
		fmt.Printf("Unknown command: %s\n", cmd)
		fmt.Println("Type 'help' for available commands")
	}
}

// findWindow finds a window by ID
func (iwm *InteractiveWindowManager) findWindow(id int) *Window {
	for _, window := range iwm.Windows {
		if window.ID == id {
			return window
		}
	}
	return nil
}

// runDemo runs a demonstration sequence
func (iwm *InteractiveWindowManager) runDemo() {
	fmt.Println("Running demo sequence...")
	
	// Clear existing windows
	iwm.Windows = make([]*Window, 0)
	iwm.NextID = 1
	iwm.FocusedWindow = nil
	
	// Create demo windows
	fmt.Println("Creating demo windows...")
	
	editor := iwm.CreateWindow(
		"Editor",
		"# Welcome to the Editor\n\nThis is a markdown editor\nwhere you can write and\nedit your documents.\n\nFeatures:\n• Syntax highlighting\n• Auto-save\n• Multiple tabs",
		5, 2, 30, 12,
		"99",
	)
	
	browser := iwm.CreateWindow(
		"Browser",
		"🌐 Web Browser\n\n📁 Bookmarks:\n• GitHub\n• Documentation\n• Stack Overflow\n\n🔍 Search: lipgloss v2",
		20, 5, 25, 10,
		"205",
	)
	
	terminal := iwm.CreateWindow(
		"Terminal",
		"$ pwd\n/home/user/projects\n$ ls -la\ntotal 48\ndrwxr-xr-x  3 user user 4096\n$ go run main.go\nStarting application...",
		35, 8, 28, 8,
		"82",
	)
	
	fmt.Println("Demo windows created!")
	fmt.Println(iwm.Render())
	
	// Demonstrate operations
	fmt.Println("\nDemonstrating window operations...")
	
	fmt.Println("1. Focusing Editor window:")
	iwm.FocusWindow(editor)
	fmt.Println(iwm.Render())
	
	fmt.Println("2. Moving Browser window:")
	iwm.MoveWindow(browser, 40, 3)
	fmt.Println(iwm.Render())
	
	fmt.Println("3. Resizing Terminal window:")
	iwm.ResizeWindow(terminal, 35, 10)
	fmt.Println(iwm.Render())
	
	fmt.Println("Demo sequence complete!")
}

// Run starts the interactive session
func (iwm *InteractiveWindowManager) Run() {
	fmt.Println("=== Interactive Window Manager ===")
	fmt.Println("Lipgloss v2 Compositing Demo")
	fmt.Printf("Screen size: %dx%d\n", iwm.ScreenWidth, iwm.ScreenHeight)
	
	iwm.ShowHelp()
	
	// Initial render
	fmt.Println("Initial desktop:")
	fmt.Println(iwm.Render())
	
	scanner := bufio.NewScanner(os.Stdin)
	
	for iwm.Running {
		fmt.Print("> ")
		if !scanner.Scan() {
			break
		}
		
		command := strings.TrimSpace(scanner.Text())
		if command != "" {
			iwm.ProcessCommand(command)
		}
	}
}

func main() {
	// Check if running in interactive mode
	if len(os.Args) > 1 && os.Args[1] == "interactive" {
		iwm := NewInteractiveWindowManager(80, 25)
		iwm.Run()
	} else {
		fmt.Println("=== Interactive Window Manager Demo ===")
		fmt.Println("Run with 'go run interactive.go interactive' for interactive mode")
		fmt.Println("Or run the basic demo below:\n")
		
		// Run basic demo
		iwm := NewInteractiveWindowManager(70, 20)
		iwm.runDemo()
	}
}

