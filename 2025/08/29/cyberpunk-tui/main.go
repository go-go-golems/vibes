package main

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/components/editor"
	"cyberpunk-tui/components/sidebar"
	"cyberpunk-tui/components/sprites"
	"cyberpunk-tui/components/terminal"
	"cyberpunk-tui/components/topbar"
	"cyberpunk-tui/models"
	"cyberpunk-tui/styles"
	"flag"
	"fmt"
	"log"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss/v2"
)

// App represents the main application
type App struct {
	width  int
	height int
	
	// Components
	topBar        *topbar.TopBar
	leftSidebar   *sidebar.FullLeftSidebar
	rightSidebar  *sidebar.FullRightSidebar
	codeEditor    *editor.CodeEditor
	terminal      *terminal.Terminal
	spriteSystem  *sprites.SpriteSystem
	
	// Animation system
	animationClock *animations.AnimationClock
	
	// Configuration
	glitchModeEnabled bool
	
	// Status bar
	statusBar string
}

// NewApp creates a new application instance
func NewApp(glitchModeEnabled bool) *App {
	statusMsg := "░▓█ CYBERPUNK EBOY TUI v1.0 █▓░ Press 'q' to quit"
	if glitchModeEnabled {
		statusMsg += ", 'g' for glitch mode"
	} else {
		statusMsg += " (use --glitch flag for chaos mode)"
	}
	
	return &App{
		width:             120, // Start with larger default
		height:            30,
		animationClock:    animations.NewAnimationClock(),
		glitchModeEnabled: glitchModeEnabled,
		statusBar:         statusMsg,
	}
}

// Init implements tea.Model
func (a *App) Init() tea.Cmd {
	// Don't initialize components here - wait for first WindowSizeMsg
	return tea.Batch(
		tea.EnterAltScreen,
		tickCmd(),
	)
}

// initializeComponents initializes all UI components
func (a *App) initializeComponents() {
	// Calculate layout dimensions
	sidebarWidth := 16
	mainContentWidth := a.width - (2 * sidebarWidth)
	contentHeight := a.height - 2 // Reserve space for top and status bars
	
	// Initialize components with new full sidebars
	a.topBar = topbar.NewTopBar(a.width)
	a.leftSidebar = sidebar.NewFullLeftSidebar(sidebarWidth, contentHeight)
	a.rightSidebar = sidebar.NewFullRightSidebar(sidebarWidth, contentHeight)
	
	// Main content area split between editor and terminal
	editorHeight := (contentHeight * 2) / 3  // 2/3 for editor
	terminalHeight := contentHeight - editorHeight // 1/3 for terminal
	
	a.codeEditor = editor.NewCodeEditor(mainContentWidth, editorHeight)
	a.terminal = terminal.NewTerminal(mainContentWidth, terminalHeight)
	a.spriteSystem = sprites.NewSpriteSystem(a.width, a.height)
}

// Update implements tea.Model
func (a *App) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		// Initialize components on first resize if not already done
		if a.topBar == nil {
			a.width = msg.Width
			a.height = msg.Height
			a.initializeComponents()
		} else {
			a.handleResize(msg.Width, msg.Height)
		}
		return a, nil
		
	case tea.KeyMsg:
		return a.handleKeyPress(msg)
		
	case tickMsg:
		a.animationClock.Tick()
		if a.topBar != nil { // Only update if components are initialized
			a.updateAllAnimations()
		}
		return a, tickCmd()
		
	case models.GlitchMsg:
		// Handle manual glitch trigger
		return a, nil
	}
	
	return a, nil
}

// handleResize handles terminal resize events
func (a *App) handleResize(width, height int) {
	a.width = width
	a.height = height
	
	// Recalculate layout
	sidebarWidth := 16
	if width < 80 {
		sidebarWidth = width / 5 // Adjust for smaller terminals
	}
	
	mainContentWidth := width - (2 * sidebarWidth)
	contentHeight := height - 2
	
	// Resize all components
	resizeMsg := models.ResizeMsg{Width: width, Height: height}
	
	a.topBar.Resize(width, 1)
	a.leftSidebar.Resize(sidebarWidth, contentHeight)
	a.rightSidebar.Resize(sidebarWidth, contentHeight)
	
	editorHeight := (contentHeight * 2) / 3
	terminalHeight := contentHeight - editorHeight
	
	a.codeEditor.Resize(mainContentWidth, editorHeight)
	a.terminal.Resize(mainContentWidth, terminalHeight)
	a.spriteSystem.Resize(width, height)
	
	// Update components
	a.topBar.Update(resizeMsg)
	a.leftSidebar.Update(resizeMsg)
	a.rightSidebar.Update(resizeMsg)
	a.codeEditor.Update(resizeMsg)
	a.terminal.Update(resizeMsg)
	a.spriteSystem.Update(resizeMsg)
}

// handleKeyPress handles keyboard input
func (a *App) handleKeyPress(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "q", "ctrl+c":
		return a, tea.Quit
		
	case "g":
		// Only allow manual glitch mode if glitch mode is enabled
		if a.glitchModeEnabled {
			return a, func() tea.Msg { return models.GlitchMsg{} }
		}
		return a, nil
		
	case "r":
		// Refresh/reset
		a.initializeComponents()
		return a, nil
	}
	
	return a, nil
}

// updateAllAnimations updates animations for all components
func (a *App) updateAllAnimations() {
	a.topBar.UpdateAnimation(a.animationClock)
	a.leftSidebar.UpdateAnimation(a.animationClock)
	a.rightSidebar.UpdateAnimation(a.animationClock)
	a.codeEditor.UpdateAnimation(a.animationClock)
	a.terminal.UpdateAnimation(a.animationClock)
	a.spriteSystem.UpdateAnimation(a.animationClock)
}

// View implements tea.Model
func (a *App) View() string {
	if a.topBar == nil {
		return "Initializing..."
	}
	
	// Create the main layout using traditional lipgloss
	topBarView := a.topBar.View()
	statusBarView := a.renderStatusBar()
	
	// Create main content layout
	leftSidebarView := a.leftSidebar.View()
	rightSidebarView := a.rightSidebar.View()
	
	// Main content area (editor + terminal)
	editorView := a.codeEditor.View()
	terminalView := a.terminal.View()
	
	// Combine editor and terminal vertically
	mainContent := lipgloss.JoinVertical(
		lipgloss.Left,
		editorView,
		terminalView,
	)
	
	// Create horizontal layout for main content area
	contentRow := lipgloss.JoinHorizontal(
		lipgloss.Top,
		leftSidebarView,
		mainContent,
		rightSidebarView,
	)
	
	// Combine all sections vertically to create base layout
	baseView := lipgloss.JoinVertical(
		lipgloss.Left,
		topBarView,
		contentRow,
		statusBarView,
	)
	
	// Apply sprite overlay using a simpler approach
	return a.applySimpleOverlay(baseView)
}

// applySimpleOverlay applies sprite overlay using a safer approach
func (a *App) applySimpleOverlay(baseView string) string {
	if a.spriteSystem == nil {
		return baseView
	}
	
	lines := strings.Split(baseView, "\n")
	overlay := a.spriteSystem.GetOverlay()
	
	// Apply overlay character by character, being very careful about bounds
	for y := 0; y < len(lines) && y < len(overlay); y++ {
		line := lines[y]
		lineRunes := []rune(line)
		overlayRow := overlay[y]
		
		// Create new line with overlay applied
		newLineRunes := make([]rune, len(lineRunes))
		copy(newLineRunes, lineRunes)
		
		// Apply overlay characters without changing string length
		for x := 0; x < len(newLineRunes) && x < len(overlayRow); x++ {
			if overlayRow[x] != "" {
				// Extract the first rune from the overlay string (which may contain ANSI codes)
				overlayRunes := []rune(overlayRow[x])
				if len(overlayRunes) > 0 {
					// For now, just use the base character without ANSI codes to avoid corruption
					// This will show sprites but without colors - safer approach
					if len(overlayRunes) == 1 {
						newLineRunes[x] = overlayRunes[0]
					}
				}
			}
		}
		
		lines[y] = string(newLineRunes)
	}
	
	return strings.Join(lines, "\n")
}

// createSpriteLayers creates sprite layers for the compositing system
func (a *App) createSpriteLayers() []*lipgloss.Layer {
	var layers []*lipgloss.Layer
	
	// Get sprite overlay data
	overlay := a.spriteSystem.GetOverlay()
	
	// Group sprites by rows to reduce layer count
	for y, row := range overlay {
		var rowContent strings.Builder
		hasContent := false
		
		for x, char := range row {
			if char != "" {
				// Pad to correct position
				for rowContent.Len() < x {
					rowContent.WriteString(" ")
				}
				rowContent.WriteString(char)
				hasContent = true
			}
		}
		
		if hasContent {
			// Create a layer for this row
			layer := lipgloss.NewLayer(rowContent.String()).X(0).Y(y).Z(10)
			layers = append(layers, layer)
		}
	}
	
	return layers
}

// renderStatusBar renders the status bar
func (a *App) renderStatusBar() string {
	statusStyle := styles.PureBlackBg.
		Foreground(lipgloss.Color(styles.NeonLime)).
		Width(a.width).
		Align(lipgloss.Center)
	
	return statusStyle.Render(a.statusBar)
}

// tickMsg represents a tick message for animations
type tickMsg time.Time

// tickCmd returns a command that sends tick messages
func tickCmd() tea.Cmd {
	return tea.Tick(100*time.Millisecond, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

func main() {
	// Parse command-line flags
	var glitchMode = flag.Bool("glitch", false, "Enable automatic glitch mode (chaos mode)")
	var help = flag.Bool("help", false, "Show help information")
	flag.Parse()
	
	if *help {
		fmt.Println("Cyberpunk Eboy TUI - A chaotic terminal interface")
		fmt.Println("")
		fmt.Println("Usage: cyberpunk-tui [options]")
		fmt.Println("")
		fmt.Println("Options:")
		fmt.Println("  --glitch    Enable automatic glitch mode (warning: maximum chaos!)")
		fmt.Println("  --help      Show this help message")
		fmt.Println("")
		fmt.Println("Controls:")
		fmt.Println("  q, Ctrl+C  - Quit")
		if *glitchMode {
			fmt.Println("  g          - Trigger manual glitch mode")
		}
		fmt.Println("  r          - Refresh/reset")
		fmt.Println("")
		fmt.Println("Minimum terminal size: 80x25")
		fmt.Println("Recommended: 120x30 or larger")
		fmt.Println("")
		fmt.Println("Note: Use TERM=xterm-256color for best color support")
		return
	}
	
	// Create and run the application
	app := NewApp(*glitchMode)
	
	p := tea.NewProgram(
		app,
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)
	
	if _, err := p.Run(); err != nil {
		log.Fatal(err)
	}
}

