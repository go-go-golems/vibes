package main

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/components/editor"
	"cyberpunk-tui/components/sidebar"
	"cyberpunk-tui/components/sprites"
	"cyberpunk-tui/components/terminal"
	"cyberpunk-tui/components/topbar"
	"cyberpunk-tui/styles"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss/v2"
)

type TestApp struct {
	animationClock *animations.AnimationClock
	frameCounter   int
}

func NewTestApp() *TestApp {
	return &TestApp{
		animationClock: animations.NewAnimationClock(),
		frameCounter:   0,
	}
}

func main() {
	if len(os.Args) < 2 {
		printUsage()
		return
	}

	app := NewTestApp()
	
	switch os.Args[1] {
	case "topbar":
		app.testTopBar()
	case "left-sidebar":
		app.testLeftSidebar()
	case "right-sidebar":
		app.testRightSidebar()
	case "editor":
		app.testEditor()
	case "terminal":
		app.testTerminal()
	case "sprites":
		app.testSprites()
	case "composite-sidebars":
		app.testCompositeSidebars()
	case "composite-main":
		app.testCompositeMain()
	case "full-layout":
		app.testFullLayout()
	case "animate":
		if len(os.Args) < 4 {
			fmt.Println("Usage: go run test_app.go animate <component> <frames>")
			return
		}
		component := os.Args[2]
		frames, _ := strconv.Atoi(os.Args[3])
		app.testAnimation(component, frames)
	case "save-frames":
		if len(os.Args) < 5 {
			fmt.Println("Usage: go run test_app.go save-frames <component> <frames> <output-dir>")
			return
		}
		component := os.Args[2]
		frames, _ := strconv.Atoi(os.Args[3])
		outputDir := os.Args[4]
		app.saveAnimationFrames(component, frames, outputDir)
	default:
		printUsage()
	}
}

func printUsage() {
	fmt.Println("Cyberpunk TUI Component Tester")
	fmt.Println("")
	fmt.Println("Usage:")
	fmt.Println("  go run test_app.go <command> [args]")
	fmt.Println("")
	fmt.Println("Commands:")
	fmt.Println("  topbar              - Test top bar component")
	fmt.Println("  left-sidebar        - Test left sidebar component")
	fmt.Println("  right-sidebar       - Test right sidebar component")
	fmt.Println("  editor              - Test code editor component")
	fmt.Println("  terminal            - Test terminal component")
	fmt.Println("  sprites             - Test sprite system")
	fmt.Println("  composite-sidebars  - Test both sidebars together")
	fmt.Println("  composite-main      - Test main content area")
	fmt.Println("  full-layout         - Test complete layout")
	fmt.Println("  animate <comp> <n>  - Show n animation frames of component")
	fmt.Println("  save-frames <comp> <n> <dir> - Save n frames to directory")
	fmt.Println("")
	fmt.Println("Components for animate/save-frames:")
	fmt.Println("  topbar, left-sidebar, right-sidebar, editor, terminal, sprites")
}

func (app *TestApp) testTopBar() {
	fmt.Println("=== TOP BAR TEST ===")
	
	topBar := topbar.NewTopBar(120)
	
	fmt.Println("Initial render:")
	view := topBar.View()
	app.printWithBorder(view, "TopBar")
	
	fmt.Println("\nAfter 5 animation ticks:")
	for i := 0; i < 5; i++ {
		app.animationClock.Tick()
		topBar.UpdateAnimation(app.animationClock)
	}
	view = topBar.View()
	app.printWithBorder(view, "TopBar (Animated)")
}

func (app *TestApp) testLeftSidebar() {
	fmt.Println("=== LEFT SIDEBAR TEST ===")
	
	leftSidebar := sidebar.NewLeftSidebar(20, 25)
	
	fmt.Println("Initial render:")
	view := leftSidebar.View()
	app.printWithBorder(view, "LeftSidebar")
	
	fmt.Println("\nAfter 10 animation ticks:")
	for i := 0; i < 10; i++ {
		app.animationClock.Tick()
		leftSidebar.UpdateAnimation(app.animationClock)
	}
	view = leftSidebar.View()
	app.printWithBorder(view, "LeftSidebar (Animated)")
}

func (app *TestApp) testRightSidebar() {
	fmt.Println("=== RIGHT SIDEBAR TEST ===")
	
	rightSidebar := sidebar.NewRightSidebar(20, 25)
	
	fmt.Println("Initial render:")
	view := rightSidebar.View()
	app.printWithBorder(view, "RightSidebar")
	
	fmt.Println("\nAfter 10 animation ticks:")
	for i := 0; i < 10; i++ {
		app.animationClock.Tick()
		rightSidebar.UpdateAnimation(app.animationClock)
	}
	view = rightSidebar.View()
	app.printWithBorder(view, "RightSidebar (Animated)")
}

func (app *TestApp) testEditor() {
	fmt.Println("=== CODE EDITOR TEST ===")
	
	codeEditor := editor.NewCodeEditor(80, 20)
	
	fmt.Println("Initial render:")
	view := codeEditor.View()
	app.printWithBorder(view, "CodeEditor")
	
	fmt.Println("\nAfter 10 animation ticks:")
	for i := 0; i < 10; i++ {
		app.animationClock.Tick()
		codeEditor.UpdateAnimation(app.animationClock)
	}
	view = codeEditor.View()
	app.printWithBorder(view, "CodeEditor (Animated)")
}

func (app *TestApp) testTerminal() {
	fmt.Println("=== TERMINAL TEST ===")
	
	terminal := terminal.NewTerminal(80, 8)
	
	fmt.Println("Initial render:")
	view := terminal.View()
	app.printWithBorder(view, "Terminal")
	
	fmt.Println("\nAfter 10 animation ticks:")
	for i := 0; i < 10; i++ {
		app.animationClock.Tick()
		terminal.UpdateAnimation(app.animationClock)
	}
	view = terminal.View()
	app.printWithBorder(view, "Terminal (Animated)")
}

func (app *TestApp) testSprites() {
	fmt.Println("=== SPRITE SYSTEM TEST ===")
	
	spriteSystem := sprites.NewSpriteSystem(80, 25)
	
	fmt.Println("Initial state:")
	overlay := spriteSystem.GetOverlay()
	app.printSpriteOverlay(overlay, "Sprites")
	
	fmt.Println("\nAfter 20 animation ticks:")
	for i := 0; i < 20; i++ {
		app.animationClock.Tick()
		spriteSystem.UpdateAnimation(app.animationClock)
	}
	overlay = spriteSystem.GetOverlay()
	app.printSpriteOverlay(overlay, "Sprites (Animated)")
}

func (app *TestApp) testCompositeSidebars() {
	fmt.Println("=== COMPOSITE SIDEBARS TEST ===")
	
	leftSidebar := sidebar.NewLeftSidebar(20, 25)
	rightSidebar := sidebar.NewRightSidebar(20, 25)
	
	// Animate both
	for i := 0; i < 5; i++ {
		app.animationClock.Tick()
		leftSidebar.UpdateAnimation(app.animationClock)
		rightSidebar.UpdateAnimation(app.animationClock)
	}
	
	leftView := leftSidebar.View()
	rightView := rightSidebar.View()
	
	// Create horizontal layout
	composite := lipgloss.JoinHorizontal(
		lipgloss.Top,
		leftView,
		strings.Repeat(" ", 40), // Spacer
		rightView,
	)
	
	app.printWithBorder(composite, "Composite Sidebars")
}

func (app *TestApp) testCompositeMain() {
	fmt.Println("=== COMPOSITE MAIN CONTENT TEST ===")
	
	codeEditor := editor.NewCodeEditor(80, 15)
	terminal := terminal.NewTerminal(80, 8)
	
	// Animate both
	for i := 0; i < 5; i++ {
		app.animationClock.Tick()
		codeEditor.UpdateAnimation(app.animationClock)
		terminal.UpdateAnimation(app.animationClock)
	}
	
	editorView := codeEditor.View()
	terminalView := terminal.View()
	
	// Create vertical layout
	composite := lipgloss.JoinVertical(
		lipgloss.Left,
		editorView,
		terminalView,
	)
	
	app.printWithBorder(composite, "Composite Main Content")
}

func (app *TestApp) testFullLayout() {
	fmt.Println("=== FULL LAYOUT TEST ===")
	
	width := 120
	height := 30
	
	// Initialize all components
	topBar := topbar.NewTopBar(width)
	leftSidebar := sidebar.NewLeftSidebar(20, height-2)
	rightSidebar := sidebar.NewRightSidebar(20, height-2)
	codeEditor := editor.NewCodeEditor(80, 18)
	terminal := terminal.NewTerminal(80, 8)
	
	// Animate all components
	for i := 0; i < 5; i++ {
		app.animationClock.Tick()
		topBar.UpdateAnimation(app.animationClock)
		leftSidebar.UpdateAnimation(app.animationClock)
		rightSidebar.UpdateAnimation(app.animationClock)
		codeEditor.UpdateAnimation(app.animationClock)
		terminal.UpdateAnimation(app.animationClock)
	}
	
	// Build layout
	topBarView := topBar.View()
	
	mainContent := lipgloss.JoinVertical(
		lipgloss.Left,
		codeEditor.View(),
		terminal.View(),
	)
	
	contentRow := lipgloss.JoinHorizontal(
		lipgloss.Top,
		leftSidebar.View(),
		mainContent,
		rightSidebar.View(),
	)
	
	statusStyle := styles.PureBlackBg.
		Foreground(lipgloss.Color(styles.NeonLime)).
		Width(width).
		Align(lipgloss.Center)
	
	statusBarView := statusStyle.Render("░▓█ CYBERPUNK EBOY TUI v1.0 █▓░ Press 'q' to quit, 'g' for glitch mode")
	
	fullLayout := lipgloss.JoinVertical(
		lipgloss.Left,
		topBarView,
		contentRow,
		statusBarView,
	)
	
	app.printWithBorder(fullLayout, "Full Layout")
}

func (app *TestApp) testAnimation(component string, frames int) {
	fmt.Printf("=== ANIMATION TEST: %s (%d frames) ===\n", component, frames)
	
	var comp interface {
		UpdateAnimation(*animations.AnimationClock)
		View() string
	}
	
	switch component {
	case "topbar":
		comp = topbar.NewTopBar(120)
	case "left-sidebar":
		comp = sidebar.NewLeftSidebar(20, 25)
	case "right-sidebar":
		comp = sidebar.NewRightSidebar(20, 25)
	case "editor":
		comp = editor.NewCodeEditor(80, 20)
	case "terminal":
		comp = terminal.NewTerminal(80, 8)
	default:
		fmt.Printf("Unknown component: %s\n", component)
		return
	}
	
	for frame := 0; frame < frames; frame++ {
		fmt.Printf("\n--- Frame %d ---\n", frame)
		
		view := comp.View()
		app.printWithBorder(view, fmt.Sprintf("%s Frame %d", component, frame))
		
		// Advance animation
		app.animationClock.Tick()
		comp.UpdateAnimation(app.animationClock)
		
		// Pause between frames for readability
		time.Sleep(100 * time.Millisecond)
	}
}

func (app *TestApp) saveAnimationFrames(component string, frames int, outputDir string) {
	fmt.Printf("=== SAVING ANIMATION FRAMES: %s (%d frames) ===\n", component, frames)
	
	// Create output directory
	err := os.MkdirAll(outputDir, 0755)
	if err != nil {
		fmt.Printf("Error creating output directory: %v\n", err)
		return
	}
	
	var comp interface {
		UpdateAnimation(*animations.AnimationClock)
		View() string
	}
	
	switch component {
	case "topbar":
		comp = topbar.NewTopBar(120)
	case "left-sidebar":
		comp = sidebar.NewLeftSidebar(20, 25)
	case "right-sidebar":
		comp = sidebar.NewRightSidebar(20, 25)
	case "editor":
		comp = editor.NewCodeEditor(80, 20)
	case "terminal":
		comp = terminal.NewTerminal(80, 8)
	case "sprites":
		spriteSystem := sprites.NewSpriteSystem(80, 25)
		for frame := 0; frame < frames; frame++ {
			filename := filepath.Join(outputDir, fmt.Sprintf("%s_frame_%03d.txt", component, frame))
			
			overlay := spriteSystem.GetOverlay()
			content := app.formatSpriteOverlay(overlay)
			
			err := os.WriteFile(filename, []byte(content), 0644)
			if err != nil {
				fmt.Printf("Error writing frame %d: %v\n", frame, err)
				continue
			}
			
			fmt.Printf("Saved frame %d to %s\n", frame, filename)
			
			// Advance animation
			app.animationClock.Tick()
			spriteSystem.UpdateAnimation(app.animationClock)
		}
		return
	default:
		fmt.Printf("Unknown component: %s\n", component)
		return
	}
	
	for frame := 0; frame < frames; frame++ {
		filename := filepath.Join(outputDir, fmt.Sprintf("%s_frame_%03d.txt", component, frame))
		
		view := comp.View()
		content := fmt.Sprintf("=== %s Frame %d ===\n%s\n", component, frame, view)
		
		err := os.WriteFile(filename, []byte(content), 0644)
		if err != nil {
			fmt.Printf("Error writing frame %d: %v\n", frame, err)
			continue
		}
		
		fmt.Printf("Saved frame %d to %s\n", frame, filename)
		
		// Advance animation
		app.animationClock.Tick()
		comp.UpdateAnimation(app.animationClock)
	}
	
	fmt.Printf("All frames saved to %s\n", outputDir)
}

func (app *TestApp) printWithBorder(content, title string) {
	border := strings.Repeat("=", 80)
	fmt.Printf("%s\n", border)
	fmt.Printf("=== %s ===\n", title)
	fmt.Printf("%s\n", border)
	fmt.Printf("%s\n", content)
	fmt.Printf("%s\n", border)
}

func (app *TestApp) printSpriteOverlay(overlay [][]string, title string) {
	border := strings.Repeat("=", 80)
	fmt.Printf("%s\n", border)
	fmt.Printf("=== %s ===\n", title)
	fmt.Printf("%s\n", border)
	
	for i, row := range overlay {
		line := ""
		for j, cell := range row {
			if cell != "" {
				line += cell
			} else {
				line += " "
			}
			if j >= 79 { // Limit width
				break
			}
		}
		fmt.Printf("%s\n", line)
		if i >= 24 { // Limit height
			break
		}
	}
	
	fmt.Printf("%s\n", border)
}

func (app *TestApp) formatSpriteOverlay(overlay [][]string) string {
	var result strings.Builder
	
	for i, row := range overlay {
		for j, cell := range row {
			if cell != "" {
				result.WriteString(cell)
			} else {
				result.WriteString(" ")
			}
			if j >= 79 { // Limit width
				break
			}
		}
		result.WriteString("\n")
		if i >= 24 { // Limit height
			break
		}
	}
	
	return result.String()
}

