package editor

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/models"
	"cyberpunk-tui/styles"
	"fmt"
	"math/rand"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss/v2"
)

// CodeEditor represents the code editor panel
type CodeEditor struct {
	models.BaseModel
	header      *EditorHeader
	background  *BackgroundPattern
	visualizer  *MusicVisualizer
	codeArea    *CodeDisplay
}

// NewCodeEditor creates a new code editor
func NewCodeEditor(width, height int) *CodeEditor {
	return &CodeEditor{
		BaseModel:  models.NewBaseModel(width, height),
		header:     NewEditorHeader(),
		background: NewBackgroundPattern(width, height-8), // Reserve space for header and visualizer
		visualizer: NewMusicVisualizer(width),
		codeArea:   NewCodeDisplay(width, height-10), // Reserve space for header, visualizer, and margins
	}
}

// Init implements tea.Model
func (m *CodeEditor) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *CodeEditor) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case models.ResizeMsg:
		m.Resize(msg.Width, msg.Height)
		m.background.Resize(msg.Width, msg.Height-8)
		m.visualizer.Resize(msg.Width)
		m.codeArea.Resize(msg.Width, msg.Height-10)
	}
	return m, nil
}

// View implements tea.Model
func (m *CodeEditor) View() string {
	var sections []string
	
	// Header bar
	sections = append(sections, m.header.Render(m.Width))
	
	// Background pattern
	bgLines := m.background.Render()
	for _, line := range bgLines {
		sections = append(sections, line)
	}
	
	// Music visualizer
	vizLines := m.visualizer.Render()
	for _, line := range vizLines {
		sections = append(sections, line)
	}
	
	// Code display area
	codeLines := m.codeArea.Render()
	for _, line := range codeLines {
		sections = append(sections, line)
	}
	
	return strings.Join(sections, "\n")
}

// UpdateAnimation implements Animatable
func (m *CodeEditor) UpdateAnimation(clock *animations.AnimationClock) {
	m.header.UpdateAnimation(clock)
	m.background.UpdateAnimation(clock)
	m.visualizer.UpdateAnimation(clock)
	m.codeArea.UpdateAnimation(clock)
}

// EditorHeader manages the editor header bar
type EditorHeader struct {
	pulseState *animations.AnimationState
}

// NewEditorHeader creates a new editor header
func NewEditorHeader() *EditorHeader {
	return &EditorHeader{
		pulseState: animations.NewAnimationState(2, 20, 0), // 2 second pulse
	}
}

// UpdateAnimation updates the header animation
func (eh *EditorHeader) UpdateAnimation(clock *animations.AnimationClock) {
	eh.pulseState.Update(clock)
}

// Render renders the editor header
func (eh *EditorHeader) Render(width int) string {
	now := time.Now()
	timestamp := now.Format("15:04:05")
	
	pattern := strings.Repeat("▓█", width/2)
	if len(pattern) > width {
		pattern = pattern[:width]
	}
	
	headerText := fmt.Sprintf("█ NEURAL_CHAOS.js • %s • STATUS: UNHINGED ▓", timestamp)
	if len(headerText) > width {
		headerText = headerText[:width-3] + "..."
	}
	
	style := styles.NeonLimeStyle.Background(lipgloss.Color(styles.PureBlack))
	if eh.pulseState.GetFrame() == 1 {
		style = style.Bold(true)
	}
	
	line1 := style.Render(pattern)
	line2 := style.Render(headerText)
	line3 := style.Render(pattern)
	
	return line1 + "\n" + line2 + "\n" + line3
}

// BackgroundPattern manages the dynamic noise field
type BackgroundPattern struct {
	width       int
	height      int
	patternState *animations.AnimationState
}

// NewBackgroundPattern creates a new background pattern
func NewBackgroundPattern(width, height int) *BackgroundPattern {
	return &BackgroundPattern{
		width:        width,
		height:       height,
		patternState: animations.NewAnimationState(8, 3, 0), // 300ms cycle
	}
}

// Resize updates the background pattern dimensions
func (bp *BackgroundPattern) Resize(width, height int) {
	bp.width = width
	bp.height = height
}

// UpdateAnimation updates the background pattern animation
func (bp *BackgroundPattern) UpdateAnimation(clock *animations.AnimationClock) {
	bp.patternState.Update(clock)
}

// Render renders the background pattern
func (bp *BackgroundPattern) Render() []string {
	lines := make([]string, bp.height)
	patternChars := []string{"░", "▒", "▓", "█", "◆", "◇", "★", "☆"}
	
	for row := 0; row < bp.height; row++ {
		line := ""
		for col := 0; col < bp.width; col++ {
			charIndex := (row + col + bp.patternState.GetFrame()) % len(patternChars)
			char := patternChars[charIndex]
			
			// 60% opacity simulation
			if rand.Float64() > 0.6 {
				char = "░"
			}
			
			line += styles.ElectricCyanStyle.Render(char)
		}
		lines[row] = line
	}
	
	return lines
}

// MusicVisualizer manages the music visualizer bars
type MusicVisualizer struct {
	width    int
	barStates []*animations.AnimationState
}

// NewMusicVisualizer creates a new music visualizer
func NewMusicVisualizer(width int) *MusicVisualizer {
	numBars := width / 3 // Each bar is 2 chars wide + 1 space
	if numBars > 20 {
		numBars = 20
	}
	
	barStates := make([]*animations.AnimationState, numBars)
	for i := range barStates {
		barStates[i] = animations.NewAnimationState(8, 1, int64(i%3)) // 100ms cycle with offset
	}
	
	return &MusicVisualizer{
		width:     width,
		barStates: barStates,
	}
}

// Resize updates the visualizer dimensions
func (mv *MusicVisualizer) Resize(width int) {
	mv.width = width
	// Recreate bar states if needed
	numBars := width / 3
	if numBars > 20 {
		numBars = 20
	}
	
	if len(mv.barStates) != numBars {
		mv.barStates = make([]*animations.AnimationState, numBars)
		for i := range mv.barStates {
			mv.barStates[i] = animations.NewAnimationState(8, 1, int64(i%3))
		}
	}
}

// UpdateAnimation updates the visualizer animation
func (mv *MusicVisualizer) UpdateAnimation(clock *animations.AnimationClock) {
	for _, state := range mv.barStates {
		state.Update(clock)
	}
}

// Render renders the music visualizer
func (mv *MusicVisualizer) Render() []string {
	header := styles.HotPinkStyle.Render("♫♪♫ VIBE VISUALIZER ♫♪♫")
	
	barLine1 := ""
	barLine2 := ""
	
	for i, state := range mv.barStates {
		height := state.GetFrame() + 1 // 1-8 height
		
		// Create bar representation
		bar1 := "█"
		bar2 := "█"
		
		if height < 4 {
			bar1 = "░"
		}
		if height < 2 {
			bar2 = "░"
		}
		
		barLine1 += styles.HotPinkStyle.Render(bar1 + bar1)
		barLine2 += styles.HotPinkStyle.Render(bar2 + bar2)
		
		if i < len(mv.barStates)-1 {
			barLine1 += " "
			barLine2 += " "
		}
	}
	
	return []string{header, barLine1, barLine2}
}

// CodeDisplay manages the code display area
type CodeDisplay struct {
	width      int
	height     int
	cursorPos  int
	cursorState *animations.AnimationState
}

// NewCodeDisplay creates a new code display
func NewCodeDisplay(width, height int) *CodeDisplay {
	return &CodeDisplay{
		width:       width,
		height:      height,
		cursorPos:   0,
		cursorState: animations.NewAnimationState(2, 4, 0), // 400ms cursor blink
	}
}

// Resize updates the code display dimensions
func (cd *CodeDisplay) Resize(width, height int) {
	cd.width = width
	cd.height = height
}

// UpdateAnimation updates the code display animation
func (cd *CodeDisplay) UpdateAnimation(clock *animations.AnimationClock) {
	cd.cursorState.Update(clock)
	
	// Move cursor occasionally
	if clock.IsFrameMultiple(50) { // Every 5 seconds
		cd.cursorPos = (cd.cursorPos + 1) % 100
	}
}

// Render renders the code display
func (cd *CodeDisplay) Render() []string {
	border := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color(styles.ElectricCyan)).
		Width(cd.width - 2).
		Height(cd.height - 2)
	
	// ASCII art title
	asciiArt := []string{
		"███╗   ██╗███████╗██╗   ██╗██████╗  █████╗ ██╗     ",
		"████╗  ██║██╔════╝██║   ██║██╔══██╗██╔══██╗██║     ",
		"██╔██╗ ██║█████╗  ██║   ██║██████╔╝███████║██║     ",
		"██║╚██╗██║██╔══╝  ██║   ██║██╔══██╗██╔══██║██║     ",
		"██║ ╚████║███████╗╚██████╔╝██║  ██║██║  ██║███████╗",
		"╚═╝  ╚═══╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝",
		"",
		" ██████╗██╗  ██╗ █████╗  ██████╗ ███████╗",
		"██╔════╝██║  ██║██╔══██╗██╔═══██╗██╔════╝",
		"██║     ███████║███████║██║   ██║███████╗",
		"██║     ██╔══██║██╔══██║██║   ██║╚════██║",
		"╚██████╗██║  ██║██║  ██║╚██████╔╝███████║",
		" ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝",
	}
	
	// Code snippet
	codeLines := []string{
		"",
		"// Initializing maximum chaos protocols...",
		"const vibeLevel = Math.max(9000, currentChaos);",
		"if (vibeLevel > REALITY_THRESHOLD) {",
		"    console.log('uwu reality.exe has stopped working');",
		"    initializeGlitchMode();",
		"}",
	}
	
	// Combine content
	content := ""
	allLines := append(asciiArt, codeLines...)
	
	for i, line := range allLines {
		if i < cd.height-4 { // Leave space for border
			styledLine := styles.NeonLimeStyle.Render(line)
			
			// Add cursor occasionally
			if i == cd.cursorPos%len(allLines) && cd.cursorState.GetFrame() == 1 {
				styledLine += styles.GhostWhiteStyle.Render("█")
			}
			
			content += styledLine + "\n"
		}
	}
	
	return strings.Split(border.Render(content), "\n")
}

