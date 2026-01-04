package terminal

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/models"
	"cyberpunk-tui/styles"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss/v2"
)

// Terminal represents the terminal panel component
type Terminal struct {
	models.BaseModel
	header     *TerminalHeader
	outputLines []*OutputLine
}

// NewTerminal creates a new terminal
func NewTerminal(width, height int) *Terminal {
	outputLines := []*OutputLine{
		NewOutputLine("> uwu initializing neural chaos pathways... ░▓█▓░", styles.NeonLimeStyle, "pulse"),
		NewOutputLine("> loading maximum aesthetic overload... ████████", styles.HotPinkStyle, "bounce"),
		NewOutputLine("> vibe check: ABSOLUTELY UNHINGED ♦♠♣♥♦♠♣♥", styles.ElectricCyanStyle, "pulse"),
		NewOutputLine("> chaos engine: MAXIMUM OVERDRIVE ⚡⚡⚡", styles.OrangeFlameStyle, "ping"),
		NewOutputLine("> ready to break reality bestie █▓░", styles.NeonLimeStyle, "cursor"),
	}
	
	return &Terminal{
		BaseModel:   models.NewBaseModel(width, height),
		header:      NewTerminalHeader(),
		outputLines: outputLines,
	}
}

// Init implements tea.Model
func (m *Terminal) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *Terminal) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case models.ResizeMsg:
		m.Resize(msg.Width, msg.Height)
	}
	return m, nil
}

// View implements tea.Model
func (m *Terminal) View() string {
	var sections []string
	
	// Header
	sections = append(sections, m.header.Render(m.Width))
	
	// Output lines
	for _, line := range m.outputLines {
		sections = append(sections, line.Render(m.Width-2))
	}
	
	// Fill remaining space
	for len(sections) < m.Height {
		sections = append(sections, "")
	}
	
	return strings.Join(sections, "\n")
}

// UpdateAnimation implements Animatable
func (m *Terminal) UpdateAnimation(clock *animations.AnimationClock) {
	m.header.UpdateAnimation(clock)
	for _, line := range m.outputLines {
		line.UpdateAnimation(clock)
	}
}

// TerminalHeader manages the terminal header
type TerminalHeader struct {
	pulseState *animations.AnimationState
}

// NewTerminalHeader creates a new terminal header
func NewTerminalHeader() *TerminalHeader {
	return &TerminalHeader{
		pulseState: animations.NewAnimationState(2, 20, 0), // 2 second pulse
	}
}

// UpdateAnimation updates the header animation
func (th *TerminalHeader) UpdateAnimation(clock *animations.AnimationClock) {
	th.pulseState.Update(clock)
}

// Render renders the terminal header
func (th *TerminalHeader) Render(width int) string {
	text := "▓░▓░▓ TERMINAL OF ABSOLUTE CHAOS ▓░▓░▓"
	if len(text) > width {
		text = text[:width-3] + "..."
	}
	
	style := styles.ElectricCyanStyle
	if th.pulseState.GetFrame() == 1 {
		style = style.Bold(true)
	}
	
	return style.Render(text)
}

// OutputLine represents a single terminal output line
type OutputLine struct {
	text        string
	style       lipgloss.Style
	animType    string
	animState   *animations.AnimationState
	cursorState *animations.AnimationState
}

// NewOutputLine creates a new output line
func NewOutputLine(text string, style lipgloss.Style, animType string) *OutputLine {
	var animState *animations.AnimationState
	
	switch animType {
	case "pulse":
		animState = animations.NewAnimationState(2, 15, 0) // 1.5 second pulse
	case "bounce":
		animState = animations.NewAnimationState(2, 20, 0) // 2 second bounce
	case "ping":
		animState = animations.NewAnimationState(3, 10, 0) // 1 second ping effect
	case "cursor":
		animState = animations.NewAnimationState(3, 4, 0) // 400ms cursor blink
	default:
		animState = animations.NewAnimationState(2, 20, 0)
	}
	
	return &OutputLine{
		text:        text,
		style:       style,
		animType:    animType,
		animState:   animState,
		cursorState: animations.NewAnimationState(3, 4, 0), // For cursor animation
	}
}

// UpdateAnimation updates the output line animation
func (ol *OutputLine) UpdateAnimation(clock *animations.AnimationClock) {
	ol.animState.Update(clock)
	ol.cursorState.Update(clock)
}

// Render renders the output line
func (ol *OutputLine) Render(width int) string {
	text := ol.text
	if len(text) > width {
		text = text[:width-3] + "..."
	}
	
	style := ol.style
	
	switch ol.animType {
	case "pulse":
		if ol.animState.GetFrame() == 1 {
			style = style.Bold(true)
		}
	case "bounce":
		// Simulate bounce with spacing
		if ol.animState.GetFrame() == 1 {
			text = " " + text
		}
	case "ping":
		// Cycle through different intensities
		intensities := []bool{false, true, false}
		if intensities[ol.animState.GetFrame()] {
			style = style.Bold(true)
		}
	case "cursor":
		// Animated cursor at the end
		cursorChars := []string{"█▓░", "░▓█", "▓░█"}
		cursor := cursorChars[ol.cursorState.GetFrame()]
		
		// Replace last few characters with animated cursor
		if len(text) > 3 {
			text = text[:len(text)-3] + cursor
		}
	}
	
	return style.Render(text)
}

