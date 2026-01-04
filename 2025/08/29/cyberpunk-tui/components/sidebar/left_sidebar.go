package sidebar

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/models"
	"cyberpunk-tui/styles"
	"fmt"
	"math/rand"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss/v2"
)

// LeftSidebar represents the left sidebar component
type LeftSidebar struct {
	models.BaseModel
	header      *SidebarHeader
	buttons     []*ToolButton
	pixelCat    *PixelCat
	pixelAlien  *PixelAlien
	pulseState  *animations.AnimationState
}

// NewLeftSidebar creates a new left sidebar
func NewLeftSidebar(width, height int) *LeftSidebar {
	buttons := []*ToolButton{
		NewToolButton("█▓ uwu file explorer", styles.OrangeFlameBg),
		NewToolButton("░█ debug demon mode", styles.NeonLimeBg),
		NewToolButton("▓░ target lock acq.", styles.ElectricCyanBg),
		NewToolButton("█░ execute order uwu", styles.HotPinkBg),
		NewToolButton("▓█ big brain neural", styles.OrangeFlameBg),
		NewToolButton("░▓ stats go brrrr", styles.NeonLimeBg),
		NewToolButton("█▓ sync with void", styles.ElectricCyanBg),
		NewToolButton("░█ chaos settings", styles.HotPinkBg),
	}
	
	return &LeftSidebar{
		BaseModel:  models.NewBaseModel(width, height),
		header:     NewSidebarHeader("CHAOS TOOLKIT"),
		buttons:    buttons,
		pixelCat:   NewPixelCat(),
		pixelAlien: NewPixelAlien(),
		pulseState: animations.NewAnimationState(2, 15, 0), // 1.5 second pulse
	}
}

// Init implements tea.Model
func (m *LeftSidebar) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *LeftSidebar) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case models.ResizeMsg:
		m.Resize(msg.Width, msg.Height)
	}
	return m, nil
}

// View implements tea.Model
func (m *LeftSidebar) View() string {
	var sections []string
	
	// Background pattern
	bgPattern := m.generateBackground()
	
	// Header (rows 1-4)
	sections = append(sections, m.header.Render(m.Width))
	sections = append(sections, "")
	
	// Tool buttons (rows 5-12)
	for _, button := range m.buttons {
		sections = append(sections, button.Render(m.Width-2))
	}
	
	// Animated mascots (rows 13-22)
	sections = append(sections, "")
	catLines := m.pixelCat.Render()
	for _, line := range catLines {
		sections = append(sections, line)
	}
	
	sections = append(sections, "")
	alienLines := m.pixelAlien.Render()
	for _, line := range alienLines {
		sections = append(sections, line)
	}
	
	// Combine with background
	result := ""
	for i, section := range sections {
		if i < len(bgPattern) {
			// Overlay section on background
			if len(section) > 0 {
				result += section + "\n"
			} else {
				result += bgPattern[i] + "\n"
			}
		} else {
			result += section + "\n"
		}
	}
	
	return strings.TrimSuffix(result, "\n")
}

// UpdateAnimation implements Animatable
func (m *LeftSidebar) UpdateAnimation(clock *animations.AnimationClock) {
	m.header.UpdateAnimation(clock)
	for _, button := range m.buttons {
		button.UpdateAnimation(clock)
	}
	m.pixelCat.UpdateAnimation(clock)
	m.pixelAlien.UpdateAnimation(clock)
	m.pulseState.Update(clock)
}

// generateBackground creates the gradient background with noise
func (m *LeftSidebar) generateBackground() []string {
	lines := make([]string, m.Height)
	
	for row := 0; row < m.Height; row++ {
		line := ""
		var bgColor lipgloss.Style
		
		// Gradient: Pink -> Purple -> Cyan
		if row < m.Height/3 {
			bgColor = styles.HotPinkBg
		} else if row < 2*m.Height/3 {
			bgColor = styles.DeepPurpleBg
		} else {
			bgColor = styles.ElectricCyanBg
		}
		
		// Add noise pattern
		for col := 0; col < m.Width; col++ {
			if rand.Float64() < 0.2 { // 20% noise
				char := styles.GradientChars[rand.Intn(len(styles.GradientChars))]
				line += bgColor.Render(char)
			} else {
				line += bgColor.Render(" ")
			}
		}
		lines[row] = line
	}
	
	return lines
}

// SidebarHeader manages the header section
type SidebarHeader struct {
	text       string
	pulseState *animations.AnimationState
}

// NewSidebarHeader creates a new sidebar header
func NewSidebarHeader(text string) *SidebarHeader {
	return &SidebarHeader{
		text:       text,
		pulseState: animations.NewAnimationState(2, 15, 0),
	}
}

// UpdateAnimation updates the header animation
func (sh *SidebarHeader) UpdateAnimation(clock *animations.AnimationClock) {
	sh.pulseState.Update(clock)
}

// Render renders the header
func (sh *SidebarHeader) Render(width int) string {
	pattern := strings.Repeat("▓░", width/2)
	if len(pattern) > width {
		pattern = pattern[:width]
	}
	
	headerStyle := styles.NeonLimeBg.Foreground(lipgloss.Color(styles.PureBlack))
	if sh.pulseState.GetFrame() == 1 {
		headerStyle = headerStyle.Bold(true)
	}
	
	line1 := headerStyle.Render(pattern)
	line2 := headerStyle.Render(fmt.Sprintf("░ %s ", sh.text))
	line3 := headerStyle.Render(pattern)
	
	return line1 + "\n" + line2 + "\n" + line3
}

// ToolButton represents a tool button
type ToolButton struct {
	text       string
	bgStyle    lipgloss.Style
	pulseState *animations.AnimationState
	selected   bool
}

// NewToolButton creates a new tool button
func NewToolButton(text string, bgStyle lipgloss.Style) *ToolButton {
	return &ToolButton{
		text:       text,
		bgStyle:    bgStyle,
		pulseState: animations.NewAnimationState(2, 30, rand.Int63n(30)), // 3 second pulse with random offset
		selected:   false,
	}
}

// UpdateAnimation updates the button animation
func (tb *ToolButton) UpdateAnimation(clock *animations.AnimationClock) {
	tb.pulseState.Update(clock)
}

// Render renders the button
func (tb *ToolButton) Render(width int) string {
	style := tb.bgStyle.Foreground(lipgloss.Color(styles.PureBlack))
	
	// Pulse effect
	if tb.pulseState.GetFrame() == 1 {
		style = style.Bold(true)
	}
	
	// Selection effect
	if tb.selected {
		style = style.Border(lipgloss.RoundedBorder())
	}
	
	text := tb.text
	if len(text) > width {
		text = text[:width-3] + "..."
	}
	
	return style.Width(width).Render(text)
}

// PixelCat represents the animated pixel cat
type PixelCat struct {
	frames     [][]string
	frameState *animations.AnimationState
	bounceState *animations.AnimationState
}

// NewPixelCat creates a new pixel cat
func NewPixelCat() *PixelCat {
	frames := [][]string{
		{
			"░░█████░░",
			"░███░███░",
			"██░█░█░██",
			"██░░░░░██",
			"░██░▲░██░",
			"░░█████░░",
			"░░█░░░█░░",
			"░█░░░░░█░",
		},
		{
			"░░█████░░",
			"░███░███░",
			"██░█░█░██",
			"██░░░░░██",
			"░██░▼░██░",
			"░░█████░░",
			"█░░░░░░░█",
			"░█░░░░░█░",
		},
	}
	
	return &PixelCat{
		frames:      frames,
		frameState:  animations.NewAnimationState(2, 10, 0), // 1 second cycle
		bounceState: animations.NewAnimationState(2, 20, 0), // 2 second bounce
	}
}

// UpdateAnimation updates the cat animation
func (pc *PixelCat) UpdateAnimation(clock *animations.AnimationClock) {
	pc.frameState.Update(clock)
	pc.bounceState.Update(clock)
}

// Render renders the pixel cat
func (pc *PixelCat) Render() []string {
	frame := pc.frames[pc.frameState.GetFrame()]
	result := make([]string, len(frame))
	
	// Apply bounce effect (vertical offset)
	offset := ""
	if pc.bounceState.GetFrame() == 1 {
		offset = " " // Slight offset for bounce effect
	}
	
	for i, line := range frame {
		result[i] = offset + styles.PureBlackStyle.Render(line)
	}
	
	return result
}

// PixelAlien represents the animated pixel alien
type PixelAlien struct {
	frames     [][]string
	frameState *animations.AnimationState
	pulseState *animations.AnimationState
}

// NewPixelAlien creates a new pixel alien
func NewPixelAlien() *PixelAlien {
	frames := [][]string{
		{
			"░░░▓▓▓░░░",
			"░░▓███▓░░",
			"░▓█▓░▓█▓░",
			"░▓█████▓░",
			"▓███████▓",
			"██▓███▓██",
			"░▓▓▓▓▓▓▓░",
			"░░▓░░░▓░░",
		},
		{
			"░░░▓▓▓░░░",
			"░░▓███▓░░",
			"░▓█░░░█▓░",
			"░▓█████▓░",
			"▓███████▓",
			"██▓█▓█▓██",
			"░▓▓░░░▓▓░",
			"░░▓░░░▓░░",
		},
	}
	
	return &PixelAlien{
		frames:     frames,
		frameState: animations.NewAnimationState(2, 30, 0), // 3 second blink cycle
		pulseState: animations.NewAnimationState(2, 15, 0), // 1.5 second pulse
	}
}

// UpdateAnimation updates the alien animation
func (pa *PixelAlien) UpdateAnimation(clock *animations.AnimationClock) {
	pa.frameState.Update(clock)
	pa.pulseState.Update(clock)
}

// Render renders the pixel alien
func (pa *PixelAlien) Render() []string {
	frame := pa.frames[pa.frameState.GetFrame()]
	result := make([]string, len(frame))
	
	style := styles.PureBlackStyle
	// Pulse effect (opacity simulation)
	if pa.pulseState.GetFrame() == 1 {
		style = style.Bold(true)
	}
	
	for i, line := range frame {
		result[i] = style.Render(line)
	}
	
	return result
}

