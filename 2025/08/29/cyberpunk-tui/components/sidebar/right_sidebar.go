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

// RightSidebar represents the right sidebar component
type RightSidebar struct {
	models.BaseModel
	header      *SidebarHeader
	metrics     *SystemMetrics
	modules     []*ModuleStatus
	pixelHeart  *PixelHeart
	pixelFire   *PixelFire
}

// NewRightSidebar creates a new right sidebar
func NewRightSidebar(width, height int) *RightSidebar {
	modules := []*ModuleStatus{
		NewModuleStatus("█▓ uwu neural analyzer"),
		NewModuleStatus("▓█ vibe detector 3000"),
		NewModuleStatus("░▓ maximum chaos engine"),
		NewModuleStatus("▓░ glitch reality master"),
		NewModuleStatus("██ unicorn mode activated"),
		NewModuleStatus("░░ skull emoji generator"),
		NewModuleStatus("▓▓ aesthetic overloader"),
		NewModuleStatus("█░ pixel creature spawn"),
	}
	
	return &RightSidebar{
		BaseModel:  models.NewBaseModel(width, height),
		header:     NewSidebarHeader("NEURAL STATUS"),
		metrics:    NewSystemMetrics(),
		modules:    modules,
		pixelHeart: NewPixelHeart(),
		pixelFire:  NewPixelFire(),
	}
}

// Init implements tea.Model
func (m *RightSidebar) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *RightSidebar) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case models.ResizeMsg:
		m.Resize(msg.Width, msg.Height)
	}
	return m, nil
}

// View implements tea.Model
func (m *RightSidebar) View() string {
	var sections []string
	
	// Background pattern (opposite gradient of left sidebar)
	bgPattern := m.generateBackground()
	
	// Header (rows 1-3)
	sections = append(sections, m.header.Render(m.Width))
	
	// System metrics (rows 4-8)
	metricLines := m.metrics.Render(m.Width-2)
	for _, line := range metricLines {
		sections = append(sections, line)
	}
	
	// Module status (rows 9-16)
	for _, module := range m.modules {
		sections = append(sections, module.Render(m.Width-2))
	}
	
	// Animated creatures (rows 17-22)
	sections = append(sections, "")
	heartLines := m.pixelHeart.Render()
	for _, line := range heartLines {
		sections = append(sections, line)
	}
	
	fireLines := m.pixelFire.Render()
	for _, line := range fireLines {
		sections = append(sections, line)
	}
	
	// Combine with background
	result := ""
	for i, section := range sections {
		if i < len(bgPattern) {
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
func (m *RightSidebar) UpdateAnimation(clock *animations.AnimationClock) {
	m.header.UpdateAnimation(clock)
	m.metrics.UpdateAnimation(clock)
	for _, module := range m.modules {
		module.UpdateAnimation(clock)
	}
	m.pixelHeart.UpdateAnimation(clock)
	m.pixelFire.UpdateAnimation(clock)
}

// generateBackground creates the reverse gradient background
func (m *RightSidebar) generateBackground() []string {
	lines := make([]string, m.Height)
	
	for row := 0; row < m.Height; row++ {
		line := ""
		var bgColor lipgloss.Style
		
		// Gradient: Cyan -> Purple -> Pink (opposite of left)
		if row < m.Height/3 {
			bgColor = styles.ElectricCyanBg
		} else if row < 2*m.Height/3 {
			bgColor = styles.DeepPurpleBg
		} else {
			bgColor = styles.HotPinkBg
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

// SystemMetrics manages the system metrics display
type SystemMetrics struct {
	barStates   []*animations.AnimationState
	glitchState *animations.GlitchState
	colorCycle  *animations.AnimationState
}

// NewSystemMetrics creates new system metrics
func NewSystemMetrics() *SystemMetrics {
	return &SystemMetrics{
		barStates: []*animations.AnimationState{
			animations.NewAnimationState(10, 3, 0), // CPU bar
			animations.NewAnimationState(10, 3, 1), // MEM bar
			animations.NewAnimationState(10, 3, 2), // GPU bar
			animations.NewAnimationState(10, 3, 3), // CHAOS bar
			animations.NewAnimationState(10, 3, 4), // VIBES bar
		},
		glitchState: animations.NewGlitchState(),
		colorCycle:  animations.NewAnimationState(3, 20, 0), // 2 second color cycle
	}
}

// UpdateAnimation updates the metrics animation
func (sm *SystemMetrics) UpdateAnimation(clock *animations.AnimationClock) {
	for _, state := range sm.barStates {
		state.Update(clock)
	}
	sm.glitchState.Update()
	sm.colorCycle.Update(clock)
}

// Render renders the system metrics
func (sm *SystemMetrics) Render(width int) []string {
	labels := []string{"CPU:", "MEM:", "GPU:", "CHAOS:", "VIBES:"}
	values := []string{"420%", "∞", "MAX", "", ""}
	
	colors := []lipgloss.Style{styles.NeonLimeStyle, styles.ElectricCyanStyle, styles.HotPinkStyle}
	currentColor := colors[sm.colorCycle.GetFrame()]
	
	lines := make([]string, len(labels))
	
	for i, label := range labels {
		barLength := 8
		filledBars := sm.barStates[i].GetFrame()
		
		// Ensure filledBars doesn't exceed barLength
		if filledBars > barLength {
			filledBars = barLength
		}
		if filledBars < 0 {
			filledBars = 0
		}
		
		emptyBars := barLength - filledBars
		if emptyBars < 0 {
			emptyBars = 0
		}
		
		bar := strings.Repeat("█", filledBars) + strings.Repeat("░", emptyBars)
		
		value := values[i]
		if sm.glitchState.IsActive() && rand.Float64() < 0.3 {
			// Glitch the values occasionally
			glitchValues := []string{"999%", "ERROR", "∞∞∞", "MAX++", "OVER9000"}
			value = glitchValues[rand.Intn(len(glitchValues))]
		}
		
		line := fmt.Sprintf("%s %s %s", label, currentColor.Render(bar), value)
		lines[i] = line
	}
	
	return lines
}

// ModuleStatus represents a module status line
type ModuleStatus struct {
	text        string
	prefixState *animations.AnimationState
}

// NewModuleStatus creates a new module status
func NewModuleStatus(text string) *ModuleStatus {
	return &ModuleStatus{
		text:        text,
		prefixState: animations.NewAnimationState(4, 5, rand.Int63n(5)), // 500ms cycle with random offset
	}
}

// UpdateAnimation updates the module status animation
func (ms *ModuleStatus) UpdateAnimation(clock *animations.AnimationClock) {
	ms.prefixState.Update(clock)
}

// Render renders the module status
func (ms *ModuleStatus) Render(width int) string {
	prefixChars := []string{"█▓", "▓█", "░▓", "▓░"}
	prefix := prefixChars[ms.prefixState.GetFrame()]
	
	text := ms.text
	if len(prefix+text) > width {
		text = text[:width-len(prefix)-3] + "..."
	}
	
	return styles.GetStyleByIndex(ms.prefixState.GetFrame()).Render(prefix + text)
}

// PixelHeart represents the animated pixel heart
type PixelHeart struct {
	frames      [][]string
	heartbeat   *animations.AnimationState
}

// NewPixelHeart creates a new pixel heart
func NewPixelHeart() *PixelHeart {
	frames := [][]string{
		{
			"░▓▓░░▓▓░",
			"▓██▓▓██▓",
			"▓████████▓",
			"░▓██████▓░",
			"░░▓████▓░░",
			"░░░▓██▓░░░",
		},
		{
			"░▓▓░░▓▓░",
			"▓██▓▓██▓",
			"████████",
			"░██████░",
			"░░████░░",
			"░░░██░░░",
		},
	}
	
	return &PixelHeart{
		frames:    frames,
		heartbeat: animations.NewAnimationState(2, 12, 0), // 1.2 second heartbeat
	}
}

// UpdateAnimation updates the heart animation
func (ph *PixelHeart) UpdateAnimation(clock *animations.AnimationClock) {
	ph.heartbeat.Update(clock)
}

// Render renders the pixel heart
func (ph *PixelHeart) Render() []string {
	frame := ph.frames[ph.heartbeat.GetFrame()]
	result := make([]string, len(frame))
	
	for i, line := range frame {
		result[i] = styles.HotPinkStyle.Render(line)
	}
	
	return result
}

// PixelFire represents the animated pixel fire
type PixelFire struct {
	frames     [][]string
	frameState *animations.AnimationState
}

// NewPixelFire creates a new pixel fire
func NewPixelFire() *PixelFire {
	frames := [][]string{
		{
			"    ░▓░    ",
			"   ░▓█▓░   ",
			"  ░▓███▓░  ",
			" ░▓█████▓░ ",
			"░▓███████▓░",
			"▓█████████▓",
			"███████████",
			"███████████",
		},
		{
			"   ░▓░░    ",
			"  ░▓█▓░    ",
			" ░▓███▓░   ",
			"░▓█████▓░  ",
			"▓███████▓░ ",
			"█████████▓░",
			"███████████",
			"███████████",
		},
		{
			"   ░▓░     ",
			"  ░▓█▓░    ",
			" ░▓███▓░   ",
			"▓█████▓░   ",
			"███████▓░░ ",
			"███████▓░  ",
			"██████████ ",
			"██████████ ",
		},
	}
	
	return &PixelFire{
		frames:     frames,
		frameState: animations.NewAnimationState(3, 2, 0), // 200ms per frame
	}
}

// UpdateAnimation updates the fire animation
func (pf *PixelFire) UpdateAnimation(clock *animations.AnimationClock) {
	pf.frameState.Update(clock)
}

// Render renders the pixel fire
func (pf *PixelFire) Render() []string {
	frame := pf.frames[pf.frameState.GetFrame()]
	result := make([]string, len(frame))
	
	// Gradient simulation: Red -> Orange -> Yellow
	colors := []lipgloss.Style{
		lipgloss.NewStyle().Foreground(lipgloss.Color("#FF0000")), // Red
		lipgloss.NewStyle().Foreground(lipgloss.Color("#FF8800")), // Orange
		lipgloss.NewStyle().Foreground(lipgloss.Color("#FFFF00")), // Yellow
	}
	
	for i, line := range frame {
		colorIndex := i % len(colors)
		result[i] = colors[colorIndex].Render(line)
	}
	
	return result
}

