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

// FullLeftSidebar represents the full left sidebar component
type FullLeftSidebar struct {
	models.BaseModel
	pulseState *animations.AnimationState
	glowState  *animations.AnimationState
}

// NewFullLeftSidebar creates a new full left sidebar (16x22)
func NewFullLeftSidebar(width, height int) *FullLeftSidebar {
	return &FullLeftSidebar{
		BaseModel:  models.NewBaseModel(16, 22), // Fixed dimensions
		pulseState: animations.NewAnimationState(8, 30, 0),  // Slow pulse for effects
		glowState:  animations.NewAnimationState(12, 40, 0), // Glow animation
	}
}

// Init implements tea.Model
func (m *FullLeftSidebar) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *FullLeftSidebar) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg.(type) {
	case models.ResizeMsg:
		// Keep fixed dimensions for sidebar
		m.Width = 16
		m.Height = 22
	}
	return m, nil
}

// UpdateAnimation implements Animatable
func (m *FullLeftSidebar) UpdateAnimation(clock *animations.AnimationClock) {
	m.pulseState.Update(clock)
	m.glowState.Update(clock)
}

// generateBackgroundNoise creates 20% opacity random noise pattern
func (m *FullLeftSidebar) generateBackgroundNoise(width int) string {
	noiseChars := []string{"░", "▒", "▓"}
	var noise strings.Builder
	
	for i := 0; i < width; i++ {
		if rand.Float64() < 0.2 { // 20% opacity
			noise.WriteString(noiseChars[rand.Intn(len(noiseChars))])
		} else {
			noise.WriteString(" ")
		}
	}
	return noise.String()
}

// renderHeader renders the header section (rows 1-4)
func (m *FullLeftSidebar) renderHeader() []string {
	lines := make([]string, 4)
	
	// Row 1: Title with glow effect
	glowChar := "█"
	if m.glowState.Frame%2 == 0 {
		glowChar = "▓"
	}
	lines[0] = fmt.Sprintf("╔══%s CHAOS %s══╗", glowChar, glowChar)
	
	// Row 2: Subtitle
	lines[1] = "║   TOOLKIT   ║"
	
	// Row 3: Separator with animation
	pulseChar := "═"
	if m.pulseState.Frame%4 < 2 {
		pulseChar = "▓"
	}
	lines[2] = fmt.Sprintf("╠%s%s%s%s%s%s%s%s%s%s%s%s%s%s╣", 
		pulseChar, pulseChar, pulseChar, pulseChar, pulseChar, pulseChar, pulseChar,
		pulseChar, pulseChar, pulseChar, pulseChar, pulseChar, pulseChar, pulseChar)
	
	// Row 4: Status indicator
	statusIcon := "●"
	if m.pulseState.Frame%8 < 4 {
		statusIcon = "◉"
	}
	lines[3] = fmt.Sprintf("║ %s ACTIVE %s ║", statusIcon, statusIcon)
	
	return lines
}

// renderToolButtons renders the tool buttons section (rows 5-12)
func (m *FullLeftSidebar) renderToolButtons() []string {
	lines := make([]string, 8)
	
	buttons := []struct {
		icon string
		name string
	}{
		{"█▓", "file explore"},
		{"░█", "debug demon"},
		{"▓░", "target lock"},
		{"█░", "execute uwu"},
		{"▓█", "big brain"},
		{"░▓", "stats brr"},
		{"█▓", "sync void"},
		{"░█", "chaos cfg"},
	}
	
	for i, button := range buttons {
		// Add pulsing effect to active button
		prefix := "║"
		suffix := "║"
		if m.pulseState.Frame%8 == i {
			prefix = "▌"
			suffix = "▐"
		}
		
		lines[i] = fmt.Sprintf("%s %s %s %s", prefix, button.icon, button.name, suffix)
	}
	
	return lines
}

// renderMascots renders the mascot section (rows 13-18)
func (m *FullLeftSidebar) renderMascots() []string {
	lines := make([]string, 6)
	
	// Pixel cat animation
	catFrames := [][]string{
		{"╔══════════════╗", "║  /\\_/\\  ♥   ║", "║ ( o.o )     ║", "║  > ^ <      ║"},
		{"╔══════════════╗", "║  /\\_/\\  ♡   ║", "║ ( ^.^ )     ║", "║  > ω <      ║"},
	}
	
	catFrame := catFrames[m.pulseState.Frame%2]
	for i, line := range catFrame {
		if i < len(lines) {
			lines[i] = line
		}
	}
	
	// Alien friend
	if len(lines) > 4 {
		alienIcon := "👽"
		if m.glowState.Frame%3 == 0 {
			alienIcon = "🛸"
		}
		lines[4] = fmt.Sprintf("║ %s ALIEN pal ║", alienIcon)
		lines[5] = "╚══════════════╝"
	}
	
	return lines
}

// renderFooter renders the footer section (rows 19-22)
func (m *FullLeftSidebar) renderFooter() []string {
	lines := make([]string, 4)
	
	// Power level indicator
	powerBars := []string{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"}
	powerLevel := m.pulseState.Frame % len(powerBars)
	
	lines[0] = "╔══ POWER ════╗"
	lines[1] = fmt.Sprintf("║ %s%s%s%s%s%s%s%s ║", 
		powerBars[min(powerLevel, 7)],
		powerBars[min(max(powerLevel-1, 0), 7)],
		powerBars[min(max(powerLevel-2, 0), 7)],
		powerBars[min(max(powerLevel-3, 0), 7)],
		powerBars[min(max(powerLevel-4, 0), 7)],
		powerBars[min(max(powerLevel-5, 0), 7)],
		powerBars[min(max(powerLevel-6, 0), 7)],
		powerBars[min(max(powerLevel-7, 0), 7)])
	
	// Chaos meter
	chaosLevel := (m.glowState.Frame % 10) + 1
	lines[2] = fmt.Sprintf("║ CHAOS: %d0%% ║", chaosLevel)
	lines[3] = "╚══════════════╝"
	
	return lines
}

// View implements tea.Model
func (m *FullLeftSidebar) View() string {
	var content []string
	
	// Render all sections
	header := m.renderHeader()
	buttons := m.renderToolButtons()
	mascots := m.renderMascots()
	footer := m.renderFooter()
	
	// Combine sections
	content = append(content, header...)
	content = append(content, buttons...)
	content = append(content, mascots...)
	content = append(content, footer...)
	
	// Apply gradient backgrounds and noise
	var styledLines []string
	for i, line := range content {
		var bgStyle lipgloss.Style
		
		// Determine background color based on row
		if i < 7 {
			// Pink background (rows 1-7)
			bgStyle = lipgloss.NewStyle().Background(lipgloss.Color(styles.HotPink))
		} else if i < 15 {
			// Purple background (rows 8-15)
			bgStyle = lipgloss.NewStyle().Background(lipgloss.Color(styles.DeepPurple))
		} else {
			// Cyan background (rows 16-22)
			bgStyle = lipgloss.NewStyle().Background(lipgloss.Color(styles.ElectricCyan))
		}
		
		// Add noise overlay (for future implementation)
		_ = m.generateBackgroundNoise(16)
		
		// Style the line
		styledLine := bgStyle.
			Foreground(lipgloss.Color(styles.GhostWhite)).
			Width(16).
			Render(line)
		
		styledLines = append(styledLines, styledLine)
	}
	
	return strings.Join(styledLines, "\n")
}

// Helper function for min
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// Helper function for max
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

