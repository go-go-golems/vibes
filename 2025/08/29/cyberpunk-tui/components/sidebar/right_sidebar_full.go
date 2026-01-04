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

// FullRightSidebar represents the full right sidebar component
type FullRightSidebar struct {
	models.BaseModel
	pulseState   *animations.AnimationState
	glowState    *animations.AnimationState
	heartState   *animations.AnimationState
	fireState    *animations.AnimationState
}

// NewFullRightSidebar creates a new full right sidebar (16x22)
func NewFullRightSidebar(width, height int) *FullRightSidebar {
	return &FullRightSidebar{
		BaseModel:   models.NewBaseModel(16, 22), // Fixed dimensions
		pulseState:  animations.NewAnimationState(8, 30, 0),  // Slow pulse
		glowState:   animations.NewAnimationState(12, 40, 0), // Glow animation
		heartState:  animations.NewAnimationState(4, 20, 0),  // Heart beat
		fireState:   animations.NewAnimationState(6, 25, 0),  // Fire flicker
	}
}

// Init implements tea.Model
func (m *FullRightSidebar) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *FullRightSidebar) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg.(type) {
	case models.ResizeMsg:
		// Keep fixed dimensions for sidebar
		m.Width = 16
		m.Height = 22
	}
	return m, nil
}

// UpdateAnimation implements Animatable
func (m *FullRightSidebar) UpdateAnimation(clock *animations.AnimationClock) {
	m.pulseState.Update(clock)
	m.glowState.Update(clock)
	m.heartState.Update(clock)
	m.fireState.Update(clock)
}

// generateBackgroundNoise creates 20% opacity random noise pattern
func (m *FullRightSidebar) generateBackgroundNoise(width int) string {
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

// renderStatusHeader renders the status header (rows 1-3)
func (m *FullRightSidebar) renderStatusHeader() []string {
	lines := make([]string, 3)
	
	// Row 1: Title with glow effect
	glowChar := "█"
	if m.glowState.Frame%2 == 0 {
		glowChar = "▓"
	}
	lines[0] = fmt.Sprintf("╔%s NEURAL %s╗", glowChar, glowChar)
	
	// Row 2: Subtitle
	lines[1] = "║  STATUS   ║"
	
	// Row 3: Separator
	lines[2] = "╠═══════════╣"
	
	return lines
}

// renderSystemMetrics renders system metrics (rows 4-10)
func (m *FullRightSidebar) renderSystemMetrics() []string {
	lines := make([]string, 7)
	
	// CPU usage with animation
	cpuLevel := (m.pulseState.Frame % 10) + 1
	lines[0] = fmt.Sprintf("║CPU: %d%d%% ███║", cpuLevel, (cpuLevel+3)%10)
	
	// GPU usage
	gpuLevel := (m.glowState.Frame % 8) + 2
	lines[1] = fmt.Sprintf("║GPU: %d%d%% ███║", gpuLevel, (gpuLevel+2)%10)
	
	// Memory usage
	memLevel := (m.heartState.Frame % 9) + 1
	lines[2] = fmt.Sprintf("║MEM: %d%d%% ███║", memLevel, (memLevel+4)%10)
	
	// Network activity
	netLevel := (m.fireState.Frame % 7) + 1
	lines[3] = fmt.Sprintf("║NET: %d%d%% ███║", netLevel, (netLevel+1)%10)
	
	// Chaos level
	chaosLevel := (m.pulseState.Frame % 10) + 1
	lines[4] = fmt.Sprintf("║CHAOS:%d%d%% ██║", chaosLevel, (chaosLevel+5)%10)
	
	// Vibe check
	vibeIcons := []string{"♥", "♡", "💖", "💕", "💗"}
	vibeIcon := vibeIcons[m.heartState.Frame%len(vibeIcons)]
	lines[5] = fmt.Sprintf("║VIBES: %s MAX║", vibeIcon)
	
	// Separator
	lines[6] = "╠═══════════╣"
	
	return lines
}

// renderCreatures renders the creatures section (rows 11-18)
func (m *FullRightSidebar) renderCreatures() []string {
	lines := make([]string, 8)
	
	// Heart creature animation
	heartFrames := []string{"♥", "♡", "💖", "💕"}
	heartIcon := heartFrames[m.heartState.Frame%len(heartFrames)]
	lines[0] = fmt.Sprintf("║ %s HEART %s ║", heartIcon, heartIcon)
	
	// Fire creature animation
	fireFrames := []string{"🔥", "🔥", "💥", "⚡"}
	fireIcon := fireFrames[m.fireState.Frame%len(fireFrames)]
	lines[1] = fmt.Sprintf("║ %s FIRE %s  ║", fireIcon, fireIcon)
	
	// Lightning creature
	lightningFrames := []string{"⚡", "🌟", "✨", "💫"}
	lightningIcon := lightningFrames[m.glowState.Frame%len(lightningFrames)]
	lines[2] = fmt.Sprintf("║ %s BOLT %s  ║", lightningIcon, lightningIcon)
	
	// Skull creature
	skullFrames := []string{"💀", "☠️", "👻", "💀"}
	skullIcon := skullFrames[m.pulseState.Frame%len(skullFrames)]
	lines[3] = fmt.Sprintf("║ %s SKULL %s ║", skullIcon, skullIcon)
	
	// Energy bars
	energyLevel := m.pulseState.Frame % 8
	energyBars := strings.Repeat("█", energyLevel) + strings.Repeat("░", 8-energyLevel)
	lines[4] = fmt.Sprintf("║ %s ║", energyBars)
	
	// Creature status
	lines[5] = "║ CREATURES ║"
	lines[6] = "║  ACTIVE   ║"
	lines[7] = "╠═══════════╣"
	
	return lines
}

// renderFooter renders the footer section (rows 19-22)
func (m *FullRightSidebar) renderFooter() []string {
	lines := make([]string, 4)
	
	// Connection status
	connectionIcons := []string{"📡", "📶", "🌐", "💫"}
	connectionIcon := connectionIcons[m.glowState.Frame%len(connectionIcons)]
	lines[0] = fmt.Sprintf("║ %s ONLINE ║", connectionIcon)
	
	// Sync status
	syncIcons := []string{"🔄", "⚡", "✨", "🔄"}
	syncIcon := syncIcons[m.fireState.Frame%len(syncIcons)]
	lines[1] = fmt.Sprintf("║ %s SYNCED ║", syncIcon)
	
	// Power status
	powerLevel := (m.heartState.Frame % 5) + 1
	powerBars := strings.Repeat("█", powerLevel) + strings.Repeat("░", 5-powerLevel)
	lines[2] = fmt.Sprintf("║PWR:%s║", powerBars)
	
	// Bottom border
	lines[3] = "╚═══════════╝"
	
	return lines
}

// View implements tea.Model
func (m *FullRightSidebar) View() string {
	var content []string
	
	// Render all sections
	header := m.renderStatusHeader()
	metrics := m.renderSystemMetrics()
	creatures := m.renderCreatures()
	footer := m.renderFooter()
	
	// Combine sections
	content = append(content, header...)
	content = append(content, metrics...)
	content = append(content, creatures...)
	content = append(content, footer...)
	
	// Apply gradient backgrounds (opposite of left sidebar)
	var styledLines []string
	for i, line := range content {
		var bgStyle lipgloss.Style
		
		// Determine background color based on row (Cyan→Purple→Pink)
		if i < 7 {
			// Cyan background (rows 1-7)
			bgStyle = lipgloss.NewStyle().Background(lipgloss.Color(styles.ElectricCyan))
		} else if i < 15 {
			// Purple background (rows 8-15)
			bgStyle = lipgloss.NewStyle().Background(lipgloss.Color(styles.DeepPurple))
		} else {
			// Pink background (rows 16-22)
			bgStyle = lipgloss.NewStyle().Background(lipgloss.Color(styles.HotPink))
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

