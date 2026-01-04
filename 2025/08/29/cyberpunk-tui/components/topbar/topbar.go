package topbar

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/models"
	"cyberpunk-tui/styles"
	"math/rand"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss/v2"
)

// TopBar represents the top bar component
type TopBar struct {
	models.BaseModel
	windowControls *WindowControls
	titleArea      *TitleArea
	rightControls  *RightControls
}

// NewTopBar creates a new top bar
func NewTopBar(width int) *TopBar {
	return &TopBar{
		BaseModel:      models.NewBaseModel(width, 1),
		windowControls: NewWindowControls(),
		titleArea:      NewTitleArea(),
		rightControls:  NewRightControls(),
	}
}

// Init implements tea.Model
func (m *TopBar) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *TopBar) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case models.ResizeMsg:
		m.Resize(msg.Width, msg.Height)
	}
	return m, nil
}

// View implements tea.Model
func (m *TopBar) View() string {
	leftSection := m.windowControls.Render()
	centerSection := m.titleArea.Render(m.Width)
	rightSection := m.rightControls.Render()
	
	// Calculate spacing
	leftWidth := lipgloss.Width(leftSection)
	rightWidth := lipgloss.Width(rightSection)
	centerWidth := lipgloss.Width(centerSection)
	
	totalUsed := leftWidth + rightWidth + centerWidth
	if totalUsed >= m.Width {
		// Truncate if too wide
		return leftSection + centerSection[:m.Width-leftWidth-rightWidth] + rightSection
	}
	
	// Center the title
	leftPadding := (m.Width - totalUsed) / 2
	rightPadding := m.Width - totalUsed - leftPadding
	
	return leftSection + strings.Repeat(" ", leftPadding) + centerSection + strings.Repeat(" ", rightPadding) + rightSection
}

// UpdateAnimation implements Animatable
func (m *TopBar) UpdateAnimation(clock *animations.AnimationClock) {
	m.windowControls.UpdateAnimation(clock)
	m.titleArea.UpdateAnimation(clock)
	m.rightControls.UpdateAnimation(clock)
}

// WindowControls manages the left window control buttons
type WindowControls struct {
	button1 *animations.AnimationState
	button2 *animations.AnimationState
	button3 *animations.AnimationState
}

// NewWindowControls creates new window controls
func NewWindowControls() *WindowControls {
	return &WindowControls{
		button1: animations.NewAnimationState(4, animations.MediumAnimation, 0),
		button2: animations.NewAnimationState(4, animations.MediumAnimation, 1),
		button3: animations.NewAnimationState(4, animations.MediumAnimation, 2),
	}
}

// UpdateAnimation updates the window controls animation
func (wc *WindowControls) UpdateAnimation(clock *animations.AnimationClock) {
	wc.button1.Update(clock)
	wc.button2.Update(clock)
	wc.button3.Update(clock)
}

// Render renders the window controls
func (wc *WindowControls) Render() string {
	chars1 := []string{"▓▓", "██", "▓▓", "░░"}
	chars2 := []string{"██", "▓▓", "▒▒", "██"}
	chars3 := []string{"░░", "▒▒", "▓▓", "██"}
	
	button1 := styles.OrangeFlameStyle.Render(chars1[wc.button1.GetFrame()])
	button2 := styles.ElectricCyanStyle.Render(chars2[wc.button2.GetFrame()])
	button3 := styles.HotPinkStyle.Render(chars3[wc.button3.GetFrame()])
	
	return button1 + " " + button2 + " " + button3
}

// TitleArea manages the center title with glitch effects
type TitleArea struct {
	glitchState *animations.GlitchState
	baseText    string
}

// NewTitleArea creates a new title area
func NewTitleArea() *TitleArea {
	return &TitleArea{
		glitchState: animations.NewGlitchState(),
		baseText:    "░▓█ NEURAL_CHAOS.exe █▓░",
	}
}

// UpdateAnimation updates the title area animation
func (ta *TitleArea) UpdateAnimation(clock *animations.AnimationClock) {
	ta.glitchState.Update()
}

// Render renders the title area
func (ta *TitleArea) Render(totalWidth int) string {
	text := ta.baseText
	
	if ta.glitchState.IsActive() {
		text = ta.applyGlitch(text)
	}
	
	// Apply alternating background colors
	result := ""
	colors := []lipgloss.Style{styles.HotPinkBg, styles.DeepPurpleBg, styles.ElectricCyanBg}
	
	for i, char := range text {
		colorIndex := i / 3 % len(colors)
		result += colors[colorIndex].Foreground(lipgloss.Color(styles.GhostWhite)).Render(string(char))
	}
	
	return result
}

// applyGlitch applies glitch effects to text
func (ta *TitleArea) applyGlitch(text string) string {
	runes := []rune(text)
	intensity := ta.glitchState.GetIntensity()
	
	for i := range runes {
		if rand.Float64() < intensity {
			runes[i] = []rune(styles.GlitchChars[rand.Intn(len(styles.GlitchChars))])[0]
		}
	}
	
	return string(runes)
}

// RightControls manages the right control symbols
type RightControls struct {
	minimize *animations.AnimationState
	close    *animations.AnimationState
}

// NewRightControls creates new right controls
func NewRightControls() *RightControls {
	return &RightControls{
		minimize: animations.NewAnimationState(2, 10, 0), // 1 second cycle
		close:    animations.NewAnimationState(2, 10, 1), // Opposite phase
	}
}

// UpdateAnimation updates the right controls animation
func (rc *RightControls) UpdateAnimation(clock *animations.AnimationClock) {
	rc.minimize.Update(clock)
	rc.close.Update(clock)
}

// Render renders the right controls
func (rc *RightControls) Render() string {
	minChars := []string{"◆", "◇"}
	closeChars := []string{"◇", "◆"}
	
	minButton := styles.ElectricCyanBg.Foreground(lipgloss.Color(styles.PureBlack)).Render(minChars[rc.minimize.GetFrame()])
	closeButton := styles.ElectricCyanBg.Foreground(lipgloss.Color(styles.PureBlack)).Render(closeChars[rc.close.GetFrame()])
	
	return minButton + " " + closeButton
}

