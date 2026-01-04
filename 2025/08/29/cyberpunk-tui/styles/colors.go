package styles

import "github.com/charmbracelet/lipgloss/v2"

// Color palette constants
const (
	HotPink      = "#EC4899"  // RGB(236, 72, 153)
	ElectricCyan = "#22D3EE"  // RGB(34, 211, 238)
	NeonLime     = "#84CC16"  // RGB(132, 204, 22)
	DeepPurple   = "#7E22CE"  // RGB(126, 34, 206)
	OrangeFlame  = "#FB923C"  // RGB(251, 146, 60)
	MatrixGreen  = "#22C55E"  // RGB(34, 197, 94)
	PureBlack    = "#000000"  // RGB(0, 0, 0)
	GhostWhite   = "#F8FAFC"  // RGB(248, 250, 252)
)

// Gradient simulation characters
var GradientChars = []string{"█", "▓", "▒", "░"}

// Glitch character set
var GlitchChars = []string{"█", "▓", "▒", "░", "◆", "◇", "★", "☆", "♦", "♠", "♣", "♥", "◉", "◎", "●", "○"}

// Color styles
var (
	HotPinkStyle      = lipgloss.NewStyle().Foreground(lipgloss.Color(HotPink))
	ElectricCyanStyle = lipgloss.NewStyle().Foreground(lipgloss.Color(ElectricCyan))
	NeonLimeStyle     = lipgloss.NewStyle().Foreground(lipgloss.Color(NeonLime))
	DeepPurpleStyle   = lipgloss.NewStyle().Foreground(lipgloss.Color(DeepPurple))
	OrangeFlameStyle  = lipgloss.NewStyle().Foreground(lipgloss.Color(OrangeFlame))
	MatrixGreenStyle  = lipgloss.NewStyle().Foreground(lipgloss.Color(MatrixGreen))
	PureBlackStyle    = lipgloss.NewStyle().Foreground(lipgloss.Color(PureBlack))
	GhostWhiteStyle   = lipgloss.NewStyle().Foreground(lipgloss.Color(GhostWhite))
)

// Background styles
var (
	HotPinkBg      = lipgloss.NewStyle().Background(lipgloss.Color(HotPink))
	ElectricCyanBg = lipgloss.NewStyle().Background(lipgloss.Color(ElectricCyan))
	NeonLimeBg     = lipgloss.NewStyle().Background(lipgloss.Color(NeonLime))
	DeepPurpleBg   = lipgloss.NewStyle().Background(lipgloss.Color(DeepPurple))
	OrangeFlameBg  = lipgloss.NewStyle().Background(lipgloss.Color(OrangeFlame))
	MatrixGreenBg  = lipgloss.NewStyle().Background(lipgloss.Color(MatrixGreen))
	PureBlackBg    = lipgloss.NewStyle().Background(lipgloss.Color(PureBlack))
	GhostWhiteBg   = lipgloss.NewStyle().Background(lipgloss.Color(GhostWhite))
)

// Combined styles for common use cases
var (
	ButtonStyle = lipgloss.NewStyle().
			Padding(0, 1).
			Margin(0, 1)
	
	PanelStyle = lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color(ElectricCyan)).
			Padding(1)
	
	HeaderStyle = lipgloss.NewStyle().
			Bold(true).
			Padding(0, 1).
			Background(lipgloss.Color(NeonLime)).
			Foreground(lipgloss.Color(PureBlack))
)

// GetColorByIndex returns a color from the palette by index
func GetColorByIndex(index int) string {
	colors := []string{HotPink, ElectricCyan, NeonLime, DeepPurple, OrangeFlame, MatrixGreen}
	return colors[index%len(colors)]
}

// GetStyleByIndex returns a style from the palette by index
func GetStyleByIndex(index int) lipgloss.Style {
	styles := []lipgloss.Style{HotPinkStyle, ElectricCyanStyle, NeonLimeStyle, DeepPurpleStyle, OrangeFlameStyle, MatrixGreenStyle}
	return styles[index%len(styles)]
}

