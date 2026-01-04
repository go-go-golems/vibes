package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/creack/pty"
	"github.com/hinshun/vt10x"
	"github.com/rs/zerolog"
)

// Enhanced styles for the TUI
var (
	// Global logger for debugging
	logger zerolog.Logger
	
	titleStyle = lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1).
			MarginBottom(1)

	infoStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#874BFD")).
			Padding(1, 2).
			Width(35)

	ptyBorderStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.DoubleBorder()).
			BorderForeground(lipgloss.Color("#04B575")).
			Padding(1).
			MarginLeft(2)

	statusStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#626262"))

	helpStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#626262")).
			MarginTop(1)

	errorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FF5F87")).
			Bold(true)

	successStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#04B575")).
			Bold(true)

	highlightStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FFD700")).
			Bold(true)

	demoStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#FF6B6B")).
			Padding(0, 1).
			MarginTop(1)
)

// Initialize logger with file output
func initLogger() {
	// Create log file
	logFile, err := os.OpenFile("pty-debug.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalf("Failed to create log file: %v", err)
	}

	// Setup zerolog with timestamp and caller info
	logger = zerolog.New(logFile).
		With().
		Timestamp().
		Caller().
		Str("component", "pty-tui").
		Logger()

	logger.Info().Msg("=== PTY TUI Debug Session Started ===")
}

// Messages
type ptyOutputMsg []byte
type resizeMsg struct{ width, height int }
type tickMsg time.Time
type commandMsg string

// Model represents the enhanced application state
type model struct {
	// PTY components
	ptmx     *os.File
	cmd      *exec.Cmd
	term     vt10x.Terminal
	ptyReady bool

	// UI state
	width       int
	height      int
	ptyWidth    int
	ptyHeight   int
	termContent string
	status      string
	err         error

	// Enhanced demo state
	demoStep      int
	demoRunning   bool
	lastResize    time.Time
	commandQueue  []string
	resizeHistory []string
	totalResizes  int
	startTime     time.Time

	// Interactive features
	autoDemo      bool
	showMetrics   bool
	borderStyle   int
	colorScheme   int
}

// Initialize the enhanced model
func initialModel() model {
	return model{
		width:         80,
		height:        24,
		ptyWidth:      60,
		ptyHeight:     20,
		status:        "🚀 Initializing Advanced PTY TUI...",
		demoStep:      0,
		commandQueue:  []string{},
		resizeHistory: []string{},
		startTime:     time.Now(),
		borderStyle:   0,
		colorScheme:   0,
		showMetrics:   true,
	}
}

// Initialize PTY with enhanced error handling and debug logging
func (m *model) initPTY() error {
	logger.Info().Msg("Starting PTY initialization")
	
	// Create command
	shell := os.Getenv("SHELL")
	if shell == "" {
		shell = "/bin/bash"
	}
	logger.Info().Str("shell", shell).Msg("Shell detected")
	
	m.cmd = exec.Command(shell)

	// Start PTY
	size := &pty.Winsize{
		Cols: uint16(m.ptyWidth),
		Rows: uint16(m.ptyHeight),
	}
	logger.Info().Int("width", m.ptyWidth).Int("height", m.ptyHeight).Msg("PTY size configured")

	ptmx, err := pty.StartWithSize(m.cmd, size)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to start PTY")
		return fmt.Errorf("failed to start PTY: %w", err)
	}
	logger.Info().Int("pid", m.cmd.Process.Pid).Msg("PTY process started")

	m.ptmx = ptmx
	m.term = vt10x.New(vt10x.WithSize(m.ptyWidth, m.ptyHeight))
	logger.Info().Msg("Terminal emulator created")
	
	// Wait a moment for PTY to be fully ready
	logger.Debug().Msg("Waiting for PTY stabilization")
	time.Sleep(100 * time.Millisecond)
	
	m.ptyReady = true
	m.status = fmt.Sprintf("✅ PTY ready (%dx%d)", m.ptyWidth, m.ptyHeight)
	logger.Info().Bool("ptyReady", m.ptyReady).Str("status", m.status).Msg("PTY initialization complete")

	// Send initial setup commands
	go func() {
		time.Sleep(200 * time.Millisecond)
		logger.Debug().Msg("Sending initial setup commands")
		m.sendCommand("clear")
		m.sendCommand("echo 'Welcome to Advanced PTY TUI Demo!'")
		m.sendCommand("echo 'PTY is fully operational and ready for commands.'")
	}()

	return nil
}

// Enhanced resize with history tracking and debug logging
func (m *model) resizePTY(width, height int) error {
	logger.Info().Int("width", width).Int("height", height).Msg("PTY resize requested")
	
	if !m.ptyReady || m.ptmx == nil {
		logger.Warn().Bool("ptyReady", m.ptyReady).Bool("ptmxNil", m.ptmx == nil).Msg("PTY not ready for resize")
		m.status = "⚠️ PTY not ready for resize"
		return fmt.Errorf("PTY not ready")
	}

	// Check if PTY process is still alive
	if m.cmd != nil && m.cmd.Process != nil {
		if err := m.cmd.Process.Signal(syscall.Signal(0)); err != nil {
			logger.Error().Err(err).Int("pid", m.cmd.Process.Pid).Msg("PTY process health check failed")
			m.ptyReady = false
			m.status = "❌ PTY process died"
			return fmt.Errorf("PTY process not running")
		}
		logger.Debug().Int("pid", m.cmd.Process.Pid).Msg("PTY process health check passed")
	}

	oldSize := fmt.Sprintf("%dx%d", m.ptyWidth, m.ptyHeight)
	m.ptyWidth = width
	m.ptyHeight = height
	logger.Debug().Str("oldSize", oldSize).Str("newSize", fmt.Sprintf("%dx%d", width, height)).Msg("Size transition")

	// Resize the PTY
	size := &pty.Winsize{
		Cols: uint16(width),
		Rows: uint16(height),
	}

	err := pty.Setsize(m.ptmx, size)
	if err != nil {
		logger.Error().Err(err).Msg("PTY resize failed")
		m.status = "❌ PTY resize failed"
		return fmt.Errorf("failed to resize PTY: %w", err)
	}
	logger.Info().Msg("PTY system resize successful")

	// Resize the terminal emulator
	vt10x.ResizePty(m.ptmx, width, height)
	logger.Debug().Msg("Terminal emulator resized")

	// Track resize history
	newSize := fmt.Sprintf("%dx%d", width, height)
	resizeInfo := fmt.Sprintf("%s → %s", oldSize, newSize)
	m.resizeHistory = append(m.resizeHistory, resizeInfo)
	if len(m.resizeHistory) > 5 {
		m.resizeHistory = m.resizeHistory[1:]
	}

	m.totalResizes++
	m.status = fmt.Sprintf("🔄 PTY resized to %dx%d (resize #%d)", width, height, m.totalResizes)
	m.lastResize = time.Now()
	logger.Info().Int("totalResizes", m.totalResizes).Str("status", m.status).Msg("Resize operation complete")

	// Send notification to PTY
	go func() {
		time.Sleep(50 * time.Millisecond)
		logger.Debug().Str("command", fmt.Sprintf("echo 'PTY resized to %dx%d'", width, height)).Msg("Sending resize notification")
		m.sendCommand(fmt.Sprintf("echo 'PTY resized to %dx%d'", width, height))
	}()

	return nil
}

// Enhanced command sending with queue and debug logging
func (m *model) sendCommand(cmd string) error {
	logger.Debug().Str("command", cmd).Msg("Command send requested")
	
	if !m.ptyReady || m.ptmx == nil {
		logger.Warn().Bool("ptyReady", m.ptyReady).Bool("ptmxNil", m.ptmx == nil).Str("command", cmd).Msg("PTY not ready for command")
		return fmt.Errorf("PTY not ready")
	}

	// Add to command history
	m.commandQueue = append(m.commandQueue, cmd)
	if len(m.commandQueue) > 10 {
		m.commandQueue = m.commandQueue[1:]
	}
	logger.Debug().Int("queueLength", len(m.commandQueue)).Msg("Command added to queue")

	_, err := m.ptmx.Write([]byte(cmd + "\n"))
	if err != nil {
		logger.Error().Err(err).Str("command", cmd).Msg("Failed to write command to PTY")
		return err
	}
	
	logger.Info().Str("command", cmd).Msg("Command sent to PTY successfully")
	return nil
}

// Get border style based on current selection
func (m *model) getBorderStyle() lipgloss.Border {
	borders := []lipgloss.Border{
		lipgloss.DoubleBorder(),
		lipgloss.RoundedBorder(),
		lipgloss.ThickBorder(),
		lipgloss.NormalBorder(),
	}
	return borders[m.borderStyle%len(borders)]
}

// Get color scheme
func (m *model) getColors() (lipgloss.Color, lipgloss.Color) {
	schemes := [][2]lipgloss.Color{
		{"#04B575", "#874BFD"}, // Green/Purple
		{"#FF6B6B", "#4ECDC4"}, // Red/Teal
		{"#FFD93D", "#6BCF7F"}, // Yellow/Green
		{"#A8E6CF", "#FF8B94"}, // Mint/Pink
	}
	scheme := schemes[m.colorScheme%len(schemes)]
	return scheme[0], scheme[1]
}

// Init initializes the enhanced Bubble Tea program
func (m model) Init() tea.Cmd {
	return tea.Batch(
		tea.EnterAltScreen,
		func() tea.Msg {
			if err := m.initPTY(); err != nil {
				return err
			}
			return nil
		},
		listenForPTYOutput(m.ptmx),
		tickCmd(),
	)
}

// Enhanced update with more interactive features
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case ptyOutputMsg:
		if m.ptyReady {
			m.term.Write([]byte(msg))
			m.termContent = m.term.String()
		}
		return m, listenForPTYOutput(m.ptmx)

	case tickMsg:
		if m.autoDemo {
			return m, m.runEnhancedDemo()
		}
		return m, tickCmd()

	case resizeMsg:
		err := m.resizePTY(msg.width, msg.height)
		if err != nil {
			m.err = err
		}
		return m, nil

	case commandMsg:
		err := m.sendCommand(string(msg))
		if err != nil {
			m.err = err
		}
		return m, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			if m.ptmx != nil {
				m.sendCommand("exit")
				time.Sleep(100 * time.Millisecond)
				m.ptmx.Close()
			}
			return m, tea.Quit

		case "a":
			// Toggle auto demo
			m.autoDemo = !m.autoDemo
			if m.autoDemo {
				m.demoStep = 0
				m.status = "🎬 Auto demo started - watch dynamic resizing!"
			} else {
				m.status = "⏸️  Auto demo paused"
			}
			return m, nil

		case "r":
			// Manual resize cycle
			sizes := [][2]int{{40, 15}, {60, 20}, {80, 25}, {100, 30}, {120, 35}}
			size := sizes[m.demoStep%len(sizes)]
			m.demoStep++
			return m, func() tea.Msg {
				return resizeMsg{width: size[0], height: size[1]}
			}

		case "b":
			// Cycle border styles
			m.borderStyle++
			m.status = fmt.Sprintf("🎨 Border style changed (#%d)", m.borderStyle%4+1)
			return m, nil

		case "t":
			// Cycle color themes
			m.colorScheme++
			m.status = fmt.Sprintf("🌈 Color theme changed (#%d)", m.colorScheme%4+1)
			return m, nil

		case "m":
			// Toggle metrics display
			m.showMetrics = !m.showMetrics
			if m.showMetrics {
				m.status = "📊 Metrics display enabled"
			} else {
				m.status = "📊 Metrics display disabled"
			}
			return m, nil

		case "c":
			// Send demo commands
			commands := []string{
				"echo '🚀 PTY Demo Command'",
				"date '+%Y-%m-%d %H:%M:%S'",
				"pwd",
				"ls -la | head -10",
				"echo 'Terminal size:' $(tput cols)x$(tput lines)",
				"echo '✨ Command executed successfully!'",
			}
			cmd := commands[m.demoStep%len(commands)]
			m.demoStep++
			return m, func() tea.Msg { return commandMsg(cmd) }

		case "1", "2", "3", "4", "5", "6":
			// Quick resize presets
			sizes := map[string][2]int{
				"1": {30, 10},  // Tiny
				"2": {50, 15},  // Small
				"3": {70, 20},  // Medium
				"4": {90, 25},  // Large
				"5": {110, 30}, // Extra Large
				"6": {130, 35}, // Huge
			}
			if size, ok := sizes[msg.String()]; ok {
				return m, func() tea.Msg {
					return resizeMsg{width: size[0], height: size[1]}
				}
			}

		case "enter":
			// Send current command
			return m, func() tea.Msg {
				return commandMsg("echo 'Interactive command sent!'")
			}

		default:
			// Forward other keys to PTY (for interactive shell)
			if m.ptyReady 		case error:
			logger.Error().Err(msg).Msg("Error received in update")
			m.err = msg
			return m, nil
		}

		return m, nil
	}

	// Auto demo step with comprehensive debug logging
	func (m *model) autoDemoStep() tea.Cmd {
		if !m.autoDemo {
			logger.Debug().Msg("Auto demo not active, skipping step")
			return nil
		}

		step := m.demoStep % 16
		logger.Info().Int("demoStep", m.demoStep).Int("stepMod", step).Bool("autoDemo", m.autoDemo).Msg("Auto demo step executing")

		switch step {
		case 0:
			logger.Debug().Msg("Demo step 0: Starting auto demo")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo 'Starting auto demo...'") 
			}
		case 1:
			logger.Debug().Msg("Demo step 1: First resize to 40x15")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 40, height: 15} 
			}
		case 2:
			logger.Debug().Msg("Demo step 2: Tiny terminal command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo 'Tiny terminal size'") 
			}
		case 3:
			logger.Debug().Msg("Demo step 3: Resize to 50x15")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 50, height: 15} 
			}
		case 4:
			logger.Debug().Msg("Demo step 4: Small terminal command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo 'Small terminal size'") 
			}
		case 5:
			logger.Debug().Msg("Demo step 5: Resize to 80x25")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 80, height: 25} 
			}
		case 6:
			logger.Debug().Msg("Demo step 6: Medium terminal command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo 'Medium terminal size - perfect for coding'") 
			}
		case 7:
			logger.Debug().Msg("Demo step 7: Resize to 120x35")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 120, height: 35} 
			}
		case 8:
			logger.Debug().Msg("Demo step 8: Large terminal command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo 'Large terminal - great for logs and data'") 
			}
		case 9:
			logger.Debug().Msg("Demo step 9: List files command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("ls -la") 
			}
		case 10:
			logger.Debug().Msg("Demo step 10: Resize to 60x20")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 60, height: 20} 
			}
		case 11:
			logger.Debug().Msg("Demo step 11: Date command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("date") 
			}
		case 12:
			logger.Debug().Msg("Demo step 12: Resize to 100x30")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 100, height: 30} 
			}
		case 13:
			logger.Debug().Msg("Demo step 13: Dynamic resizing works command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo 'Dynamic resizing works perfectly!'") 
			}
		case 14:
			logger.Debug().Msg("Demo step 14: Resize to 70x22")
			m.demoStep++
			return func() tea.Msg { 
				return resizeMsg{width: 70, height: 22} 
			}
		case 15:
			logger.Debug().Msg("Demo step 15: Demo complete command")
			m.demoStep++
			return func() tea.Msg { 
				return commandMsg("echo '✅ Demo cycle complete!'") 
			}
		case 16:
			logger.Info().Msg("Demo step 16: Resetting demo cycle")
			m.demoStep = 0
			return tea.Tick(time.Second*3, func(t time.Time) tea.Msg { 
				return tickMsg(t) 
			})
		}

		logger.Debug().Int("step", step).Msg("Default case: scheduling next tick")
		return tea.Tick(time.Second*2, func(t time.Time) tea.Msg { 
			return tickMsg(t) 
		})
	}

	// Enhanced view with better layout and metricsring {
	if m.width == 0 {
		return "Loading advanced PTY TUI..."
	}

	// Get current colors
	primaryColor, secondaryColor := m.getColors()

	// Dynamic title with status
	title := titleStyle.
		Background(primaryColor).
		Render("🖥️  Advanced PTY TUI Demo with Dynamic Resizing")

	// Enhanced info panel with fixed alignment
	uptime := time.Since(m.startTime).Truncate(time.Second)
	info := fmt.Sprintf(
		"📏 Terminal: %dx%d\n🔲 PTY Size: %dx%d\n📊 Status: %s\n⏱️ Uptime: %v\n🔄 Resizes: %d",
		m.width, m.height,
		m.ptyWidth, m.ptyHeight,
		m.status,
		uptime,
		m.totalResizes,
	)

	if !m.lastResize.IsZero() {
		timeSince := time.Since(m.lastResize).Truncate(time.Millisecond)
		info += fmt.Sprintf("\n⏰ Last Resize: %v ago", timeSince)
	}

	if m.showMetrics && len(m.resizeHistory) > 0 {
		info += "\n\n📈 Recent Resizes:"
		for i, resize := range m.resizeHistory {
			if i == len(m.resizeHistory)-1 {
				info += "\n" + highlightStyle.Render("→ "+resize)
			} else {
				info += "\n  " + resize
			}
		}
	}

	if len(m.commandQueue) > 0 && m.showMetrics {
		info += "\n\n💻 Recent Commands:"
		for i := len(m.commandQueue) - 3; i < len(m.commandQueue) && i >= 0; i++ {
			if i >= 0 {
				cmd := m.commandQueue[i]
				if len(cmd) > 25 {
					cmd = cmd[:22] + "..."
				}
				info += "\n  " + cmd
			}
		}
	}

	if m.err != nil {
		info += "\n\n" + errorStyle.Render("❌ Error: "+m.err.Error())
	}

	infoPanel := infoStyle.
		BorderForeground(secondaryColor).
		Render(info)

	// Enhanced PTY panel with dynamic border
	ptyContent := m.termContent
	if ptyContent == "" {
		ptyContent = "🔄 PTY output will appear here...\n\nPress 'a' to start auto demo\nPress 'c' to send commands\nPress 'r' for manual resize"
	}

	// Ensure PTY content fits within the border
	lines := strings.Split(ptyContent, "\n")
	if len(lines) > m.ptyHeight {
		lines = lines[len(lines)-m.ptyHeight:]
		ptyContent = strings.Join(lines, "\n")
	}

	ptyPanel := ptyBorderStyle.
		BorderStyle(m.getBorderStyle()).
		BorderForeground(primaryColor).
		Width(m.ptyWidth + 4).
		Height(m.ptyHeight + 2).
		Render(ptyContent)

	// Demo status
	var demoStatus string
	if m.autoDemo {
		demoStatus = demoStyle.
			BorderForeground(primaryColor).
			Render("🎬 AUTO DEMO RUNNING")
	}

	// Enhanced help text
	help := helpStyle.Render(
		"🎮 Controls: [a] auto demo • [r] manual resize • [c] send command • [1-6] size presets\n" +
			"🎨 Styling: [b] border style • [t] color theme • [m] toggle metrics • [q] quit",
	)

	// Layout with better spacing
	var content string
	if demoStatus != "" {
		content = lipgloss.JoinVertical(
			lipgloss.Left,
			title,
			demoStatus,
			"",
			lipgloss.JoinHorizontal(
				lipgloss.Top,
				infoPanel,
				ptyPanel,
			),
			"",
			help,
		)
	} else {
		content = lipgloss.JoinVertical(
			lipgloss.Left,
			title,
			"",
			lipgloss.JoinHorizontal(
				lipgloss.Top,
				infoPanel,
				ptyPanel,
			),
			"",
			help,
		)
	}

	return content
}

// Listen for PTY output
func listenForPTYOutput(ptmx *os.File) tea.Cmd {
	return func() tea.Msg {
		if ptmx == nil {
			return nil
		}

		buf := make([]byte, 4096)
		n, err := ptmx.Read(buf)
		if err != nil {
			return err
		}
		return ptyOutputMsg(buf[:n])
	}
}

// Tick command for demo progression
func tickCmd() tea.Cmd {
	return tea.Tick(time.Second, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

func main() {
	// Create and run the enhanced program
	p := tea.NewProgram(
		initialModel(),
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)

	if _, err := p.Run(); err != nil {
		log.Fatal(err)
	}
}

