package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"syscall"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/creack/pty"
	"github.com/hinshun/vt10x"
	"github.com/rs/zerolog"
)

// Global logger for debugging
var logger zerolog.Logger

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

// Enhanced styles for the TUI
var (
	titleStyle = lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1).
			MarginBottom(1)

	infoStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#874BFD")).
			Padding(0, 1).
			MarginRight(2)

	ptyStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#04B575")).
			Padding(0, 1)

	errorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FF5555")).
			Bold(true)

	demoStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#FF6B6B")).
			Padding(0, 1).
			MarginTop(1)
)

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
	autoDemo      bool
	lastResize    time.Time
	commandQueue  []string
	resizeHistory []string
	totalResizes  int
	startTime     time.Time

	// UI customization
	borderStyle   int
	colorTheme    int
	showMetrics   bool
}

// Initialize model with enhanced features
func initialModel() model {
	logger.Info().Msg("Initializing model")
	return model{
		ptyWidth:      60,
		ptyHeight:     20,
		status:        "🔄 Initializing PTY...",
		startTime:     time.Now(),
		commandQueue:  make([]string, 0),
		resizeHistory: make([]string, 0),
		borderStyle:   0,
		colorTheme:    0,
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

// Auto demo step with comprehensive debug logging
func (m *model) autoDemoStep() tea.Cmd {
	if !m.autoDemo {
		logger.Debug().Msg("Auto demo not active, skipping step")
		return nil
	}

	step := m.demoStep % 8  // Reduced steps for debugging
	logger.Info().Int("demoStep", m.demoStep).Int("stepMod", step).Bool("autoDemo", m.autoDemo).Msg("Auto demo step executing")

	switch step {
	case 0:
		logger.Debug().Msg("Demo step 0: Starting auto demo")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("echo 'Starting auto demo...'") 
		}
	case 1:
		logger.Debug().Msg("Demo step 1: First resize to 50x15")
		m.demoStep++
		return func() tea.Msg { 
			return resizeMsg{width: 50, height: 15} 
		}
	case 2:
		logger.Debug().Msg("Demo step 2: Small terminal command")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("echo 'Small terminal size'") 
		}
	case 3:
		logger.Debug().Msg("Demo step 3: Resize to 80x25")
		m.demoStep++
		return func() tea.Msg { 
			return resizeMsg{width: 80, height: 25} 
		}
	case 4:
		logger.Debug().Msg("Demo step 4: Medium terminal command")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("echo 'Medium terminal size'") 
		}
	case 5:
		logger.Debug().Msg("Demo step 5: Resize to 60x20")
		m.demoStep++
		return func() tea.Msg { 
			return resizeMsg{width: 60, height: 20} 
		}
	case 6:
		logger.Debug().Msg("Demo step 6: Date command")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("date") 
		}
	case 7:
		logger.Debug().Msg("Demo step 7: Demo complete, resetting")
		m.demoStep = 0
		return func() tea.Msg { 
			return commandMsg("echo '✅ Demo cycle complete!'") 
		}
	}

	logger.Debug().Int("step", step).Msg("Default case: scheduling next tick")
	return tea.Tick(time.Second*2, func(t time.Time) tea.Msg { 
		return tickMsg(t) 
	})
}

// Listen for PTY output
func listenForPTYOutput(ptmx *os.File) tea.Cmd {
	return func() tea.Msg {
		buf := make([]byte, 1024)
		n, err := ptmx.Read(buf)
		if err != nil {
			logger.Error().Err(err).Msg("PTY read error")
			return err
		}
		logger.Debug().Int("bytes", n).Msg("PTY output received")
		return ptyOutputMsg(buf[:n])
	}
}

// Init function
func (m model) Init() tea.Cmd {
	logger.Info().Msg("Bubble Tea Init called")
	
	// Initialize PTY and update model state
	if err := (&m).initPTY(); err != nil {
		logger.Error().Err(err).Msg("PTY initialization failed")
		return func() tea.Msg { return err }
	}

	return tea.Batch(
		listenForPTYOutput(m.ptmx),
		tea.Tick(time.Second*3, func(t time.Time) tea.Msg { return tickMsg(t) }),
	)
}

// Update function with enhanced message handling
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	logger.Debug().Str("msgType", fmt.Sprintf("%T", msg)).Msg("Update called")
	
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		logger.Info().Int("width", msg.Width).Int("height", msg.Height).Msg("Window size changed")
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case tea.KeyMsg:
		logger.Debug().Str("key", msg.String()).Msg("Key pressed")
		
		switch msg.String() {
		case "ctrl+c", "q":
			logger.Info().Msg("Quit requested")
			return m, tea.Quit
		case "a":
			logger.Info().Bool("autoDemo", !m.autoDemo).Msg("Auto demo toggled")
			m.autoDemo = !m.autoDemo
			if m.autoDemo {
				m.demoStep = 0
				return m, (&m).autoDemoStep()
			}
			return m, nil
		case "r":
			logger.Info().Msg("Manual resize requested")
			sizes := []struct{ w, h int }{
				{40, 15}, {60, 20}, {80, 25}, {100, 30}, {120, 35}, {70, 22},
			}
			size := sizes[m.totalResizes%len(sizes)]
			return m, func() tea.Msg { return resizeMsg{width: size.w, height: size.h} }
		case "c":
			logger.Info().Msg("Demo command requested")
			return m, func() tea.Msg { return commandMsg("echo 'Hello from PTY TUI!'") }
		}

	case ptyOutputMsg:
		logger.Debug().Int("bytes", len(msg)).Msg("PTY output processed")
		// Write to terminal emulator
		if m.term != nil {
			m.term.Write([]byte(msg))
		}
		// Store raw output for display (simplified approach)
		m.termContent += string(msg)
		// Keep only recent content to prevent memory issues
		if len(m.termContent) > 4000 {
			m.termContent = m.termContent[len(m.termContent)-4000:]
		}
		return m, listenForPTYOutput(m.ptmx)

	case resizeMsg:
		logger.Info().Int("width", msg.width).Int("height", msg.height).Msg("Resize message received")
		if err := (&m).resizePTY(msg.width, msg.height); err != nil {
			logger.Error().Err(err).Msg("Resize failed")
			m.err = err
		}
		return m, nil

	case commandMsg:
		logger.Info().Str("command", string(msg)).Msg("Command message received")
		if err := (&m).sendCommand(string(msg)); err != nil {
			logger.Error().Err(err).Msg("Command send failed")
			m.err = err
		}
		return m, nil

	case tickMsg:
		logger.Debug().Msg("Tick message received")
		if m.autoDemo {
			return m, (&m).autoDemoStep()
		}
		return m, tea.Tick(time.Second*3, func(t time.Time) tea.Msg { return tickMsg(t) })

	case error:
		logger.Error().Err(msg).Msg("Error received in update")
		m.err = msg
		return m, nil
	}

	return m, nil
}

// Enhanced view with better layout and metrics
func (m model) View() string {
	if m.width == 0 {
		return "Loading advanced PTY TUI..."
	}

	// Enhanced info panel with fixed spacing
	uptime := time.Since(m.startTime).Round(time.Second)
	info := fmt.Sprintf(
		"📏 Terminal: %dx%d\n🔲 PTY Size: %dx%d\n📊 Status: %s\n⏱️ Uptime: %v\n🔄 Resizes: %d",
		m.width, m.height, m.ptyWidth, m.ptyHeight, m.status, uptime, m.totalResizes,
	)

	if m.autoDemo {
		info += fmt.Sprintf("\n🎬 Auto Demo: Step %d", m.demoStep)
	}

	infoPanel := infoStyle.Width(30).Render(info)

	// PTY content with proper sizing
	ptyContent := "🔄 PTY output will appear here...\nPress 'a' to start auto demo\nPress 'c' to send commands\nPress 'r' for manual resize"
	if m.termContent != "" {
		ptyContent = m.termContent
	}

	ptyPanel := ptyStyle.Width(m.width - 35).Height(m.height - 8).Render(ptyContent)

	// Layout
	mainContent := lipgloss.JoinHorizontal(lipgloss.Top, infoPanel, ptyPanel)

	// Title and controls
	title := titleStyle.Render("🖥️ Advanced PTY TUI Demo with Dynamic Resizing")
	controls := "Controls: 'a' = auto demo | 'r' = resize | 'c' = command | 'q' = quit"

	// Error display
	errorMsg := ""
	if m.err != nil {
		errorMsg = errorStyle.Render(fmt.Sprintf("Error: %v", m.err))
	}

	return lipgloss.JoinVertical(lipgloss.Left,
		title,
		controls,
		mainContent,
		errorMsg,
	)
}

func main() {
	// Initialize logger first
	initLogger()
	logger.Info().Msg("Application starting")

	// Create and run the program
	p := tea.NewProgram(initialModel(), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		logger.Fatal().Err(err).Msg("Application failed")
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	
	logger.Info().Msg("Application exited normally")
}

