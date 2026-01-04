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
	logFile, err := os.OpenFile("pty-debug.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalf("Failed to create log file: %v", err)
	}

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
)

// Messages
type ptyOutputMsg []byte
type resizeMsg struct{ width, height int }
type tickMsg time.Time
type commandMsg string
type initCompleteMsg struct{}

// PTY Manager - separate struct to manage PTY lifecycle
type ptyManager struct {
	ptmx     *os.File
	cmd      *exec.Cmd
	term     vt10x.Terminal
	ready    bool
	width    int
	height   int
}

// Initialize PTY manager
func newPTYManager(width, height int) (*ptyManager, error) {
	logger.Info().Int("width", width).Int("height", height).Msg("Creating PTY manager")
	
	pm := &ptyManager{
		width:  width,
		height: height,
	}
	
	if err := pm.start(); err != nil {
		return nil, err
	}
	
	return pm, nil
}

// Start the PTY
func (pm *ptyManager) start() error {
	logger.Info().Msg("Starting PTY")
	
	// Create command
	shell := os.Getenv("SHELL")
	if shell == "" {
		shell = "/bin/bash"
	}
	logger.Info().Str("shell", shell).Msg("Shell detected")
	
	pm.cmd = exec.Command(shell)

	// Start PTY
	size := &pty.Winsize{
		Cols: uint16(pm.width),
		Rows: uint16(pm.height),
	}

	ptmx, err := pty.StartWithSize(pm.cmd, size)
	if err != nil {
		logger.Error().Err(err).Msg("Failed to start PTY")
		return fmt.Errorf("failed to start PTY: %w", err)
	}
	logger.Info().Int("pid", pm.cmd.Process.Pid).Msg("PTY process started")

	pm.ptmx = ptmx
	pm.term = vt10x.New(vt10x.WithSize(pm.width, pm.height))
	logger.Info().Msg("Terminal emulator created")
	
	// Wait for PTY to be ready
	time.Sleep(100 * time.Millisecond)
	pm.ready = true
	
	logger.Info().Bool("ready", pm.ready).Msg("PTY manager initialization complete")
	return nil
}

// Check if PTY is healthy
func (pm *ptyManager) isHealthy() bool {
	if !pm.ready || pm.ptmx == nil || pm.cmd == nil || pm.cmd.Process == nil {
		return false
	}
	
	// Check if process is still alive
	if err := pm.cmd.Process.Signal(syscall.Signal(0)); err != nil {
		logger.Error().Err(err).Msg("PTY process health check failed")
		pm.ready = false
		return false
	}
	
	return true
}

// Resize PTY
func (pm *ptyManager) resize(width, height int) error {
	logger.Info().Int("width", width).Int("height", height).Msg("PTY resize requested")
	
	if !pm.isHealthy() {
		logger.Warn().Msg("PTY not healthy for resize")
		return fmt.Errorf("PTY not healthy")
	}

	oldSize := fmt.Sprintf("%dx%d", pm.width, pm.height)
	pm.width = width
	pm.height = height

	size := &pty.Winsize{
		Cols: uint16(width),
		Rows: uint16(height),
	}

	err := pty.Setsize(pm.ptmx, size)
	if err != nil {
		logger.Error().Err(err).Msg("PTY resize failed")
		return fmt.Errorf("failed to resize PTY: %w", err)
	}

	// Resize terminal emulator
	vt10x.ResizePty(pm.ptmx, width, height)
	
	logger.Info().Str("oldSize", oldSize).Str("newSize", fmt.Sprintf("%dx%d", width, height)).Msg("PTY resize successful")
	return nil
}

// Send command to PTY
func (pm *ptyManager) sendCommand(cmd string) error {
	logger.Debug().Str("command", cmd).Msg("Command send requested")
	
	if !pm.isHealthy() {
		logger.Warn().Str("command", cmd).Msg("PTY not healthy for command")
		return fmt.Errorf("PTY not healthy")
	}

	_, err := pm.ptmx.Write([]byte(cmd + "\n"))
	if err != nil {
		logger.Error().Err(err).Str("command", cmd).Msg("Failed to write command to PTY")
		return err
	}
	
	logger.Info().Str("command", cmd).Msg("Command sent successfully")
	return nil
}

// Close PTY
func (pm *ptyManager) close() {
	logger.Info().Msg("Closing PTY manager")
	if pm.ptmx != nil {
		pm.ptmx.Close()
	}
	if pm.cmd != nil && pm.cmd.Process != nil {
		pm.cmd.Process.Kill()
	}
	pm.ready = false
}

// Model represents the application state
type model struct {
	// PTY manager
	ptyMgr *ptyManager

	// UI state
	width       int
	height      int
	termContent string
	status      string
	err         error

	// Demo state
	demoStep      int
	autoDemo      bool
	lastResize    time.Time
	commandQueue  []string
	resizeHistory []string
	totalResizes  int
	startTime     time.Time
	initialized   bool
}

// Initialize model
func initialModel() model {
	logger.Info().Msg("Initializing model")
	return model{
		status:        "🔄 Initializing PTY...",
		startTime:     time.Now(),
		commandQueue:  make([]string, 0),
		resizeHistory: make([]string, 0),
	}
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

// Auto demo step
func (m *model) autoDemoStep() tea.Cmd {
	if !m.autoDemo || m.ptyMgr == nil {
		logger.Debug().Msg("Auto demo not active or PTY not ready")
		return nil
	}

	step := m.demoStep % 6  // Simplified demo
	logger.Info().Int("demoStep", m.demoStep).Int("stepMod", step).Msg("Auto demo step executing")

	switch step {
	case 0:
		logger.Debug().Msg("Demo step 0: Starting auto demo")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("echo 'Starting auto demo...'") 
		}
	case 1:
		logger.Debug().Msg("Demo step 1: Resize to 50x15")
		m.demoStep++
		return func() tea.Msg { 
			return resizeMsg{width: 50, height: 15} 
		}
	case 2:
		logger.Debug().Msg("Demo step 2: Small size command")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("echo 'Small terminal'") 
		}
	case 3:
		logger.Debug().Msg("Demo step 3: Resize to 80x25")
		m.demoStep++
		return func() tea.Msg { 
			return resizeMsg{width: 80, height: 25} 
		}
	case 4:
		logger.Debug().Msg("Demo step 4: Medium size command")
		m.demoStep++
		return func() tea.Msg { 
			return commandMsg("echo 'Medium terminal'") 
		}
	case 5:
		logger.Debug().Msg("Demo step 5: Demo complete")
		m.demoStep = 0
		return func() tea.Msg { 
			return commandMsg("echo '✅ Demo cycle complete!'") 
		}
	}

	return tea.Tick(time.Second*2, func(t time.Time) tea.Msg { 
		return tickMsg(t) 
	})
}
	// Init function
func (m model) Init() tea.Cmd {
	logger.Info().Msg("Bubble Tea Init called")
	
	// Initialize PTY manager in a separate command
	return func() tea.Msg {
		_, err := newPTYManager(60, 20)
		if err != nil {
			logger.Error().Err(err).Msg("PTY manager creation failed")
			return err
		}
		
		return initCompleteMsg{}
	}
}

// Update function
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	logger.Debug().Str("msgType", fmt.Sprintf("%T", msg)).Msg("Update called")
	
	switch msg := msg.(type) {
	case initCompleteMsg:
		logger.Info().Msg("PTY initialization complete")
		// Create PTY manager
		ptyMgr, err := newPTYManager(60, 20)
		if err != nil {
			logger.Error().Err(err).Msg("PTY manager creation failed")
			m.err = err
			return m, nil
		}
		m.ptyMgr = ptyMgr
		m.initialized = true
		m.status = "✅ PTY ready (60x20)"
		
		return m, tea.Batch(
			listenForPTYOutput(m.ptyMgr.ptmx),
			tea.Tick(time.Second*3, func(t time.Time) tea.Msg { return tickMsg(t) }),
		)

	case tea.WindowSizeMsg:
		logger.Info().Int("width", msg.Width).Int("height", msg.Height).Msg("Window size changed")
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case tea.KeyMsg:
		logger.Debug().Str("key", msg.String()).Msg("Key pressed")
		
		if !m.initialized {
			return m, nil
		}
		
		switch msg.String() {
		case "ctrl+c", "q":
			logger.Info().Msg("Quit requested")
			if m.ptyMgr != nil {
				m.ptyMgr.close()
			}
			return m, tea.Quit
		case "a":
			logger.Info().Bool("autoDemo", !m.autoDemo).Msg("Auto demo toggled")
			m.autoDemo = !m.autoDemo
			if m.autoDemo {
				m.demoStep = 0
				return m, m.autoDemoStep()
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
		if m.ptyMgr != nil && m.ptyMgr.term != nil {
			m.ptyMgr.term.Write([]byte(msg))
		}
		m.termContent += string(msg)
		if len(m.termContent) > 4000 {
			m.termContent = m.termContent[len(m.termContent)-4000:]
		}
		return m, listenForPTYOutput(m.ptyMgr.ptmx)

	case resizeMsg:
		logger.Info().Int("width", msg.width).Int("height", msg.height).Msg("Resize message received")
		if m.ptyMgr != nil {
			if err := m.ptyMgr.resize(msg.width, msg.height); err != nil {
				logger.Error().Err(err).Msg("Resize failed")
				m.err = err
			} else {
				m.totalResizes++
				m.status = fmt.Sprintf("🔄 PTY resized to %dx%d (resize #%d)", msg.width, msg.height, m.totalResizes)
				m.lastResize = time.Now()
			}
		}
		return m, nil

	case commandMsg:
		logger.Info().Str("command", string(msg)).Msg("Command message received")
		if m.ptyMgr != nil {
			if err := m.ptyMgr.sendCommand(string(msg)); err != nil {
				logger.Error().Err(err).Msg("Command send failed")
				m.err = err
			}
		}
		return m, nil

	case tickMsg:
		logger.Debug().Msg("Tick message received")
		if m.autoDemo {
			return m, m.autoDemoStep()
		}
		return m, tea.Tick(time.Second*3, func(t time.Time) tea.Msg { return tickMsg(t) })

	case error:
		logger.Error().Err(msg).Msg("Error received in update")
		m.err = msg
		return m, nil
	}

	return m, nil
}

// View function
func (m model) View() string {
	if m.width == 0 {
		return "Loading advanced PTY TUI..."
	}

	if !m.initialized {
		return "🔄 Initializing PTY manager..."
	}

	// Info panel
	uptime := time.Since(m.startTime).Round(time.Second)
	ptySize := "Unknown"
	if m.ptyMgr != nil {
		ptySize = fmt.Sprintf("%dx%d", m.ptyMgr.width, m.ptyMgr.height)
	}
	
	info := fmt.Sprintf(
		"📏 Terminal: %dx%d\n🔲 PTY Size: %s\n📊 Status: %s\n⏱️ Uptime: %v\n🔄 Resizes: %d",
		m.width, m.height, ptySize, m.status, uptime, m.totalResizes,
	)

	if m.autoDemo {
		info += fmt.Sprintf("\n🎬 Auto Demo: Step %d", m.demoStep)
	}

	infoPanel := infoStyle.Width(30).Render(info)

	// PTY content
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

