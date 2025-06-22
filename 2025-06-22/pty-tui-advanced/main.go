package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/creack/pty"
	"github.com/hinshun/vt10x"
)

// Styles for the TUI
var (
	titleStyle = lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1)

	infoStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#874BFD")).
			Padding(1, 2)

	ptyBorderStyle = lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#04B575")).
			Padding(1)

	statusStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#626262")).
			Render

	helpStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#626262")).
			Render

	errorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FF5F87")).
			Bold(true).
			Render
)

// Messages
type ptyOutputMsg []byte
type resizeMsg struct{ width, height int }
type tickMsg time.Time

// Model represents the application state
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

	// Demo state
	demoStep    int
	demoRunning bool
	lastResize  time.Time
}

// Initialize the model
func initialModel() model {
	return model{
		width:     80,
		height:    24,
		ptyWidth:  60,
		ptyHeight: 20,
		status:    "Initializing PTY...",
		demoStep:  0,
	}
}

// Initialize PTY
func (m *model) initPTY() error {
	// Create command
	shell := os.Getenv("SHELL")
	if shell == "" {
		shell = "/bin/bash"
	}
	m.cmd = exec.Command(shell)

	// Start PTY
	size := &pty.Winsize{
		Cols: uint16(m.ptyWidth),
		Rows: uint16(m.ptyHeight),
	}

	ptmx, err := pty.StartWithSize(m.cmd, size)
	if err != nil {
		return fmt.Errorf("failed to start PTY: %w", err)
	}

	m.ptmx = ptmx
	m.term = vt10x.New(vt10x.WithSize(m.ptyWidth, m.ptyHeight))
	m.ptyReady = true
	m.status = fmt.Sprintf("PTY ready (%dx%d) - PID: %d", m.ptyWidth, m.ptyHeight, m.cmd.Process.Pid)

	return nil
}

// Resize PTY
func (m *model) resizePTY(width, height int) error {
	if !m.ptyReady || m.ptmx == nil {
		return fmt.Errorf("PTY not ready")
	}

	m.ptyWidth = width
	m.ptyHeight = height

	// Resize the PTY
	size := &pty.Winsize{
		Cols: uint16(width),
		Rows: uint16(height),
	}

	err := pty.Setsize(m.ptmx, size)
	if err != nil {
		return fmt.Errorf("failed to resize PTY: %w", err)
	}

	// Resize the terminal emulator
	vt10x.ResizePty(m.ptmx, width, height)

	m.status = fmt.Sprintf("PTY resized to %dx%d", width, height)
	m.lastResize = time.Now()

	return nil
}

// Send command to PTY
func (m *model) sendCommand(cmd string) error {
	if !m.ptyReady || m.ptmx == nil {
		return fmt.Errorf("PTY not ready")
	}

	_, err := m.ptmx.Write([]byte(cmd + "\n"))
	return err
}

// Init initializes the Bubble Tea program
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

// Update handles messages
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
		// Auto-demo progression
		if m.demoRunning {
			return m, m.runDemoStep()
		}
		return m, tickCmd()

	case resizeMsg:
		err := m.resizePTY(msg.width, msg.height)
		if err != nil {
			m.err = err
		}
		return m, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			if m.ptmx != nil {
				m.ptmx.Close()
			}
			return m, tea.Quit

		case "r":
			// Manual resize demo
			newWidth := 40 + (m.demoStep%3)*20
			newHeight := 15 + (m.demoStep%3)*5
			m.demoStep++
			return m, func() tea.Msg {
				return resizeMsg{width: newWidth, height: newHeight}
			}

		case "d":
			// Start/stop demo
			m.demoRunning = !m.demoRunning
			if m.demoRunning {
				m.demoStep = 0
				m.status = "Demo started - watch the PTY resize and commands!"
			} else {
				m.status = "Demo stopped"
			}
			return m, nil

		case "c":
			// Send a test command
			cmd := []string{
				"echo 'Hello from PTY!'",
				"date",
				"pwd",
				"ls -la",
				"echo 'PTY is working!'",
			}[m.demoStep%5]
			m.demoStep++
			
			err := m.sendCommand(cmd)
			if err != nil {
				m.err = err
			} else {
				m.status = fmt.Sprintf("Sent command: %s", cmd)
			}
			return m, nil

		case "1", "2", "3", "4", "5":
			// Quick resize presets
			sizes := map[string][2]int{
				"1": {40, 15},  // Small
				"2": {60, 20},  // Medium
				"3": {80, 25},  // Large
				"4": {100, 30}, // Extra Large
				"5": {120, 35}, // Huge
			}
			if size, ok := sizes[msg.String()]; ok {
				return m, func() tea.Msg {
					return resizeMsg{width: size[0], height: size[1]}
				}
			}

		default:
			// Forward other keys to PTY
			if m.ptyReady && m.ptmx != nil {
				m.ptmx.Write([]byte(msg.String()))
			}
		}

	case error:
		m.err = msg
		return m, nil
	}

	return m, nil
}

// Run demo step
func (m model) runDemoStep() tea.Cmd {
	if !m.demoRunning {
		return nil
	}

	switch m.demoStep % 10 {
	case 0:
		// Start with medium size
		m.demoStep++
		return func() tea.Msg {
			return resizeMsg{width: 60, height: 20}
		}
	case 1:
		// Send welcome command
		m.sendCommand("echo 'Welcome to PTY Demo!'")
		m.demoStep++
		return tea.Tick(time.Second*2, func(t time.Time) tea.Msg { return tickMsg(t) })
	case 2:
		// Resize to small
		m.demoStep++
		return func() tea.Msg {
			return resizeMsg{width: 40, height: 15}
		}
	case 3:
		// Show directory
		m.sendCommand("pwd")
		m.demoStep++
		return tea.Tick(time.Second*2, func(t time.Time) tea.Msg { return tickMsg(t) })
	case 4:
		// Resize to large
		m.demoStep++
		return func() tea.Msg {
			return resizeMsg{width: 80, height: 25}
		}
	case 5:
		// List files
		m.sendCommand("ls -la")
		m.demoStep++
		return tea.Tick(time.Second*3, func(t time.Time) tea.Msg { return tickMsg(t) })
	case 6:
		// Resize to extra large
		m.demoStep++
		return func() tea.Msg {
			return resizeMsg{width: 100, height: 30}
		}
	case 7:
		// Show date
		m.sendCommand("date")
		m.demoStep++
		return tea.Tick(time.Second*2, func(t time.Time) tea.Msg { return tickMsg(t) })
	case 8:
		// Final resize
		m.demoStep++
		return func() tea.Msg {
			return resizeMsg{width: 70, height: 22}
		}
	case 9:
		// Final command
		m.sendCommand("echo 'Demo complete! PTY resizing works perfectly!'")
		m.demoStep = 0
		return tea.Tick(time.Second*3, func(t time.Time) tea.Msg { return tickMsg(t) })
	}

	return tea.Tick(time.Second, func(t time.Time) tea.Msg { return tickMsg(t) })
}

// View renders the UI
func (m model) View() string {
	if m.width == 0 {
		return "Loading..."
	}

	// Title
	title := titleStyle.Render("🖥️  Advanced PTY TUI Demo")

	// Info panel
	info := fmt.Sprintf(
		"Terminal Size: %dx%d\nPTY Size: %dx%d\nStatus: %s",
		m.width, m.height,
		m.ptyWidth, m.ptyHeight,
		m.status,
	)

	if !m.lastResize.IsZero() {
		timeSince := time.Since(m.lastResize)
		info += fmt.Sprintf("\nLast Resize: %v ago", timeSince.Truncate(time.Millisecond))
	}

	if m.err != nil {
		info += "\n" + errorStyle(fmt.Sprintf("Error: %v", m.err))
	}

	infoPanel := infoStyle.Render(info)

	// PTY content with border
	ptyContent := m.termContent
	if ptyContent == "" {
		ptyContent = "PTY output will appear here..."
	}

	// Ensure PTY content fits within the border
	lines := strings.Split(ptyContent, "\n")
	if len(lines) > m.ptyHeight {
		lines = lines[len(lines)-m.ptyHeight:]
		ptyContent = strings.Join(lines, "\n")
	}

	ptyPanel := ptyBorderStyle.
		Width(m.ptyWidth + 2).
		Height(m.ptyHeight + 2).
		Render(ptyContent)

	// Help text
	help := helpStyle(
		"Controls: [d] demo on/off • [r] manual resize • [c] send command • [1-5] size presets • [q] quit",
	)

	// Layout
	content := lipgloss.JoinVertical(
		lipgloss.Left,
		title,
		"",
		lipgloss.JoinHorizontal(
			lipgloss.Top,
			infoPanel,
			"  ",
			ptyPanel,
		),
		"",
		help,
	)

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
	// Create and run the program
	p := tea.NewProgram(
		initialModel(),
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)

	if _, err := p.Run(); err != nil {
		log.Fatal(err)
	}
}

