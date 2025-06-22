package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"

	"github.com/charmbracelet/bubbletea"
	"github.com/creack/pty"
	"github.com/hinshun/vt10x"
)

// shellMsg represents output from the PTY
type shellMsg []byte

// model represents the application state
type model struct {
	term   vt10x.Terminal
	ptmx   *os.File
	cmd    *exec.Cmd
	width  int
	height int
}

// initialModel creates the initial model
func initialModel() model {
	// Start with a reasonable default size
	cols, rows := 80, 24
	
	// Create the command (use shell or a simple command for demo)
	shell := os.Getenv("SHELL")
	if shell == "" {
		shell = "/bin/bash"
	}
	cmd := exec.Command(shell)
	
	// Create PTY with initial size
	size := &pty.Winsize{Cols: uint16(cols), Rows: uint16(rows)}
	ptmx, err := pty.StartWithSize(cmd, size)
	if err != nil {
		log.Fatal(err)
	}
	
	// Create terminal emulator
	term := vt10x.New(vt10x.WithSize(cols, rows))
	
	m := model{
		term:   term,
		ptmx:   ptmx,
		cmd:    cmd,
		width:  cols,
		height: rows,
	}
	
	return m
}

// Init initializes the model
func (m model) Init() tea.Cmd {
	return tea.Batch(
		listenForPTYOutput(m.ptmx),
		tea.EnterAltScreen,
	)
}

// Update handles messages
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case shellMsg:
		// Write PTY output to terminal emulator
		m.term.Write([]byte(msg))
		return m, listenForPTYOutput(m.ptmx)
		
	case tea.KeyMsg:
		switch msg.Type {
		case tea.KeyCtrlC:
			return m, tea.Quit
		case tea.KeyEsc:
			return m, tea.Quit
		default:
			// Forward keystrokes to PTY
			return m, sendKeyToPTY(m.ptmx, msg)
		}
		
	case tea.WindowSizeMsg:
		// Update both PTY and terminal emulator size
		m.width = msg.Width
		m.height = msg.Height
		
		// Resize PTY
		pty.Setsize(m.ptmx, &pty.Winsize{
			Cols: uint16(msg.Width),
			Rows: uint16(msg.Height),
		})
		
		// Resize terminal emulator
		vt10x.ResizePty(m.ptmx, msg.Width, msg.Height)
		
		return m, nil
		
	case tea.QuitMsg:
		// Clean up
		if m.ptmx != nil {
			m.ptmx.Close()
		}
		if m.cmd != nil && m.cmd.Process != nil {
			m.cmd.Process.Kill()
		}
		return m, tea.Quit
	}
	
	return m, nil
}

// View renders the current state
func (m model) View() string {
	// Get the terminal content
	content := m.term.String()
	
	// Add some helpful information at the bottom
	help := "\n\nPTY Demo - Press Esc or Ctrl+C to quit"
	
	return content + help
}

// listenForPTYOutput reads from PTY and sends as messages
func listenForPTYOutput(ptmx *os.File) tea.Cmd {
	return func() tea.Msg {
		buf := make([]byte, 4096)
		n, err := ptmx.Read(buf)
		if err != nil {
			return tea.Quit()
		}
		return shellMsg(buf[:n])
	}
}

// sendKeyToPTY forwards key input to the PTY
func sendKeyToPTY(ptmx *os.File, key tea.KeyMsg) tea.Cmd {
	return func() tea.Msg {
		var bytes []byte
		
		switch key.Type {
		case tea.KeyEnter:
			bytes = []byte("\r")
		case tea.KeyTab:
			bytes = []byte("\t")
		case tea.KeyBackspace:
			bytes = []byte("\b")
		case tea.KeyDelete:
			bytes = []byte("\x1b[3~")
		case tea.KeyUp:
			bytes = []byte("\x1b[A")
		case tea.KeyDown:
			bytes = []byte("\x1b[B")
		case tea.KeyRight:
			bytes = []byte("\x1b[C")
		case tea.KeyLeft:
			bytes = []byte("\x1b[D")
		case tea.KeyHome:
			bytes = []byte("\x1b[H")
		case tea.KeyEnd:
			bytes = []byte("\x1b[F")
		default:
			// For regular characters
			if len(key.Runes) > 0 {
				bytes = []byte(string(key.Runes))
			}
		}
		
		if len(bytes) > 0 {
			ptmx.Write(bytes)
		}
		
		return nil
	}
}

func main() {
	// Create and run the Bubble Tea program
	p := tea.NewProgram(
		initialModel(),
		tea.WithAltScreen(),
		tea.WithMouseCellMotion(),
	)
	
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v", err)
		os.Exit(1)
	}
}

