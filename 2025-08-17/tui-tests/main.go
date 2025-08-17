package main

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Block struct {
	Kind        string
	Label       string
	Description string
	Details     []string // simple lines to render in the detail pane
}

type model struct {
	blocks   []Block
	width    int
	selected int
}

func initialModel() model {
	return model{
		blocks: []Block{
			{
				Kind:        "readFile",
				Label:       "readFile",
				Description: "Read a file from disk and return its contents.",
				Details: []string{
					"path: /app/main.go",
					"bytes: 2_184",
				},
			},
			{
				Kind:        "directoryTree",
				Label:       "directoryTree",
				Description: "List a directory tree for quick inspection.",
				Details:     []string{"root: /app", "depth: 2"},
			},
			{
				Kind:        "bash",
				Label:       "bash",
				Description: "Execute a shell command in a sandbox.",
				Details:     []string{"cmd: go test ./...", "status: 0"},
			},
			{
				Kind:        "think",
				Label:       "think",
				Description: "Internal reasoning step with no side effects.",
				Details:     []string{"tokens: 512", "elapsed: 120ms"},
			},
			{
				Kind:        "saveFile",
				Label:       "saveFile",
				Description: "Write or overwrite a file on disk.",
				Details:     []string{"path: /app/main.go", "bytes: 3_102"},
			},
			{
				Kind:        "editFile",
				Label:       "editFile",
				Description: "Patch a file in-place using diffs.",
				Details:     []string{"hunks: 3", "insertions: 11", "deletions: 2"},
			},
			{
				Kind:        "deleteFile",
				Label:       "deleteFile",
				Description: "Remove a file from disk.",
				Details:     []string{"path: /tmp/temp.txt"},
			},
			{
				Kind:        "text",
				Label:       "text",
				Description: "Plain text message in the transcript.",
				Details:     []string{"role: assistant", "tokens: 146"},
			},
			{
				Kind:        "toolCall",
				Label:       "toolCall",
				Description: "Invoke a tool with arguments.",
				Details:     []string{"name: fetchURL", "args: {url: ...}"},
			},
			{
				Kind:        "fileRead",
				Label:       "fileRead",
				Description: "Tool returned file contents.",
				Details:     []string{"mime: text/plain", "preview: package main…"},
			},
			{
				Kind:        "model",
				Label:       "model",
				Description: "LLM generation step.",
				Details:     []string{"model: z-ai/glm-4.5", "temp: 0.2"},
			},
			{
				Kind:        "toolReturn",
				Label:       "toolReturn",
				Description: "Tool returned a result.",
				Details:     []string{"status: ok", "latency: 380ms"},
			},
		},
		width:    80,
		selected: 0,
	}
}

func (m model) Init() tea.Cmd { return nil }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
	case tea.KeyMsg:
		switch msg.Type {
		case tea.KeyCtrlC:
			return m, tea.Quit
		case tea.KeyRunes:
			if len(msg.Runes) == 1 && (msg.Runes[0] == 'q' || msg.Runes[0] == 'Q') {
				return m, tea.Quit
			}
		case tea.KeyRight, tea.KeyDown:
			if len(m.blocks) > 0 {
				m.selected = (m.selected + 1) % len(m.blocks)
			}
		case tea.KeyLeft, tea.KeyUp:
			if len(m.blocks) > 0 {
				m.selected = (m.selected - 1 + len(m.blocks)) % len(m.blocks)
			}
		}
	}
	return m, nil
}

func (m model) View() string {
	var b strings.Builder

	// Header
	b.WriteString(titleStyle.Render("Tools:") + "\n")

	// Boxes
	b.WriteString(m.renderBoxes() + "\n\n")

	// Legend
	b.WriteString(m.renderLegend() + "\n\n")

	// Details
	if len(m.blocks) > 0 {
		b.WriteString(m.renderDetails(m.blocks[m.selected]))
	}

	return b.String()
}

// ---- rendering ----

var (
	titleStyle = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("#9BE1FF"))

	boxStyle = lipgloss.NewStyle().
			Width(2).Height(1).MarginRight(1)

	selectedBoxStyle = lipgloss.NewStyle().
				Width(2).Height(1).MarginRight(1).
				Padding(0)

	legendName = lipgloss.NewStyle().Foreground(lipgloss.Color("#B3B3B3"))
	legendSep  = lipgloss.NewStyle().Foreground(lipgloss.Color("#666666")).Render(" - ")

	sectionTitle = lipgloss.NewStyle().Bold(true).Underline(true)
	descStyle    = lipgloss.NewStyle().Foreground(lipgloss.Color("#C5C5C5"))
	detailKey    = lipgloss.NewStyle().Foreground(lipgloss.Color("#9BE1FF"))
)

func colorFor(kind string) lipgloss.Color {
	switch kind {
	case "readFile":
		return lipgloss.Color("#FF6B6B")
	case "directoryTree":
		return lipgloss.Color("#B28DFF")
	case "bash":
		return lipgloss.Color("#FF8F6B")
	case "think":
		return lipgloss.Color("#8FAFFF")
	case "saveFile":
		return lipgloss.Color("#EEDC5B")
	case "editFile":
		return lipgloss.Color("#A9D36E")
	case "deleteFile":
		return lipgloss.Color("#8E8E93")
	case "text":
		return lipgloss.Color("#6CE5E8")
	case "toolCall":
		return lipgloss.Color("#F9A8D4")
	case "fileRead":
		return lipgloss.Color("#C4F1BE")
	case "model":
		return lipgloss.Color("#FFD3A3")
	case "toolReturn":
		return lipgloss.Color("#C7D2FE")
	default:
		return lipgloss.Color("#AAAAAA")
	}
}

func (m model) renderBoxes() string {
	if m.width <= 0 {
		m.width = 80
	}
	var (
		curW  int
		lines []string
		row   []string
	)
	for i, bl := range m.blocks {
		var it string
		box := boxStyle.Background(colorFor(bl.Kind)).Render("  ")
		if i == m.selected {
			// Add an underline for the selected box
			underline := lipgloss.NewStyle().
				Width(2).
				BorderStyle(lipgloss.NormalBorder()).
				BorderBottom(true).
				BorderForeground(lipgloss.Color("#9BE1FF")).
				Height(1).
				Render(" ")
			it = lipgloss.JoinVertical(lipgloss.Left, box, underline)
		} else {
			// Add empty space to maintain consistent height
			spacer := lipgloss.NewStyle().Width(2).Height(1).Render(" ")
			it = lipgloss.JoinVertical(lipgloss.Left, box, spacer)
		}
		w := lipgloss.Width(it)
		if curW+w > m.width && len(row) > 0 {
			lines = append(lines, strings.Join(row, ""))
			row = row[:0]
			curW = 0
		}
		row = append(row, it)
		curW += w
	}
	if len(row) > 0 {
		lines = append(lines, strings.Join(row, ""))
	}
	return strings.Join(lines, "\n")
}

func (m model) renderLegend() string {
	names := make([]string, len(m.blocks))
	for i, bl := range m.blocks {
		n := bl.Label
		if i == m.selected {
			n = lipgloss.NewStyle().
				Foreground(lipgloss.Color("#FFFFFF")).
				Background(colorFor(bl.Kind)).
				Bold(true).Padding(0, 1).
				Render(bl.Label)
		} else {
			n = legendName.Render(bl.Label)
		}
		names[i] = n
	}
	joined := strings.Join(names, legendSep)
	if lipgloss.Width(joined) <= m.width {
		return joined
	}
	// soft wrap legend
	var out []string
	var cur string
	for _, tok := range strings.Split(joined, " ") {
		if lipgloss.Width(cur)+1+lipgloss.Width(tok) > m.width && cur != "" {
			out = append(out, cur)
			cur = tok
		} else {
			if cur == "" {
				cur = tok
			} else {
				cur += " " + tok
			}
		}
	}
	if cur != "" {
		out = append(out, cur)
	}
	return strings.Join(out, "\n")
}

func (m model) renderDetails(bl Block) string {
	var b strings.Builder
	b.WriteString(sectionTitle.Render(fmt.Sprintf("Details · %s", bl.Label)) + "\n")
	b.WriteString(descStyle.Render(bl.Description) + "\n")
	for _, line := range bl.Details {
		parts := strings.SplitN(line, ":", 2)
		if len(parts) == 2 {
			b.WriteString("• " + detailKey.Render(strings.TrimSpace(parts[0])) + ": " + strings.TrimSpace(parts[1]) + "\n")
		} else {
			b.WriteString("• " + line + "\n")
		}
	}
	b.WriteString("\n" + descStyle.Render("←/→ (or ↑/↓) move · q quits"))
	return b.String()
}

func main() {
	if err := tea.NewProgram(initialModel(), tea.WithAltScreen()).Start(); err != nil {
		panic(err)
	}
}
