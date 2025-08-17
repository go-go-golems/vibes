package main

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Block struct {
	Kind  string // e.g. "readFile", "directoryTree", "bash", "think", ...
	Label string // what to show in the legend (can be same as Kind)
}

type model struct {
	blocks []Block
	width  int
}

func initialModel() model {
	// Example sequence; tweak or generate however you like.
	return model{
		blocks: []Block{
			{Kind: "readFile", Label: "readFile"},
			{Kind: "directoryTree", Label: "directoryTree"},
			{Kind: "bash", Label: "bash"},
			{Kind: "think", Label: "think"},
			{Kind: "saveFile", Label: "saveFile"},
			{Kind: "editFile", Label: "editFile"},
			{Kind: "deleteFile", Label: "deleteFile"},
			{Kind: "text", Label: "text"},
			{Kind: "toolCall", Label: "toolCall"},
			{Kind: "fileRead", Label: "fileRead"},
			{Kind: "model", Label: "model"},
			{Kind: "toolReturn", Label: "toolReturn"},
		},
		width: 80,
	}
}

func (m model) Init() tea.Cmd { return nil }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
	}
	return m, nil
}

func (m model) View() string {
	var b strings.Builder

	// Title
	title := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#9BE1FF")). // light-cyan
		Render("Tools:")
	b.WriteString(title + "\n")

	// Render the colored boxes (tight squares)
	line := renderBoxesRow(m.blocks, m.width)
	b.WriteString(line + "\n\n")

	// Legend: name list separated by " - "
	legend := renderLegend(m.blocks, m.width)
	b.WriteString(legend)

	return b.String()
}

// ----- rendering helpers -----

// palette for kinds -> colors
func colorFor(kind string) lipgloss.Color {
	switch kind {
	case "readFile":
		return lipgloss.Color("#FF6B6B") // red
	case "directoryTree":
		return lipgloss.Color("#B28DFF") // violet
	case "bash":
		return lipgloss.Color("#FF8F6B") // orange-red
	case "think":
		return lipgloss.Color("#8FAFFF") // blue
	case "saveFile":
		return lipgloss.Color("#EEDC5B") // yellow
	case "editFile":
		return lipgloss.Color("#A9D36E") // green
	case "deleteFile":
		return lipgloss.Color("#8E8E93") // gray
	case "text":
		return lipgloss.Color("#6CE5E8") // teal
	case "toolCall":
		return lipgloss.Color("#F9A8D4") // pink
	case "fileRead":
		return lipgloss.Color("#C4F1BE") // mint
	case "model":
		return lipgloss.Color("#FFD3A3") // peach
	case "toolReturn":
		return lipgloss.Color("#C7D2FE") // periwinkle
	default:
		return lipgloss.Color("#AAAAAA")
	}
}

var (
	boxStyle = lipgloss.NewStyle().
			MarginRight(1).
			Width(2).
			Height(1)
	// legend styling
	legendName = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#B3B3B3"))
	legendSep = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#666666")).
			Render(" - ")
)

// draw a single 2x1 colored square
func box(kind string) string {
	return boxStyle.
		Background(colorFor(kind)).
		Render("  ")
}

// Render a wrapped row of boxes that fits within the given width.
func renderBoxesRow(blocks []Block, maxWidth int) string {
	if maxWidth <= 0 {
		maxWidth = 80
	}
	var curWidth int
	var lines []string
	var row []string

	spaceW := lipgloss.Width(boxStyle.MarginRight(1).Render(""))

	for _, bl := range blocks {
		item := box(bl.Kind)
		w := lipgloss.Width(item)
		if curWidth+w > maxWidth && len(row) > 0 {
			lines = append(lines, strings.Join(row, ""))
			row = row[:0]
			curWidth = 0
		}
		row = append(row, item)
		curWidth += w + spaceW
	}
	if len(row) > 0 {
		lines = append(lines, strings.Join(row, ""))
	}
	return strings.Join(lines, "\n")
}

func renderLegend(blocks []Block, maxWidth int) string {
	names := make([]string, 0, len(blocks))
	for _, bl := range blocks {
		names = append(names, legendName.Render(bl.Label))
	}
	joined := strings.Join(names, legendSep)
	// wrap soft by splitting on spaces if it exceeds width
	if lipgloss.Width(joined) <= maxWidth {
		return joined
	}
	var out []string
	var cur string
	for _, tok := range strings.Split(joined, " ") {
		if lipgloss.Width(cur)+1+lipgloss.Width(tok) > maxWidth && cur != "" {
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

func main() {
	if err := tea.NewProgram(initialModel(), tea.WithAltScreen()).Start(); err != nil {
		panic(err)
	}
}
