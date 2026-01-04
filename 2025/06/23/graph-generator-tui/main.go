package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/charmbracelet/bubbles/list"
	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/AlexanderGrooff/mermaid-ascii/cmd"
)

// Application states
type state int

const (
	stateMenu state = iota
	stateGraphType
	stateInput
	stateViewing
)

// Graph types with enhanced templates
type graphType struct {
	name        string
	description string
	template    string
	domain      string
}

var graphTypes = []graphType{
	{
		name:        "Organizational Chart",
		description: "Company hierarchy and reporting structure",
		domain:      "Business",
		template: `graph TD
    CEO[Chief Executive Officer]
    CTO[Chief Technology Officer]
    CFO[Chief Financial Officer]
    CMO[Chief Marketing Officer]
    CEO --> CTO
    CEO --> CFO
    CEO --> CMO
    CTO --> DevMgr[Development Manager]
    CTO --> QAMgr[QA Manager]
    DevMgr --> SeniorDev[Senior Developer]
    DevMgr --> JuniorDev[Junior Developer]
    QAMgr --> QALead[QA Lead]
    CFO --> Accountant[Senior Accountant]
    CFO --> Analyst[Financial Analyst]
    CMO --> DigitalMgr[Digital Marketing Manager]
    CMO --> ContentMgr[Content Manager]`,
	},
	{
		name:        "Network Topology",
		description: "Network infrastructure and connections",
		domain:      "Technology",
		template: `graph LR
    Internet((Internet))
    Firewall[Firewall]
    Router[Main Router]
    CoreSwitch[Core Switch]
    Switch1[Access Switch 1]
    Switch2[Access Switch 2]
    Server1[Web Server]
    Server2[Database Server]
    Server3[File Server]
    PC1[Workstation 1]
    PC2[Workstation 2]
    PC3[Workstation 3]
    Printer[Network Printer]
    
    Internet --> Firewall
    Firewall --> Router
    Router --> CoreSwitch
    CoreSwitch --> Switch1
    CoreSwitch --> Switch2
    CoreSwitch --> Server1
    CoreSwitch --> Server2
    CoreSwitch --> Server3
    Switch1 --> PC1
    Switch1 --> PC2
    Switch2 --> PC3
    Switch2 --> Printer`,
	},
	{
		name:        "Simple Process Flow",
		description: "Basic workflow with decision points",
		domain:      "Process",
		template: `graph TD
    Start[Start Process]
    Input[Gather Input]
    Process[Process Data]
    Decision{Valid Data?}
    Output[Generate Output]
    Error[Handle Error]
    End[End Process]
    Start --> Input
    Input --> Process
    Process --> Decision
    Decision -->|Yes| Output
    Decision -->|No| Error
    Output --> End
    Error --> Input`,
	},
	{
		name:        "System Architecture",
		description: "Software system components",
		domain:      "Architecture",
		template: `graph LR
    User[User Interface]
    API[REST API]
    Auth[Authentication Service]
    DB[(Database)]
    Cache[(Redis Cache)]
    Queue[Message Queue]
    Worker[Background Worker]
    User --> API
    API --> Auth
    API --> DB
    API --> Cache
    API --> Queue
    Queue --> Worker
    Worker --> DB`,
	},
	{
		name:        "Decision Tree",
		description: "Decision making process",
		domain:      "Logic",
		template: `graph TD
    Problem[Identify Problem]
    Research[Research Options]
    Option1{Option A Available?}
    Option2{Option B Available?}
    ChoiceA[Choose Option A]
    ChoiceB[Choose Option B]
    Fallback[Use Fallback Plan]
    Implement[Implement Solution]
    Problem --> Research
    Research --> Option1
    Option1 -->|Yes| ChoiceA
    Option1 -->|No| Option2
    Option2 -->|Yes| ChoiceB
    Option2 -->|No| Fallback
    ChoiceA --> Implement
    ChoiceB --> Implement
    Fallback --> Implement`,
	},
	{
		name:        "Database Schema",
		description: "Entity relationship diagram",
		domain:      "Database",
		template: `graph TD
    Users[Users Table]
    Orders[Orders Table]
    Products[Products Table]
    OrderItems[Order Items Table]
    Categories[Categories Table]
    
    Users -->|1:N| Orders
    Orders -->|1:N| OrderItems
    Products -->|1:N| OrderItems
    Categories -->|1:N| Products`,
	},
	{
		name:        "Git Workflow",
		description: "Version control branching strategy",
		domain:      "Development",
		template: `graph TD
    Main[Main Branch]
    Develop[Develop Branch]
    Feature1[Feature Branch 1]
    Feature2[Feature Branch 2]
    Release[Release Branch]
    Hotfix[Hotfix Branch]
    
    Main --> Develop
    Develop --> Feature1
    Develop --> Feature2
    Feature1 --> Develop
    Feature2 --> Develop
    Develop --> Release
    Release --> Main
    Main --> Hotfix
    Hotfix --> Main
    Hotfix --> Develop`,
	},
}

// Styles with improved colors and spacing
var (
	titleStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1).
			Bold(true)

	domainStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#04B575")).
			Bold(true)

	helpStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#626262")).
			Italic(true)

	selectedStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1)

	normalStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FAFAFA"))

	diagramStyle = lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#7D56F4")).
			Padding(1).
			MarginTop(1)

	errorStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FF5F87")).
			Bold(true).
			Background(lipgloss.Color("#2D1B69")).
			Padding(0, 1)

	successStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#04B575")).
			Bold(true)
)

// Model represents the application state
type model struct {
	state        state
	list         list.Model
	textarea     textarea.Model
	viewport     viewport.Model
	selectedType graphType
	diagram      string
	width        int
	height       int
	error        string
	ready        bool
}

// List item for graph types
type item struct {
	graphType
}

func (i item) FilterValue() string { return i.name + " " + i.domain }
func (i item) Title() string       { return i.name }
func (i item) Description() string { 
	return domainStyle.Render("[" + i.domain + "] ") + i.description 
}

func initialModel() model {
	// Create list items
	items := make([]list.Item, len(graphTypes))
	for i, gt := range graphTypes {
		items[i] = item{gt}
	}

	// Initialize list with better styling
	l := list.New(items, list.NewDefaultDelegate(), 0, 0)
	l.Title = "Graph Generator - Select Graph Type"
	l.SetShowStatusBar(false)
	l.SetFilteringEnabled(true)
	l.SetShowHelp(true)

	// Initialize textarea with better dimensions
	ta := textarea.New()
	ta.Placeholder = "Enter your mermaid diagram here..."
	ta.Focus()
	ta.CharLimit = 5000
	ta.SetWidth(100)
	ta.SetHeight(15)
	ta.ShowLineNumbers = true

	// Initialize viewport with better dimensions
	vp := viewport.New(100, 25)

	return model{
		state:    stateMenu,
		list:     l,
		textarea: ta,
		viewport: vp,
		ready:    false,
	}
}

func (m model) Init() tea.Cmd {
	return textarea.Blink
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.ready = true
		
		// Update component dimensions
		m.list.SetWidth(msg.Width - 4)
		m.list.SetHeight(msg.Height - 6)
		m.textarea.SetWidth(msg.Width - 8)
		m.textarea.SetHeight(msg.Height - 15)
		m.viewport.Width = msg.Width - 8
		m.viewport.Height = msg.Height - 12
		return m, nil

	case tea.KeyMsg:
		switch m.state {
		case stateMenu:
			switch msg.String() {
			case "q", "ctrl+c":
				return m, tea.Quit
			case "enter":
				if selectedItem, ok := m.list.SelectedItem().(item); ok {
					m.selectedType = selectedItem.graphType
					m.textarea.SetValue(selectedItem.template)
					m.state = stateInput
					m.error = ""
				}
				return m, nil
			}
			m.list, cmd = m.list.Update(msg)

		case stateInput:
			switch msg.String() {
			case "ctrl+c":
				return m, tea.Quit
			case "esc":
				m.state = stateMenu
				return m, nil
			case "ctrl+g":
				// Generate diagram
				mermaidSrc := m.textarea.Value()
				if mermaidSrc != "" {
					diagram, err := generateDiagram(mermaidSrc)
					if err != nil {
						m.error = fmt.Sprintf("Error: %v", err)
						m.diagram = ""
					} else {
						m.diagram = diagram
						m.error = ""
						m.viewport.SetContent(m.diagram)
						m.state = stateViewing
					}
				}
				return m, nil
			}
			m.textarea, cmd = m.textarea.Update(msg)

		case stateViewing:
			switch msg.String() {
			case "ctrl+c":
				return m, tea.Quit
			case "esc":
				m.state = stateInput
				return m, nil
			case "q":
				m.state = stateMenu
				return m, nil
			}
			m.viewport, cmd = m.viewport.Update(msg)
		}
	}

	return m, cmd
}

func (m model) View() string {
	if !m.ready {
		return "Initializing..."
	}

	switch m.state {
	case stateMenu:
		return m.renderMenu()
	case stateInput:
		return m.renderInput()
	case stateViewing:
		return m.renderViewing()
	default:
		return "Unknown state"
	}
}

func (m model) renderMenu() string {
	var b strings.Builder
	
	b.WriteString(titleStyle.Render("🎨 Graph Generator"))
	b.WriteString("\n\n")
	b.WriteString(m.list.View())
	b.WriteString("\n")
	b.WriteString(helpStyle.Render("Enter to select • / to filter • q to quit"))
	
	return b.String()
}

func (m model) renderInput() string {
	var b strings.Builder
	
	b.WriteString(titleStyle.Render(fmt.Sprintf("✏️  Editing: %s", m.selectedType.name)))
	b.WriteString("\n")
	b.WriteString(domainStyle.Render(fmt.Sprintf("Domain: %s", m.selectedType.domain)))
	b.WriteString("\n\n")
	b.WriteString("Edit the mermaid diagram below:\n\n")
	b.WriteString(m.textarea.View())
	b.WriteString("\n")
	
	if m.error != "" {
		b.WriteString(errorStyle.Render(m.error))
		b.WriteString("\n")
	}
	
	b.WriteString(helpStyle.Render("Ctrl+G to generate • Esc to go back • Ctrl+C to quit"))
	
	return b.String()
}

func (m model) renderViewing() string {
	var b strings.Builder
	
	b.WriteString(titleStyle.Render(fmt.Sprintf("📊 Generated: %s", m.selectedType.name)))
	b.WriteString(" ")
	b.WriteString(successStyle.Render("✓ Success"))
	b.WriteString("\n")
	b.WriteString(diagramStyle.Render(m.viewport.View()))
	b.WriteString("\n")
	b.WriteString(helpStyle.Render("Esc to edit • q to menu • ↑/↓ to scroll • Ctrl+C to quit"))
	
	return b.String()
}

func generateDiagram(mermaidSrc string) (string, error) {
	// Set mermaid-ascii options for better output
	cmd.UseAscii = false
	cmd.PaddingBetweenX = 2
	cmd.PaddingBetweenY = 1
	cmd.BoxBorderPadding = 1
	cmd.Coords = false

	// Parse and render the diagram
	properties, err := cmd.MermaidFileToMap(mermaidSrc, "app")
	if err != nil {
		return "", fmt.Errorf("failed to parse mermaid: %w", err)
	}

	// Capture the output instead of printing to stdout
	result := cmd.DrawMap(properties)
	return result, nil
}

func main() {
	p := tea.NewProgram(initialModel(), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v", err)
		os.Exit(1)
	}
}

