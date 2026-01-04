import { useState } from 'react'
import './App.css'
import courseData from './courseData.js'
import Modal from './components/Modal.jsx'
import CodeBlock from './components/CodeBlock.jsx'

// Icon components (simplified for this demo)
const Icon = ({ name, className = "" }) => {
  const icons = {
    BookOpen: "📖",
    Lightbulb: "💡", 
    Layers: "🔄",
    Code: "💻",
    Zap: "⚡",
    Terminal: "🖥️",
    Eye: "👁️",
    Palette: "🎨",
    Award: "🏆",
    Play: "▶️",
    Download: "⬇️",
    ExternalLink: "🔗",
    CheckCircle: "✅",
    Star: "⭐"
  }
  return <span className={className}>{icons[name] || "📄"}</span>
}

// Sample code for examples
const exampleCodes = {
  1: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Basic Lipgloss v2 Styling Examples ===\\n")

	// Example 1: Basic text styling
	basicStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("205")).
		Background(lipgloss.Color("235"))

	fmt.Println("1. Basic Text Styling:")
	fmt.Println(basicStyle.Render("Bold pink text on dark background"))
	fmt.Println()

	// Example 2: Borders and padding
	boxStyle := lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("63")).
		Padding(1, 2).
		Margin(1).
		Width(40).
		Align(lipgloss.Center)

	fmt.Println("2. Borders and Padding:")
	fmt.Println(boxStyle.Render("Centered text in a bordered box"))
	fmt.Println()
}`,
  2: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	fmt.Println("=== Simple Layer Examples ===\\n")

	// Create basic layers
	layer1 := lipgloss.NewLayer().
		Content("Layer 1\\nContent").
		Style(lipgloss.NewStyle().
			Background(lipgloss.Color("63")).
			Foreground(lipgloss.Color("230")).
			Padding(1, 2).
			Border(lipgloss.NormalBorder()))

	layer2 := lipgloss.NewLayer().
		Content("Layer 2\\nOverlay").
		Style(lipgloss.NewStyle().
			Background(lipgloss.Color("205")).
			Foreground(lipgloss.Color("230")).
			Padding(1, 2).
			Border(lipgloss.RoundedBorder()))

	// Position layers and render
	canvas := lipgloss.NewCanvas(
		layer1.X(0).Y(0),
		layer2.X(5).Y(2),
	)

	fmt.Println("Simple Layer Composition:")
	fmt.Println(canvas.Render())
}`,
  6: `package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
	"github.com/charmbracelet/lipgloss/v2"
)

type WindowManager struct {
	windows []Window
	canvas  *lipgloss.Canvas
}

type Window struct {
	ID       string
	Title    string
	Content  string
	X, Y     int
	Width    int
	Height   int
	Color    string
	ZIndex   int
	Focused  bool
}

func (wm *WindowManager) CreateWindow(title, content string, x, y, width, height int, color string) *Window {
	window := Window{
		ID:      fmt.Sprintf("win_%d", len(wm.windows)+1),
		Title:   title,
		Content: content,
		X:       x,
		Y:       y,
		Width:   width,
		Height:  height,
		Color:   color,
		ZIndex:  len(wm.windows),
		Focused: true,
	}
	
	// Unfocus other windows
	for i := range wm.windows {
		wm.windows[i].Focused = false
	}
	
	wm.windows = append(wm.windows, window)
	return &wm.windows[len(wm.windows)-1]
}

func (wm *WindowManager) Render() string {
	if len(wm.windows) == 0 {
		return "No windows to display"
	}

	var layers []*lipgloss.Layer
	
	for _, window := range wm.windows {
		layer := wm.createWindowLayer(window)
		layers = append(layers, layer.X(window.X).Y(window.Y).Z(window.ZIndex))
	}
	
	canvas := lipgloss.NewCanvas(layers...)
	return canvas.Render()
}

func main() {
	wm := &WindowManager{}
	
	// Create demo windows
	wm.CreateWindow("Terminal", "$ ls -la\\ntotal 42\\ndrwxr-xr-x  8 user  staff   256 Jun 27 10:30 .\\n-rw-r--r--  1 user  staff  1024 Jun 27 10:29 main.go", 2, 2, 35, 8, "63")
	wm.CreateWindow("Editor", "package main\\n\\nimport \\"fmt\\"\\n\\nfunc main() {\\n    fmt.Println(\\"Hello!\\")\\n}", 15, 5, 30, 10, "205")
	wm.CreateWindow("Browser", "🌐 Lipgloss v2 Documentation\\n\\n• Layer-based compositing\\n• Advanced positioning\\n• Professional styling", 8, 12, 40, 8, "99")
	
	fmt.Println("=== Interactive Window Manager Demo ===\\n")
	fmt.Println(wm.Render())
}`
}

function App() {
  const [activeTab, setActiveTab] = useState('overview')
  const [selectedChapter, setSelectedChapter] = useState(null)
  const [modalOpen, setModalOpen] = useState(false)
  const [modalContent, setModalContent] = useState({ title: '', content: '', type: 'code' })

  const showCode = (exampleNumber, title) => {
    const code = exampleCodes[exampleNumber] || `// Code for ${title} example\n// This would contain the actual Go code for the example`
    setModalContent({
      title: `${title} - Source Code`,
      content: code,
      type: 'code'
    })
    setModalOpen(true)
  }

  const showDemo = (exampleNumber, title) => {
    const demoOutput = getDemoOutput(exampleNumber)
    setModalContent({
      title: `${title} - Demo Output`,
      content: demoOutput,
      type: 'demo'
    })
    setModalOpen(true)
  }

  const getDemoOutput = (exampleNumber) => {
    const outputs = {
      1: `=== Basic Lipgloss v2 Styling Examples ===

1. Basic Text Styling:
Bold pink text on dark background

2. Borders and Padding:
┌──────────────────────────────────────┐
│                                      │
│     Centered text in a bordered box  │
│                                      │
└──────────────────────────────────────┘`,
      2: `=== Simple Layer Examples ===

Simple Layer Composition:
┌─────────────┐
│ Layer 1     │
│ Content     │
└─────────────┘
     ╭─────────────╮
     │ Layer 2     │
     │ Overlay     │
     ╰─────────────╯`,
      6: `=== Interactive Window Manager Demo ===

┌─ Terminal ──────────────────────┐  ╭─ Editor ─────────────────────╮
│ $ ls -la                        │  │ package main                 │
│ total 42                        │  │                              │
│ drwxr-xr-x  8 user  staff   256 │  │ import "fmt"                 │
│ -rw-r--r--  1 user  staff  1024 │  │                              │
│                                 │  │ func main() {                │
│                                 │  │     fmt.Println("Hello!")    │
│                                 │  │ }                            │
└─────────────────────────────────┘  ╰──────────────────────────────╯
        ┌─ Browser ──────────────────────────────┐
        │ 🌐 Lipgloss v2 Documentation           │
        │                                        │
        │ • Layer-based compositing              │
        │ • Advanced positioning                 │
        │ • Professional styling                 │
        │                                        │
        └────────────────────────────────────────┘`
    }
    return outputs[exampleNumber] || `Demo output for example ${exampleNumber} would be displayed here with actual terminal rendering.`
  }

  const renderOverview = () => (
    <div className="content-section">
      <div className="hero-section">
        <div className="hero-content">
          <h1>{courseData.title}</h1>
          <p className="subtitle">{courseData.subtitle}</p>
          <div className="hero-meta">
            <span>By {courseData.author}</span>
            <span>Version {courseData.version}</span>
            <span>{courseData.date}</span>
          </div>
        </div>
      </div>

      <div className="stats-grid">
        <div className="stat-card">
          <div className="stat-number">{courseData.stats.chapters}</div>
          <div className="stat-label">Comprehensive Chapters</div>
        </div>
        <div className="stat-card">
          <div className="stat-number">{courseData.stats.examples}</div>
          <div className="stat-label">Progressive Examples</div>
        </div>
        <div className="stat-card">
          <div className="stat-number">{courseData.stats.validationRate}%</div>
          <div className="stat-label">Validation Pass Rate</div>
        </div>
        <div className="stat-card">
          <div className="stat-number">{courseData.stats.vhsRecordings}</div>
          <div className="stat-label">VHS Recordings</div>
        </div>
      </div>

      <div className="features-section">
        <h2>🚀 Revolutionary Capabilities</h2>
        <div className="features-grid">
          <div className="feature-card">
            <Icon name="Layers" className="feature-icon" />
            <h3>Advanced Compositing</h3>
            <p>Master the revolutionary layer-based compositing system that enables overlapping elements and sophisticated visual hierarchies.</p>
          </div>
          <div className="feature-card">
            <Icon name="Terminal" className="feature-icon" />
            <h3>Window Management</h3>
            <p>Build a complete window manager with drag & drop, resizing, focus management, and professional styling.</p>
          </div>
          <div className="feature-card">
            <Icon name="Eye" className="feature-icon" />
            <h3>VHS Validation</h3>
            <p>Automated testing and documentation using VHS recordings with text screenshot validation for quality assurance.</p>
          </div>
          <div className="feature-card">
            <Icon name="Palette" className="feature-icon" />
            <h3>Visual Design</h3>
            <p>Comprehensive color and border techniques with 256-color support, accessibility considerations, and professional styling.</p>
          </div>
        </div>
      </div>

      <div className="learning-path">
        <h2>📚 Learning Path</h2>
        <div className="path-steps">
          <div className="path-step">
            <div className="step-number">1</div>
            <div className="step-content">
              <h3>Foundation</h3>
              <p>Master basic styling, layers, and canvas concepts</p>
            </div>
          </div>
          <div className="path-step">
            <div className="step-number">2</div>
            <div className="step-content">
              <h3>Advanced Techniques</h3>
              <p>Learn complex positioning, Z-index management, and nested compositions</p>
            </div>
          </div>
          <div className="path-step">
            <div className="step-number">3</div>
            <div className="step-content">
              <h3>Real-World Application</h3>
              <p>Build a complete window manager with interactive features</p>
            </div>
          </div>
          <div className="path-step">
            <div className="step-number">4</div>
            <div className="step-content">
              <h3>Validation & Testing</h3>
              <p>Implement comprehensive testing with VHS recordings</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  )

  const renderChapters = () => (
    <div className="content-section">
      <h2>📖 Course Chapters</h2>
      <p className="section-description">
        Comprehensive coverage of Lipgloss v2 from fundamentals to advanced applications
      </p>
      
      {selectedChapter ? (
        <div className="chapter-detail">
          <button 
            className="back-button"
            onClick={() => setSelectedChapter(null)}
          >
            ← Back to Chapters
          </button>
          <div className="chapter-content">
            <div className="chapter-header">
              <Icon name={selectedChapter.icon} className="chapter-icon" />
              <div>
                <h1>Chapter {selectedChapter.number}: {selectedChapter.title}</h1>
                <p className="chapter-description">{selectedChapter.description}</p>
              </div>
            </div>
            <div 
              className="chapter-body"
              dangerouslySetInnerHTML={{ __html: selectedChapter.content }}
            />
          </div>
        </div>
      ) : (
        <div className="chapters-grid">
          {courseData.chapters.map((chapter) => (
            <div 
              key={chapter.id} 
              className="chapter-card"
              onClick={() => setSelectedChapter(chapter)}
            >
              <div className="chapter-card-header">
                <Icon name={chapter.icon} className="chapter-icon" />
                <div className="chapter-number">Chapter {chapter.number}</div>
              </div>
              <h3>{chapter.title}</h3>
              <p>{chapter.description}</p>
              <div className="chapter-card-footer">
                <span className="read-more">Read Chapter →</span>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )

  const renderExamples = () => (
    <div className="content-section">
      <h2>💻 Progressive Examples</h2>
      <p className="section-description">
        10 hands-on examples progressing from basic concepts to advanced techniques
      </p>
      
      <div className="examples-grid">
        {courseData.examples.map((example) => (
          <div key={example.id} className="example-card">
            <div className="example-header">
              <div className="example-number">{example.number}</div>
              <div className={`difficulty-badge ${example.difficulty.toLowerCase()}`}>
                {example.difficulty}
              </div>
            </div>
            <h3>{example.title}</h3>
            <p>{example.description}</p>
            
            {example.gifUrl && (
              <div className="example-gif">
                <img 
                  src={example.gifUrl} 
                  alt={`${example.title} demonstration`}
                  className="demo-gif"
                />
              </div>
            )}
            
            <div className="concepts-list">
              <h4>Key Concepts:</h4>
              <ul>
                {example.concepts.map((concept, index) => (
                  <li key={index}>{concept}</li>
                ))}
              </ul>
            </div>
            
            <div className="example-actions">
              <button 
                className="btn-secondary"
                onClick={() => showCode(example.number, example.title)}
              >
                <Icon name="Code" /> View Code
              </button>
              <button 
                className="btn-primary"
                onClick={() => showDemo(example.number, example.title)}
              >
                <Icon name="Play" /> Run Demo
              </button>
            </div>
          </div>
        ))}
      </div>
    </div>
  )

  const renderDemo = () => (
    <div className="content-section">
      <h2>🖥️ Window Manager Demo</h2>
      <p className="section-description">
        {courseData.windowManager.description}
      </p>

      <div className="demo-showcase">
        <div className="demo-header">
          <Icon name="Terminal" className="demo-icon" />
          <div>
            <h3>{courseData.windowManager.title}</h3>
            <p>A fully functional window manager demonstrating overlapping windows, drag & drop, resizing, focus management, and professional styling.</p>
          </div>
        </div>

        <div className="features-comparison">
          <div className="features-column">
            <Icon name="CheckCircle" className="features-title-icon" />
            <h4>Core Features</h4>
            <ul>
              {courseData.windowManager.features.core.map((feature, index) => (
                <li key={index}>
                  <Icon name="CheckCircle" className="check-icon" />
                  {feature}
                </li>
              ))}
            </ul>
          </div>
          
          <div className="features-column">
            <Icon name="Star" className="features-title-icon" />
            <h4>Advanced Features</h4>
            <ul>
              {courseData.windowManager.features.advanced.map((feature, index) => (
                <li key={index}>
                  <Icon name="Star" className="check-icon" />
                  {feature}
                </li>
              ))}
            </ul>
          </div>
        </div>

        <div className="commands-section">
          <h4>Available Commands</h4>
          <div className="commands-grid">
            {courseData.windowManager.commands.map((cmd, index) => (
              <div key={index} className="command-item">
                <code className="command-syntax">{cmd.command}</code>
                <span className="command-description">{cmd.description}</span>
              </div>
            ))}
          </div>
        </div>

        <div className="validation-section">
          <h4>VHS Validation Results</h4>
          <div className="validation-stats">
            <div className="validation-stat">
              <div className="stat-number">{courseData.validation.results.passRate}%</div>
              <div className="stat-label">Pass Rate</div>
            </div>
            <div className="validation-stat">
              <div className="stat-number">{courseData.validation.results.filesValidated}</div>
              <div className="stat-label">Files Validated</div>
            </div>
            <div className="validation-stat">
              <div className="stat-number">{courseData.validation.results.passedTests}</div>
              <div className="stat-label">Tests Passed</div>
            </div>
          </div>
          
          <div className="validation-features">
            <h5>Validation Features:</h5>
            <ul>
              {courseData.validation.features.map((feature, index) => (
                <li key={index}>
                  <Icon name="CheckCircle" className="check-icon" />
                  {feature}
                </li>
              ))}
            </ul>
          </div>
        </div>

        <div className="demo-actions">
          <button 
            className="btn-primary large"
            onClick={() => showDemo(6, "Window Manager")}
          >
            <Icon name="Play" /> Launch Interactive Demo
          </button>
          <button 
            className="btn-secondary large"
            onClick={() => showCode(6, "Window Manager")}
          >
            <Icon name="Download" /> View Source Code
          </button>
        </div>
      </div>
    </div>
  )

  return (
    <div className="app">
      <header className="header">
        <div className="header-content">
          <div className="logo">
            <Icon name="Terminal" className="logo-icon" />
            <div className="logo-text">
              <h1>Mastering Lipgloss v2</h1>
              <p>Building Cool UIs with Overlays and Canvas</p>
            </div>
          </div>
          
          <div className="header-badges">
            <div className="badge success">
              <Icon name="CheckCircle" />
              100% Validated
            </div>
            <button className="btn-outline">
              <Icon name="ExternalLink" />
              View Source
            </button>
          </div>
        </div>
      </header>

      <nav className="navigation">
        <div className="nav-tabs">
          <button 
            className={`nav-tab ${activeTab === 'overview' ? 'active' : ''}`}
            onClick={() => setActiveTab('overview')}
          >
            <Icon name="BookOpen" />
            Overview
            <span className="tab-number">1</span>
          </button>
          <button 
            className={`nav-tab ${activeTab === 'chapters' ? 'active' : ''}`}
            onClick={() => setActiveTab('chapters')}
          >
            <Icon name="Lightbulb" />
            Chapters
            <span className="tab-number">9</span>
          </button>
          <button 
            className={`nav-tab ${activeTab === 'examples' ? 'active' : ''}`}
            onClick={() => setActiveTab('examples')}
          >
            <Icon name="Code" />
            Examples
            <span className="tab-number">10</span>
          </button>
          <button 
            className={`nav-tab ${activeTab === 'demo' ? 'active' : ''}`}
            onClick={() => setActiveTab('demo')}
          >
            <Icon name="Terminal" />
            Demo
            <span className="tab-number">1</span>
          </button>
        </div>
      </nav>

      <main className="main-content">
        {activeTab === 'overview' && renderOverview()}
        {activeTab === 'chapters' && renderChapters()}
        {activeTab === 'examples' && renderExamples()}
        {activeTab === 'demo' && renderDemo()}
      </main>

      <footer className="footer">
        <div className="footer-content">
          <div className="footer-section">
            <h4>Course Statistics</h4>
            <ul>
              <li>{courseData.stats.chapters} Comprehensive Chapters</li>
              <li>{courseData.stats.examples} Progressive Examples</li>
              <li>{courseData.stats.validationRate}% Validation Success</li>
              <li>{courseData.stats.vhsRecordings} VHS Recordings</li>
            </ul>
          </div>
          <div className="footer-section">
            <h4>Technologies</h4>
            <ul>
              <li>Lipgloss v2.0.0-beta.2</li>
              <li>VHS with Text Screenshots</li>
              <li>Go 1.23+ Required</li>
              <li>Cross-platform Compatible</li>
            </ul>
          </div>
          <div className="footer-section">
            <h4>About</h4>
            <p>Created by {courseData.author} • {courseData.date}</p>
            <p>A comprehensive guide to mastering terminal UI development with Lipgloss v2's revolutionary compositing capabilities.</p>
          </div>
        </div>
      </footer>

      <Modal 
        isOpen={modalOpen} 
        onClose={() => setModalOpen(false)}
        title={modalContent.title}
      >
        {modalContent.type === 'code' ? (
          <CodeBlock 
            code={modalContent.content}
            language="go"
            title={modalContent.title}
          />
        ) : (
          <div className="demo-output">
            <pre>{modalContent.content}</pre>
          </div>
        )}
      </Modal>
    </div>
  )
}

export default App

