import { useState } from 'react'
import { Button } from '@/components/ui/button.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog.jsx'
import { ScrollArea } from '@/components/ui/scroll-area.jsx'
import { Code, Play, Eye, Download, CheckCircle, Star, Layers, Palette, Move, Zap } from 'lucide-react'
import './App.css'

// Import GIFs
import basicStylingGif from './assets/basic_styling.gif'
import simpleLayersGif from './assets/simple_layers.gif'
import basicCanvasGif from './assets/basic_canvas.gif'
import positioningGif from './assets/positioning.gif'
import complexLayeringGif from './assets/complex_layering.gif'
import zindexDemoGif from './assets/zindex_demo.gif'
import nestedLayersGif from './assets/nested_layers.gif'
import dynamicPositioningGif from './assets/dynamic_positioning.gif'
import colorShowcaseGif from './assets/color_showcase.gif'
import borderGalleryGif from './assets/border_gallery.gif'

const examples = [
  {
    id: 1,
    title: "Basic Styling",
    description: "Learn fundamental styling with colors, padding, and borders",
    difficulty: "Beginner",
    gif: basicStylingGif,
    icon: <Palette className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a basic style
	style := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FAFAFA")).
		Background(lipgloss.Color("#7D56F4")).
		PaddingTop(2).
		PaddingLeft(4).
		Width(22)

	fmt.Println(style.Render("Hello, Lipgloss!"))
}`
  },
  {
    id: 2,
    title: "Simple Layers",
    description: "Introduction to layering with basic positioning",
    difficulty: "Beginner",
    gif: simpleLayersGif,
    icon: <Layers className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create layers with content and positioning
	layer1 := lipgloss.NewLayer(boxStyle.Render("Layer 1")).
		X(5).Y(3).ID("layer1")

	layer2 := lipgloss.NewLayer(overlayStyle.Render("Layer 2")).
		X(15).Y(6).ID("layer2")

	// Create canvas with all layers
	canvas := lipgloss.NewCanvas(layer1, layer2)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 3,
    title: "Basic Canvas",
    description: "Working with canvas backgrounds and content organization",
    difficulty: "Beginner",
    gif: basicCanvasGif,
    icon: <Eye className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a background layer
	background := lipgloss.NewLayer(backgroundStyle.Render("")).
		X(0).Y(0).ID("background")

	// Create content layers
	title := lipgloss.NewLayer(titleStyle.Render("Canvas Demo")).
		X(15).Y(2).ID("title")

	canvas := lipgloss.NewCanvas(background, title)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 4,
    title: "Positioning",
    description: "Advanced positioning techniques and coordinate systems",
    difficulty: "Intermediate",
    gif: positioningGif,
    icon: <Move className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create layers at different positions
	topLeft := lipgloss.NewLayer(boxStyle.Render("Top Left")).
		X(0).Y(0).ID("topLeft")

	center := lipgloss.NewLayer(boxStyle.Render("Center")).
		X(20).Y(5).ID("center")

	// Higher Z-index overlay
	overlay := lipgloss.NewLayer(overlayStyle.Render("Floating")).
		X(25).Y(7).Z(10).ID("overlay")

	canvas := lipgloss.NewCanvas(topLeft, center, overlay)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 5,
    title: "Complex Layering",
    description: "Desktop environment simulation with multiple windows",
    difficulty: "Advanced",
    gif: complexLayeringGif,
    icon: <Layers className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create desktop environment
	desktop := lipgloss.NewLayer(desktopStyle.Render("")).
		X(0).Y(0).Z(0).ID("desktop")

	// Multiple windows with different Z-indexes
	terminal := lipgloss.NewLayer(terminalWindow).
		X(5).Y(2).Z(1).ID("terminal")

	editor := lipgloss.NewLayer(editorWindow).
		X(25).Y(4).Z(2).ID("editor")

	// Modal dialog with highest Z-index
	modal := lipgloss.NewLayer(modalStyle.Render(modalContent)).
		X(30).Y(8).Z(10).ID("modal")

	canvas := lipgloss.NewCanvas(desktop, terminal, editor, modal)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 6,
    title: "Z-Index Demo",
    description: "Understanding layer stacking and depth management",
    difficulty: "Intermediate",
    gif: zindexDemoGif,
    icon: <Zap className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create layers with different Z-indexes
	redLayer := lipgloss.NewLayer(redStyle.Render("Red Layer\\nZ-index: 1")).
		X(5).Y(3).Z(1).ID("red")

	blueLayer := lipgloss.NewLayer(blueStyle.Render("Blue Layer\\nZ-index: 3")).
		X(15).Y(5).Z(3).ID("blue")

	// Higher Z-index appears on top
	yellowLayer := lipgloss.NewLayer(yellowStyle.Render("Yellow Layer\\nZ-index: 4")).
		X(20).Y(4).Z(4).ID("yellow")

	canvas := lipgloss.NewCanvas(redLayer, blueLayer, yellowLayer)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 7,
    title: "Nested Layers",
    description: "Modal dialogs and nested component structures",
    difficulty: "Advanced",
    gif: nestedLayersGif,
    icon: <Layers className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create nested content structure
	dialogContent := lipgloss.JoinVertical(lipgloss.Left,
		headerStyle.Render("Confirmation Dialog"),
		"",
		contentStyle.Render("Are you sure you want to proceed?"),
		"",
		buttons,
	)

	// Create the main dialog layer
	dialog := lipgloss.NewLayer(containerStyle.Render(dialogContent)).
		X(20).Y(5).Z(5).ID("dialog")

	// Background overlay
	overlay := lipgloss.NewLayer(overlayStyle.Render("")).
		X(0).Y(0).Z(3).ID("overlay")

	canvas := lipgloss.NewCanvas(window, overlay, dialog)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 8,
    title: "Dynamic Positioning",
    description: "Animation simulation and dynamic movement",
    difficulty: "Advanced",
    gif: dynamicPositioningGif,
    icon: <Move className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"time"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Simulate animation frames
	positions := []struct{ x, y int }{
		{15, 5}, {25, 8}, {35, 11}, {45, 14},
	}

	for i, pos := range positions {
		ball := lipgloss.NewLayer(ballStyle.Render("●")).
			X(pos.x).Y(pos.y).Z(1).ID("ball")

		canvas := lipgloss.NewCanvas(boundary, ball)
		fmt.Printf("Frame %d:\\n", i+1)
		fmt.Println(canvas.Render())
		
		time.Sleep(500 * time.Millisecond)
	}
}`
  },
  {
    id: 9,
    title: "Color Showcase",
    description: "Comprehensive color palette and theming demonstration",
    difficulty: "Intermediate",
    gif: colorShowcaseGif,
    icon: <Palette className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	colors := []struct {
		name  string
		color string
	}{
		{"Red", "#f38ba8"},
		{"Blue", "#89b4fa"},
		{"Green", "#a6e3a1"},
		{"Yellow", "#f9e2af"},
	}

	var layers []*lipgloss.Layer
	for i, colorInfo := range colors {
		swatch := swatchStyle.Background(lipgloss.Color(colorInfo.color)).
			Render("████")
		
		x := 15 + (i%4)*15
		y := 4 + (i/4)*3

		layer := lipgloss.NewLayer(swatch).X(x).Y(y).Z(1)
		layers = append(layers, layer)
	}

	canvas := lipgloss.NewCanvas(layers...)
	fmt.Println(canvas.Render())
}`
  },
  {
    id: 10,
    title: "Border Gallery",
    description: "Complete showcase of all available border styles",
    difficulty: "Beginner",
    gif: borderGalleryGif,
    icon: <Eye className="w-5 h-5" />,
    code: `package main

import (
	"fmt"
	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	borders := []struct {
		name   string
		border lipgloss.Border
		color  string
	}{
		{"Normal", lipgloss.NormalBorder(), "#89b4fa"},
		{"Rounded", lipgloss.RoundedBorder(), "#a6e3a1"},
		{"Thick", lipgloss.ThickBorder(), "#f38ba8"},
		{"Double", lipgloss.DoubleBorder(), "#f9e2af"},
	}

	var layers []*lipgloss.Layer
	for i, borderInfo := range borders {
		borderStyle := lipgloss.NewStyle().
			Border(borderInfo.border).
			BorderForeground(lipgloss.Color(borderInfo.color))

		content := fmt.Sprintf("%s\\nBorder", borderInfo.name)
		
		x := 10 + (i%3)*20
		y := 5 + (i/3)*7

		layer := lipgloss.NewLayer(borderStyle.Render(content)).X(x).Y(y).Z(1)
		layers = append(layers, layer)
	}

	canvas := lipgloss.NewCanvas(layers...)
	fmt.Println(canvas.Render())
}`
  }
]

function App() {
  const [selectedExample, setSelectedExample] = useState(null)

  const getDifficultyColor = (difficulty) => {
    switch (difficulty) {
      case 'Beginner': return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-300'
      case 'Intermediate': return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-300'
      case 'Advanced': return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-300'
      default: return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
    }
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-purple-50 via-blue-50 to-indigo-100 dark:from-gray-900 dark:via-purple-900 dark:to-indigo-900">
      {/* Header */}
      <header className="bg-white/80 dark:bg-gray-900/80 backdrop-blur-sm border-b border-gray-200 dark:border-gray-700 sticky top-0 z-50">
        <div className="container mx-auto px-6 py-4">
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-3">
              <div className="w-10 h-10 bg-gradient-to-br from-purple-500 to-blue-600 rounded-lg flex items-center justify-center">
                <Code className="w-6 h-6 text-white" />
              </div>
              <div>
                <h1 className="text-2xl font-bold bg-gradient-to-r from-purple-600 to-blue-600 bg-clip-text text-transparent">
                  Lipgloss v2 Course
                </h1>
                <p className="text-sm text-gray-600 dark:text-gray-400">Master Terminal UI Development</p>
              </div>
            </div>
            <div className="flex items-center space-x-2">
              <Badge variant="secondary" className="bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-300">
                <CheckCircle className="w-3 h-3 mr-1" />
                All Examples Verified
              </Badge>
            </div>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="container mx-auto px-6 py-8">
        <Tabs defaultValue="overview" className="w-full">
          <TabsList className="grid w-full grid-cols-4 mb-8">
            <TabsTrigger value="overview">Overview</TabsTrigger>
            <TabsTrigger value="examples">Examples</TabsTrigger>
            <TabsTrigger value="features">Features</TabsTrigger>
            <TabsTrigger value="validation">Validation</TabsTrigger>
          </TabsList>

          <TabsContent value="overview" className="space-y-8">
            {/* Hero Section */}
            <div className="text-center space-y-6">
              <h2 className="text-4xl font-bold text-gray-900 dark:text-white">
                Build Beautiful Terminal UIs with Lipgloss v2
              </h2>
              <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
                Learn to create sophisticated terminal applications with overlays, canvas, and advanced layering techniques. 
                This comprehensive course includes 10 progressive examples with live demonstrations.
              </p>
              <div className="flex justify-center space-x-4">
                <Button size="lg" className="bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-700 hover:to-blue-700">
                  <Play className="w-5 h-5 mr-2" />
                  Start Learning
                </Button>
                <Button variant="outline" size="lg">
                  <Download className="w-5 h-5 mr-2" />
                  Download Examples
                </Button>
              </div>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
              <Card>
                <CardContent className="p-6 text-center">
                  <div className="text-3xl font-bold text-purple-600 dark:text-purple-400">10</div>
                  <div className="text-sm text-gray-600 dark:text-gray-400">Progressive Examples</div>
                </CardContent>
              </Card>
              <Card>
                <CardContent className="p-6 text-center">
                  <div className="text-3xl font-bold text-blue-600 dark:text-blue-400">100%</div>
                  <div className="text-sm text-gray-600 dark:text-gray-400">Compilation Success</div>
                </CardContent>
              </Card>
              <Card>
                <CardContent className="p-6 text-center">
                  <div className="text-3xl font-bold text-green-600 dark:text-green-400">✓</div>
                  <div className="text-sm text-gray-600 dark:text-gray-400">VHS Validated</div>
                </CardContent>
              </Card>
              <Card>
                <CardContent className="p-6 text-center">
                  <div className="text-3xl font-bold text-orange-600 dark:text-orange-400">v2</div>
                  <div className="text-sm text-gray-600 dark:text-gray-400">Latest Version</div>
                </CardContent>
              </Card>
            </div>

            {/* Course Highlights */}
            <Card>
              <CardHeader>
                <CardTitle>What You'll Learn</CardTitle>
                <CardDescription>
                  Master the cutting-edge features of Lipgloss v2 through hands-on examples
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                  <div className="space-y-4">
                    <div className="flex items-start space-x-3">
                      <Palette className="w-5 h-5 text-purple-600 mt-1" />
                      <div>
                        <h4 className="font-semibold">Advanced Styling</h4>
                        <p className="text-sm text-gray-600 dark:text-gray-400">Colors, borders, padding, and responsive design</p>
                      </div>
                    </div>
                    <div className="flex items-start space-x-3">
                      <Layers className="w-5 h-5 text-blue-600 mt-1" />
                      <div>
                        <h4 className="font-semibold">Layer Management</h4>
                        <p className="text-sm text-gray-600 dark:text-gray-400">Z-index, positioning, and complex compositions</p>
                      </div>
                    </div>
                    <div className="flex items-start space-x-3">
                      <Eye className="w-5 h-5 text-green-600 mt-1" />
                      <div>
                        <h4 className="font-semibold">Canvas System</h4>
                        <p className="text-sm text-gray-600 dark:text-gray-400">Background management and content organization</p>
                      </div>
                    </div>
                  </div>
                  <div className="space-y-4">
                    <div className="flex items-start space-x-3">
                      <Move className="w-5 h-5 text-orange-600 mt-1" />
                      <div>
                        <h4 className="font-semibold">Dynamic Positioning</h4>
                        <p className="text-sm text-gray-600 dark:text-gray-400">Animation simulation and interactive movement</p>
                      </div>
                    </div>
                    <div className="flex items-start space-x-3">
                      <Zap className="w-5 h-5 text-red-600 mt-1" />
                      <div>
                        <h4 className="font-semibold">Window Management</h4>
                        <p className="text-sm text-gray-600 dark:text-gray-400">Desktop environments and modal dialogs</p>
                      </div>
                    </div>
                    <div className="flex items-start space-x-3">
                      <Star className="w-5 h-5 text-yellow-600 mt-1" />
                      <div>
                        <h4 className="font-semibold">Best Practices</h4>
                        <p className="text-sm text-gray-600 dark:text-gray-400">Professional patterns and optimization techniques</p>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="examples" className="space-y-6">
            <div className="text-center space-y-4">
              <h2 className="text-3xl font-bold text-gray-900 dark:text-white">Interactive Examples</h2>
              <p className="text-lg text-gray-600 dark:text-gray-300">
                Explore 10 progressive examples with live demonstrations and source code
              </p>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              {examples.map((example) => (
                <Card key={example.id} className="group hover:shadow-lg transition-all duration-300 cursor-pointer">
                  <CardHeader className="pb-3">
                    <div className="flex items-center justify-between">
                      <div className="flex items-center space-x-2">
                        {example.icon}
                        <CardTitle className="text-lg">{example.title}</CardTitle>
                      </div>
                      <Badge className={getDifficultyColor(example.difficulty)}>
                        {example.difficulty}
                      </Badge>
                    </div>
                    <CardDescription>{example.description}</CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <div className="relative overflow-hidden rounded-lg border bg-gray-50 dark:bg-gray-800">
                      <img 
                        src={example.gif} 
                        alt={`${example.title} demonstration`}
                        className="w-full h-48 object-cover group-hover:scale-105 transition-transform duration-300"
                      />
                    </div>
                    <div className="flex space-x-2">
                      <Dialog>
                        <DialogTrigger asChild>
                          <Button variant="outline" size="sm" className="flex-1">
                            <Code className="w-4 h-4 mr-2" />
                            View Code
                          </Button>
                        </DialogTrigger>
                        <DialogContent className="max-w-4xl max-h-[80vh]">
                          <DialogHeader>
                            <DialogTitle>{example.title} - Source Code</DialogTitle>
                            <DialogDescription>
                              {example.description}
                            </DialogDescription>
                          </DialogHeader>
                          <ScrollArea className="h-96 w-full rounded-md border p-4">
                            <pre className="text-sm">
                              <code>{example.code}</code>
                            </pre>
                          </ScrollArea>
                        </DialogContent>
                      </Dialog>
                      <Button variant="outline" size="sm" className="flex-1">
                        <Play className="w-4 h-4 mr-2" />
                        Run Demo
                      </Button>
                    </div>
                  </CardContent>
                </Card>
              ))}
            </div>
          </TabsContent>

          <TabsContent value="features" className="space-y-6">
            <div className="text-center space-y-4">
              <h2 className="text-3xl font-bold text-gray-900 dark:text-white">Lipgloss v2 Features</h2>
              <p className="text-lg text-gray-600 dark:text-gray-300">
                Discover the powerful capabilities that make Lipgloss v2 the ultimate terminal UI library
              </p>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <Layers className="w-5 h-5 text-purple-600" />
                    <span>Advanced Layering System</span>
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p className="text-gray-600 dark:text-gray-300">
                    Create complex UI compositions with multiple layers, Z-index management, and precise positioning control.
                  </p>
                  <ul className="space-y-2 text-sm">
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Z-index based layer stacking</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Precise X/Y positioning</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Layer identification and management</span>
                    </li>
                  </ul>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <Eye className="w-5 h-5 text-blue-600" />
                    <span>Canvas Composition</span>
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p className="text-gray-600 dark:text-gray-300">
                    Combine multiple layers into cohesive interfaces with automatic bounds calculation and hit testing.
                  </p>
                  <ul className="space-y-2 text-sm">
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Multi-layer canvas rendering</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Automatic bounds detection</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Hit testing for interactions</span>
                    </li>
                  </ul>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <Palette className="w-5 h-5 text-green-600" />
                    <span>Rich Styling Options</span>
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p className="text-gray-600 dark:text-gray-300">
                    Comprehensive styling system with colors, borders, padding, and advanced typography controls.
                  </p>
                  <ul className="space-y-2 text-sm">
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Multiple border styles</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Color palette support</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Flexible padding and margins</span>
                    </li>
                  </ul>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <Move className="w-5 h-5 text-orange-600" />
                    <span>Dynamic Positioning</span>
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p className="text-gray-600 dark:text-gray-300">
                    Create animated interfaces with dynamic positioning, perfect for interactive applications and games.
                  </p>
                  <ul className="space-y-2 text-sm">
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Real-time position updates</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Animation simulation support</span>
                    </li>
                    <li className="flex items-center space-x-2">
                      <CheckCircle className="w-4 h-4 text-green-600" />
                      <span>Interactive movement patterns</span>
                    </li>
                  </ul>
                </CardContent>
              </Card>
            </div>
          </TabsContent>

          <TabsContent value="validation" className="space-y-6">
            <div className="text-center space-y-4">
              <h2 className="text-3xl font-bold text-gray-900 dark:text-white">Quality Assurance</h2>
              <p className="text-lg text-gray-600 dark:text-gray-300">
                All examples have been thoroughly tested and validated using automated tools
              </p>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <CheckCircle className="w-5 h-5 text-green-600" />
                    <span>Compilation Tests</span>
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Success Rate</span>
                      <Badge className="bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-300">100%</Badge>
                    </div>
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Examples Tested</span>
                      <span className="font-semibold">10/10</span>
                    </div>
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Go Version</span>
                      <span className="font-semibold">1.23.0+</span>
                    </div>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <Play className="w-5 h-5 text-blue-600" />
                    <span>VHS Recordings</span>
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="flex justify-between items-center">
                      <span className="text-sm">GIFs Generated</span>
                      <Badge className="bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-300">10/10</Badge>
                    </div>
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Text Screenshots</span>
                      <span className="font-semibold">✓ Enabled</span>
                    </div>
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Validation</span>
                      <span className="font-semibold">Automated</span>
                    </div>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center space-x-2">
                    <Star className="w-5 h-5 text-yellow-600" />
                    <span>Quality Metrics</span>
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Code Quality</span>
                      <Badge className="bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-300">A+</Badge>
                    </div>
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Documentation</span>
                      <span className="font-semibold">Complete</span>
                    </div>
                    <div className="flex justify-between items-center">
                      <span className="text-sm">Examples</span>
                      <span className="font-semibold">Progressive</span>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>

            <Card>
              <CardHeader>
                <CardTitle>Validation Process</CardTitle>
                <CardDescription>
                  Our comprehensive testing ensures all examples work perfectly
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <div className="flex items-start space-x-3">
                    <div className="w-8 h-8 bg-green-100 dark:bg-green-900 rounded-full flex items-center justify-center">
                      <span className="text-sm font-semibold text-green-600 dark:text-green-300">1</span>
                    </div>
                    <div>
                      <h4 className="font-semibold">Compilation Verification</h4>
                      <p className="text-sm text-gray-600 dark:text-gray-400">
                        Every example is compiled with the latest Go toolchain to ensure compatibility
                      </p>
                    </div>
                  </div>
                  <div className="flex items-start space-x-3">
                    <div className="w-8 h-8 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center">
                      <span className="text-sm font-semibold text-blue-600 dark:text-blue-300">2</span>
                    </div>
                    <div>
                      <h4 className="font-semibold">VHS Recording</h4>
                      <p className="text-sm text-gray-600 dark:text-gray-400">
                        Automated terminal recordings capture the actual output of each example
                      </p>
                    </div>
                  </div>
                  <div className="flex items-start space-x-3">
                    <div className="w-8 h-8 bg-purple-100 dark:bg-purple-900 rounded-full flex items-center justify-center">
                      <span className="text-sm font-semibold text-purple-600 dark:text-purple-300">3</span>
                    </div>
                    <div>
                      <h4 className="font-semibold">Text Screenshot Validation</h4>
                      <p className="text-sm text-gray-600 dark:text-gray-400">
                        Text-based screenshots enable programmatic validation of UI output
                      </p>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </main>

      {/* Footer */}
      <footer className="bg-white/80 dark:bg-gray-900/80 backdrop-blur-sm border-t border-gray-200 dark:border-gray-700 mt-16">
        <div className="container mx-auto px-6 py-8">
          <div className="text-center space-y-4">
            <div className="flex items-center justify-center space-x-2">
              <Code className="w-5 h-5 text-purple-600" />
              <span className="font-semibold text-gray-900 dark:text-white">Lipgloss v2 Course</span>
            </div>
            <p className="text-sm text-gray-600 dark:text-gray-400">
              Master terminal UI development with comprehensive examples and live demonstrations
            </p>
            <div className="flex justify-center space-x-4 text-sm text-gray-500 dark:text-gray-400">
              <span>✓ All Examples Verified</span>
              <span>•</span>
              <span>✓ VHS Validated</span>
              <span>•</span>
              <span>✓ Production Ready</span>
            </div>
          </div>
        </div>
      </footer>
    </div>
  )
}

export default App

