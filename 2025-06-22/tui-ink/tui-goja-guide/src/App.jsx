import { useState, useEffect } from 'react'
import { Button } from '@/components/ui/button.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { BlogPost } from './components/BlogPost.jsx'
import { 
  Terminal, 
  Code, 
  Play, 
  Download, 
  Github, 
  CheckCircle, 
  ArrowRight,
  Zap,
  Layers,
  Settings,
  BookOpen,
  Cpu,
  Monitor,
  Menu,
  X
} from 'lucide-react'
import demoGif from './assets/corrected_demo.gif'
import './App.css'

function App() {
  const [activeSection, setActiveSection] = useState('overview')
  const [currentView, setCurrentView] = useState('guide') // 'guide' or 'blog'
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false)

  const scrollToSection = (sectionId) => {
    const element = document.getElementById(sectionId)
    if (element) {
      element.scrollIntoView({ behavior: 'smooth' })
      setActiveSection(sectionId)
    }
  }

  const features = [
    {
      icon: <Terminal className="h-6 w-6" />,
      title: "JavaScript TUI",
      description: "Build terminal interfaces using familiar JavaScript syntax"
    },
    {
      icon: <Cpu className="h-6 w-6" />,
      title: "Goja Integration", 
      description: "Run JavaScript code within Go applications using the goja VM"
    },
    {
      icon: <Zap className="h-6 w-6" />,
      title: "Real-time Input",
      description: "Single character input handling with immediate response"
    },
    {
      icon: <Layers className="h-6 w-6" />,
      title: "ES5 Compatible",
      description: "Webpack-bundled JavaScript that works in goja's ES5 environment"
    },
    {
      icon: <Monitor className="h-6 w-6" />,
      title: "VHS Recording",
      description: "Text screenshots and GIF recordings for validation and demos"
    },
    {
      icon: <Settings className="h-6 w-6" />,
      title: "Terminal Control",
      description: "Raw mode terminal handling with proper cleanup"
    }
  ]

  const codeExamples = {
    javascript: `// Enhanced TUI Library (ES5 Compatible)
function SimpleTUI() {
  this.components = [];
  this.inputHandlers = [];
}

SimpleTUI.prototype.addComponent = function(component) {
  this.components.push(component);
};

SimpleTUI.prototype.render = function() {
  var output = '';
  for (var i = 0; i < this.components.length; i++) {
    output += this.components[i].render() + '\\n';
  }
  return output;
};

// Box Component with Unicode borders
function BoxComponent(title, content, width, height) {
  this.title = title || '';
  this.content = content || '';
  this.width = width || 40;
  this.height = height || 10;
}

BoxComponent.prototype.render = function() {
  var output = '';
  var border = '┌' + Array(this.width - 1).join('─') + '┐';
  output += border + '\\n';
  
  if (this.title) {
    var titleLine = '│ ' + this.title + Array(this.width - this.title.length - 2).join(' ') + '│';
    output += titleLine + '\\n';
    output += '├' + Array(this.width - 1).join('─') + '┤\\n';
  }
  
  // Content lines...
  return output;
};`,

    go: `package main

import (
    "fmt"
    "syscall"
    "unsafe"
    "github.com/dop251/goja"
)

type TUIApp struct {
    vm      *goja.Runtime
    app     goja.Value
    running bool
}

func (t *TUIApp) LoadJSBundle(filename string) error {
    jsCode, err := ioutil.ReadFile(filename)
    if err != nil {
        return err
    }

    // Execute JavaScript in goja
    _, err = t.vm.RunString(string(jsCode))
    if err != nil {
        return err
    }

    // Get TUI library and create app instance
    simpleTUILib := t.vm.Get("SimpleTUILib")
    counterAppConstructor := simpleTUILib.ToObject(t.vm).Get("CounterApp")
    app, err := t.vm.New(counterAppConstructor)
    if err != nil {
        return err
    }

    t.app = app
    return nil
}

func (t *TUIApp) runInteractive() {
    oldTermios, err := enableRawMode()
    if err != nil {
        log.Fatalf("Failed to enable raw mode: %v", err)
    }
    defer restoreTerminal(oldTermios)

    t.render()

    for t.running {
        char, err := readChar()
        if err != nil {
            continue
        }

        input := string(char)
        if input == "q" {
            t.running = false
            break
        }

        t.handleInput(input)
        t.render()
    }
}`,

    webpack: `// webpack.config.js - ES5 Compatibility
const path = require('path');

module.exports = {
  mode: 'development',
  entry: './src/enhanced-tui.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'simple-tui-bundle.js',
    library: 'SimpleTUILib',
    libraryTarget: 'var'
  },
  module: {
    rules: [{
      test: /\\.(js|jsx)$/,
      exclude: /node_modules/,
      use: {
        loader: 'babel-loader',
        options: {
          presets: [['@babel/preset-env', {
            targets: { browsers: ['ie >= 9'] },
            modules: false
          }]]
        }
      }
    }]
  },
  target: ['web', 'es5']
};`,

    vhs: `# VHS Recording Script
Output demo.gif
Output demo.txt

Set FontSize 14
Set Width 1200
Set Height 800
Set Theme "Dracula"

# Start TUI application
Type "cd ~/project && ./tui-app"
Enter
Sleep 2s

# Demonstrate functionality
Type "+"
Sleep 500ms
Type "+"
Sleep 500ms
Type "p"
Sleep 500ms
Type "q"
Sleep 1s`
  }

  const steps = [
    {
      title: "Setup Go Environment",
      description: "Install Go and initialize your project with goja dependency",
      code: `go mod init your-tui-project
go get github.com/dop251/goja`
    },
    {
      title: "Create JavaScript TUI Library", 
      description: "Build ES5-compatible TUI components using Webpack and Babel",
      code: `npm init -y
npm install webpack webpack-cli babel-loader @babel/core @babel/preset-env
# Create your TUI components...`
    },
    {
      title: "Implement Go Integration",
      description: "Create Go application that loads and executes JavaScript via goja",
      code: `// Load JavaScript bundle
jsCode, err := ioutil.ReadFile("bundle.js")
vm.RunString(string(jsCode))

// Create TUI instance
app, err := vm.New(constructor)`
    },
    {
      title: "Add Terminal Control",
      description: "Implement raw mode terminal handling for single character input",
      code: `func enableRawMode() (*termios, error) {
    // Terminal syscalls for raw mode
    // Handle immediate character input
}`
    },
    {
      title: "Test and Record",
      description: "Use VHS to create recordings and validate functionality",
      code: `vhs demo.tape  # Creates GIF and text screenshots
go run test.go  # Automated validation`
    }
  ]

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800">
      {/* Navigation */}
      <nav className="sticky top-0 z-50 bg-white/80 dark:bg-slate-900/80 backdrop-blur-sm border-b">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center h-16">
            <div className="flex items-center space-x-2">
              <Terminal className="h-8 w-8 text-blue-600" />
              <span className="text-xl font-bold">TUI + Goja Guide</span>
            </div>
            
            {/* Desktop Navigation */}
            <div className="hidden md:flex items-center space-x-4">
              <Button 
                variant={currentView === 'guide' ? 'default' : 'ghost'} 
                size="sm"
                onClick={() => setCurrentView('guide')}
              >
                <BookOpen className="h-4 w-4 mr-2" />
                Guide
              </Button>
              <Button 
                variant={currentView === 'blog' ? 'default' : 'ghost'} 
                size="sm"
                onClick={() => setCurrentView('blog')}
              >
                <Code className="h-4 w-4 mr-2" />
                Blog Post
              </Button>
              <Button variant="outline" size="sm">
                <Github className="h-4 w-4 mr-2" />
                GitHub
              </Button>
              <Button size="sm">
                <Download className="h-4 w-4 mr-2" />
                Download
              </Button>
            </div>

            {/* Mobile Menu Button */}
            <div className="md:hidden">
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setMobileMenuOpen(!mobileMenuOpen)}
              >
                {mobileMenuOpen ? <X className="h-4 w-4" /> : <Menu className="h-4 w-4" />}
              </Button>
            </div>
          </div>

          {/* Mobile Menu */}
          {mobileMenuOpen && (
            <div className="md:hidden py-4 border-t">
              <div className="flex flex-col space-y-2">
                <Button 
                  variant={currentView === 'guide' ? 'default' : 'ghost'} 
                  size="sm"
                  onClick={() => {
                    setCurrentView('guide')
                    setMobileMenuOpen(false)
                  }}
                  className="justify-start"
                >
                  <BookOpen className="h-4 w-4 mr-2" />
                  Guide
                </Button>
                <Button 
                  variant={currentView === 'blog' ? 'default' : 'ghost'} 
                  size="sm"
                  onClick={() => {
                    setCurrentView('blog')
                    setMobileMenuOpen(false)
                  }}
                  className="justify-start"
                >
                  <Code className="h-4 w-4 mr-2" />
                  Blog Post
                </Button>
                <Button variant="outline" size="sm" className="justify-start">
                  <Github className="h-4 w-4 mr-2" />
                  GitHub
                </Button>
                <Button size="sm" className="justify-start">
                  <Download className="h-4 w-4 mr-2" />
                  Download
                </Button>
              </div>
            </div>
          )}
        </div>
      </nav>

      {/* Main Content - Conditional Rendering */}
      {currentView === 'guide' ? (
        <>
          {/* Hero Section */}
          <section className="py-20 px-4 sm:px-6 lg:px-8">
            <div className="max-w-7xl mx-auto text-center">
              <Badge className="mb-4" variant="secondary">
                <Zap className="h-3 w-3 mr-1" />
                JavaScript TUI in Go
              </Badge>
              <h1 className="text-4xl sm:text-6xl font-bold text-slate-900 dark:text-white mb-6">
                Build Terminal UIs with
                <span className="text-blue-600 block">JavaScript + Goja</span>
              </h1>
              <p className="text-xl text-slate-600 dark:text-slate-300 mb-8 max-w-3xl mx-auto">
                A comprehensive guide to creating interactive terminal user interfaces using JavaScript 
                that runs within the goja JavaScript VM for Go applications.
              </p>
              <div className="flex flex-col sm:flex-row gap-4 justify-center">
                <Button size="lg" className="text-lg px-8" onClick={() => scrollToSection('demo')}>
                  <Play className="h-5 w-5 mr-2" />
                  View Demo
                </Button>
                <Button size="lg" variant="outline" className="text-lg px-8" onClick={() => setCurrentView('blog')}>
                  <BookOpen className="h-5 w-5 mr-2" />
                  Read Blog
                </Button>
              </div>
            </div>
          </section>

      {/* Demo Section */}
      <section id="demo" className="py-16 px-4 sm:px-6 lg:px-8 bg-white dark:bg-slate-800">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl font-bold text-slate-900 dark:text-white mb-4">
              See It In Action
            </h2>
            <p className="text-lg text-slate-600 dark:text-slate-300">
              Interactive TUI with real-time input handling and visual feedback
            </p>
          </div>
          
          <div className="grid lg:grid-cols-2 gap-8 items-center">
            <div className="order-2 lg:order-1">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center">
                    <Terminal className="h-5 w-5 mr-2" />
                    Live Demo Recording
                  </CardTitle>
                  <CardDescription>
                    VHS recording showing the TUI responding to single character input
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="bg-slate-900 rounded-lg p-4 overflow-hidden">
                    <img 
                      src={demoGif} 
                      alt="TUI Demo Recording" 
                      className="w-full rounded border"
                    />
                  </div>
                </CardContent>
              </Card>
            </div>
            
            <div className="order-1 lg:order-2 space-y-6">
              <div className="grid grid-cols-2 gap-4">
                {features.slice(0, 4).map((feature, index) => (
                  <Card key={index} className="p-4">
                    <div className="flex items-center space-x-3">
                      <div className="text-blue-600">{feature.icon}</div>
                      <div>
                        <h3 className="font-semibold text-sm">{feature.title}</h3>
                        <p className="text-xs text-slate-600 dark:text-slate-400">
                          {feature.description}
                        </p>
                      </div>
                    </div>
                  </Card>
                ))}
              </div>
              
              <Card className="p-6">
                <h3 className="font-semibold mb-3 flex items-center">
                  <CheckCircle className="h-5 w-5 mr-2 text-green-600" />
                  Key Achievements
                </h3>
                <ul className="space-y-2 text-sm">
                  <li className="flex items-center">
                    <ArrowRight className="h-4 w-4 mr-2 text-blue-600" />
                    Single character input without Enter key
                  </li>
                  <li className="flex items-center">
                    <ArrowRight className="h-4 w-4 mr-2 text-blue-600" />
                    Real-time UI updates and state management
                  </li>
                  <li className="flex items-center">
                    <ArrowRight className="h-4 w-4 mr-2 text-blue-600" />
                    ES5 compatibility with modern JavaScript features
                  </li>
                  <li className="flex items-center">
                    <ArrowRight className="h-4 w-4 mr-2 text-blue-600" />
                    VHS text screenshots for automated validation
                  </li>
                </ul>
              </Card>
            </div>
          </div>
        </div>
      </section>

      {/* Implementation Guide */}
      <section className="py-16 px-4 sm:px-6 lg:px-8">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl font-bold text-slate-900 dark:text-white mb-4">
              Implementation Guide
            </h2>
            <p className="text-lg text-slate-600 dark:text-slate-300">
              Step-by-step instructions to build your own JavaScript TUI in Go
            </p>
          </div>

          <div className="grid gap-8">
            {steps.map((step, index) => (
              <Card key={index} className="overflow-hidden">
                <CardHeader>
                  <div className="flex items-center space-x-4">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-600 text-white rounded-full flex items-center justify-center font-semibold">
                      {index + 1}
                    </div>
                    <div>
                      <CardTitle>{step.title}</CardTitle>
                      <CardDescription>{step.description}</CardDescription>
                    </div>
                  </div>
                </CardHeader>
                <CardContent>
                  <div className="bg-slate-900 rounded-lg p-4 overflow-x-auto">
                    <pre className="text-green-400 text-sm">
                      <code>{step.code}</code>
                    </pre>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* Code Examples */}
      <section className="py-16 px-4 sm:px-6 lg:px-8 bg-white dark:bg-slate-800">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl font-bold text-slate-900 dark:text-white mb-4">
              Code Examples
            </h2>
            <p className="text-lg text-slate-600 dark:text-slate-300">
              Complete code samples for each component of the system
            </p>
          </div>

          <Tabs defaultValue="javascript" className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="javascript">JavaScript TUI</TabsTrigger>
              <TabsTrigger value="go">Go Integration</TabsTrigger>
              <TabsTrigger value="webpack">Webpack Config</TabsTrigger>
              <TabsTrigger value="vhs">VHS Recording</TabsTrigger>
            </TabsList>
            
            {Object.entries(codeExamples).map(([key, code]) => (
              <TabsContent key={key} value={key}>
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center">
                      <Code className="h-5 w-5 mr-2" />
                      {key === 'javascript' && 'JavaScript TUI Library'}
                      {key === 'go' && 'Go Application with Goja'}
                      {key === 'webpack' && 'Webpack Configuration'}
                      {key === 'vhs' && 'VHS Recording Script'}
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="bg-slate-900 rounded-lg p-6 overflow-x-auto">
                      <pre className="text-green-400 text-sm">
                        <code>{code}</code>
                      </pre>
                    </div>
                  </CardContent>
                </Card>
              </TabsContent>
            ))}
          </Tabs>
        </div>
      </section>

      {/* Technical Deep Dive */}
      <section className="py-16 px-4 sm:px-6 lg:px-8">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl font-bold text-slate-900 dark:text-white mb-4">
              Technical Deep Dive
            </h2>
            <p className="text-lg text-slate-600 dark:text-slate-300">
              Understanding the architecture and implementation details
            </p>
          </div>

          <div className="grid lg:grid-cols-3 gap-8">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center">
                  <Layers className="h-5 w-5 mr-2" />
                  Architecture
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <h4 className="font-semibold">JavaScript Layer</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    ES5-compatible TUI library with components for text, boxes, and progress bars
                  </p>
                </div>
                <div className="space-y-2">
                  <h4 className="font-semibold">Go Integration</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Goja VM executes JavaScript with function call bridging
                  </p>
                </div>
                <div className="space-y-2">
                  <h4 className="font-semibold">Terminal Control</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Raw mode terminal handling for immediate input response
                  </p>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center">
                  <Settings className="h-5 w-5 mr-2" />
                  Challenges Solved
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <h4 className="font-semibold">ES5 Limitations</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Webpack + Babel transpilation for goja compatibility
                  </p>
                </div>
                <div className="space-y-2">
                  <h4 className="font-semibold">Node.js Dependencies</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Custom TUI library avoiding Node.js-specific modules
                  </p>
                </div>
                <div className="space-y-2">
                  <h4 className="font-semibold">Input Handling</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Terminal raw mode with proper cleanup and error handling
                  </p>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center">
                  <Monitor className="h-5 w-5 mr-2" />
                  Validation
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <h4 className="font-semibold">VHS Text Screenshots</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Automated validation using text-based output capture
                  </p>
                </div>
                <div className="space-y-2">
                  <h4 className="font-semibold">Integration Tests</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Comprehensive testing of JavaScript-Go function calls
                  </p>
                </div>
                <div className="space-y-2">
                  <h4 className="font-semibold">Demo Recordings</h4>
                  <p className="text-sm text-slate-600 dark:text-slate-400">
                    Visual demonstrations with GIF and text output
                  </p>
                </div>
              </CardContent>
            </Card>
          </div>
        </div>
      </section>
        </>
      ) : (
        /* Blog Post View */
        <BlogPost />
      )}

      {/* Footer */}
      <footer className="py-12 px-4 sm:px-6 lg:px-8 bg-slate-900 text-white">
        <div className="max-w-7xl mx-auto text-center">
          <div className="flex items-center justify-center space-x-2 mb-4">
            <Terminal className="h-6 w-6" />
            <span className="text-lg font-semibold">TUI + Goja Integration</span>
          </div>
          <p className="text-slate-400 mb-6">
            A complete guide to building JavaScript terminal interfaces in Go applications
          </p>
          <div className="flex justify-center space-x-6">
            <Button variant="ghost" size="sm">
              <Github className="h-4 w-4 mr-2" />
              Source Code
            </Button>
            <Button variant="ghost" size="sm">
              <BookOpen className="h-4 w-4 mr-2" />
              Documentation
            </Button>
            <Button variant="ghost" size="sm">
              <Download className="h-4 w-4 mr-2" />
              Download Project
            </Button>
          </div>
        </div>
      </footer>
    </div>
  )
}

export default App

