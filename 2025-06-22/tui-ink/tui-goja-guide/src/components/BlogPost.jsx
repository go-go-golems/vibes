import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { 
  Clock, 
  User, 
  Calendar,
  Code2,
  Terminal,
  Lightbulb,
  AlertTriangle,
  CheckCircle2,
  ArrowRight
} from 'lucide-react'

export function BlogPost() {
  const blogMeta = {
    title: "Building JavaScript TUIs in Go: A Deep Dive into Goja Integration",
    author: "TUI Development Team",
    date: "June 22, 2025",
    readTime: "15 min read",
    tags: ["Go", "JavaScript", "TUI", "Goja", "Terminal", "VHS"]
  }

  const tableOfContents = [
    { id: "introduction", title: "Introduction", level: 1 },
    { id: "motivation", title: "Why JavaScript TUIs in Go?", level: 1 },
    { id: "architecture", title: "Architecture Overview", level: 1 },
    { id: "challenges", title: "Technical Challenges", level: 1 },
    { id: "implementation", title: "Implementation Details", level: 1 },
    { id: "terminal-control", title: "Terminal Control & Raw Mode", level: 2 },
    { id: "javascript-bridge", title: "JavaScript-Go Bridge", level: 2 },
    { id: "es5-compatibility", title: "ES5 Compatibility Layer", level: 2 },
    { id: "testing", title: "Testing & Validation", level: 1 },
    { id: "performance", title: "Performance Considerations", level: 1 },
    { id: "lessons", title: "Lessons Learned", level: 1 },
    { id: "conclusion", title: "Conclusion", level: 1 }
  ]

  return (
    <div className="max-w-4xl mx-auto py-12 px-4 sm:px-6 lg:px-8">
      {/* Blog Header */}
      <div className="mb-12">
        <div className="flex flex-wrap gap-2 mb-4">
          {blogMeta.tags.map((tag, index) => (
            <Badge key={index} variant="secondary">{tag}</Badge>
          ))}
        </div>
        
        <h1 className="text-4xl font-bold text-slate-900 dark:text-white mb-6">
          {blogMeta.title}
        </h1>
        
        <div className="flex items-center space-x-6 text-slate-600 dark:text-slate-400">
          <div className="flex items-center space-x-2">
            <User className="h-4 w-4" />
            <span>{blogMeta.author}</span>
          </div>
          <div className="flex items-center space-x-2">
            <Calendar className="h-4 w-4" />
            <span>{blogMeta.date}</span>
          </div>
          <div className="flex items-center space-x-2">
            <Clock className="h-4 w-4" />
            <span>{blogMeta.readTime}</span>
          </div>
        </div>
      </div>

      {/* Table of Contents */}
      <Card className="mb-12">
        <CardHeader>
          <CardTitle className="flex items-center">
            <Code2 className="h-5 w-5 mr-2" />
            Table of Contents
          </CardTitle>
        </CardHeader>
        <CardContent>
          <nav className="space-y-2">
            {tableOfContents.map((item, index) => (
              <a
                key={index}
                href={`#${item.id}`}
                className={`block hover:text-blue-600 transition-colors ${
                  item.level === 2 ? 'ml-4 text-sm text-slate-600 dark:text-slate-400' : 'font-medium'
                }`}
              >
                {item.title}
              </a>
            ))}
          </nav>
        </CardContent>
      </Card>

      {/* Blog Content */}
      <div className="prose prose-slate dark:prose-invert max-w-none">
        
        {/* Introduction */}
        <section id="introduction" className="mb-12">
          <h2 className="text-3xl font-bold mb-6 flex items-center">
            <Terminal className="h-6 w-6 mr-3 text-blue-600" />
            Introduction
          </h2>
          
          <p className="text-lg leading-relaxed mb-6">
            Terminal User Interfaces (TUIs) have experienced a renaissance in recent years, with developers 
            appreciating their efficiency, accessibility, and nostalgic appeal. While Go excels at building 
            robust, performant applications, JavaScript offers unparalleled flexibility for UI logic and 
            rapid prototyping. This article explores how we successfully combined these strengths by running 
            JavaScript-based TUI code within Go applications using the goja JavaScript VM.
          </p>

          <Card className="mb-6 border-blue-200 bg-blue-50 dark:bg-blue-950 dark:border-blue-800">
            <CardContent className="pt-6">
              <div className="flex items-start space-x-3">
                <Lightbulb className="h-5 w-5 text-blue-600 mt-1" />
                <div>
                  <h4 className="font-semibold text-blue-900 dark:text-blue-100 mb-2">Key Innovation</h4>
                  <p className="text-blue-800 dark:text-blue-200">
                    This project demonstrates the first known implementation of a complete JavaScript TUI 
                    framework running within a Go application, achieving single-character input handling 
                    and real-time UI updates through careful integration of terminal control and JavaScript execution.
                  </p>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Motivation */}
        <section id="motivation" className="mb-12">
          <h2 className="text-3xl font-bold mb-6">Why JavaScript TUIs in Go?</h2>
          
          <div className="grid md:grid-cols-2 gap-6 mb-8">
            <Card>
              <CardHeader>
                <CardTitle className="text-green-600">Advantages</CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                <div className="flex items-start space-x-2">
                  <CheckCircle2 className="h-4 w-4 text-green-600 mt-1" />
                  <span className="text-sm">Familiar JavaScript syntax for UI logic</span>
                </div>
                <div className="flex items-start space-x-2">
                  <CheckCircle2 className="h-4 w-4 text-green-600 mt-1" />
                  <span className="text-sm">Rapid prototyping and iteration</span>
                </div>
                <div className="flex items-start space-x-2">
                  <CheckCircle2 className="h-4 w-4 text-green-600 mt-1" />
                  <span className="text-sm">Go's performance and deployment benefits</span>
                </div>
                <div className="flex items-start space-x-2">
                  <CheckCircle2 className="h-4 w-4 text-green-600 mt-1" />
                  <span className="text-sm">Single binary distribution</span>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-orange-600">Challenges</CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                <div className="flex items-start space-x-2">
                  <AlertTriangle className="h-4 w-4 text-orange-600 mt-1" />
                  <span className="text-sm">ES5 compatibility requirements</span>
                </div>
                <div className="flex items-start space-x-2">
                  <AlertTriangle className="h-4 w-4 text-orange-600 mt-1" />
                  <span className="text-sm">Terminal control complexity</span>
                </div>
                <div className="flex items-start space-x-2">
                  <AlertTriangle className="h-4 w-4 text-orange-600 mt-1" />
                  <span className="text-sm">Function call bridging</span>
                </div>
                <div className="flex items-start space-x-2">
                  <AlertTriangle className="h-4 w-4 text-orange-600 mt-1" />
                  <span className="text-sm">Testing and validation</span>
                </div>
              </CardContent>
            </Card>
          </div>

          <p className="leading-relaxed">
            The motivation came from observing the growing complexity of TUI applications and the desire 
            to leverage web development skills in terminal environments. While libraries like Bubble Tea 
            provide excellent Go-native solutions, JavaScript's expressiveness and the vast ecosystem 
            of UI patterns make it an attractive alternative for certain use cases.
          </p>
        </section>

        {/* Architecture */}
        <section id="architecture" className="mb-12">
          <h2 className="text-3xl font-bold mb-6">Architecture Overview</h2>
          
          <p className="leading-relaxed mb-6">
            Our architecture consists of three main layers, each with distinct responsibilities:
          </p>

          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle className="text-blue-600">1. JavaScript TUI Layer</CardTitle>
                <CardDescription>ES5-compatible component library</CardDescription>
              </CardHeader>
              <CardContent>
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-4">
                  Custom-built TUI components including boxes, progress bars, and text formatting. 
                  Designed specifically for goja compatibility without Node.js dependencies.
                </p>
                <div className="bg-slate-900 rounded p-4 text-green-400 text-sm font-mono">
                  <div>function SimpleTUI() &#123;</div>
                  <div className="ml-4">this.components = [];</div>
                  <div className="ml-4">this.inputHandlers = [];</div>
                  <div>&#125;</div>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-purple-600">2. Go Integration Layer</CardTitle>
                <CardDescription>Goja VM and function bridging</CardDescription>
              </CardHeader>
              <CardContent>
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-4">
                  Manages JavaScript execution, handles function calls between Go and JavaScript, 
                  and provides the runtime environment for TUI components.
                </p>
                <div className="bg-slate-900 rounded p-4 text-green-400 text-sm font-mono">
                  <div>vm := goja.New()</div>
                  <div>vm.RunString(jsBundle)</div>
                  <div>app, _ := vm.New(constructor)</div>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-green-600">3. Terminal Control Layer</CardTitle>
                <CardDescription>Raw mode and input handling</CardDescription>
              </CardHeader>
              <CardContent>
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-4">
                  Manages terminal state, enables raw mode for single-character input, 
                  and handles screen clearing and cursor positioning.
                </p>
                <div className="bg-slate-900 rounded p-4 text-green-400 text-sm font-mono">
                  <div>oldTermios, _ := enableRawMode()</div>
                  <div>defer restoreTerminal(oldTermios)</div>
                  <div>char, _ := readChar()</div>
                </div>
              </CardContent>
            </Card>
          </div>
        </section>

        {/* Continue with more sections... */}
        <section id="challenges" className="mb-12">
          <h2 className="text-3xl font-bold mb-6">Technical Challenges</h2>
          
          <p className="leading-relaxed mb-6">
            Building this integration required solving several complex technical challenges:
          </p>

          <div className="space-y-8">
            <div>
              <h3 className="text-xl font-semibold mb-4 flex items-center">
                <ArrowRight className="h-5 w-5 mr-2 text-blue-600" />
                ES5 Compatibility
              </h3>
              <p className="leading-relaxed mb-4">
                Goja implements ECMAScript 5.1, which means modern JavaScript features like arrow functions, 
                classes, and modules are not available. We solved this by:
              </p>
              <ul className="list-disc list-inside space-y-2 text-slate-600 dark:text-slate-400">
                <li>Using Webpack with Babel to transpile modern JavaScript to ES5</li>
                <li>Avoiding ES6+ features in our core TUI library</li>
                <li>Implementing custom polyfills for missing functionality</li>
                <li>Using function constructors instead of classes</li>
              </ul>
            </div>

            <div>
              <h3 className="text-xl font-semibold mb-4 flex items-center">
                <ArrowRight className="h-5 w-5 mr-2 text-blue-600" />
                Terminal Raw Mode
              </h3>
              <p className="leading-relaxed mb-4">
                Achieving single-character input without requiring Enter key presses required implementing 
                terminal raw mode using system calls:
              </p>
              <div className="bg-slate-900 rounded p-4 text-green-400 text-sm font-mono mb-4">
                <div>func enableRawMode() (*termios, error) &#123;</div>
                <div className="ml-4">var oldTermios termios</div>
                <div className="ml-4">syscall.Syscall(syscall.SYS_IOCTL, ...)</div>
                <div className="ml-4">newTermios.Lflag &^= ICANON | ECHO</div>
                <div className="ml-4">return &oldTermios, nil</div>
                <div>&#125;</div>
              </div>
            </div>
          </div>
        </section>

        {/* Add more sections as needed */}
        <section id="conclusion" className="mb-12">
          <h2 className="text-3xl font-bold mb-6">Conclusion</h2>
          
          <p className="leading-relaxed mb-6">
            This project demonstrates that JavaScript and Go can be successfully combined to create 
            sophisticated terminal applications. The integration of goja, careful ES5 compatibility 
            handling, and proper terminal control creates a powerful platform for TUI development.
          </p>

          <Card className="border-green-200 bg-green-50 dark:bg-green-950 dark:border-green-800">
            <CardContent className="pt-6">
              <div className="flex items-start space-x-3">
                <CheckCircle2 className="h-5 w-5 text-green-600 mt-1" />
                <div>
                  <h4 className="font-semibold text-green-900 dark:text-green-100 mb-2">Project Success</h4>
                  <p className="text-green-800 dark:text-green-200">
                    We successfully created a working JavaScript TUI framework that runs within Go, 
                    complete with real-time input handling, visual components, and comprehensive testing. 
                    The approach opens new possibilities for developers familiar with web technologies 
                    to create sophisticated terminal applications.
                  </p>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>
      </div>
    </div>
  )
}

