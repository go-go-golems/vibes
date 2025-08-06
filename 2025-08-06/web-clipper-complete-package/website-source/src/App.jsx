import { useState } from 'react'
import { Button } from '@/components/ui/button.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { Download, Github, Chrome, Code, FileText, Shield, Zap, Database, Settings, Globe } from 'lucide-react'
import icon128 from './assets/icon128.png'
import './App.css'

function App() {
  const [activeTab, setActiveTab] = useState('overview')

  const features = [
    {
      icon: <Shield className="h-6 w-6" />,
      title: "Privacy First",
      description: "All data stored locally. No cloud services, no tracking, no data collection."
    },
    {
      icon: <Zap className="h-6 w-6" />,
      title: "Cross-Browser",
      description: "Works seamlessly on both Chrome and Firefox with native extension APIs."
    },
    {
      icon: <Database className="h-6 w-6" />,
      title: "Rich Metadata",
      description: "Captures URLs, selected text, notes, titles, and timestamps automatically."
    },
    {
      icon: <FileText className="h-6 w-6" />,
      title: "Organized Storage",
      description: "Saves clips as markdown files organized by date and category."
    }
  ]

  const categories = [
    { name: "TIL", description: "Today I Learned - for new knowledge and insights", color: "bg-blue-100 text-blue-800" },
    { name: "article", description: "Full articles, blog posts, and long-form content", color: "bg-green-100 text-green-800" },
    { name: "thought", description: "Personal thoughts, reflections, and ideas", color: "bg-purple-100 text-purple-800" },
    { name: "quote", description: "Notable quotes, excerpts, and memorable passages", color: "bg-orange-100 text-orange-800" }
  ]

  const installSteps = [
    {
      step: 1,
      title: "Download & Extract",
      description: "Download the extension zip file and extract it to your preferred location."
    },
    {
      step: 2,
      title: "Build Backend",
      description: "Navigate to the backend directory and run: go build -o clipper-backend main.go"
    },
    {
      step: 3,
      title: "Install Native Messaging",
      description: "Run the install script: ./scripts/install-native-messaging.sh"
    },
    {
      step: 4,
      title: "Load Extension",
      description: "Load the extension in your browser's developer mode from the extension directory."
    }
  ]

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100">
      {/* Header */}
      <header className="border-b bg-white/80 backdrop-blur-sm sticky top-0 z-50">
        <div className="container mx-auto px-4 py-4">
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-3">
              <img src={icon128} alt="Web Clipper" className="h-10 w-10" />
              <div>
                <h1 className="text-2xl font-bold text-slate-900">Web Clipper Extension</h1>
                <p className="text-sm text-slate-600">Clip, organize, and save web content locally</p>
              </div>
            </div>
            <div className="flex items-center space-x-3">
              <Button variant="outline" size="sm">
                <Github className="h-4 w-4 mr-2" />
                GitHub
              </Button>
            <Button size="sm" className="bg-blue-600 hover:bg-blue-700">
              <Download className="h-4 w-4 mr-2" />
              <a href="/web-clipper-extension.zip" download className="text-white no-underline">Download</a>
            </Button>
            </div>
          </div>
        </div>
      </header>

      {/* Hero Section */}
      <section className="py-20 px-4">
        <div className="container mx-auto text-center">
          <div className="max-w-3xl mx-auto">
            <h2 className="text-5xl font-bold text-slate-900 mb-6">
              Clip Web Content with
              <span className="text-blue-600"> Privacy & Control</span>
            </h2>
            <p className="text-xl text-slate-600 mb-8 leading-relaxed">
              A cross-browser extension that lets you save web content, selected text, and personal notes 
              directly to your local machine. No cloud services, no tracking, complete privacy.
            </p>
            <div className="flex items-center justify-center space-x-4 mb-8">
              <Badge variant="secondary" className="px-3 py-1">
                <Chrome className="h-4 w-4 mr-1" />
                Chrome
              </Badge>
              <Badge variant="secondary" className="px-3 py-1">
                <Globe className="h-4 w-4 mr-1" />
                Firefox
              </Badge>
              <Badge variant="secondary" className="px-3 py-1">
                <Code className="h-4 w-4 mr-1" />
                Go Backend
              </Badge>
            </div>
            <Button size="lg" className="bg-blue-600 hover:bg-blue-700 text-lg px-8 py-3">
              <Download className="h-5 w-5 mr-2" />
              <a href="/web-clipper-extension.zip" download className="text-white no-underline">Download Extension</a>
            </Button>
          </div>
        </div>
      </section>

      {/* Features Grid */}
      <section className="py-16 px-4 bg-white">
        <div className="container mx-auto">
          <h3 className="text-3xl font-bold text-center text-slate-900 mb-12">
            Why Choose Web Clipper?
          </h3>
          <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-6">
            {features.map((feature, index) => (
              <Card key={index} className="text-center hover:shadow-lg transition-shadow">
                <CardHeader>
                  <div className="mx-auto mb-4 p-3 bg-blue-100 rounded-full w-fit">
                    {feature.icon}
                  </div>
                  <CardTitle className="text-lg">{feature.title}</CardTitle>
                </CardHeader>
                <CardContent>
                  <p className="text-slate-600">{feature.description}</p>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* Main Content Tabs */}
      <section className="py-16 px-4">
        <div className="container mx-auto">
          <Tabs value={activeTab} onValueChange={setActiveTab} className="max-w-6xl mx-auto">
            <TabsList className="grid w-full grid-cols-4 mb-8">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="installation">Installation</TabsTrigger>
              <TabsTrigger value="usage">Usage</TabsTrigger>
              <TabsTrigger value="api">API Reference</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-8">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center">
                    <Settings className="h-5 w-5 mr-2" />
                    How It Works
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="grid md:grid-cols-3 gap-6">
                    <div className="text-center">
                      <div className="bg-blue-100 rounded-full w-12 h-12 flex items-center justify-center mx-auto mb-3">
                        <span className="font-bold text-blue-600">1</span>
                      </div>
                      <h4 className="font-semibold mb-2">Browser Extension</h4>
                      <p className="text-sm text-slate-600">Clean popup interface for capturing content with metadata</p>
                    </div>
                    <div className="text-center">
                      <div className="bg-blue-100 rounded-full w-12 h-12 flex items-center justify-center mx-auto mb-3">
                        <span className="font-bold text-blue-600">2</span>
                      </div>
                      <h4 className="font-semibold mb-2">Native Messaging</h4>
                      <p className="text-sm text-slate-600">Secure communication between browser and local backend</p>
                    </div>
                    <div className="text-center">
                      <div className="bg-blue-100 rounded-full w-12 h-12 flex items-center justify-center mx-auto mb-3">
                        <span className="font-bold text-blue-600">3</span>
                      </div>
                      <h4 className="font-semibold mb-2">Local Storage</h4>
                      <p className="text-sm text-slate-600">Organized markdown files saved directly to your machine</p>
                    </div>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Content Categories</CardTitle>
                  <CardDescription>Organize your clips with built-in categories</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="grid md:grid-cols-2 gap-4">
                    {categories.map((category, index) => (
                      <div key={index} className="flex items-start space-x-3 p-3 border rounded-lg">
                        <Badge className={category.color}>{category.name}</Badge>
                        <div>
                          <p className="text-sm text-slate-600">{category.description}</p>
                        </div>
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="installation" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Installation Guide</CardTitle>
                  <CardDescription>Step-by-step instructions to get Web Clipper running</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    {installSteps.map((step, index) => (
                      <div key={index} className="flex items-start space-x-4">
                        <div className="bg-blue-600 text-white rounded-full w-8 h-8 flex items-center justify-center font-bold text-sm">
                          {step.step}
                        </div>
                        <div className="flex-1">
                          <h4 className="font-semibold text-lg mb-1">{step.title}</h4>
                          <p className="text-slate-600">{step.description}</p>
                        </div>
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Requirements</CardTitle>
                </CardHeader>
                <CardContent>
                  <ul className="space-y-2 text-slate-600">
                    <li>• Go 1.24.5 or later</li>
                    <li>• Chrome or Firefox browser</li>
                    <li>• Linux, macOS, or Windows</li>
                    <li>• Basic command line knowledge</li>
                  </ul>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="usage" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Using Web Clipper</CardTitle>
                  <CardDescription>Learn how to clip and organize web content</CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="bg-slate-50 p-4 rounded-lg">
                    <h4 className="font-semibold mb-2">Basic Workflow</h4>
                    <ol className="list-decimal list-inside space-y-1 text-slate-600">
                      <li>Navigate to any webpage</li>
                      <li>Optionally select text you want to clip</li>
                      <li>Click the Web Clipper extension icon</li>
                      <li>Fill in the form (title, category, notes)</li>
                      <li>Click "Save Clip"</li>
                    </ol>
                  </div>
                  
                  <div className="bg-slate-50 p-4 rounded-lg">
                    <h4 className="font-semibold mb-2">File Organization</h4>
                    <p className="text-slate-600 mb-2">Clips are automatically organized in the following structure:</p>
                    <pre className="bg-slate-800 text-slate-100 p-3 rounded text-sm overflow-x-auto">
{`clips/
├── 2025-08-06/
│   ├── TIL-interesting-fact.md
│   ├── article-great-blog-post.md
│   ├── quote-memorable-passage.md
│   └── thought-personal-reflection.md`}
                    </pre>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="api" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Native Messaging API</CardTitle>
                  <CardDescription>Technical reference for the communication protocol</CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <h4 className="font-semibold mb-2">Message Format</h4>
                    <pre className="bg-slate-800 text-slate-100 p-4 rounded text-sm overflow-x-auto">
{`{
  "action": "saveClip",
  "data": {
    "timestamp": "2025-08-06T12:00:00Z",
    "url": "https://example.com",
    "title": "Custom Title",
    "category": "TIL",
    "selectedText": "Selected content",
    "note": "Personal notes",
    "pageTitle": "Original Page Title",
    "domain": "example.com"
  }
}`}
                    </pre>
                  </div>
                  
                  <div>
                    <h4 className="font-semibold mb-2">Response Format</h4>
                    <pre className="bg-slate-800 text-slate-100 p-4 rounded text-sm overflow-x-auto">
{`{
  "success": true,
  "message": "Clip saved successfully"
}`}
                    </pre>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>
      </section>

      {/* Download Section */}
      <section className="py-16 px-4 bg-blue-600 text-white">
        <div className="container mx-auto text-center">
          <h3 className="text-3xl font-bold mb-4">Ready to Get Started?</h3>
          <p className="text-xl mb-8 text-blue-100">
            Download Web Clipper and start organizing your web content today.
          </p>
          <Button size="lg" variant="secondary" className="text-lg px-8 py-3">
            <Download className="h-5 w-5 mr-2" />
            <a href="/web-clipper-extension.zip" download className="text-slate-900 no-underline">Download Extension (8.1 MB)</a>
          </Button>
          <p className="text-sm text-blue-200 mt-4">
            Free and open source • MIT License • No registration required
          </p>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-8 px-4 bg-slate-900 text-slate-300">
        <div className="container mx-auto text-center">
          <p>&copy; 2025 Web Clipper Extension. Released under MIT License.</p>
          <p className="text-sm mt-2">Built with privacy and user control in mind.</p>
        </div>
      </footer>
    </div>
  )
}

export default App

