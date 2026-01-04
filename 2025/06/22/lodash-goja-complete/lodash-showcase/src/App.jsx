import { useState } from 'react'
import { Button } from '@/components/ui/button.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { Progress } from '@/components/ui/progress.jsx'
import { InteractiveCharts } from '@/components/InteractiveCharts.jsx'
import { CodeExamples } from '@/components/CodeExamples.jsx'
import { 
  Code2, 
  Zap, 
  Package, 
  BarChart3, 
  CheckCircle, 
  ArrowRight,
  Github,
  Download,
  Cpu,
  FileText,
  Gauge
} from 'lucide-react'
import './App.css'

function App() {
  const [activeTab, setActiveTab] = useState('overview')

  const minificationResults = [
    {
      method: 'Terser',
      size: 71124,
      reduction: 86.9,
      performance: 8.55,
      badge: 'Best Compression',
      color: 'bg-green-500'
    },
    {
      method: 'tdewolff',
      size: 71895,
      reduction: 86.8,
      performance: 12.06,
      badge: 'Pure Go',
      color: 'bg-blue-500'
    },
    {
      method: 'esbuild',
      size: 72859,
      reduction: 86.6,
      performance: 13.91,
      badge: 'Best Performance',
      color: 'bg-purple-500'
    }
  ]

  const originalSize = 544098

  const features = [
    {
      icon: <Package className="h-6 w-6" />,
      title: 'Self-Contained',
      description: 'No external dependencies at runtime. Everything embedded in the Go binary.'
    },
    {
      icon: <Zap className="h-6 w-6" />,
      title: 'High Performance',
      description: 'Up to 13.91 operations per millisecond with optimized minification.'
    },
    {
      icon: <Code2 className="h-6 w-6" />,
      title: 'Full Lodash Support',
      description: 'All lodash functions work perfectly in the Goja JavaScript VM.'
    },
    {
      icon: <BarChart3 className="h-6 w-6" />,
      title: 'Multiple Minifiers',
      description: 'Compare and choose between terser, esbuild, and tdewolff minification.'
    }
  ]

  const codeExample = `package main

import (
    _ "embed"
    "fmt"
    "github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func main() {
    vm := goja.New()
    vm.RunString(lodashSrc)
    
    result, _ := vm.RunString("_.uniq([1,1,2,3])")
    fmt.Println(result.Export()) // [1 2 3]
}`

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800">
      {/* Hero Section */}
      <div className="relative overflow-hidden">
        <div className="absolute inset-0 bg-gradient-to-r from-blue-600/10 to-purple-600/10" />
        <div className="absolute inset-0 bg-[url('data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iNjAiIGhlaWdodD0iNjAiIHZpZXdCb3g9IjAgMCA2MCA2MCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48ZyBmaWxsPSJub25lIiBmaWxsLXJ1bGU9ImV2ZW5vZGQiPjxnIGZpbGw9IiM5QzkyQUMiIGZpbGwtb3BhY2l0eT0iMC4wNSI+PGNpcmNsZSBjeD0iMzAiIGN5PSIzMCIgcj0iMiIvPjwvZz48L2c+PC9zdmc+')] opacity-20" />
        <div className="relative max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-24">
          <div className="text-center">
            <Badge className="mb-4 bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200 float-animation">
              <Code2 className="h-3 w-3 mr-1" />
              Go + JavaScript Integration
            </Badge>
            <h1 className="hero-title text-5xl font-bold text-gray-900 dark:text-white mb-6">
              Lodash <span className="text-blue-600 bg-gradient-to-r from-blue-600 to-purple-600 bg-clip-text text-transparent">Goja VM</span>
            </h1>
            <p className="hero-subtitle text-xl text-gray-600 dark:text-gray-300 mb-8 max-w-3xl mx-auto">
              A comprehensive implementation of building lodash from source and running it efficiently 
              in a Goja JavaScript VM with multiple minification methods comparison.
            </p>
            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <Button size="lg" className="bg-blue-600 hover:bg-blue-700 btn-enhanced pulse-glow">
                <Download className="h-4 w-4 mr-2" />
                Download Implementation
              </Button>
              <Button size="lg" variant="outline" className="btn-enhanced">
                <Github className="h-4 w-4 mr-2" />
                View on GitHub
              </Button>
            </div>
          </div>
        </div>
      </div>

      {/* Features Section */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16">
        <div className="feature-grid grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          {features.map((feature, index) => (
            <Card key={index} className="card-hover hover:shadow-lg transition-all duration-300 border-0 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm">
              <CardHeader>
                <div className="flex items-center space-x-3">
                  <div className="p-2 bg-blue-100 dark:bg-blue-900 rounded-lg float-animation" style={{animationDelay: `${index * 0.2}s`}}>
                    {feature.icon}
                  </div>
                  <CardTitle className="text-lg">{feature.title}</CardTitle>
                </div>
              </CardHeader>
              <CardContent>
                <p className="text-gray-600 dark:text-gray-300">{feature.description}</p>
              </CardContent>
            </Card>
          ))}
        </div>
      </div>

      {/* Main Content Tabs */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-16">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
          <TabsList className="tab-list grid w-full grid-cols-4 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm">
            <TabsTrigger value="overview" className="transition-all duration-300">Overview</TabsTrigger>
            <TabsTrigger value="comparison" className="transition-all duration-300">Comparison</TabsTrigger>
            <TabsTrigger value="performance" className="transition-all duration-300">Performance</TabsTrigger>
            <TabsTrigger value="implementation" className="transition-all duration-300">Implementation</TabsTrigger>
          </TabsList>

          <TabsContent value="overview" className="mt-8 tab-content">
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
              <Card className="card-hover border-0 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm">
                <CardHeader>
                  <CardTitle className="flex items-center">
                    <FileText className="h-5 w-5 mr-2" />
                    Project Overview
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p className="text-gray-600 dark:text-gray-300">
                    This project demonstrates a complete pipeline for integrating lodash into Go applications 
                    using the Goja JavaScript VM. It includes automated downloading, multiple minification 
                    strategies, and comprehensive testing.
                  </p>
                  <div className="space-y-2">
                    {[
                      "Automated lodash download from npm",
                      "Three minification methods tested",
                      "Full lodash functionality validated",
                      "Production-ready implementation"
                    ].map((item, index) => (
                      <div key={index} className="flex items-center" style={{animationDelay: `${index * 0.1}s`}}>
                        <CheckCircle className="h-4 w-4 text-green-500 mr-2 animate-pulse" />
                        <span>{item}</span>
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>

              <Card className="card-hover border-0 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm">
                <CardHeader>
                  <CardTitle>Quick Stats</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="stats-grid grid grid-cols-2 gap-4">
                    <div className="text-center p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg card-hover">
                      <div className="text-2xl font-bold text-blue-600">86.9%</div>
                      <div className="text-sm text-gray-600 dark:text-gray-300">Best Compression</div>
                    </div>
                    <div className="text-center p-4 bg-green-50 dark:bg-green-900/20 rounded-lg card-hover">
                      <div className="text-2xl font-bold text-green-600">13.91</div>
                      <div className="text-sm text-gray-600 dark:text-gray-300">Ops/ms</div>
                    </div>
                    <div className="text-center p-4 bg-purple-50 dark:bg-purple-900/20 rounded-lg card-hover">
                      <div className="text-2xl font-bold text-purple-600">71KB</div>
                      <div className="text-sm text-gray-600 dark:text-gray-300">Minified Size</div>
                    </div>
                    <div className="text-center p-4 bg-orange-50 dark:bg-orange-900/20 rounded-lg card-hover">
                      <div className="text-2xl font-bold text-orange-600">100%</div>
                      <div className="text-sm text-gray-600 dark:text-gray-300">Functions Work</div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>
          </TabsContent>

          <TabsContent value="comparison" className="mt-8 tab-content">
            <div className="chart-container">
              <InteractiveCharts />
            </div>
          </TabsContent>

          <TabsContent value="performance" className="mt-8 tab-content">
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
              <Card className="card-hover border-0 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm">
                <CardHeader>
                  <CardTitle className="flex items-center">
                    <Gauge className="h-5 w-5 mr-2" />
                    Runtime Performance
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    {[
                      { method: "esbuild", performance: "13.91 ops/ms", color: "green" },
                      { method: "tdewolff", performance: "12.06 ops/ms", color: "blue" },
                      { method: "terser", performance: "8.55 ops/ms", color: "purple" }
                    ].map((item, index) => (
                      <div key={index} className={`flex justify-between items-center p-3 bg-${item.color}-50 dark:bg-${item.color}-900/20 rounded-lg card-hover`} style={{animationDelay: `${index * 0.1}s`}}>
                        <span className="font-medium">{item.method}</span>
                        <span className={`text-${item.color}-600 font-bold`}>{item.performance}</span>
                      </div>
                    ))}
                  </div>
                  <p className="text-sm text-gray-600 dark:text-gray-300 mt-4">
                    Performance measured with 10,000 operations including uniq, chunk, and merge functions.
                  </p>
                </CardContent>
              </Card>

              <Card className="card-hover border-0 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm">
                <CardHeader>
                  <CardTitle className="flex items-center">
                    <Cpu className="h-5 w-5 mr-2" />
                    Memory Usage
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    <div className="text-center p-6 bg-gray-50 dark:bg-gray-800 rounded-lg card-hover">
                      <div className="text-3xl font-bold text-gray-900 dark:text-white">~71KB</div>
                      <div className="text-sm text-gray-600 dark:text-gray-300">Embedded in binary</div>
                    </div>
                    <div className="text-center p-6 bg-gray-50 dark:bg-gray-800 rounded-lg card-hover">
                      <div className="text-3xl font-bold text-gray-900 dark:text-white">2-4MB</div>
                      <div className="text-sm text-gray-600 dark:text-gray-300">Runtime memory</div>
                    </div>
                  </div>
                  <p className="text-sm text-gray-600 dark:text-gray-300 mt-4">
                    Minimal memory footprint with no external dependencies at runtime.
                  </p>
                </CardContent>
              </Card>
            </div>
          </TabsContent>

          <TabsContent value="implementation" className="mt-8 tab-content">
            <CodeExamples />
          </TabsContent>
        </Tabs>
      </div>

      {/* Footer */}
      <footer className="bg-gray-900 text-white py-12 relative overflow-hidden">
        <div className="absolute inset-0 bg-gradient-to-r from-blue-900/20 to-purple-900/20" />
        <div className="relative max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="text-center">
            <h3 className="text-2xl font-bold mb-4">Ready to Get Started?</h3>
            <p className="text-gray-300 mb-6">
              Download the complete implementation and start using lodash in your Go applications today.
            </p>
            <Button size="lg" className="bg-blue-600 hover:bg-blue-700 btn-enhanced">
              <Download className="h-4 w-4 mr-2" />
              Download Now
            </Button>
          </div>
        </div>
      </footer>
    </div>
  )
}

export default App

