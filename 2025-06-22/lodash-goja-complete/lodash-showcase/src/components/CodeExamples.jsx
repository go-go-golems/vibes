import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Button } from '@/components/ui/button.jsx'
import { Copy, Play, Download } from 'lucide-react'
import { useState } from 'react'

export function CodeExamples() {
  const [copiedCode, setCopiedCode] = useState('')

  const copyToClipboard = (code, id) => {
    navigator.clipboard.writeText(code)
    setCopiedCode(id)
    setTimeout(() => setCopiedCode(''), 2000)
  }

  const examples = {
    basic: {
      title: 'Basic Usage',
      description: 'Simple lodash integration with Goja VM',
      code: `package main

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
    
    // Array operations
    result, _ := vm.RunString("_.uniq([1,1,2,3,3,4])")
    fmt.Println("Unique:", result.Export()) // [1 2 3 4]
    
    // Object operations
    result, _ = vm.RunString("_.merge({a:1}, {b:2}, {c:3})")
    fmt.Println("Merged:", result.Export()) // map[a:1 b:2 c:3]
    
    // String operations
    result, _ = vm.RunString("_.camelCase('hello world test')")
    fmt.Println("CamelCase:", result.Export()) // helloWorldTest
}`
    },
    generator: {
      title: 'Lodash Generator',
      description: 'Automated lodash download and minification',
      code: `//go:build ignore
//go:generate go run .

package main

import (
    "archive/tar"
    "compress/gzip"
    "fmt"
    "io"
    "net/http"
    "os"
    "path/filepath"
    "github.com/tdewolff/minify/v2"
    "github.com/tdewolff/minify/v2/js"
)

const (
    version = "4.17.21"
    tgzURL  = "https://registry.npmjs.org/lodash/-/lodash-" + version + ".tgz"
    outFile = "../js/lodash.min.js"
)

func main() {
    fmt.Printf("Downloading lodash %s from npm...\\n", version)
    
    // Download the official tarball
    resp, err := http.Get(tgzURL)
    if err != nil {
        panic(fmt.Errorf("failed to download lodash: %w", err))
    }
    defer resp.Body.Close()

    // Extract lodash.js from tarball
    gzr, _ := gzip.NewReader(resp.Body)
    tr := tar.NewReader(gzr)

    var src []byte
    for {
        hdr, err := tr.Next()
        if err == io.EOF {
            break
        }
        if filepath.Base(hdr.Name) == "lodash.js" {
            src, _ = io.ReadAll(tr)
            break
        }
    }

    // Minify with tdewolff/minify
    m := minify.New()
    m.AddFunc("application/javascript", js.Minify)
    minified, _ := m.Bytes("application/javascript", src)

    // Write output
    os.MkdirAll(filepath.Dir(outFile), 0o755)
    os.WriteFile(outFile, minified, 0o644)
    
    fmt.Printf("Generated %s (%d bytes, %.1f%% reduction)\\n", 
        outFile, len(minified), 
        float64(len(src)-len(minified))/float64(len(src))*100)
}`
    },
    advanced: {
      title: 'Advanced Usage',
      description: 'Data processing pipeline with lodash',
      code: `package main

import (
    _ "embed"
    "fmt"
    "github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func processData() {
    vm := goja.New()
    vm.RunString(lodashSrc)
    
    // Set up sample data
    vm.RunString(\`
        var users = [
            {name: "John", age: 30, city: "NYC", salary: 75000},
            {name: "Jane", age: 25, city: "LA", salary: 85000},
            {name: "Bob", age: 35, city: "NYC", salary: 95000},
            {name: "Alice", age: 28, city: "Chicago", salary: 70000}
        ];
    \`)
    
    // Complex data processing with lodash
    result, _ := vm.RunString(\`
        _.chain(users)
         .groupBy('city')
         .mapValues(group => ({
             count: group.length,
             avgAge: _.meanBy(group, 'age'),
             avgSalary: _.meanBy(group, 'salary'),
             names: _.map(group, 'name')
         }))
         .value()
    \`)
    
    fmt.Printf("Processed data: %v\\n", result.Export())
}

func main() {
    processData()
}`
    },
    performance: {
      title: 'Performance Optimization',
      description: 'Optimized usage patterns for production',
      code: `package main

import (
    _ "embed"
    "fmt"
    "runtime"
    "time"
    "github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

type LodashVM struct {
    vm *goja.Runtime
}

func NewLodashVM() *LodashVM {
    vm := goja.New()
    _, err := vm.RunString(lodashSrc)
    if err != nil {
        panic(err)
    }
    
    // Pre-compile frequently used functions
    vm.RunString(\`
        var processArray = function(arr) {
            return _.chain(arr)
                    .uniq()
                    .chunk(2)
                    .flatten()
                    .value();
        };
        
        var processObject = function(obj, keys) {
            return _.pick(obj, keys);
        };
    \`)
    
    return &LodashVM{vm: vm}
}

func (l *LodashVM) ProcessArray(data []interface{}) interface{} {
    l.vm.Set("data", data)
    result, _ := l.vm.RunString("processArray(data)")
    return result.Export()
}

func (l *LodashVM) ProcessObject(data map[string]interface{}, keys []string) interface{} {
    l.vm.Set("data", data)
    l.vm.Set("keys", keys)
    result, _ := l.vm.RunString("processObject(data, keys)")
    return result.Export()
}

func main() {
    // Create reusable VM instance
    lodash := NewLodashVM()
    
    // Benchmark performance
    start := time.Now()
    for i := 0; i < 10000; i++ {
        data := []interface{}{1, 1, 2, 3, 3, 4}
        lodash.ProcessArray(data)
    }
    duration := time.Since(start)
    
    fmt.Printf("10,000 operations: %v (%.2f ops/ms)\\n", 
        duration, float64(10000)/float64(duration.Milliseconds()))
    
    // Memory usage
    var m runtime.MemStats
    runtime.GC()
    runtime.ReadMemStats(&m)
    fmt.Printf("Memory usage: %d KB\\n", m.Alloc/1024)
}`
    },
    testing: {
      title: 'Testing & Validation',
      description: 'Comprehensive testing of lodash functions',
      code: `package main

import (
    _ "embed"
    "fmt"
    "testing"
    "github.com/dop251/goja"
)

//go:embed internal/js/lodash.min.js
var lodashSrc string

func setupVM() *goja.Runtime {
    vm := goja.New()
    _, err := vm.RunString(lodashSrc)
    if err != nil {
        panic(err)
    }
    return vm
}

func TestLodashFunctions(t *testing.T) {
    vm := setupVM()
    
    tests := []struct {
        name     string
        code     string
        expected interface{}
    }{
        {
            name:     "Array uniq",
            code:     "_.uniq([1,1,2,3,3,4])",
            expected: []interface{}{1, 2, 3, 4},
        },
        {
            name:     "Object merge",
            code:     "_.merge({a:1}, {b:2})",
            expected: map[string]interface{}{"a": 1, "b": 2},
        },
        {
            name:     "String camelCase",
            code:     "_.camelCase('hello world')",
            expected: "helloWorld",
        },
        {
            name:     "Math sum",
            code:     "_.sum([1,2,3,4,5])",
            expected: 15,
        },
    }
    
    for _, test := range tests {
        t.Run(test.name, func(t *testing.T) {
            result, err := vm.RunString(test.code)
            if err != nil {
                t.Fatalf("Error executing %s: %v", test.code, err)
            }
            
            actual := result.Export()
            fmt.Printf("%s: %v\\n", test.name, actual)
            
            // Add your assertions here
        })
    }
}

func BenchmarkLodashOperations(b *testing.B) {
    vm := setupVM()
    
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        vm.RunString("_.uniq([1,1,2,3,3,4])")
    }
}`
    }
  }

  return (
    <div className="space-y-8">
      <div className="text-center">
        <h2 className="text-3xl font-bold mb-4">Code Examples & Implementation</h2>
        <p className="text-gray-600 dark:text-gray-300 max-w-2xl mx-auto">
          Complete code examples showing how to integrate lodash with Goja VM, 
          from basic usage to advanced optimization techniques.
        </p>
      </div>

      <Tabs defaultValue="basic" className="w-full">
        <TabsList className="grid w-full grid-cols-5">
          <TabsTrigger value="basic">Basic</TabsTrigger>
          <TabsTrigger value="generator">Generator</TabsTrigger>
          <TabsTrigger value="advanced">Advanced</TabsTrigger>
          <TabsTrigger value="performance">Performance</TabsTrigger>
          <TabsTrigger value="testing">Testing</TabsTrigger>
        </TabsList>

        {Object.entries(examples).map(([key, example]) => (
          <TabsContent key={key} value={key} className="mt-6">
            <Card>
              <CardHeader>
                <div className="flex items-center justify-between">
                  <div>
                    <CardTitle className="flex items-center gap-2">
                      {example.title}
                      <Badge variant="secondary">Go</Badge>
                    </CardTitle>
                    <CardDescription className="mt-2">
                      {example.description}
                    </CardDescription>
                  </div>
                  <div className="flex gap-2">
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => copyToClipboard(example.code, key)}
                    >
                      <Copy className="h-4 w-4 mr-1" />
                      {copiedCode === key ? 'Copied!' : 'Copy'}
                    </Button>
                    <Button variant="outline" size="sm">
                      <Download className="h-4 w-4 mr-1" />
                      Download
                    </Button>
                  </div>
                </div>
              </CardHeader>
              <CardContent>
                <div className="relative">
                  <SyntaxHighlighter
                    language="go"
                    style={vscDarkPlus}
                    customStyle={{
                      margin: 0,
                      borderRadius: '0.5rem',
                      fontSize: '0.875rem',
                      lineHeight: '1.5'
                    }}
                    showLineNumbers={true}
                  >
                    {example.code}
                  </SyntaxHighlighter>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        ))}
      </Tabs>

      {/* Quick Start Guide */}
      <Card>
        <CardHeader>
          <CardTitle>Quick Start Guide</CardTitle>
          <CardDescription>
            Get up and running with lodash Goja VM in minutes
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-6">
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
              <div className="text-center p-6 border rounded-lg">
                <div className="w-8 h-8 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center mx-auto mb-4">
                  <span className="text-blue-600 font-bold">1</span>
                </div>
                <h3 className="font-semibold mb-2">Install Dependencies</h3>
                <p className="text-sm text-gray-600 dark:text-gray-300">
                  Install Go 1.23+ and required packages
                </p>
              </div>
              <div className="text-center p-6 border rounded-lg">
                <div className="w-8 h-8 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center mx-auto mb-4">
                  <span className="text-blue-600 font-bold">2</span>
                </div>
                <h3 className="font-semibold mb-2">Generate Lodash</h3>
                <p className="text-sm text-gray-600 dark:text-gray-300">
                  Run the generator to download and minify lodash
                </p>
              </div>
              <div className="text-center p-6 border rounded-lg">
                <div className="w-8 h-8 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center mx-auto mb-4">
                  <span className="text-blue-600 font-bold">3</span>
                </div>
                <h3 className="font-semibold mb-2">Use in Your App</h3>
                <p className="text-sm text-gray-600 dark:text-gray-300">
                  Embed and use lodash in your Goja VM
                </p>
              </div>
            </div>

            <div className="bg-gray-50 dark:bg-gray-800 p-4 rounded-lg">
              <h4 className="font-semibold mb-2">Terminal Commands:</h4>
              <SyntaxHighlighter
                language="bash"
                style={vscDarkPlus}
                customStyle={{
                  margin: 0,
                  borderRadius: '0.25rem',
                  fontSize: '0.875rem'
                }}
              >
{`# Install dependencies
go get github.com/dop251/goja@latest
go get github.com/tdewolff/minify/v2

# Generate lodash
cd internal/tools
go run gen_lodash_compare.go

# Build your application
go build -o myapp main.go`}
              </SyntaxHighlighter>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}

