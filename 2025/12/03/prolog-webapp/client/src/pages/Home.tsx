import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { APP_LOGO, APP_TITLE } from "@/const";
import { BookOpen, Code, Lightbulb, Zap } from "lucide-react";
import { Link } from "wouter";

export default function Home() {
  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 via-indigo-50 to-purple-50">
      {/* Hero Section */}
      <div className="container mx-auto px-4 py-16">
        <div className="text-center space-y-6 max-w-3xl mx-auto">
          <div className="flex justify-center mb-6">
            {APP_LOGO && (
              <img src={APP_LOGO} alt={APP_TITLE} className="h-20 w-20 object-contain" />
            )}
          </div>
          
          <h1 className="text-5xl font-bold text-gray-900 leading-tight">
            {APP_TITLE}
          </h1>
          
          <p className="text-xl text-gray-600 leading-relaxed">
            An interactive web implementation of the Prolog interpreter from{" "}
            <strong>Paradigms of Artificial Intelligence Programming</strong> (PAIP) Chapter 11 by Peter Norvig
          </p>

          <div className="flex justify-center gap-4 pt-4">
            <Link href="/playground">
              <Button size="lg" className="text-lg px-8">
                <Code className="mr-2 h-5 w-5" />
                Launch Playground
              </Button>
            </Link>
          </div>
        </div>
      </div>

      {/* Features Section */}
      <div className="container mx-auto px-4 py-16">
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          <Card className="border-2 hover:border-blue-300 transition-colors">
            <CardHeader>
              <div className="h-12 w-12 bg-blue-100 rounded-lg flex items-center justify-center mb-2">
                <BookOpen className="h-6 w-6 text-blue-600" />
              </div>
              <CardTitle>PAIP Chapter 11</CardTitle>
              <CardDescription>
                Based on Peter Norvig's classic AI textbook implementation
              </CardDescription>
            </CardHeader>
          </Card>

          <Card className="border-2 hover:border-indigo-300 transition-colors">
            <CardHeader>
              <div className="h-12 w-12 bg-indigo-100 rounded-lg flex items-center justify-center mb-2">
                <Lightbulb className="h-6 w-6 text-indigo-600" />
              </div>
              <CardTitle>Logic Programming</CardTitle>
              <CardDescription>
                Explore declarative programming with facts, rules, and queries
              </CardDescription>
            </CardHeader>
          </Card>

          <Card className="border-2 hover:border-purple-300 transition-colors">
            <CardHeader>
              <div className="h-12 w-12 bg-purple-100 rounded-lg flex items-center justify-center mb-2">
                <Zap className="h-6 w-6 text-purple-600" />
              </div>
              <CardTitle>Interactive</CardTitle>
              <CardDescription>
                Real-time query execution with instant feedback and results
              </CardDescription>
            </CardHeader>
          </Card>

          <Card className="border-2 hover:border-pink-300 transition-colors">
            <CardHeader>
              <div className="h-12 w-12 bg-pink-100 rounded-lg flex items-center justify-center mb-2">
                <Code className="h-6 w-6 text-pink-600" />
              </div>
              <CardTitle>Example Presets</CardTitle>
              <CardDescription>
                Pre-loaded examples covering lists, graphs, family relations, and more
              </CardDescription>
            </CardHeader>
          </Card>
        </div>
      </div>

      {/* About Section */}
      <div className="container mx-auto px-4 py-16">
        <Card className="max-w-4xl mx-auto">
          <CardHeader>
            <CardTitle className="text-2xl">About This Implementation</CardTitle>
          </CardHeader>
          <CardContent className="prose prose-gray max-w-none">
            <p className="text-gray-700 leading-relaxed">
              This web application brings Peter Norvig's Prolog interpreter from <em>Paradigms of Artificial Intelligence Programming</em> to the browser. 
              The implementation includes the complete unification algorithm, backtracking search, and support for facts, rules, and complex queries.
            </p>
            
            <p className="text-gray-700 leading-relaxed mt-4">
              The interpreter is written in Common Lisp (SBCL) and exposed through a Node.js backend API, allowing you to experiment with logic programming 
              concepts interactively. Whether you're learning Prolog, exploring AI techniques, or just curious about declarative programming, this playground 
              provides an accessible way to get started.
            </p>

            <div className="mt-6">
              <h3 className="text-lg font-semibold text-gray-900 mb-3">Key Features:</h3>
              <ul className="space-y-2 text-gray-700">
                <li className="flex items-start">
                  <span className="mr-2">•</span>
                  <span><strong>Unification:</strong> Pattern matching with logic variables</span>
                </li>
                <li className="flex items-start">
                  <span className="mr-2">•</span>
                  <span><strong>Backtracking:</strong> Automatic search for multiple solutions</span>
                </li>
                <li className="flex items-start">
                  <span className="mr-2">•</span>
                  <span><strong>Rules & Facts:</strong> Define relationships and derive new knowledge</span>
                </li>
                <li className="flex items-start">
                  <span className="mr-2">•</span>
                  <span><strong>Example Programs:</strong> Family trees, list operations, graph traversal, and more</span>
                </li>
              </ul>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Footer */}
      <footer className="border-t bg-white/80 backdrop-blur-sm mt-16">
        <div className="container mx-auto px-4 py-8 text-center text-gray-600">
          <p>
            Based on <em>Paradigms of Artificial Intelligence Programming</em> by Peter Norvig
          </p>
          <p className="mt-2 text-sm">
            <a
              href="https://github.com/norvig/paip-lisp"
              target="_blank"
              rel="noopener noreferrer"
              className="text-blue-600 hover:underline"
            >
              View Original Source on GitHub
            </a>
          </p>
        </div>
      </footer>
    </div>
  );
}
