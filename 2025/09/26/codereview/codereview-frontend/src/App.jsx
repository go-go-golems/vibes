import { useState, useEffect } from 'react'
import { Button } from '@/components/ui/button.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { ScrollArea } from '@/components/ui/scroll-area.jsx'
import { Separator } from '@/components/ui/separator.jsx'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select.jsx'
import { 
  GitBranch, 
  FileText, 
  MessageSquare, 
  AlertTriangle, 
  Lightbulb, 
  ThumbsUp, 
  HelpCircle,
  Clock,
  User,
  Hash,
  Plus,
  Eye,
  Download,
  FolderOpen,
  Code
} from 'lucide-react'
import ReviewList from './components/ReviewList'
import ReviewDetail from './components/ReviewDetail'
import DiffViewer from './components/DiffViewer'
import AnnotationPanel from './components/AnnotationPanel'
import './App.css'

// Enhanced mock data with multi-file PR
const mockReviews = [
  {
    id: 'rev-1758903979',
    title: 'User Management System Implementation',
    branch: 'feature/user-management',
    commit: '97a6429a',
    baseCommit: 'master',
    reviewer: 'senior.dev@example.com',
    status: 'pending',
    filesChanged: 7,
    created: '2025-09-26T12:26:19Z',
    files: [
      'app.js',
      'package.json',
      'src/services/UserService.js',
      'src/components/UserComponent.jsx',
      'src/utils/validation.js',
      'src/config/api.js'
    ],
    annotations: [
      {
        id: 3,
        file: 'src/services/UserService.js',
        line: 7,
        type: 'issue',
        severity: 'major',
        message: 'Missing input validation - API calls should validate parameters before making requests',
        status: 'open',
        created: '2025-09-26T12:26:26Z'
      },
      {
        id: 4,
        file: 'src/services/UserService.js',
        line: 15,
        type: 'suggestion',
        severity: 'minor',
        message: 'Consider adding request timeout handling',
        suggestion: 'Add timeout configuration to fetch requests',
        status: 'open',
        created: '2025-09-26T12:26:31Z'
      },
      {
        id: 5,
        file: 'src/components/UserComponent.jsx',
        line: 45,
        type: 'issue',
        severity: 'critical',
        message: 'Security vulnerability: Missing confirmation dialog validation',
        suggestion: 'Add proper confirmation with user re-authentication for delete operations',
        status: 'open',
        created: '2025-09-26T12:26:36Z'
      },
      {
        id: 6,
        file: 'src/utils/validation.js',
        line: 5,
        type: 'praise',
        severity: 'minor',
        message: 'Excellent regex pattern for email validation - comprehensive and follows RFC standards',
        status: 'open',
        created: '2025-09-26T12:26:42Z'
      },
      {
        id: 7,
        file: 'src/config/api.js',
        line: 25,
        type: 'question',
        severity: 'minor',
        message: 'Should we consider using a more secure token storage method instead of localStorage?',
        status: 'open',
        created: '2025-09-26T12:26:47Z'
      },
      {
        id: 8,
        file: 'package.json',
        line: null,
        type: 'suggestion',
        severity: 'minor',
        message: 'Consider adding security audit scripts and dependency vulnerability checking',
        suggestion: 'Add scripts: "audit": "npm audit", "audit-fix": "npm audit fix"',
        status: 'open',
        created: '2025-09-26T12:26:54Z'
      }
    ]
  },
  {
    id: 'rev-1758903504',
    title: 'Review user authentication feature',
    branch: 'feature/user-auth',
    commit: '47f767fc',
    baseCommit: 'master',
    reviewer: 'reviewer@example.com',
    status: 'pending',
    filesChanged: 1,
    created: '2025-09-26T12:18:24Z',
    files: ['app.js'],
    annotations: [
      {
        id: 1,
        file: 'app.js',
        line: 2,
        type: 'issue',
        severity: 'major',
        message: 'Email validation is too simple, should use proper regex',
        status: 'open',
        created: '2025-09-26T12:18:54Z'
      },
      {
        id: 2,
        file: 'app.js',
        line: 3,
        type: 'suggestion',
        severity: 'minor',
        message: 'Consider adding more comprehensive validation',
        suggestion: '// TODO: Add proper email regex validation and length checks',
        status: 'open',
        created: '2025-09-26T12:18:59Z'
      }
    ]
  }
]

// Mock diff data for different files
const mockDiffs = {
  'src/services/UserService.js': {
    file: "src/services/UserService.js",
    changes: [
      { type: 'added', oldLine: null, newLine: 1, content: "export class UserService {" },
      { type: 'added', oldLine: null, newLine: 2, content: "  constructor(apiUrl) {" },
      { type: 'added', oldLine: null, newLine: 3, content: "    this.apiUrl = apiUrl;" },
      { type: 'added', oldLine: null, newLine: 4, content: "  }" },
      { type: 'added', oldLine: null, newLine: 5, content: "" },
      { type: 'added', oldLine: null, newLine: 6, content: "  async getUser(id) {" },
      { type: 'added', oldLine: null, newLine: 7, content: "    const response = await fetch(`${this.apiUrl}/users/${id}`);" },
      { type: 'added', oldLine: null, newLine: 8, content: "    if (!response.ok) {" },
      { type: 'added', oldLine: null, newLine: 9, content: "      throw new Error('User not found');" },
      { type: 'added', oldLine: null, newLine: 10, content: "    }" },
      { type: 'added', oldLine: null, newLine: 11, content: "    return response.json();" },
      { type: 'added', oldLine: null, newLine: 12, content: "  }" },
      { type: 'added', oldLine: null, newLine: 13, content: "" },
      { type: 'added', oldLine: null, newLine: 14, content: "  async createUser(userData) {" },
      { type: 'added', oldLine: null, newLine: 15, content: "    const response = await fetch(`${this.apiUrl}/users`, {" },
      { type: 'added', oldLine: null, newLine: 16, content: "      method: 'POST'," },
      { type: 'added', oldLine: null, newLine: 17, content: "      headers: { 'Content-Type': 'application/json' }," },
      { type: 'added', oldLine: null, newLine: 18, content: "      body: JSON.stringify(userData)" },
      { type: 'added', oldLine: null, newLine: 19, content: "    });" },
      { type: 'added', oldLine: null, newLine: 20, content: "    return response.json();" },
      { type: 'added', oldLine: null, newLine: 21, content: "  }" },
      { type: 'added', oldLine: null, newLine: 22, content: "}" }
    ]
  },
  'src/components/UserComponent.jsx': {
    file: "src/components/UserComponent.jsx",
    changes: [
      { type: 'added', oldLine: null, newLine: 1, content: "import React, { useState, useEffect } from 'react';" },
      { type: 'added', oldLine: null, newLine: 2, content: "import { UserService } from '../services/UserService';" },
      { type: 'added', oldLine: null, newLine: 3, content: "" },
      { type: 'added', oldLine: null, newLine: 4, content: "const UserComponent = ({ userId, apiUrl }) => {" },
      { type: 'added', oldLine: null, newLine: 5, content: "  const [user, setUser] = useState(null);" },
      { type: 'context', oldLine: null, newLine: 40, content: "  const handleDelete = async () => {" },
      { type: 'context', oldLine: null, newLine: 41, content: "    if (window.confirm('Are you sure you want to delete this user?')) {" },
      { type: 'context', oldLine: null, newLine: 42, content: "      try {" },
      { type: 'context', oldLine: null, newLine: 43, content: "        await userService.deleteUser(userId);" },
      { type: 'context', oldLine: null, newLine: 44, content: "        // Handle successful deletion" },
      { type: 'added', oldLine: null, newLine: 45, content: "      } catch (err) {" },
      { type: 'added', oldLine: null, newLine: 46, content: "        setError(err.message);" },
      { type: 'added', oldLine: null, newLine: 47, content: "      }" },
      { type: 'added', oldLine: null, newLine: 48, content: "    }" },
      { type: 'added', oldLine: null, newLine: 49, content: "  };" }
    ]
  },
  'src/utils/validation.js': {
    file: "src/utils/validation.js",
    changes: [
      { type: 'added', oldLine: null, newLine: 1, content: "export const validateEmail = (email) => {" },
      { type: 'added', oldLine: null, newLine: 2, content: "  const emailRegex = /^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$/;" },
      { type: 'added', oldLine: null, newLine: 3, content: "  return emailRegex.test(email);" },
      { type: 'added', oldLine: null, newLine: 4, content: "};" },
      { type: 'added', oldLine: null, newLine: 5, content: "" },
      { type: 'added', oldLine: null, newLine: 6, content: "export const validatePassword = (password) => {" },
      { type: 'added', oldLine: null, newLine: 7, content: "  // Password must be at least 8 characters with uppercase, lowercase, and number" },
      { type: 'added', oldLine: null, newLine: 8, content: "  const passwordRegex = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[a-zA-Z\\d@$!%*?&]{8,}$/;" },
      { type: 'added', oldLine: null, newLine: 9, content: "  return passwordRegex.test(password);" },
      { type: 'added', oldLine: null, newLine: 10, content: "};" }
    ]
  },
  'package.json': {
    file: "package.json",
    changes: [
      { type: 'added', oldLine: null, newLine: 1, content: "{" },
      { type: 'added', oldLine: null, newLine: 2, content: '  "name": "user-management-app",' },
      { type: 'added', oldLine: null, newLine: 3, content: '  "version": "1.0.0",' },
      { type: 'added', oldLine: null, newLine: 4, content: '  "description": "A user management application with React components",' },
      { type: 'added', oldLine: null, newLine: 5, content: '  "main": "app.js",' },
      { type: 'added', oldLine: null, newLine: 6, content: '  "type": "module",' },
      { type: 'added', oldLine: null, newLine: 7, content: '  "scripts": {' },
      { type: 'added', oldLine: null, newLine: 8, content: '    "start": "node app.js",' },
      { type: 'added', oldLine: null, newLine: 9, content: '    "test": "jest",' },
      { type: 'added', oldLine: null, newLine: 10, content: '    "lint": "eslint src/",' },
      { type: 'added', oldLine: null, newLine: 11, content: '    "build": "webpack --mode production"' },
      { type: 'added', oldLine: null, newLine: 12, content: '  },' }
    ]
  }
}

function App() {
  const [selectedReview, setSelectedReview] = useState(null)
  const [selectedFile, setSelectedFile] = useState(null)
  const [reviews, setReviews] = useState(mockReviews)
  const [activeTab, setActiveTab] = useState('reviews')

  useEffect(() => {
    // Auto-select the multi-file review for demonstration
    const multiFileReview = mockReviews.find(r => r.id === 'rev-1758903979')
    if (multiFileReview) {
      setSelectedReview(multiFileReview)
      setSelectedFile(multiFileReview.files[0])
      setActiveTab('review')
    }
  }, [])

  const handleSelectReview = (review) => {
    setSelectedReview(review)
    setSelectedFile(review.files?.[0] || null)
    setActiveTab('review')
  }

  const handleFileSelect = (file) => {
    setSelectedFile(file)
  }

  const getStatusColor = (status) => {
    switch (status) {
      case 'pending': return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-300'
      case 'approved': return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-300'
      case 'changes_requested': return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-300'
      case 'draft': return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
      default: return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
    }
  }

  const getTypeIcon = (type) => {
    switch (type) {
      case 'issue': return <AlertTriangle className="h-4 w-4 text-red-500" />
      case 'suggestion': return <Lightbulb className="h-4 w-4 text-blue-500" />
      case 'praise': return <ThumbsUp className="h-4 w-4 text-green-500" />
      case 'question': return <HelpCircle className="h-4 w-4 text-purple-500" />
      default: return <MessageSquare className="h-4 w-4" />
    }
  }

  const getSeverityColor = (severity) => {
    switch (severity) {
      case 'critical': return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-300'
      case 'major': return 'bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-300'
      case 'minor': return 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-300'
      default: return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
    }
  }

  const getFileAnnotations = (file) => {
    if (!selectedReview) return []
    return selectedReview.annotations.filter(a => a.file === file)
  }

  const getFileIcon = (filename) => {
    const ext = filename.split('.').pop()?.toLowerCase()
    switch (ext) {
      case 'js':
      case 'jsx':
      case 'ts':
      case 'tsx':
        return <Code className="h-4 w-4 text-yellow-600" />
      case 'json':
        return <FileText className="h-4 w-4 text-blue-600" />
      default:
        return <FileText className="h-4 w-4 text-gray-600" />
    }
  }

  return (
    <div className="min-h-screen bg-background">
      <header className="border-b bg-card">
        <div className="container mx-auto px-4 py-4">
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-4">
              <div className="flex items-center space-x-2">
                <GitBranch className="h-6 w-6 text-primary" />
                <h1 className="text-2xl font-bold">Code Review</h1>
              </div>
              <Badge variant="secondary">Local Git Tool</Badge>
            </div>
            <div className="flex items-center space-x-2">
              <Button variant="outline" size="sm">
                <Plus className="h-4 w-4 mr-2" />
                New Review
              </Button>
            </div>
          </div>
        </div>
      </header>

      <main className="container mx-auto px-4 py-6">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-6">
          <TabsList className="grid w-full grid-cols-2">
            <TabsTrigger value="reviews">All Reviews</TabsTrigger>
            <TabsTrigger value="review" disabled={!selectedReview}>
              {selectedReview ? `Review: ${selectedReview.id}` : 'Select Review'}
            </TabsTrigger>
          </TabsList>

          <TabsContent value="reviews" className="space-y-6">
            <div className="grid gap-6">
              <div className="flex items-center justify-between">
                <h2 className="text-xl font-semibold">Code Reviews</h2>
                <div className="flex items-center space-x-2">
                  <Badge variant="outline">{reviews.length} reviews</Badge>
                </div>
              </div>

              <div className="grid gap-4">
                {reviews.map((review) => (
                  <Card key={review.id} className="cursor-pointer hover:shadow-md transition-shadow" onClick={() => handleSelectReview(review)}>
                    <CardHeader className="pb-3">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center space-x-2">
                          <CardTitle className="text-lg">{review.title}</CardTitle>
                          <Badge className={getStatusColor(review.status)}>
                            {review.status}
                          </Badge>
                        </div>
                        <Button variant="ghost" size="sm">
                          <Eye className="h-4 w-4" />
                        </Button>
                      </div>
                      <CardDescription className="flex items-center space-x-4 text-sm">
                        <span className="flex items-center space-x-1">
                          <GitBranch className="h-3 w-3" />
                          <span>{review.branch}</span>
                        </span>
                        <span className="flex items-center space-x-1">
                          <Hash className="h-3 w-3" />
                          <span>{review.commit}</span>
                        </span>
                        <span className="flex items-center space-x-1">
                          <User className="h-3 w-3" />
                          <span>{review.reviewer}</span>
                        </span>
                        <span className="flex items-center space-x-1">
                          <Clock className="h-3 w-3" />
                          <span>{new Date(review.created).toLocaleDateString()}</span>
                        </span>
                      </CardDescription>
                    </CardHeader>
                    <CardContent className="pt-0">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center space-x-4 text-sm text-muted-foreground">
                          <span className="flex items-center space-x-1">
                            <FileText className="h-3 w-3" />
                            <span>{review.filesChanged} files</span>
                          </span>
                          <span className="flex items-center space-x-1">
                            <MessageSquare className="h-3 w-3" />
                            <span>{review.annotations.length} annotations</span>
                          </span>
                        </div>
                        <div className="flex items-center space-x-2">
                          {review.annotations.filter(a => a.type === 'issue').length > 0 && (
                            <Badge variant="outline" className="text-red-600">
                              {review.annotations.filter(a => a.type === 'issue').length} issues
                            </Badge>
                          )}
                          {review.annotations.filter(a => a.type === 'suggestion').length > 0 && (
                            <Badge variant="outline" className="text-blue-600">
                              {review.annotations.filter(a => a.type === 'suggestion').length} suggestions
                            </Badge>
                          )}
                          {review.annotations.filter(a => a.severity === 'critical').length > 0 && (
                            <Badge variant="outline" className="text-red-800 bg-red-50">
                              {review.annotations.filter(a => a.severity === 'critical').length} critical
                            </Badge>
                          )}
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </div>
          </TabsContent>

          <TabsContent value="review" className="space-y-6">
            {selectedReview && (
              <div className="grid gap-6">
                <Card>
                  <CardHeader>
                    <div className="flex items-center justify-between">
                      <div>
                        <CardTitle className="flex items-center space-x-2">
                          <span>{selectedReview.title}</span>
                          <Badge className={getStatusColor(selectedReview.status)}>
                            {selectedReview.status}
                          </Badge>
                        </CardTitle>
                        <CardDescription className="mt-2 flex items-center space-x-4">
                          <span className="flex items-center space-x-1">
                            <GitBranch className="h-3 w-3" />
                            <span>{selectedReview.branch}</span>
                          </span>
                          <span className="flex items-center space-x-1">
                            <Hash className="h-3 w-3" />
                            <span>{selectedReview.commit}</span>
                          </span>
                          <span className="flex items-center space-x-1">
                            <User className="h-3 w-3" />
                            <span>{selectedReview.reviewer}</span>
                          </span>
                        </CardDescription>
                      </div>
                      <div className="flex items-center space-x-2">
                        <Button variant="outline" size="sm">
                          <Download className="h-4 w-4 mr-2" />
                          Export
                        </Button>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <div className="grid grid-cols-4 gap-4 text-sm">
                      <div>
                        <div className="font-medium text-muted-foreground">Files Changed</div>
                        <div className="text-2xl font-bold">{selectedReview.filesChanged}</div>
                      </div>
                      <div>
                        <div className="font-medium text-muted-foreground">Annotations</div>
                        <div className="text-2xl font-bold">{selectedReview.annotations.length}</div>
                      </div>
                      <div>
                        <div className="font-medium text-muted-foreground">Issues</div>
                        <div className="text-2xl font-bold text-red-600">
                          {selectedReview.annotations.filter(a => a.type === 'issue').length}
                        </div>
                      </div>
                      <div>
                        <div className="font-medium text-muted-foreground">Critical</div>
                        <div className="text-2xl font-bold text-red-800">
                          {selectedReview.annotations.filter(a => a.severity === 'critical').length}
                        </div>
                      </div>
                    </div>
                  </CardContent>
                </Card>

                {/* File Navigation */}
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center space-x-2">
                      <FolderOpen className="h-5 w-5" />
                      <span>Changed Files</span>
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-2">
                      {selectedReview.files?.map((file) => {
                        const fileAnnotations = getFileAnnotations(file)
                        const isSelected = file === selectedFile
                        return (
                          <Button
                            key={file}
                            variant={isSelected ? "default" : "outline"}
                            size="sm"
                            className="justify-start h-auto p-3"
                            onClick={() => handleFileSelect(file)}
                          >
                            <div className="flex items-center space-x-2 w-full">
                              {getFileIcon(file)}
                              <div className="flex-1 text-left">
                                <div className="font-medium text-sm truncate">{file}</div>
                                {fileAnnotations.length > 0 && (
                                  <div className="flex items-center space-x-1 mt-1">
                                    <Badge variant="secondary" className="text-xs">
                                      {fileAnnotations.length} annotations
                                    </Badge>
                                    {fileAnnotations.some(a => a.severity === 'critical') && (
                                      <Badge variant="destructive" className="text-xs">
                                        critical
                                      </Badge>
                                    )}
                                  </div>
                                )}
                              </div>
                            </div>
                          </Button>
                        )
                      })}
                    </div>
                  </CardContent>
                </Card>

                <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                  <div className="lg:col-span-2">
                    {selectedFile && mockDiffs[selectedFile] ? (
                      <Card>
                        <CardHeader>
                          <CardTitle className="flex items-center space-x-2">
                            <FileText className="h-5 w-5" />
                            <span>Diff View: {selectedFile}</span>
                            <Badge variant="outline">
                              {getFileAnnotations(selectedFile).length} annotations
                            </Badge>
                          </CardTitle>
                        </CardHeader>
                        <CardContent className="p-0">
                          <div className="border rounded-lg overflow-hidden">
                            {mockDiffs[selectedFile].changes.map((line, index) => {
                              const lineClass = line.type === 'added' 
                                ? 'bg-green-50 border-l-4 border-green-400 dark:bg-green-950' 
                                : line.type === 'removed' 
                                ? 'bg-red-50 border-l-4 border-red-400 dark:bg-red-950' 
                                : 'bg-background border-l-4 border-gray-200'
                              
                              const annotation = selectedReview.annotations.find(a => 
                                a.file === selectedFile && a.line === (line.newLine || line.oldLine)
                              )

                              return (
                                <div key={index}>
                                  <div className={`flex font-mono text-sm ${lineClass} hover:bg-opacity-80 cursor-pointer`}>
                                    <div className="flex-shrink-0 w-16 text-right px-2 py-1 text-muted-foreground bg-muted border-r select-none">
                                      {line.oldLine || '-'}
                                    </div>
                                    <div className="flex-shrink-0 w-16 text-right px-2 py-1 text-muted-foreground bg-muted border-r select-none">
                                      {line.newLine || '-'}
                                    </div>
                                    <div className="flex-1 px-4 py-1">
                                      <span className="mr-2">
                                        {line.type === 'added' && '+'}
                                        {line.type === 'removed' && '-'}
                                        {line.type === 'context' && ' '}
                                      </span>
                                      {line.content}
                                    </div>
                                  </div>
                                  
                                  {annotation && (
                                    <div className="bg-yellow-50 border-l-4 border-yellow-400 p-3 ml-32 dark:bg-yellow-950">
                                      <div className="flex items-start space-x-2">
                                        {getTypeIcon(annotation.type)}
                                        <div className="flex-1">
                                          <div className="flex items-center space-x-2 mb-1">
                                            <Badge className={getSeverityColor(annotation.severity)}>
                                              {annotation.severity}
                                            </Badge>
                                            <span className="text-xs text-muted-foreground">{annotation.type}</span>
                                          </div>
                                          <p className="text-sm text-foreground mb-2">{annotation.message}</p>
                                          {annotation.suggestion && (
                                            <div className="bg-card p-2 rounded border text-sm">
                                              <div className="text-xs text-muted-foreground mb-1">Suggestion:</div>
                                              <code className="text-green-700 dark:text-green-300">{annotation.suggestion}</code>
                                            </div>
                                          )}
                                        </div>
                                      </div>
                                    </div>
                                  )}
                                </div>
                              )
                            })}
                          </div>
                        </CardContent>
                      </Card>
                    ) : (
                      <Card>
                        <CardContent className="text-center py-8">
                          <p className="text-muted-foreground">Select a file to view its diff</p>
                        </CardContent>
                      </Card>
                    )}
                  </div>

                  <div className="space-y-6">
                    <Card>
                      <CardHeader>
                        <CardTitle className="flex items-center space-x-2">
                          <MessageSquare className="h-5 w-5" />
                          <span>All Annotations</span>
                          <Badge variant="outline">{selectedReview.annotations.length}</Badge>
                        </CardTitle>
                      </CardHeader>
                      <CardContent>
                        <ScrollArea className="h-96">
                          <div className="space-y-4">
                            {selectedReview.annotations.map((annotation) => (
                              <div key={annotation.id} className="border rounded-lg p-3 cursor-pointer hover:bg-muted/50" onClick={() => handleFileSelect(annotation.file)}>
                                <div className="flex items-start space-x-2">
                                  {getTypeIcon(annotation.type)}
                                  <div className="flex-1">
                                    <div className="flex items-center space-x-2 mb-1">
                                      <Badge className={getSeverityColor(annotation.severity)}>
                                        {annotation.severity}
                                      </Badge>
                                      <span className="text-xs text-muted-foreground">
                                        {annotation.file}
                                        {annotation.line ? `:L${annotation.line}` : ' (file-level)'}
                                      </span>
                                    </div>
                                    <p className="text-sm mb-2">{annotation.message}</p>
                                    {annotation.suggestion && (
                                      <div className="bg-muted p-2 rounded text-xs">
                                        <div className="font-medium mb-1">Suggestion:</div>
                                        <code>{annotation.suggestion}</code>
                                      </div>
                                    )}
                                    <div className="text-xs text-muted-foreground mt-2">
                                      {new Date(annotation.created).toLocaleString()}
                                    </div>
                                  </div>
                                </div>
                              </div>
                            ))}
                          </div>
                        </ScrollArea>
                      </CardContent>
                    </Card>

                    <Card>
                      <CardHeader>
                        <CardTitle>File Annotations</CardTitle>
                        <CardDescription>
                          {selectedFile ? `Annotations for ${selectedFile}` : 'Select a file to see its annotations'}
                        </CardDescription>
                      </CardHeader>
                      <CardContent>
                        {selectedFile ? (
                          <div className="space-y-3">
                            {getFileAnnotations(selectedFile).map((annotation) => (
                              <div key={annotation.id} className="border rounded-lg p-2">
                                <div className="flex items-center space-x-2 mb-1">
                                  {getTypeIcon(annotation.type)}
                                  <Badge className={getSeverityColor(annotation.severity)}>
                                    {annotation.severity}
                                  </Badge>
                                  <span className="text-xs text-muted-foreground">
                                    {annotation.line ? `L${annotation.line}` : 'file-level'}
                                  </span>
                                </div>
                                <p className="text-sm">{annotation.message}</p>
                              </div>
                            ))}
                            {getFileAnnotations(selectedFile).length === 0 && (
                              <p className="text-sm text-muted-foreground text-center py-4">
                                No annotations for this file
                              </p>
                            )}
                          </div>
                        ) : (
                          <p className="text-sm text-muted-foreground text-center py-4">
                            Select a file to view its annotations
                          </p>
                        )}
                      </CardContent>
                    </Card>
                  </div>
                </div>
              </div>
            )}
          </TabsContent>
        </Tabs>
      </main>
    </div>
  )
}

export default App
