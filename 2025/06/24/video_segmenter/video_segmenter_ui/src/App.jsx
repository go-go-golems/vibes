import { useState, useCallback } from 'react'
import { Upload, Video, Brain, Sparkles, Download, Play, Eye, TrendingUp } from 'lucide-react'
import { Button } from '@/components/ui/button.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Input } from '@/components/ui/input.jsx'
import { Label } from '@/components/ui/label.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Progress } from '@/components/ui/progress.jsx'
import { Alert, AlertDescription } from '@/components/ui/alert.jsx'
import './App.css'

const API_BASE_URL = 'http://localhost:5000/api/video'

function App() {
  const [selectedFile, setSelectedFile] = useState(null)
  const [apiKey, setApiKey] = useState('AIzaSyC4ShT-r48DUVDC95pBlKX3m4aIoJVruI4')
  const [uploadProgress, setUploadProgress] = useState(0)
  const [analysisProgress, setAnalysisProgress] = useState(0)
  const [currentStep, setCurrentStep] = useState('upload') // upload, processing, results
  const [videoInfo, setVideoInfo] = useState(null)
  const [analysisResults, setAnalysisResults] = useState(null)
  const [error, setError] = useState(null)
  const [isProcessing, setIsProcessing] = useState(false)
  const [fileId, setFileId] = useState(null)

  const handleFileSelect = useCallback((event) => {
    const file = event.target.files[0]
    if (file) {
      setSelectedFile(file)
      setError(null)
    }
  }, [])

  const uploadVideo = async () => {
    if (!selectedFile) {
      setError('Please select a video file')
      return
    }

    setIsProcessing(true)
    setCurrentStep('processing')
    setUploadProgress(0)

    try {
      const formData = new FormData()
      formData.append('video', selectedFile)

      const response = await fetch(`${API_BASE_URL}/upload`, {
        method: 'POST',
        body: formData,
      })

      if (!response.ok) {
        throw new Error('Upload failed')
      }

      const result = await response.json()
      setFileId(result.file_id)
      setVideoInfo(result.video_info)
      setUploadProgress(100)
      
      // Start analysis
      await analyzeVideo(result.file_id)
      
    } catch (err) {
      setError(`Upload failed: ${err.message}`)
      setIsProcessing(false)
    }
  }

  const analyzeVideo = async (fileId) => {
    if (!apiKey) {
      setError('Please provide a Gemini API key')
      setIsProcessing(false)
      return
    }

    setAnalysisProgress(0)

    try {
      const response = await fetch(`${API_BASE_URL}/analyze`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          file_id: fileId,
          api_key: apiKey,
          mode: 'quick' // Start with quick mode
        }),
      })

      if (!response.ok) {
        throw new Error('Analysis failed')
      }

      const result = await response.json()
      setAnalysisResults(result)
      setAnalysisProgress(100)
      setCurrentStep('results')
      setIsProcessing(false)
      
    } catch (err) {
      setError(`Analysis failed: ${err.message}`)
      setIsProcessing(false)
    }
  }

  const runFullAnalysis = async () => {
    if (!fileId || !apiKey) return

    setIsProcessing(true)
    setAnalysisProgress(0)

    try {
      const response = await fetch(`${API_BASE_URL}/analyze`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          file_id: fileId,
          api_key: apiKey,
          mode: 'full'
        }),
      })

      if (!response.ok) {
        throw new Error('Full analysis failed')
      }

      const result = await response.json()
      setAnalysisResults(result)
      setAnalysisProgress(100)
      setIsProcessing(false)
      
    } catch (err) {
      setError(`Full analysis failed: ${err.message}`)
      setIsProcessing(false)
    }
  }

  const resetApp = () => {
    setSelectedFile(null)
    setVideoInfo(null)
    setAnalysisResults(null)
    setError(null)
    setIsProcessing(false)
    setFileId(null)
    setCurrentStep('upload')
    setUploadProgress(0)
    setAnalysisProgress(0)
  }

  const renderUploadStep = () => (
    <div className="space-y-6">
      <div className="text-center">
        <Video className="mx-auto h-12 w-12 text-blue-500 mb-4" />
        <h2 className="text-2xl font-bold mb-2">Upload Your Video</h2>
        <p className="text-gray-600 mb-6">
          Upload a stream video to break it into engaging social media segments
        </p>
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Upload className="h-5 w-5" />
            Video Upload
          </CardTitle>
          <CardDescription>
            Select a video file (MP4, AVI, MOV, MKV, WebM) up to 500MB
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div>
            <Label htmlFor="video-file">Video File</Label>
            <Input
              id="video-file"
              type="file"
              accept="video/*"
              onChange={handleFileSelect}
              className="mt-1"
            />
          </div>

          <div>
            <Label htmlFor="api-key">Gemini API Key</Label>
            <Input
              id="api-key"
              type="password"
              value={apiKey}
              onChange={(e) => setApiKey(e.target.value)}
              placeholder="Enter your Gemini API key"
              className="mt-1"
            />
          </div>

          {selectedFile && (
            <div className="p-4 bg-gray-50 rounded-lg">
              <p className="font-medium">{selectedFile.name}</p>
              <p className="text-sm text-gray-600">
                Size: {(selectedFile.size / (1024 * 1024)).toFixed(2)} MB
              </p>
            </div>
          )}

          <Button 
            onClick={uploadVideo} 
            disabled={!selectedFile || !apiKey || isProcessing}
            className="w-full"
          >
            <Brain className="mr-2 h-4 w-4" />
            Upload & Analyze Video
          </Button>
        </CardContent>
      </Card>
    </div>
  )

  const renderProcessingStep = () => (
    <div className="space-y-6">
      <div className="text-center">
        <Sparkles className="mx-auto h-12 w-12 text-purple-500 mb-4 animate-pulse" />
        <h2 className="text-2xl font-bold mb-2">AI Analysis in Progress</h2>
        <p className="text-gray-600 mb-6">
          Our advanced AI is analyzing your video for optimal social media segments
        </p>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Processing Status</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div>
            <div className="flex justify-between text-sm mb-2">
              <span>Upload Progress</span>
              <span>{uploadProgress}%</span>
            </div>
            <Progress value={uploadProgress} className="mb-4" />
          </div>

          <div>
            <div className="flex justify-between text-sm mb-2">
              <span>AI Analysis Progress</span>
              <span>{analysisProgress}%</span>
            </div>
            <Progress value={analysisProgress} />
          </div>

          {videoInfo && (
            <div className="p-4 bg-blue-50 rounded-lg">
              <h4 className="font-medium mb-2">Video Information</h4>
              <div className="grid grid-cols-2 gap-2 text-sm">
                <div>Duration: {videoInfo.duration_formatted}</div>
                <div>Resolution: {videoInfo.resolution[0]}x{videoInfo.resolution[1]}</div>
                <div>FPS: {videoInfo.fps}</div>
                <div>Frames: {videoInfo.total_frames}</div>
              </div>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  )

  const renderResultsStep = () => {
    if (!analysisResults) return null

    const { analysis_summary, strategic_plan, frame_analyses, sequence_analyses } = analysisResults

    return (
      <div className="space-y-6">
        <div className="text-center">
          <TrendingUp className="mx-auto h-12 w-12 text-green-500 mb-4" />
          <h2 className="text-2xl font-bold mb-2">Analysis Complete</h2>
          <p className="text-gray-600 mb-6">
            Your video has been analyzed and optimized for social media success
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
          <Card>
            <CardContent className="p-4 text-center">
              <div className="text-2xl font-bold text-blue-600">
                {analysis_summary?.successful_frame_analyses || 0}
              </div>
              <div className="text-sm text-gray-600">Frames Analyzed</div>
            </CardContent>
          </Card>
          <Card>
            <CardContent className="p-4 text-center">
              <div className="text-2xl font-bold text-purple-600">
                {analysis_summary?.successful_sequence_analyses || 0}
              </div>
              <div className="text-sm text-gray-600">Segments Analyzed</div>
            </CardContent>
          </Card>
          <Card>
            <CardContent className="p-4 text-center">
              <div className="text-2xl font-bold text-green-600">
                {strategic_plan?.success ? '✓' : '✗'}
              </div>
              <div className="text-sm text-gray-600">Strategic Plan</div>
            </CardContent>
          </Card>
        </div>

        <Tabs defaultValue="overview" className="w-full">
          <TabsList className="grid w-full grid-cols-4">
            <TabsTrigger value="overview">Overview</TabsTrigger>
            <TabsTrigger value="frames">Best Frames</TabsTrigger>
            <TabsTrigger value="segments">Segments</TabsTrigger>
            <TabsTrigger value="strategy">Strategy</TabsTrigger>
          </TabsList>

          <TabsContent value="overview" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Analysis Overview</CardTitle>
                <CardDescription>
                  Quick summary of your video's social media potential
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <h4 className="font-medium mb-2">Video Quality</h4>
                    <div className="space-y-2">
                      <div className="flex justify-between">
                        <span>Duration</span>
                        <span>{videoInfo?.duration_formatted}</span>
                      </div>
                      <div className="flex justify-between">
                        <span>Resolution</span>
                        <span>{videoInfo?.resolution[0]}x{videoInfo?.resolution[1]}</span>
                      </div>
                    </div>
                  </div>
                  <div>
                    <h4 className="font-medium mb-2">Analysis Mode</h4>
                    <Badge variant={analysisResults.mode === 'quick' ? 'secondary' : 'default'}>
                      {analysisResults.mode === 'quick' ? 'Quick Analysis' : 'Full Analysis'}
                    </Badge>
                    {analysisResults.mode === 'quick' && (
                      <div className="mt-2">
                        <Button onClick={runFullAnalysis} disabled={isProcessing} size="sm">
                          Run Full Analysis
                        </Button>
                      </div>
                    )}
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="frames" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Best Frames for Social Media</CardTitle>
                <CardDescription>
                  Frames with highest viral potential and engagement scores
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  {frame_analyses?.slice(0, 4).map((analysis, index) => (
                    <div key={index} className="border rounded-lg p-4">
                      <div className="flex justify-between items-start mb-2">
                        <h5 className="font-medium">Frame {index + 1}</h5>
                        {analysis.success && analysis.final_scores && (
                          <Badge variant="outline">
                            {analysis.final_scores.overall_viral_potential}/10
                          </Badge>
                        )}
                      </div>
                      {analysis.success ? (
                        <div className="space-y-2 text-sm">
                          <p className="text-gray-600">
                            {analysis.step1_observation?.visual_description?.substring(0, 100)}...
                          </p>
                          {analysis.actionable_insights?.suggested_caption && (
                            <p className="font-medium">
                              "{analysis.actionable_insights.suggested_caption}"
                            </p>
                          )}
                        </div>
                      ) : (
                        <p className="text-red-500 text-sm">Analysis failed</p>
                      )}
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="segments" className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Video Segments Analysis</CardTitle>
                <CardDescription>
                  Coherent segments identified for social media clips
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  {sequence_analyses?.map((analysis, index) => (
                    <div key={index} className="border rounded-lg p-4">
                      <div className="flex justify-between items-start mb-2">
                        <h5 className="font-medium">Segment {index + 1}</h5>
                        {analysis.success && analysis.overall_assessment && (
                          <div className="flex gap-2">
                            <Badge variant="outline">
                              Viral: {analysis.overall_assessment.viral_potential}/10
                            </Badge>
                            <Badge variant="outline">
                              Quality: {analysis.overall_assessment.content_quality}/10
                            </Badge>
                          </div>
                        )}
                      </div>
                      {analysis.success ? (
                        <div className="space-y-2 text-sm">
                          <p className="text-gray-600">
                            {analysis.content_classification?.narrative_structure}
                          </p>
                          {analysis.strategic_recommendations?.best_platform && (
                            <p>
                              <span className="font-medium">Best Platform:</span>{' '}
                              {analysis.strategic_recommendations.best_platform}
                            </p>
                          )}
                          {analysis.strategic_recommendations?.posting_strategy && (
                            <p className="text-gray-600">
                              {analysis.strategic_recommendations.posting_strategy.substring(0, 150)}...
                            </p>
                          )}
                        </div>
                      ) : (
                        <p className="text-red-500 text-sm">Analysis failed</p>
                      )}
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="strategy" className="space-y-4">
            {strategic_plan?.success ? (
              <Card>
                <CardHeader>
                  <CardTitle>Strategic Recommendations</CardTitle>
                  <CardDescription>
                    AI-generated strategy for maximizing your content's impact
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  {strategic_plan.executive_summary && (
                    <div>
                      <h4 className="font-medium mb-2">Executive Summary</h4>
                      <p className="text-gray-600 mb-2">
                        {strategic_plan.executive_summary.primary_recommendation}
                      </p>
                      <Badge variant={
                        strategic_plan.executive_summary.investment_priority === 'high' ? 'default' :
                        strategic_plan.executive_summary.investment_priority === 'medium' ? 'secondary' : 'outline'
                      }>
                        {strategic_plan.executive_summary.investment_priority} Priority
                      </Badge>
                    </div>
                  )}

                  {strategic_plan.platform_strategy?.primary_platform && (
                    <div>
                      <h4 className="font-medium mb-2">Primary Platform</h4>
                      <div className="p-3 bg-blue-50 rounded-lg">
                        <p className="font-medium">
                          {strategic_plan.platform_strategy.primary_platform.platform}
                        </p>
                        <p className="text-sm text-gray-600">
                          {strategic_plan.platform_strategy.primary_platform.rationale}
                        </p>
                      </div>
                    </div>
                  )}

                  {strategic_plan.content_strategy?.priority_segments && (
                    <div>
                      <h4 className="font-medium mb-2">Priority Segments</h4>
                      <div className="space-y-2">
                        {strategic_plan.content_strategy.priority_segments.slice(0, 3).map((segment, index) => (
                          <div key={index} className="p-3 border rounded-lg">
                            <div className="flex justify-between items-center mb-1">
                              <span className="font-medium">Segment {segment.segment_index + 1}</span>
                              <Badge variant={
                                segment.priority_level === 'high' ? 'default' :
                                segment.priority_level === 'medium' ? 'secondary' : 'outline'
                              }>
                                {segment.priority_level}
                              </Badge>
                            </div>
                            <p className="text-sm text-gray-600">{segment.rationale}</p>
                          </div>
                        ))}
                      </div>
                    </div>
                  )}
                </CardContent>
              </Card>
            ) : (
              <Card>
                <CardContent className="p-6 text-center">
                  <p className="text-gray-600">Strategic plan not available</p>
                  <p className="text-sm text-gray-500 mt-2">
                    Try running a full analysis for detailed strategic recommendations
                  </p>
                </CardContent>
              </Card>
            )}
          </TabsContent>
        </Tabs>

        <div className="flex gap-4">
          <Button onClick={resetApp} variant="outline">
            <Upload className="mr-2 h-4 w-4" />
            Analyze Another Video
          </Button>
          {analysisResults.mode === 'quick' && (
            <Button onClick={runFullAnalysis} disabled={isProcessing}>
              <Brain className="mr-2 h-4 w-4" />
              Run Full Analysis
            </Button>
          )}
        </div>
      </div>
    )
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-purple-50">
      <div className="container mx-auto px-4 py-8">
        <div className="text-center mb-8">
          <h1 className="text-4xl font-bold text-gray-900 mb-2">
            AI Video Segmenter
          </h1>
          <p className="text-xl text-gray-600">
            Transform your stream videos into viral social media content
          </p>
        </div>

        {error && (
          <Alert className="mb-6 border-red-200 bg-red-50">
            <AlertDescription className="text-red-800">
              {error}
            </AlertDescription>
          </Alert>
        )}

        <div className="max-w-4xl mx-auto">
          {currentStep === 'upload' && renderUploadStep()}
          {currentStep === 'processing' && renderProcessingStep()}
          {currentStep === 'results' && renderResultsStep()}
        </div>
      </div>
    </div>
  )
}

export default App

