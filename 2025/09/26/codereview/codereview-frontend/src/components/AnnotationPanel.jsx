import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Button } from '@/components/ui/button.jsx'
import { ScrollArea } from '@/components/ui/scroll-area.jsx'
import { 
  MessageSquare,
  AlertTriangle, 
  Lightbulb, 
  ThumbsUp, 
  HelpCircle,
  Plus
} from 'lucide-react'

const AnnotationPanel = ({ annotations = [], onAddAnnotation }) => {
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

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <MessageSquare className="h-5 w-5" />
            <span>Annotations</span>
          </CardTitle>
        </CardHeader>
        <CardContent>
          <ScrollArea className="h-96">
            <div className="space-y-4">
              {annotations.map((annotation) => (
                <div key={annotation.id} className="border rounded-lg p-3">
                  <div className="flex items-start space-x-2">
                    {getTypeIcon(annotation.type)}
                    <div className="flex-1">
                      <div className="flex items-center space-x-2 mb-1">
                        <Badge className={getSeverityColor(annotation.severity)}>
                          {annotation.severity}
                        </Badge>
                        <span className="text-xs text-muted-foreground">
                          {annotation.file}:L{annotation.line}
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
              {annotations.length === 0 && (
                <div className="text-center text-muted-foreground py-8">
                  No annotations yet
                </div>
              )}
            </div>
          </ScrollArea>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Add Annotation</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <div className="text-sm text-muted-foreground">
              Click on any added (+) or removed (-) line to add an annotation
            </div>
            <Button className="w-full" disabled onClick={onAddAnnotation}>
              <Plus className="h-4 w-4 mr-2" />
              Select a line first
            </Button>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}

export default AnnotationPanel
