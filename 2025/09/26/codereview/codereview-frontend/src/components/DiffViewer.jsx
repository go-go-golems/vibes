import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { 
  FileText,
  AlertTriangle, 
  Lightbulb, 
  ThumbsUp, 
  HelpCircle,
  MessageSquare
} from 'lucide-react'

const DiffViewer = ({ diff, annotations = [] }) => {
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

  if (!diff) {
    return (
      <Card>
        <CardContent className="text-center py-8">
          <p className="text-muted-foreground">No diff available</p>
        </CardContent>
      </Card>
    )
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <FileText className="h-5 w-5" />
          <span>Diff View: {diff.file}</span>
        </CardTitle>
      </CardHeader>
      <CardContent className="p-0">
        <div className="border rounded-lg overflow-hidden">
          {diff.changes?.map((line, index) => {
            const lineClass = line.type === 'added' 
              ? 'bg-green-50 border-l-4 border-green-400 dark:bg-green-950' 
              : line.type === 'removed' 
              ? 'bg-red-50 border-l-4 border-red-400 dark:bg-red-950' 
              : 'bg-background border-l-4 border-gray-200'
            
            const annotation = annotations.find(a => 
              a.file === diff.file && a.line === (line.newLine || line.oldLine)
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
  )
}

export default DiffViewer
