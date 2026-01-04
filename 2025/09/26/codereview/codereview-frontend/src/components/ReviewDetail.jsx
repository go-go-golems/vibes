import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Button } from '@/components/ui/button.jsx'
import { 
  GitBranch, 
  Hash,
  User,
  Download
} from 'lucide-react'

const ReviewDetail = ({ review }) => {
  const getStatusColor = (status) => {
    switch (status) {
      case 'pending': return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-300'
      case 'approved': return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-300'
      case 'changes_requested': return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-300'
      case 'draft': return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
      default: return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
    }
  }

  if (!review) {
    return (
      <Card>
        <CardContent className="text-center py-8">
          <p className="text-muted-foreground">Select a review to view details</p>
        </CardContent>
      </Card>
    )
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div>
            <CardTitle className="flex items-center space-x-2">
              <span>{review.title}</span>
              <Badge className={getStatusColor(review.status)}>
                {review.status}
              </Badge>
            </CardTitle>
            <CardDescription className="mt-2 flex items-center space-x-4">
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
            <div className="text-2xl font-bold">{review.filesChanged}</div>
          </div>
          <div>
            <div className="font-medium text-muted-foreground">Annotations</div>
            <div className="text-2xl font-bold">{review.annotations?.length || 0}</div>
          </div>
          <div>
            <div className="font-medium text-muted-foreground">Issues</div>
            <div className="text-2xl font-bold text-red-600">
              {review.annotations?.filter(a => a.type === 'issue').length || 0}
            </div>
          </div>
          <div>
            <div className="font-medium text-muted-foreground">Suggestions</div>
            <div className="text-2xl font-bold text-blue-600">
              {review.annotations?.filter(a => a.type === 'suggestion').length || 0}
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  )
}

export default ReviewDetail
