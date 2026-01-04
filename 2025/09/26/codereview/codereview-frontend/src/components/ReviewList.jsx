import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Button } from '@/components/ui/button.jsx'
import { 
  GitBranch, 
  FileText, 
  MessageSquare, 
  Clock,
  User,
  Hash,
  Eye
} from 'lucide-react'

const ReviewList = ({ reviews, onSelectReview }) => {
  const getStatusColor = (status) => {
    switch (status) {
      case 'pending': return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-300'
      case 'approved': return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-300'
      case 'changes_requested': return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-300'
      case 'draft': return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
      default: return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-300'
    }
  }

  return (
    <div className="grid gap-4">
      {reviews.map((review) => (
        <Card key={review.id} className="cursor-pointer hover:shadow-md transition-shadow" onClick={() => onSelectReview(review)}>
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
                  <span>{review.annotations?.length || 0} annotations</span>
                </span>
              </div>
              <div className="flex items-center space-x-2">
                {review.annotations?.filter(a => a.type === 'issue').length > 0 && (
                  <Badge variant="outline" className="text-red-600">
                    {review.annotations.filter(a => a.type === 'issue').length} issues
                  </Badge>
                )}
                {review.annotations?.filter(a => a.type === 'suggestion').length > 0 && (
                  <Badge variant="outline" className="text-blue-600">
                    {review.annotations.filter(a => a.type === 'suggestion').length} suggestions
                  </Badge>
                )}
              </div>
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  )
}

export default ReviewList
