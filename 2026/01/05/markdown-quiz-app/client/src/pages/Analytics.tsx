import React from 'react';
import { useParams, useLocation } from 'wouter';
import { trpc } from '@/lib/trpc';
import { useAuth } from '@/_core/hooks/useAuth';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table';
import { Progress } from '@/components/ui/progress';
import { ArrowLeft, BarChart3, Users, Trophy, Target, Loader2, Eye } from 'lucide-react';
import { format } from 'date-fns';
import { getLoginUrl } from '@/const';

export default function Analytics() {
  const params = useParams<{ id: string }>();
  const [, navigate] = useLocation();
  const { user, loading: authLoading, isAuthenticated } = useAuth();
  
  const documentId = parseInt(params.id!);

  const { data: document, isLoading: docLoading } = trpc.documents.getById.useQuery(
    { id: documentId },
    { enabled: !!documentId && isAuthenticated }
  );

  const { data: analytics, isLoading: analyticsLoading } = trpc.documents.analytics.useQuery(
    { id: documentId },
    { enabled: !!documentId && isAuthenticated }
  );

  const { data: submissions, isLoading: submissionsLoading } = trpc.documents.submissions.useQuery(
    { id: documentId },
    { enabled: !!documentId && isAuthenticated }
  );

  if (authLoading || docLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-blue-600" />
      </div>
    );
  }

  if (!isAuthenticated) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-slate-50 dark:bg-slate-950">
        <Card className="w-full max-w-md mx-4">
          <CardHeader>
            <CardTitle>Sign In Required</CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-muted-foreground mb-4">
              You need to sign in to view analytics.
            </p>
            <Button asChild className="w-full">
              <a href={getLoginUrl()}>Sign In</a>
            </Button>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (!document) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-slate-50 dark:bg-slate-950">
        <div className="text-center">
          <h1 className="text-2xl font-bold mb-2">Document Not Found</h1>
          <p className="text-muted-foreground mb-4">
            The document you're looking for doesn't exist or you don't have access to it.
          </p>
          <Button onClick={() => navigate('/admin')}>
            <ArrowLeft className="mr-2 h-4 w-4" />
            Back to Dashboard
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-950">
      {/* Header */}
      <header className="sticky top-0 z-50 bg-white dark:bg-slate-900 border-b border-slate-200 dark:border-slate-800">
        <div className="container flex items-center justify-between h-16">
          <div className="flex items-center gap-4">
            <Button variant="ghost" size="icon" onClick={() => navigate('/admin')}>
              <ArrowLeft className="h-5 w-5" />
            </Button>
            <div>
              <h1 className="text-lg font-semibold">Analytics</h1>
              <p className="text-sm text-muted-foreground">{document.title}</p>
            </div>
          </div>
          <Button variant="outline" onClick={() => navigate(`/documents/${document.slug}`)}>
            <Eye className="mr-2 h-4 w-4" />
            View Document
          </Button>
        </div>
      </header>

      <main className="container py-8">
        {/* Stats Cards */}
        <div className="grid sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Total Submissions</CardDescription>
              <CardTitle className="text-3xl">
                {analyticsLoading ? '...' : analytics?.totalSubmissions || 0}
              </CardTitle>
            </CardHeader>
            <CardContent>
              <Users className="h-8 w-8 text-blue-500 opacity-50" />
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Average Score</CardDescription>
              <CardTitle className="text-3xl">
                {analyticsLoading ? '...' : `${analytics?.averageScore || 0}%`}
              </CardTitle>
            </CardHeader>
            <CardContent>
              <BarChart3 className="h-8 w-8 text-green-500 opacity-50" />
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Highest Score</CardDescription>
              <CardTitle className="text-3xl">
                {analyticsLoading ? '...' : `${analytics?.highestScore || 0}%`}
              </CardTitle>
            </CardHeader>
            <CardContent>
              <Trophy className="h-8 w-8 text-yellow-500 opacity-50" />
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Lowest Score</CardDescription>
              <CardTitle className="text-3xl">
                {analyticsLoading ? '...' : `${analytics?.lowestScore || 0}%`}
              </CardTitle>
            </CardHeader>
            <CardContent>
              <Target className="h-8 w-8 text-red-500 opacity-50" />
            </CardContent>
          </Card>
        </div>

        {/* Submissions Table */}
        <Card>
          <CardHeader>
            <CardTitle>Submissions</CardTitle>
            <CardDescription>
              All quiz submissions for this document
            </CardDescription>
          </CardHeader>
          <CardContent>
            {submissionsLoading ? (
              <div className="flex items-center justify-center py-12">
                <Loader2 className="h-8 w-8 animate-spin text-blue-600" />
              </div>
            ) : submissions && submissions.length > 0 ? (
              <div className="overflow-x-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>User</TableHead>
                      <TableHead>Form</TableHead>
                      <TableHead>Score</TableHead>
                      <TableHead>Progress</TableHead>
                      <TableHead>Submitted</TableHead>
                      <TableHead className="text-right">Actions</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {submissions.map((item) => {
                      const percentage = item.submission.maxScore 
                        ? Math.round((item.submission.score! / item.submission.maxScore) * 100)
                        : null;
                      
                      return (
                        <TableRow key={item.submission.id}>
                          <TableCell className="font-medium">
                            {item.userName || 'Anonymous'}
                          </TableCell>
                          <TableCell>
                            <Badge variant="outline">{item.submission.formId}</Badge>
                          </TableCell>
                          <TableCell>
                            {item.submission.score !== null && item.submission.maxScore !== null ? (
                              <span className={percentage! >= 70 ? 'text-green-600' : percentage! >= 50 ? 'text-yellow-600' : 'text-red-600'}>
                                {item.submission.score}/{item.submission.maxScore}
                              </span>
                            ) : (
                              <span className="text-muted-foreground">N/A</span>
                            )}
                          </TableCell>
                          <TableCell className="w-32">
                            {percentage !== null ? (
                              <div className="flex items-center gap-2">
                                <Progress value={percentage} className="h-2" />
                                <span className="text-sm text-muted-foreground w-12">
                                  {percentage}%
                                </span>
                              </div>
                            ) : (
                              <span className="text-muted-foreground">—</span>
                            )}
                          </TableCell>
                          <TableCell className="text-muted-foreground">
                            {format(new Date(item.submission.submittedAt), 'MMM d, yyyy HH:mm')}
                          </TableCell>
                          <TableCell className="text-right">
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => navigate(`/submission/${item.submission.id}`)}
                            >
                              <Eye className="h-4 w-4" />
                            </Button>
                          </TableCell>
                        </TableRow>
                      );
                    })}
                  </TableBody>
                </Table>
              </div>
            ) : (
              <div className="text-center py-12">
                <Users className="h-12 w-12 mx-auto text-muted-foreground mb-4" />
                <h3 className="text-lg font-medium mb-2">No submissions yet</h3>
                <p className="text-muted-foreground">
                  Share your document to start collecting quiz responses.
                </p>
              </div>
            )}
          </CardContent>
        </Card>
      </main>
    </div>
  );
}
