import React from 'react';
import { useLocation } from 'wouter';
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
import { ArrowLeft, ClipboardList, Loader2, Eye, Trophy, Target, BarChart3 } from 'lucide-react';
import { format } from 'date-fns';
import { getLoginUrl } from '@/const';

export default function MySubmissions() {
  const [, navigate] = useLocation();
  const { user, loading: authLoading, isAuthenticated } = useAuth();

  const { data: submissions, isLoading } = trpc.quiz.mySubmissions.useQuery(
    undefined,
    { enabled: isAuthenticated }
  );

  if (authLoading) {
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
              You need to sign in to view your submissions.
            </p>
            <Button asChild className="w-full">
              <a href={getLoginUrl()}>Sign In</a>
            </Button>
          </CardContent>
        </Card>
      </div>
    );
  }

  // Calculate stats
  const totalSubmissions = submissions?.length || 0;
  const scoredSubmissions = submissions?.filter(s => 
    s.submission.score !== null && s.submission.maxScore !== null
  ) || [];
  
  const averageScore = scoredSubmissions.length > 0
    ? Math.round(
        scoredSubmissions.reduce((acc, s) => 
          acc + (s.submission.score! / s.submission.maxScore!) * 100, 0
        ) / scoredSubmissions.length
      )
    : 0;

  const highestScore = scoredSubmissions.length > 0
    ? Math.round(
        Math.max(...scoredSubmissions.map(s => 
          (s.submission.score! / s.submission.maxScore!) * 100
        ))
      )
    : 0;

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-950">
      {/* Header */}
      <header className="sticky top-0 z-50 bg-white dark:bg-slate-900 border-b border-slate-200 dark:border-slate-800">
        <div className="container flex items-center justify-between h-16">
          <div className="flex items-center gap-4">
            <Button variant="ghost" size="icon" onClick={() => navigate('/admin')}>
              <ArrowLeft className="h-5 w-5" />
            </Button>
            <h1 className="text-lg font-semibold">My Submissions</h1>
          </div>
        </div>
      </header>

      <main className="container py-8">
        {/* Stats Cards */}
        <div className="grid sm:grid-cols-3 gap-4 mb-8">
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Total Quizzes Taken</CardDescription>
              <CardTitle className="text-3xl">{totalSubmissions}</CardTitle>
            </CardHeader>
            <CardContent>
              <ClipboardList className="h-8 w-8 text-blue-500 opacity-50" />
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Average Score</CardDescription>
              <CardTitle className="text-3xl">{averageScore}%</CardTitle>
            </CardHeader>
            <CardContent>
              <BarChart3 className="h-8 w-8 text-green-500 opacity-50" />
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="pb-2">
              <CardDescription>Best Score</CardDescription>
              <CardTitle className="text-3xl">{highestScore}%</CardTitle>
            </CardHeader>
            <CardContent>
              <Trophy className="h-8 w-8 text-yellow-500 opacity-50" />
            </CardContent>
          </Card>
        </div>

        {/* Submissions Table */}
        <Card>
          <CardHeader>
            <CardTitle>Submission History</CardTitle>
            <CardDescription>
              All your quiz submissions across documents
            </CardDescription>
          </CardHeader>
          <CardContent>
            {isLoading ? (
              <div className="flex items-center justify-center py-12">
                <Loader2 className="h-8 w-8 animate-spin text-blue-600" />
              </div>
            ) : submissions && submissions.length > 0 ? (
              <div className="overflow-x-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Document</TableHead>
                      <TableHead>Quiz</TableHead>
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
                            <button
                              onClick={() => item.documentSlug && navigate(`/documents/${item.documentSlug}`)}
                              className="hover:text-blue-600 text-left"
                            >
                              {item.documentTitle || 'Unknown Document'}
                            </button>
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
                              <Eye className="h-4 w-4 mr-1" />
                              Review
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
                <ClipboardList className="h-12 w-12 mx-auto text-muted-foreground mb-4" />
                <h3 className="text-lg font-medium mb-2">No submissions yet</h3>
                <p className="text-muted-foreground mb-4">
                  Complete quizzes in documents to see your history here.
                </p>
                <Button onClick={() => navigate('/')}>
                  Browse Documents
                </Button>
              </div>
            )}
          </CardContent>
        </Card>
      </main>
    </div>
  );
}
