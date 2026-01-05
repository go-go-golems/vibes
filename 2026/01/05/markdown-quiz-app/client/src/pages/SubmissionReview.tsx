import React from 'react';
import { useParams, useLocation } from 'wouter';
import { trpc } from '@/lib/trpc';
import { useAuth } from '@/_core/hooks/useAuth';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Label } from '@/components/ui/label';
import { Progress } from '@/components/ui/progress';
import { ArrowLeft, CheckCircle2, XCircle, Loader2, FileText, Calendar } from 'lucide-react';
import { format } from 'date-fns';
import { getLoginUrl } from '@/const';

interface FieldDefinition {
  name?: string;
  key?: string;
  label?: string;
  title?: string;
  type: string;
  options?: Array<{ label: string; value: string } | string>;
  correct?: any;
}

interface FormDefinition {
  name?: string;
  title?: string;
  description?: string;
  fields?: FieldDefinition[];
  form?: {
    fields?: FieldDefinition[];
    groups?: Array<{ fields: FieldDefinition[] }>;
  };
}

export default function SubmissionReview() {
  const params = useParams<{ id: string }>();
  const [, navigate] = useLocation();
  const { user, loading: authLoading, isAuthenticated } = useAuth();
  
  const submissionId = parseInt(params.id!);

  const { data, isLoading, error } = trpc.quiz.getSubmission.useQuery(
    { id: submissionId },
    { enabled: !!submissionId && isAuthenticated }
  );

  if (authLoading || isLoading) {
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
              You need to sign in to view submission details.
            </p>
            <Button asChild className="w-full">
              <a href={getLoginUrl()}>Sign In</a>
            </Button>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (error || !data) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-slate-50 dark:bg-slate-950">
        <div className="text-center">
          <h1 className="text-2xl font-bold mb-2">Submission Not Found</h1>
          <p className="text-muted-foreground mb-4">
            The submission you're looking for doesn't exist or you don't have access to it.
          </p>
          <Button onClick={() => navigate('/admin/submissions')}>
            <ArrowLeft className="mr-2 h-4 w-4" />
            Back to Submissions
          </Button>
        </div>
      </div>
    );
  }

  const { submission, documentTitle, documentSlug, formDefinition } = data;
  const responses = submission.responses as Record<string, any>;
  const definition = formDefinition as FormDefinition | undefined;

  // Extract fields from definition
  const getFields = (): FieldDefinition[] => {
    if (!definition) return [];
    if (definition.fields) return definition.fields;
    if (definition.form?.fields) return definition.form.fields;
    if (definition.form?.groups) {
      return definition.form.groups.flatMap(g => g.fields || []);
    }
    return [];
  };

  const fields = getFields();
  const percentage = submission.maxScore 
    ? Math.round((submission.score! / submission.maxScore) * 100)
    : null;

  const getFieldKey = (field: FieldDefinition) => field.name || field.key || '';
  const getFieldLabel = (field: FieldDefinition) => field.label || field.title || field.name || field.key || '';

  const normalizeOptions = (options: FieldDefinition['options']): Array<{ label: string; value: string }> => {
    if (!options) return [];
    return options.map(opt => {
      if (typeof opt === 'string') {
        return { label: opt, value: opt };
      }
      return opt;
    });
  };

  const isCorrect = (field: FieldDefinition, value: any): boolean | null => {
    if (field.correct === undefined) return null;
    if (Array.isArray(field.correct)) {
      if (!Array.isArray(value)) return false;
      return field.correct.length === value.length && 
             field.correct.every(c => value.includes(c));
    }
    return value === field.correct;
  };

  const formatAnswer = (value: any): string => {
    if (value === undefined || value === null) return '(no answer)';
    if (Array.isArray(value)) return value.join(', ');
    if (typeof value === 'boolean') return value ? 'Yes' : 'No';
    return String(value);
  };

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-950">
      {/* Header */}
      <header className="sticky top-0 z-50 bg-white dark:bg-slate-900 border-b border-slate-200 dark:border-slate-800">
        <div className="container flex items-center justify-between h-16">
          <div className="flex items-center gap-4">
            <Button variant="ghost" size="icon" onClick={() => navigate('/admin/submissions')}>
              <ArrowLeft className="h-5 w-5" />
            </Button>
            <h1 className="text-lg font-semibold">Submission Review</h1>
          </div>
          {documentSlug && (
            <Button variant="outline" onClick={() => navigate(`/documents/${documentSlug}`)}>
              <FileText className="mr-2 h-4 w-4" />
              View Document
            </Button>
          )}
        </div>
      </header>

      <main className="container py-8 max-w-3xl">
        {/* Summary Card */}
        <Card className="mb-6">
          <CardHeader>
            <div className="flex items-start justify-between">
              <div>
                <CardTitle>{documentTitle || 'Unknown Document'}</CardTitle>
                <CardDescription className="mt-1">
                  <Badge variant="outline" className="mr-2">{submission.formId}</Badge>
                  <span className="inline-flex items-center gap-1">
                    <Calendar className="h-3 w-3" />
                    {format(new Date(submission.submittedAt), 'MMMM d, yyyy \'at\' h:mm a')}
                  </span>
                </CardDescription>
              </div>
              {percentage !== null && (
                <div className="text-right">
                  <div className={`text-3xl font-bold ${
                    percentage >= 70 ? 'text-green-600' : percentage >= 50 ? 'text-yellow-600' : 'text-red-600'
                  }`}>
                    {percentage}%
                  </div>
                  <div className="text-sm text-muted-foreground">
                    {submission.score}/{submission.maxScore} correct
                  </div>
                </div>
              )}
            </div>
          </CardHeader>
          {percentage !== null && (
            <CardContent>
              <Progress value={percentage} className="h-3" />
            </CardContent>
          )}
        </Card>

        {/* Answers Review */}
        <Card>
          <CardHeader>
            <CardTitle className="text-lg">Your Answers</CardTitle>
            <CardDescription>
              Review your responses and see the correct answers
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-6">
            {fields.length > 0 ? (
              fields.map((field, index) => {
                const fieldKey = getFieldKey(field);
                const fieldLabel = getFieldLabel(field);
                const value = responses[fieldKey];
                const correct = isCorrect(field, value);
                const options = normalizeOptions(field.options);

                return (
                  <div 
                    key={`${fieldKey}-${index}`} 
                    className={`p-4 rounded-lg border ${
                      correct === true 
                        ? 'bg-green-50 dark:bg-green-950/30 border-green-200 dark:border-green-800' 
                        : correct === false 
                          ? 'bg-red-50 dark:bg-red-950/30 border-red-200 dark:border-red-800'
                          : 'bg-slate-50 dark:bg-slate-900 border-slate-200 dark:border-slate-800'
                    }`}
                  >
                    <div className="flex items-start justify-between gap-4">
                      <div className="flex-1">
                        <Label className="text-base font-medium flex items-center gap-2">
                          {fieldLabel}
                          {correct === true && (
                            <CheckCircle2 className="h-5 w-5 text-green-500" />
                          )}
                          {correct === false && (
                            <XCircle className="h-5 w-5 text-red-500" />
                          )}
                        </Label>
                        
                        <div className="mt-3 space-y-2">
                          <div>
                            <span className="text-sm text-muted-foreground">Your answer: </span>
                            <span className={`font-medium ${
                              correct === false ? 'text-red-600 dark:text-red-400' : ''
                            }`}>
                              {formatAnswer(value)}
                            </span>
                          </div>
                          
                          {correct === false && field.correct !== undefined && (
                            <div>
                              <span className="text-sm text-muted-foreground">Correct answer: </span>
                              <span className="font-medium text-green-600 dark:text-green-400">
                                {formatAnswer(field.correct)}
                              </span>
                            </div>
                          )}

                          {options.length > 0 && (
                            <div className="mt-2 text-sm text-muted-foreground">
                              <span>Options: </span>
                              {options.map((opt, i) => (
                                <span key={opt.value}>
                                  {opt.label}
                                  {i < options.length - 1 && ', '}
                                </span>
                              ))}
                            </div>
                          )}
                        </div>
                      </div>
                    </div>
                  </div>
                );
              })
            ) : (
              // Show raw responses if no field definitions
              <div className="space-y-4">
                <p className="text-sm text-muted-foreground">
                  Form definition not available. Showing raw responses:
                </p>
                {Object.entries(responses).map(([key, value]) => (
                  <div key={key} className="p-4 rounded-lg bg-slate-50 dark:bg-slate-900 border">
                    <Label className="text-base font-medium">{key}</Label>
                    <p className="mt-1">{formatAnswer(value)}</p>
                  </div>
                ))}
              </div>
            )}
          </CardContent>
        </Card>
      </main>
    </div>
  );
}
