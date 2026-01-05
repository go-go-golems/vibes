import React, { useMemo, useState } from 'react';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import rehypeHighlight from 'rehype-highlight';
import 'highlight.js/styles/github-dark.css';
import { QuizForm } from './QuizForm';
import { Button } from './ui/button';
import { Card } from './ui/card';
import { parse as parseYaml } from 'yaml';
import { trpc } from '@/lib/trpc';
import { toast } from 'sonner';
import { Loader2, CheckCircle2 } from 'lucide-react';

interface FormDefinition {
  formId: string;
  definition: any;
  rawYaml: string;
}

interface MarkdownRendererProps {
  content: string;
  documentId: number;
  forms?: Array<{ formId: string; definition: any }>;
  onFormSubmit?: (formId: string, responses: Record<string, any>) => void;
  readOnly?: boolean;
}

// Parse content and extract form tags
function parseContentWithForms(content: string): { 
  segments: Array<{ type: 'markdown' | 'form'; content: string; formId?: string; parsedDefinition?: any }>;
  allForms: Array<{ formId: string; definition: any }>;
} {
  const formRegex = /<form\s+id=["']?([^"'\s>]+)["']?\s*>([\s\S]*?)<\/form>/gi;
  const segments: Array<{ type: 'markdown' | 'form'; content: string; formId?: string; parsedDefinition?: any }> = [];
  const allForms: Array<{ formId: string; definition: any }> = [];
  
  let lastIndex = 0;
  let match;
  
  while ((match = formRegex.exec(content)) !== null) {
    // Add markdown before the form
    if (match.index > lastIndex) {
      const mdContent = content.slice(lastIndex, match.index).trim();
      if (mdContent) {
        segments.push({ type: 'markdown', content: mdContent });
      }
    }
    
    // Parse the YAML content
    const yamlContent = match[2].trim();
    let parsedDefinition = null;
    
    try {
      parsedDefinition = parseYaml(yamlContent);
      if (parsedDefinition) {
        allForms.push({ formId: match[1], definition: parsedDefinition });
      }
    } catch (e) {
      console.error(`Failed to parse YAML for form ${match[1]}:`, e);
    }
    
    // Add the form
    segments.push({
      type: 'form',
      content: yamlContent,
      formId: match[1],
      parsedDefinition,
    });
    
    lastIndex = match.index + match[0].length;
  }
  
  // Add remaining markdown
  if (lastIndex < content.length) {
    const mdContent = content.slice(lastIndex).trim();
    if (mdContent) {
      segments.push({ type: 'markdown', content: mdContent });
    }
  }
  
  return { segments, allForms };
}

export function MarkdownRenderer({ 
  content, 
  documentId, 
  forms = [], 
  onFormSubmit,
  readOnly = false 
}: MarkdownRendererProps) {
  const { segments, allForms } = useMemo(() => parseContentWithForms(content), [content]);
  
  // State to track all form responses
  const [allResponses, setAllResponses] = useState<Record<string, Record<string, any>>>({});
  const [submitted, setSubmitted] = useState(false);
  const [results, setResults] = useState<Array<{ formId: string; score: number; maxScore: number }>>([]);
  
  // Create a map of form definitions from server (if provided)
  const formMap = useMemo(() => {
    const map = new Map<string, any>();
    forms.forEach(f => map.set(f.formId, f.definition));
    // Also add locally parsed forms
    allForms.forEach(f => {
      if (!map.has(f.formId)) {
        map.set(f.formId, f.definition);
      }
    });
    return map;
  }, [forms, allForms]);

  const utils = trpc.useUtils();

  const submitAllMutation = trpc.quiz.submitMultiple.useMutation({
    onSuccess: (data) => {
      setSubmitted(true);
      setResults(data.results);
      
      const totalScore = data.results.reduce((sum, r) => sum + r.score, 0);
      const totalMax = data.results.reduce((sum, r) => sum + r.maxScore, 0);
      
      toast.success(`All quizzes submitted! Total Score: ${totalScore}/${totalMax}`);
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to submit quizzes');
    },
  });

  const handleFormChange = (formId: string, responses: Record<string, any>) => {
    setAllResponses(prev => ({
      ...prev,
      [formId]: responses,
    }));
  };

  const handleSubmitAll = () => {
    if (readOnly) return;
    
    // Prepare submissions for all forms
    const submissions = Array.from(formMap.keys()).map(formId => ({
      formId,
      responses: allResponses[formId] || {},
    }));

    submitAllMutation.mutate({
      documentId,
      submissions,
    });
  };

  const getResultForForm = (formId: string) => {
    return results.find(r => r.formId === formId);
  };

  const hasAnyForms = formMap.size > 0;

  return (
    <div className="markdown-content prose prose-slate dark:prose-invert max-w-none">
      {segments.map((segment, index) => {
        if (segment.type === 'markdown') {
          return (
            <ReactMarkdown
              key={index}
              remarkPlugins={[remarkGfm]}
              rehypePlugins={[rehypeHighlight]}
              components={{
                // Custom styling for code blocks
                pre: ({ children }) => (
                  <pre className="bg-slate-900 rounded-lg p-4 overflow-x-auto">
                    {children}
                  </pre>
                ),
                code: ({ className, children, ...props }) => {
                  const isInline = !className;
                  return isInline ? (
                    <code className="bg-slate-100 dark:bg-slate-800 px-1.5 py-0.5 rounded text-sm" {...props}>
                      {children}
                    </code>
                  ) : (
                    <code className={className} {...props}>
                      {children}
                    </code>
                  );
                },
                // Style tables
                table: ({ children }) => (
                  <div className="overflow-x-auto">
                    <table className="min-w-full divide-y divide-slate-200 dark:divide-slate-700">
                      {children}
                    </table>
                  </div>
                ),
                th: ({ children }) => (
                  <th className="px-4 py-3 text-left text-sm font-semibold text-slate-900 dark:text-slate-100 bg-slate-50 dark:bg-slate-800">
                    {children}
                  </th>
                ),
                td: ({ children }) => (
                  <td className="px-4 py-3 text-sm text-slate-700 dark:text-slate-300 border-b border-slate-100 dark:border-slate-800">
                    {children}
                  </td>
                ),
                // Style blockquotes
                blockquote: ({ children }) => (
                  <blockquote className="border-l-4 border-blue-500 pl-4 italic text-slate-600 dark:text-slate-400">
                    {children}
                  </blockquote>
                ),
              }}
            >
              {segment.content}
            </ReactMarkdown>
          );
        } else if (segment.type === 'form' && segment.formId) {
          // First try to get definition from server (forms prop)
          // Then fall back to locally parsed YAML
          const definition = formMap.get(segment.formId) || segment.parsedDefinition;
          
          if (definition) {
            return (
              <QuizForm
                key={`form-${segment.formId}-${index}`}
                formId={segment.formId}
                definition={definition}
                documentId={documentId}
                onChange={handleFormChange}
                readOnly={readOnly || submitted}
                showCorrectAnswers={submitted}
                existingResponses={allResponses[segment.formId]}
                result={getResultForForm(segment.formId)}
                hideSubmitButton={true}
              />
            );
          } else {
            // Show error if YAML couldn't be parsed
            return (
              <div key={index} className="my-6 p-4 bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg">
                <p className="text-sm text-yellow-800 dark:text-yellow-200 mb-2">
                  Form "{segment.formId}" - Failed to parse YAML definition
                </p>
                <pre className="text-xs bg-slate-100 dark:bg-slate-800 p-2 rounded overflow-x-auto">
                  {segment.content}
                </pre>
              </div>
            );
          }
        }
        return null;
      })}

      {/* Single submit button for all forms */}
      {hasAnyForms && !readOnly && (
        <Card className="my-8 p-6 bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-950/50 dark:to-indigo-950/50 border-2 border-blue-200 dark:border-blue-800">
          {!submitted ? (
            <div className="flex flex-col sm:flex-row items-center justify-between gap-4">
              <div>
                <h3 className="font-semibold text-lg">Ready to submit?</h3>
                <p className="text-sm text-muted-foreground">
                  Submit all {formMap.size} quiz{formMap.size > 1 ? 'zes' : ''} to see your results
                </p>
              </div>
              <Button 
                onClick={handleSubmitAll}
                disabled={submitAllMutation.isPending}
                size="lg"
                className="w-full sm:w-auto"
              >
                {submitAllMutation.isPending ? (
                  <>
                    <Loader2 className="mr-2 h-5 w-5 animate-spin" />
                    Submitting...
                  </>
                ) : (
                  <>
                    <CheckCircle2 className="mr-2 h-5 w-5" />
                    Submit All Quizzes
                  </>
                )}
              </Button>
            </div>
          ) : (
            <div className="text-center">
              <CheckCircle2 className="h-12 w-12 text-green-500 mx-auto mb-3" />
              <h3 className="font-semibold text-lg mb-2">All Quizzes Submitted!</h3>
              <div className="space-y-2">
                {results.map(result => (
                  <div key={result.formId} className="text-sm">
                    <span className="font-medium">{result.formId}:</span>{' '}
                    <span className={result.score === result.maxScore ? 'text-green-600 dark:text-green-400' : 'text-blue-600 dark:text-blue-400'}>
                      {result.score}/{result.maxScore} ({Math.round((result.score / result.maxScore) * 100)}%)
                    </span>
                  </div>
                ))}
                <div className="mt-4 pt-4 border-t border-blue-200 dark:border-blue-800">
                  <span className="font-semibold">Total Score: </span>
                  <span className="text-lg font-bold text-blue-600 dark:text-blue-400">
                    {results.reduce((sum, r) => sum + r.score, 0)}/
                    {results.reduce((sum, r) => sum + r.maxScore, 0)}
                    {' '}
                    ({Math.round((results.reduce((sum, r) => sum + r.score, 0) / results.reduce((sum, r) => sum + r.maxScore, 0)) * 100)}%)
                  </span>
                </div>
              </div>
            </div>
          )}
        </Card>
      )}
    </div>
  );
}
