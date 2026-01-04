import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { RadioGroup, RadioGroupItem } from "@/components/ui/radio-group";
import { Label } from "@/components/ui/label";
import { Input } from "@/components/ui/input";
import { Progress } from "@/components/ui/progress";
import { trpc } from "@/lib/trpc";
import { useAuth } from "@/_core/hooks/useAuth";
import { getLoginUrl } from "@/const";
import { Code2, ArrowLeft, GraduationCap, CheckCircle2, XCircle, LogIn, Loader2 } from "lucide-react";
import { useState, useEffect } from "react";
import { Link, useParams, useSearch } from "wouter";
import { toast } from "sonner";

export default function QuizTake() {
  const params = useParams<{ commit: string }>();
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  const commit = params.commit || "";
  const { isAuthenticated } = useAuth();
  
  const [answers, setAnswers] = useState<Record<string, unknown>>({});
  const [submitted, setSubmitted] = useState(false);
  const [result, setResult] = useState<{
    score: number;
    maxScore: number;
    percentage: number;
    passed: boolean;
    grading: Array<{
      questionIndex: number;
      correct: boolean;
      userAnswer: unknown;
      correctAnswer?: unknown;
      explanation?: string;
    }>;
  } | null>(null);
  
  // Get demo repo status and path
  const demoStatus = trpc.repository.demoStatus.useQuery();
  const initDemo = trpc.repository.initDemo.useMutation({
    onSuccess: () => {
      demoStatus.refetch();
    }
  });

  // Initialize demo if not ready
  useEffect(() => {
    if (demoStatus.data && !demoStatus.data.initialized && !initDemo.isPending) {
      initDemo.mutate();
    }
  }, [demoStatus.data]);

  const repoPath = searchParams.get("repo") || demoStatus.data?.path || "";
  const quizId = searchParams.get("quizId") || undefined;
  
  const quizQuery = trpc.quizzes.get.useQuery(
    { repoPath, commit, quizId },
    { enabled: !!commit && !!repoPath && demoStatus.data?.initialized }
  );
  
  const submitMutation = trpc.quizzes.submit.useMutation({
    onSuccess: (data) => {
      setResult(data);
      setSubmitted(true);
      toast.success(`Quiz completed! Score: ${data.score}/${data.maxScore}`);
    },
    onError: (error) => {
      toast.error(`Error: ${error.message}`);
    },
  });

  const quiz = quizQuery.data;
  const questions = quiz?.questionsForTaking || [];

  const handleSubmit = () => {
    if (!quiz) return;
    submitMutation.mutate({
      repoPath,
      commit,
      quizId: quiz.id,
      answers,
    });
  };

  const getGrading = (index: number) => {
    return result?.grading.find(g => g.questionIndex === index);
  };

  // Show loading while demo initializes
  if (!demoStatus.data?.initialized || initDemo.isPending) {
    return (
      <div className="min-h-screen bg-background flex items-center justify-center">
        <div className="text-center">
          <Loader2 className="h-8 w-8 animate-spin mx-auto mb-4 text-primary" />
          <p className="text-muted-foreground">Initializing demo repository...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
      <header className="border-b border-border bg-card">
        <div className="container flex h-16 items-center gap-4">
          <Link href={`/quizzes?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="ghost" size="icon">
              <ArrowLeft className="h-5 w-5" />
            </Button>
          </Link>
          <div className="flex items-center gap-2">
            <GraduationCap className="h-6 w-6 text-yellow-500" />
            <span className="text-xl font-bold">{quiz?.title || "Loading..."}</span>
          </div>
        </div>
      </header>

      <main className="container py-8 max-w-3xl">
        {quizQuery.isLoading && (
          <div className="text-muted-foreground">Loading quiz...</div>
        )}
        
        {quizQuery.error && (
          <div className="text-destructive">Error: {quizQuery.error.message}</div>
        )}
        
        {quiz && (
          <div className="space-y-6">
            {/* Quiz info */}
            <Card>
              <CardHeader>
                <CardTitle>{quiz.title}</CardTitle>
                <CardDescription>
                  {quiz.description || quiz.context}
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="flex gap-4 text-sm text-muted-foreground">
                  <span>{questions.length} questions</span>
                  {quiz.difficulty && <Badge variant="outline">{quiz.difficulty}</Badge>}
                  {quiz.estimatedTime && <span>{quiz.estimatedTime}</span>}
                </div>
              </CardContent>
            </Card>

            {/* Result summary */}
            {result && (
              <Card className={result.passed ? "border-green-500" : "border-red-500"}>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    {result.passed ? (
                      <CheckCircle2 className="h-6 w-6 text-green-500" />
                    ) : (
                      <XCircle className="h-6 w-6 text-red-500" />
                    )}
                    {result.passed ? "Quiz Passed!" : "Quiz Not Passed"}
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-2">
                    <div className="flex justify-between">
                      <span>Score: {result.score}/{result.maxScore}</span>
                      <span>{result.percentage}%</span>
                    </div>
                    <Progress value={result.percentage} className="h-3" />
                    <p className="text-sm text-muted-foreground">
                      {result.passed 
                        ? "Congratulations! You've demonstrated understanding of the material."
                        : "You need 70% to pass. Review the explanations below and try again."}
                    </p>
                  </div>
                </CardContent>
              </Card>
            )}

            {/* Questions */}
            <div className="space-y-4">
              {questions.map((question: any, index: number) => {
                const grading = getGrading(index);
                const isCorrect = grading?.correct;
                
                return (
                  <Card 
                    key={index}
                    className={
                      submitted 
                        ? isCorrect 
                          ? "border-green-500/50" 
                          : "border-red-500/50"
                        : ""
                    }
                  >
                    <CardHeader>
                      <CardTitle className="text-base flex items-center gap-2">
                        <span className="text-muted-foreground">Q{index + 1}.</span>
                        {question.question}
                        {submitted && (
                          isCorrect 
                            ? <CheckCircle2 className="h-5 w-5 text-green-500 ml-auto" />
                            : <XCircle className="h-5 w-5 text-red-500 ml-auto" />
                        )}
                      </CardTitle>
                    </CardHeader>
                    <CardContent>
                      {question.type === "multiple_choice" || question.type === "scenario" ? (
                        <RadioGroup
                          value={String(answers[`q${index}`] ?? "")}
                          onValueChange={(value) => {
                            if (!submitted) {
                              setAnswers(prev => ({ ...prev, [`q${index}`]: parseInt(value) }));
                            }
                          }}
                          disabled={submitted}
                        >
                          {question.options?.map((option: string, optIndex: number) => (
                            <div 
                              key={optIndex} 
                              className={`flex items-center space-x-2 p-2 rounded ${
                                submitted && grading?.correctAnswer === optIndex
                                  ? "bg-green-500/10"
                                  : submitted && answers[`q${index}`] === optIndex && !isCorrect
                                  ? "bg-red-500/10"
                                  : ""
                              }`}
                            >
                              <RadioGroupItem value={String(optIndex)} id={`q${index}-${optIndex}`} />
                              <Label htmlFor={`q${index}-${optIndex}`} className="cursor-pointer flex-1">
                                {option}
                              </Label>
                            </div>
                          ))}
                        </RadioGroup>
                      ) : question.type === "code_completion" ? (
                        <div className="space-y-2">
                          {question.codeContext && (
                            <pre className="p-3 bg-muted rounded text-sm font-mono overflow-x-auto">
                              {question.codeContext}
                            </pre>
                          )}
                          <Input
                            placeholder="Enter your answer..."
                            value={String(answers[`q${index}`] || "")}
                            onChange={(e) => {
                              if (!submitted) {
                                setAnswers(prev => ({ ...prev, [`q${index}`]: e.target.value }));
                              }
                            }}
                            disabled={submitted}
                            className={
                              submitted 
                                ? isCorrect 
                                  ? "border-green-500" 
                                  : "border-red-500"
                                : ""
                            }
                          />
                          {submitted && !isCorrect && grading?.correctAnswer && (
                            <p className="text-sm text-green-500">
                              Correct answer: {String(grading.correctAnswer)}
                            </p>
                          )}
                        </div>
                      ) : null}
                      
                      {submitted && grading?.explanation && (
                        <div className="mt-3 p-3 bg-muted/50 rounded text-sm">
                          <strong>Explanation:</strong> {grading.explanation}
                        </div>
                      )}
                    </CardContent>
                  </Card>
                );
              })}
            </div>

            {/* Submit button */}
            {!submitted && (
              <div className="flex justify-end gap-4">
                {!isAuthenticated ? (
                  <Button asChild>
                    <a href={getLoginUrl()}>
                      <LogIn className="h-4 w-4 mr-2" />
                      Sign in to Submit
                    </a>
                  </Button>
                ) : (
                  <Button 
                    onClick={handleSubmit}
                    disabled={submitMutation.isPending || Object.keys(answers).length < questions.length}
                  >
                    {submitMutation.isPending ? "Submitting..." : "Submit Quiz"}
                  </Button>
                )}
              </div>
            )}

            {submitted && (
              <div className="flex justify-end gap-4">
                <Button variant="outline" onClick={() => {
                  setSubmitted(false);
                  setResult(null);
                  setAnswers({});
                }}>
                  Retake Quiz
                </Button>
                <Link href={`/quizzes?repo=${encodeURIComponent(repoPath)}`}>
                  <Button>Back to Quizzes</Button>
                </Link>
              </div>
            )}
          </div>
        )}
      </main>
    </div>
  );
}
