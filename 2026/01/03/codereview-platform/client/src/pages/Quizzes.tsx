import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Progress } from "@/components/ui/progress";
import { trpc } from "@/lib/trpc";
import { useAuth } from "@/_core/hooks/useAuth";
import { Code2, ArrowLeft, GraduationCap, Clock, BarChart3, CheckCircle2, Loader2 } from "lucide-react";
import { Link, useSearch } from "wouter";
import { useEffect } from "react";

const difficultyColors: Record<string, string> = {
  beginner: "bg-green-500/20 text-green-400",
  intermediate: "bg-yellow-500/20 text-yellow-400",
  advanced: "bg-red-500/20 text-red-400",
};

export default function Quizzes() {
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  const { isAuthenticated } = useAuth();
  
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
  
  const quizzesQuery = trpc.quizzes.list.useQuery(
    { repoPath },
    { enabled: !!repoPath && demoStatus.data?.initialized }
  );
  const progressQuery = trpc.quizzes.userProgress.useQuery(
    { repoPath },
    { enabled: isAuthenticated && !!repoPath && demoStatus.data?.initialized }
  );

  // Create a map of quiz progress
  const progressMap = new Map(
    progressQuery.data?.map(p => [p.quizId, p]) || []
  );

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
          <Link href="/">
            <Button variant="ghost" size="icon">
              <ArrowLeft className="h-5 w-5" />
            </Button>
          </Link>
          <div className="flex items-center gap-2">
            <GraduationCap className="h-6 w-6 text-yellow-500" />
            <span className="text-xl font-bold">Quizzes</span>
          </div>
        </div>
      </header>

      <main className="container py-8">
        {/* Stats */}
        {isAuthenticated && progressQuery.data && (
          <div className="grid grid-cols-3 gap-4 mb-8">
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center gap-2">
                  <BarChart3 className="h-5 w-5 text-primary" />
                  <div>
                    <p className="text-2xl font-bold">
                      {progressQuery.data.filter(p => p.submitted).length}
                    </p>
                    <p className="text-sm text-muted-foreground">Completed</p>
                  </div>
                </div>
              </CardContent>
            </Card>
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center gap-2">
                  <CheckCircle2 className="h-5 w-5 text-green-500" />
                  <div>
                    <p className="text-2xl font-bold">
                      {progressQuery.data.filter(p => p.score && p.maxScore && p.score >= p.maxScore * 0.7).length}
                    </p>
                    <p className="text-sm text-muted-foreground">Passed</p>
                  </div>
                </div>
              </CardContent>
            </Card>
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center gap-2">
                  <GraduationCap className="h-5 w-5 text-yellow-500" />
                  <div>
                    <p className="text-2xl font-bold">
                      {quizzesQuery.data?.length || 0}
                    </p>
                    <p className="text-sm text-muted-foreground">Total Quizzes</p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>
        )}

        {/* Quiz list */}
        <div className="grid md:grid-cols-2 gap-4">
          {quizzesQuery.isLoading && (
            <div className="text-muted-foreground col-span-2">Loading quizzes...</div>
          )}
          
          {quizzesQuery.error && (
            <div className="text-destructive col-span-2">Error: {quizzesQuery.error.message}</div>
          )}
          
          {quizzesQuery.data?.length === 0 && (
            <div className="text-muted-foreground col-span-2">No quizzes found</div>
          )}
          
          {quizzesQuery.data?.map((quiz) => {
            const progress = progressMap.get(quiz.id);
            const percentage = progress?.score && progress?.maxScore 
              ? Math.round((progress.score / progress.maxScore) * 100) 
              : 0;
            
            return (
              <Link key={quiz.id} href={`/quiz/${quiz.commit}?repo=${encodeURIComponent(repoPath)}&quizId=${encodeURIComponent(quiz.id)}`}>
                <Card className="hover:shadow-lg transition-shadow cursor-pointer h-full">
                  <CardHeader>
                    <div className="flex items-start justify-between">
                      <div>
                        <CardTitle className="flex items-center gap-2">
                          <GraduationCap className="h-5 w-5 text-yellow-500" />
                          {quiz.title}
                        </CardTitle>
                        <CardDescription className="mt-1">
                          {quiz.description || quiz.context || "No description"}
                        </CardDescription>
                      </div>
                      {progress?.submitted && (
                        <Badge variant={percentage >= 70 ? "default" : "secondary"}>
                          {percentage}%
                        </Badge>
                      )}
                    </div>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center gap-4 text-sm text-muted-foreground mb-3">
                      <span>{quiz.questionCount} questions</span>
                      {quiz.estimatedTime && (
                        <span className="flex items-center gap-1">
                          <Clock className="h-4 w-4" />
                          {quiz.estimatedTime}
                        </span>
                      )}
                      {quiz.difficulty && (
                        <Badge className={difficultyColors[quiz.difficulty] || "bg-muted"}>
                          {quiz.difficulty}
                        </Badge>
                      )}
                    </div>
                    
                    <div className="flex gap-2 flex-wrap mb-3">
                      {Object.entries(quiz.questionTypes).map(([type, count]) => (
                        <Badge key={type} variant="outline" className="text-xs">
                          {type}: {count as number}
                        </Badge>
                      ))}
                    </div>
                    
                    {progress?.submitted && (
                      <div className="space-y-1">
                        <div className="flex justify-between text-xs">
                          <span>Score: {progress.score}/{progress.maxScore}</span>
                          <span>{percentage >= 70 ? "Passed" : "Not passed"}</span>
                        </div>
                        <Progress value={percentage} className="h-2" />
                      </div>
                    )}
                  </CardContent>
                </Card>
              </Link>
            );
          })}
        </div>
      </main>
    </div>
  );
}
