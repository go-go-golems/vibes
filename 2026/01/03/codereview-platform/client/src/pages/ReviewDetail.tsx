import { Button } from "@/components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { trpc } from "@/lib/trpc";
import { Code2, ArrowLeft, GitPullRequest, FileCode, MessageSquare, GraduationCap, Loader2 } from "lucide-react";
import { Link, useParams, useSearch } from "wouter";
import { Streamdown } from "streamdown";
import { useEffect } from "react";

// Annotation type colors
const annotationColors: Record<string, string> = {
  educational: "bg-blue-500/20 border-blue-500 text-blue-400",
  gotcha: "bg-yellow-500/20 border-yellow-500 text-yellow-400",
  best_practice: "bg-green-500/20 border-green-500 text-green-400",
  "best-practice": "bg-green-500/20 border-green-500 text-green-400",
  warning: "bg-red-500/20 border-red-500 text-red-400",
  tip: "bg-purple-500/20 border-purple-500 text-purple-400",
};

export default function ReviewDetail() {
  const params = useParams<{ commit: string }>();
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  const commit = params.commit || "";
  
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
  const reviewId = searchParams.get("reviewId") || undefined;
  
  const reviewQuery = trpc.reviews.getWithDiff.useQuery(
    { repoPath, commit, reviewId },
    { enabled: !!commit && !!repoPath && demoStatus.data?.initialized }
  );

  const review = reviewQuery.data;

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
          <Link href={`/reviews?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="ghost" size="icon">
              <ArrowLeft className="h-5 w-5" />
            </Button>
          </Link>
          <div className="flex items-center gap-2">
            <GitPullRequest className="h-6 w-6 text-green-500" />
            <span className="text-xl font-bold">{review?.title || "Loading..."}</span>
          </div>
          {review?.pr && (
            <Badge variant="outline" className="ml-2">PR #{review.pr}</Badge>
          )}
        </div>
      </header>

      <main className="container py-6">
        {reviewQuery.isLoading && (
          <div className="text-muted-foreground">Loading review...</div>
        )}
        
        {reviewQuery.error && (
          <div className="text-destructive">Error: {reviewQuery.error.message}</div>
        )}
        
        {review && (
          <div className="space-y-6">
            {/* Review info */}
            <Card>
              <CardHeader>
                <CardTitle className="text-lg">Review Details</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                {review.description && (
                  <div>
                    <h4 className="text-sm font-medium text-muted-foreground mb-1">Description</h4>
                    <p>{review.description}</p>
                  </div>
                )}
                <div className="flex gap-6 text-sm">
                  {review.baseBranch && (
                    <div>
                      <span className="text-muted-foreground">Base:</span>{" "}
                      <Badge variant="outline">{review.baseBranch}</Badge>
                    </div>
                  )}
                  {review.headBranch && (
                    <div>
                      <span className="text-muted-foreground">Head:</span>{" "}
                      <Badge variant="outline">{review.headBranch}</Badge>
                    </div>
                  )}
                  <div>
                    <span className="text-muted-foreground">Annotations:</span>{" "}
                    {review.annotations.length}
                  </div>
                </div>
              </CardContent>
            </Card>

            {/* Tabs for files and annotations */}
            <Tabs defaultValue="annotations">
              <TabsList>
                <TabsTrigger value="annotations">
                  <MessageSquare className="h-4 w-4 mr-2" />
                  Annotations ({review.annotations.length})
                </TabsTrigger>
                <TabsTrigger value="files">
                  <FileCode className="h-4 w-4 mr-2" />
                  Files ({Object.keys(review.annotationsByFile || {}).length})
                </TabsTrigger>
              </TabsList>

              <TabsContent value="annotations" className="space-y-4 mt-4">
                {review.annotations.map((annotation, index) => (
                  <Card key={index} className={`border-l-4 ${annotationColors[annotation.type]?.split(" ")[1] || "border-border"}`}>
                    <CardHeader className="pb-2">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center gap-2">
                          <Badge className={annotationColors[annotation.type] || "bg-muted"}>
                            {annotation.type}
                          </Badge>
                          {annotation.title && (
                            <span className="font-medium">{annotation.title}</span>
                          )}
                        </div>
                        <Link href={`/file/${annotation.file}?repo=${encodeURIComponent(repoPath)}`}>
                          <Button variant="ghost" size="sm">
                            <FileCode className="h-4 w-4 mr-1" />
                            {annotation.file}:{annotation.line}
                          </Button>
                        </Link>
                      </div>
                    </CardHeader>
                    <CardContent className="space-y-3">
                      <div className="prose prose-sm dark:prose-invert max-w-none">
                        <Streamdown>{annotation.content}</Streamdown>
                      </div>
                      
                      {annotation.tags && annotation.tags.length > 0 && (
                        <div className="flex gap-1 flex-wrap">
                          {annotation.tags.map(tag => (
                            <Badge key={tag} variant="outline" className="text-xs">
                              {tag}
                            </Badge>
                          ))}
                        </div>
                      )}
                      
                      {annotation.quiz && (
                        <Card className="bg-muted/50">
                          <CardHeader className="pb-2">
                            <CardTitle className="text-sm flex items-center gap-2">
                              <GraduationCap className="h-4 w-4 text-yellow-500" />
                              Quiz: {annotation.quiz.title}
                            </CardTitle>
                          </CardHeader>
                          <CardContent>
                            <p className="text-sm text-muted-foreground mb-3">
                              {annotation.quiz.questions.length} questions
                            </p>
                            <div className="space-y-2">
                              {annotation.quiz.questions.map((q: any, qi: number) => (
                                <div key={qi} className="text-sm p-2 bg-background rounded">
                                  <span className="font-medium">Q{qi + 1}:</span> {q.question}
                                </div>
                              ))}
                            </div>
                          </CardContent>
                        </Card>
                      )}
                    </CardContent>
                  </Card>
                ))}
              </TabsContent>

              <TabsContent value="files" className="space-y-4 mt-4">
                {review.annotationsByFile && Object.entries(review.annotationsByFile).map(([file, fileAnnotations]) => (
                  <Card key={file}>
                    <CardHeader className="pb-2">
                      <CardTitle className="text-sm flex items-center gap-2">
                        <FileCode className="h-4 w-4" />
                        <Link href={`/file/${file}?repo=${encodeURIComponent(repoPath)}`}>
                          <span className="hover:underline">{file}</span>
                        </Link>
                        <Badge variant="secondary">{fileAnnotations.length} annotations</Badge>
                      </CardTitle>
                    </CardHeader>
                    <CardContent>
                      <div className="space-y-2">
                        {fileAnnotations.map((a: any, i: number) => (
                          <div key={i} className="flex items-center gap-2 text-sm">
                            <span className="text-muted-foreground">Line {a.line}:</span>
                            <Badge className={`${annotationColors[a.type] || "bg-muted"} text-xs`}>
                              {a.type}
                            </Badge>
                            <span>{a.title || a.content.slice(0, 50)}...</span>
                          </div>
                        ))}
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </TabsContent>
            </Tabs>
          </div>
        )}
      </main>
    </div>
  );
}
