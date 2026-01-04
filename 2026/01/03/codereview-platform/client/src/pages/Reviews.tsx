import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { trpc } from "@/lib/trpc";
import { Code2, ArrowLeft, GitPullRequest, FileCode, MessageSquare, Loader2 } from "lucide-react";
import { Link, useSearch } from "wouter";
import { useEffect } from "react";

export default function Reviews() {
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  
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
  
  const reviewsQuery = trpc.reviews.list.useQuery(
    { repoPath },
    { enabled: !!repoPath && demoStatus.data?.initialized }
  );
  const tagsQuery = trpc.reviews.allTags.useQuery(
    { repoPath },
    { enabled: !!repoPath && demoStatus.data?.initialized }
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
            <Code2 className="h-6 w-6 text-primary" />
            <span className="text-xl font-bold">Code Reviews</span>
          </div>
        </div>
      </header>

      <main className="container py-8">
        <div className="grid lg:grid-cols-4 gap-6">
          {/* Reviews list */}
          <div className="lg:col-span-3 space-y-4">
            {reviewsQuery.isLoading && (
              <div className="text-muted-foreground">Loading reviews...</div>
            )}
            
            {reviewsQuery.error && (
              <div className="text-destructive">Error: {reviewsQuery.error.message}</div>
            )}
            
            {reviewsQuery.data?.length === 0 && (
              <div className="text-muted-foreground">No reviews found</div>
            )}
            
            {reviewsQuery.data?.map((review) => (
              <Link key={review.id} href={`/review/${review.commit}?repo=${encodeURIComponent(repoPath)}&reviewId=${encodeURIComponent(review.id)}`}>
                <Card className="hover:shadow-lg transition-shadow cursor-pointer">
                  <CardHeader>
                    <div className="flex items-start justify-between">
                      <div>
                        <CardTitle className="flex items-center gap-2">
                          <GitPullRequest className="h-5 w-5 text-green-500" />
                          {review.title}
                        </CardTitle>
                        <CardDescription className="mt-1">
                          {review.description || "No description"}
                        </CardDescription>
                      </div>
                      {review.pr && (
                        <Badge variant="outline">PR #{review.pr}</Badge>
                      )}
                    </div>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center gap-4 text-sm text-muted-foreground">
                      <span className="flex items-center gap-1">
                        <MessageSquare className="h-4 w-4" />
                        {review.annotationCount} annotations
                      </span>
                      <span className="flex items-center gap-1">
                        <FileCode className="h-4 w-4" />
                        {review.files.length} files
                      </span>
                      {review.baseBranch && review.headBranch && (
                        <span className="text-xs">
                          {review.baseBranch} ← {review.headBranch}
                        </span>
                      )}
                    </div>
                    <div className="mt-3 flex gap-2 flex-wrap">
                      {review.files.slice(0, 3).map((file) => (
                        <Badge key={file} variant="secondary" className="text-xs">
                          {file.split("/").pop()}
                        </Badge>
                      ))}
                      {review.files.length > 3 && (
                        <Badge variant="secondary" className="text-xs">
                          +{review.files.length - 3} more
                        </Badge>
                      )}
                    </div>
                  </CardContent>
                </Card>
              </Link>
            ))}
          </div>

          {/* Sidebar */}
          <div className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle className="text-sm">Popular Tags</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="flex flex-wrap gap-2">
                  {tagsQuery.data?.slice(0, 10).map(({ tag, count }) => (
                    <Badge key={tag} variant="outline" className="cursor-pointer hover:bg-muted">
                      {tag} ({count})
                    </Badge>
                  ))}
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-sm">Quick Links</CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <Link href={`/browse?repo=${encodeURIComponent(repoPath)}`}>
                  <Button variant="outline" size="sm" className="w-full justify-start">
                    Browse Files
                  </Button>
                </Link>
                <Link href={`/quizzes?repo=${encodeURIComponent(repoPath)}`}>
                  <Button variant="outline" size="sm" className="w-full justify-start">
                    Take Quizzes
                  </Button>
                </Link>
                <Link href={`/guides?repo=${encodeURIComponent(repoPath)}`}>
                  <Button variant="outline" size="sm" className="w-full justify-start">
                    Follow Guides
                  </Button>
                </Link>
              </CardContent>
            </Card>
          </div>
        </div>
      </main>
    </div>
  );
}
