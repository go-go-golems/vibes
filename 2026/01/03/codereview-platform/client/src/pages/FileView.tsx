import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Popover, PopoverContent, PopoverTrigger } from "@/components/ui/popover";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { trpc } from "@/lib/trpc";
import { Code2, ArrowLeft, GitBranch, MessageSquare, GraduationCap, BookOpen, ChevronRight, Home, Loader2 } from "lucide-react";
import { useState, useMemo, useEffect } from "react";
import { Link, useSearch, useRoute } from "wouter";

// Annotation type colors
const annotationColors: Record<string, string> = {
  educational: "bg-blue-500/20 border-blue-500 text-blue-400",
  gotcha: "bg-yellow-500/20 border-yellow-500 text-yellow-400",
  best_practice: "bg-green-500/20 border-green-500 text-green-400",
  "best-practice": "bg-green-500/20 border-green-500 text-green-400",
  warning: "bg-red-500/20 border-red-500 text-red-400",
  tip: "bg-purple-500/20 border-purple-500 text-purple-400",
};

export default function FileView() {
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  const branchParam = searchParams.get("branch");
  // Use wildcard route params
  const [, routeParams] = useRoute("/file/*");
  const filePath = (routeParams as Record<string, string> | null)?.["*"] || "";
  
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>(branchParam || undefined);
  
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
  
  const branchesQuery = trpc.repository.branches.useQuery(
    { repoPath },
    { enabled: !!repoPath && demoStatus.data?.initialized }
  );
  
  const currentBranch = useMemo(() => {
    if (selectedBranch) return selectedBranch;
    // Default to 'main' or first branch
    return branchesQuery.data?.[0] || 'main';
  }, [selectedBranch, branchesQuery.data]);
  
  const fileContentQuery = trpc.repository.fileContent.useQuery(
    { repoPath, filePath, branch: currentBranch },
    { enabled: !!currentBranch && !!filePath && !!repoPath }
  );
  
  const annotationsQuery = trpc.repository.fileAnnotations.useQuery(
    { repoPath, filePath, branch: currentBranch },
    { enabled: !!currentBranch && !!filePath && !!repoPath }
  );
  
  const reviewsQuery = trpc.repository.fileReviews.useQuery(
    { repoPath, filePath },
    { enabled: !!filePath && !!repoPath }
  );
  
  const guidesQuery = trpc.guides.byFile.useQuery(
    { repoPath, filePath },
    { enabled: !!filePath && !!repoPath }
  );

  // Group annotations by line
  const annotationsByLine = useMemo(() => {
    const map = new Map<number, typeof annotationsQuery.data>();
    annotationsQuery.data?.forEach(a => {
      const existing = map.get(a.annotation.line) || [];
      map.set(a.annotation.line, [...existing, a]);
    });
    return map;
  }, [annotationsQuery.data]);

  // Build breadcrumb parts
  const pathParts = filePath ? filePath.split("/").filter(Boolean) : [];
  const fileName = pathParts[pathParts.length - 1] || "";
  const dirPath = pathParts.slice(0, -1).join("/");

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
          <Link href={dirPath ? `/browse/${dirPath}?repo=${encodeURIComponent(repoPath)}` : `/browse?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="ghost" size="icon">
              <ArrowLeft className="h-5 w-5" />
            </Button>
          </Link>
          <div className="flex items-center gap-2">
            <Code2 className="h-6 w-6 text-primary" />
            <span className="text-xl font-bold">{fileName}</span>
          </div>
          
          {/* Branch selector */}
          <div className="ml-auto flex items-center gap-2">
            <GitBranch className="h-4 w-4 text-muted-foreground" />
            <Select
              value={currentBranch}
              onValueChange={setSelectedBranch}
            >
              <SelectTrigger className="w-40">
                <SelectValue placeholder="Select branch" />
              </SelectTrigger>
              <SelectContent>
                {branchesQuery.data?.map((branch) => (
                  <SelectItem key={branch} value={branch}>
                    {branch}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </div>
      </header>

      <main className="container py-6">
        {/* Breadcrumb */}
        <nav className="flex items-center gap-1 text-sm mb-6 flex-wrap">
          <Link href={`/browse?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="ghost" size="sm" className="h-7 px-2">
              <Home className="h-4 w-4" />
            </Button>
          </Link>
          {pathParts.map((part, index) => {
            const pathTo = pathParts.slice(0, index + 1).join("/");
            const isLast = index === pathParts.length - 1;
            return (
              <div key={pathTo} className="flex items-center">
                <ChevronRight className="h-4 w-4 text-muted-foreground" />
                {isLast ? (
                  <span className="px-2 text-foreground">{part}</span>
                ) : (
                  <Link href={`/browse/${pathTo}?repo=${encodeURIComponent(repoPath)}`}>
                    <Button variant="ghost" size="sm" className="h-7 px-2">
                      {part}
                    </Button>
                  </Link>
                )}
              </div>
            );
          })}
        </nav>

        <div className="grid lg:grid-cols-4 gap-6">
          {/* Code view */}
          <div className="lg:col-span-3">
            <div className="border border-border rounded-lg bg-card overflow-hidden">
              {fileContentQuery.isLoading && (
                <div className="p-4 text-muted-foreground">Loading...</div>
              )}
              
              {fileContentQuery.error && (
                <div className="p-4 text-destructive">
                  Error: {fileContentQuery.error.message}
                </div>
              )}
              
              {fileContentQuery.data && (
                <div className="overflow-x-auto">
                  <table className="w-full text-sm font-mono">
                    <tbody>
                      {fileContentQuery.data.lines.map((line, index) => {
                        const lineNum = index + 1;
                        const lineAnnotations = annotationsByLine.get(lineNum);
                        const hasAnnotations = lineAnnotations && lineAnnotations.length > 0;
                        
                        return (
                          <tr
                            key={lineNum}
                            className={`hover:bg-muted/30 ${hasAnnotations ? "bg-blue-500/5" : ""}`}
                          >
                            <td className="w-12 px-3 py-0.5 text-right text-muted-foreground select-none border-r border-border">
                              {lineNum}
                            </td>
                            <td className="px-4 py-0.5 whitespace-pre">
                              <span dangerouslySetInnerHTML={{ __html: line || "&nbsp;" }} />
                            </td>
                            <td className="w-8 px-2">
                              {hasAnnotations && (
                                <Popover>
                                  <PopoverTrigger asChild>
                                    <Button
                                      variant="ghost"
                                      size="icon"
                                      className="h-6 w-6"
                                    >
                                      <MessageSquare className="h-4 w-4 text-blue-400" />
                                    </Button>
                                  </PopoverTrigger>
                                  <PopoverContent className="w-96" align="end">
                                    <div className="space-y-4">
                                      {lineAnnotations.map((a, i) => (
                                        <div key={i} className="space-y-2">
                                          <div className="flex items-center gap-2">
                                            <Badge className={annotationColors[a.annotation.type] || "bg-muted"}>
                                              {a.annotation.type}
                                            </Badge>
                                            {a.annotation.title && (
                                              <span className="font-medium">{a.annotation.title}</span>
                                            )}
                                          </div>
                                          <p className="text-sm text-muted-foreground whitespace-pre-wrap">
                                            {a.annotation.content}
                                          </p>
                                          {a.annotation.tags && a.annotation.tags.length > 0 && (
                                            <div className="flex gap-1 flex-wrap">
                                              {a.annotation.tags.map(tag => (
                                                <Badge key={tag} variant="outline" className="text-xs">
                                                  {tag}
                                                </Badge>
                                              ))}
                                            </div>
                                          )}
                                          {(a.annotation.quiz as any) && (
                                            <Link href={`/review/${a.commit}?repo=${encodeURIComponent(repoPath)}`}>
                                              <Button size="sm" variant="outline" className="mt-2">
                                                <GraduationCap className="h-4 w-4 mr-2" />
                                                Take Quiz
                                              </Button>
                                            </Link>
                                          )}
                                        </div>
                                      ))}
                                    </div>
                                  </PopoverContent>
                                </Popover>
                              )}
                            </td>
                          </tr>
                        );
                      })}
                    </tbody>
                  </table>
                </div>
              )}
            </div>
          </div>

          {/* Sidebar */}
          <div className="space-y-4">
            {/* File info */}
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm">File Info</CardTitle>
              </CardHeader>
              <CardContent className="text-sm space-y-1">
                <p><span className="text-muted-foreground">Language:</span> {fileContentQuery.data?.language}</p>
                <p><span className="text-muted-foreground">Lines:</span> {fileContentQuery.data?.lineCount}</p>
              </CardContent>
            </Card>

            {/* Related reviews */}
            {reviewsQuery.data && reviewsQuery.data.length > 0 && (
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm flex items-center gap-2">
                    <MessageSquare className="h-4 w-4" />
                    Reviews
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-2">
                  {reviewsQuery.data.map((review) => (
                    <Link key={review.commit} href={`/review/${review.commit}?repo=${encodeURIComponent(repoPath)}`}>
                      <div className="p-2 rounded hover:bg-muted cursor-pointer">
                        <p className="font-medium text-sm">{review.title}</p>
                        <p className="text-xs text-muted-foreground">
                          {review.annotationCount} annotations • PR #{review.pr}
                        </p>
                      </div>
                    </Link>
                  ))}
                </CardContent>
              </Card>
            )}

            {/* Related guides */}
            {guidesQuery.data && guidesQuery.data.length > 0 && (
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm flex items-center gap-2">
                    <BookOpen className="h-4 w-4" />
                    Guides
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-2">
                  {guidesQuery.data.map((guide) => (
                    <Link key={guide.commit} href={`/guide/${guide.commit}?repo=${encodeURIComponent(repoPath)}`}>
                      <div className="p-2 rounded hover:bg-muted cursor-pointer">
                        <p className="font-medium text-sm">{guide.title}</p>
                        <p className="text-xs text-muted-foreground">
                          {guide.stopsInFile.length} stops in this file
                        </p>
                      </div>
                    </Link>
                  ))}
                </CardContent>
              </Card>
            )}
          </div>
        </div>
      </main>
    </div>
  );
}
