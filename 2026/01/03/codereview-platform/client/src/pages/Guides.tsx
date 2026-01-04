import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { trpc } from "@/lib/trpc";
import { Code2, ArrowLeft, BookOpen, Clock, MapPin, FileCode, Loader2 } from "lucide-react";
import { Link, useSearch } from "wouter";
import { useEffect } from "react";

const difficultyColors: Record<string, string> = {
  beginner: "bg-green-500/20 text-green-400",
  intermediate: "bg-yellow-500/20 text-yellow-400",
  advanced: "bg-red-500/20 text-red-400",
};

export default function Guides() {
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
  
  const guidesQuery = trpc.guides.list.useQuery(
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
            <BookOpen className="h-6 w-6 text-purple-500" />
            <span className="text-xl font-bold">Guided Tours</span>
          </div>
        </div>
      </header>

      <main className="container py-8">
        <div className="mb-8">
          <h1 className="text-2xl font-bold mb-2">Learn Through Guided Tours</h1>
          <p className="text-muted-foreground">
            Follow step-by-step walkthroughs that explain how different parts of the codebase work together.
          </p>
        </div>

        {/* Guide list */}
        <div className="grid md:grid-cols-2 gap-6">
          {guidesQuery.isLoading && (
            <div className="text-muted-foreground col-span-2">Loading guides...</div>
          )}
          
          {guidesQuery.error && (
            <div className="text-destructive col-span-2">Error: {guidesQuery.error.message}</div>
          )}
          
          {guidesQuery.data?.length === 0 && (
            <div className="text-muted-foreground col-span-2">No guides found</div>
          )}
          
          {guidesQuery.data?.map((guide) => (
            <Link key={guide.id} href={`/guide/${guide.commit}?repo=${encodeURIComponent(repoPath)}&guideId=${encodeURIComponent(guide.id)}`}>
              <Card className="hover:shadow-lg transition-shadow cursor-pointer h-full">
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <BookOpen className="h-5 w-5 text-purple-500" />
                    {guide.title}
                  </CardTitle>
                  <CardDescription>
                    {guide.description || "No description"}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="flex items-center gap-4 text-sm text-muted-foreground mb-3">
                    <span className="flex items-center gap-1">
                      <MapPin className="h-4 w-4" />
                      {guide.stopCount} stops
                    </span>
                    {guide.estimatedTime && (
                      <span className="flex items-center gap-1">
                        <Clock className="h-4 w-4" />
                        {guide.estimatedTime}
                      </span>
                    )}
                    {guide.difficulty && (
                      <Badge className={difficultyColors[guide.difficulty] || "bg-muted"}>
                        {guide.difficulty}
                      </Badge>
                    )}
                  </div>
                  
                  {guide.prerequisites && guide.prerequisites.length > 0 && (
                    <div className="mb-3">
                      <p className="text-xs text-muted-foreground mb-1">Prerequisites:</p>
                      <div className="flex gap-1 flex-wrap">
                        {guide.prerequisites.slice(0, 2).map((prereq, i) => (
                          <Badge key={i} variant="outline" className="text-xs">
                            {prereq}
                          </Badge>
                        ))}
                        {guide.prerequisites.length > 2 && (
                          <Badge variant="outline" className="text-xs">
                            +{guide.prerequisites.length - 2} more
                          </Badge>
                        )}
                      </div>
                    </div>
                  )}
                  
                  <div className="flex gap-2 flex-wrap">
                    {guide.files.slice(0, 3).map((file) => (
                      <Badge key={file} variant="secondary" className="text-xs">
                        <FileCode className="h-3 w-3 mr-1" />
                        {file.split("/").pop()}
                      </Badge>
                    ))}
                    {guide.files.length > 3 && (
                      <Badge variant="secondary" className="text-xs">
                        +{guide.files.length - 3} more files
                      </Badge>
                    )}
                  </div>
                </CardContent>
              </Card>
            </Link>
          ))}
        </div>
      </main>
    </div>
  );
}
