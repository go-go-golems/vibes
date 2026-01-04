import { Button } from "@/components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Progress } from "@/components/ui/progress";
import { trpc } from "@/lib/trpc";
import { Code2, ArrowLeft, BookOpen, ChevronLeft, ChevronRight, FileCode, MapPin, Home, Loader2 } from "lucide-react";
import { useState, useEffect, useMemo } from "react";
import { Link, useParams, useSearch, useLocation } from "wouter";
import { Streamdown } from "streamdown";

export default function GuideWalkthrough() {
  const params = useParams<{ commit: string; stopId?: string }>();
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  const commit = params.commit || "";
  const [, setLocation] = useLocation();
  
  const [visitedStops, setVisitedStops] = useState<string[]>([]);
  
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
  const guideId = searchParams.get("guideId") || undefined;
  
  const guideQuery = trpc.guides.get.useQuery(
    { repoPath, commit, guideId },
    { enabled: !!commit && !!repoPath && demoStatus.data?.initialized }
  );

  const guide = guideQuery.data;
  
  // Determine current stop
  const currentStopId = params.stopId || guide?.stops[0]?.id;
  const currentStopIndex = guide?.stops.findIndex(s => s.id === currentStopId) ?? 0;
  const currentStop = guide?.stops[currentStopIndex];
  
  // Get file content for current stop
  const fileContentQuery = trpc.repository.fileContent.useQuery(
    { repoPath, filePath: currentStop?.file || "" },
    { enabled: !!currentStop?.file && !!repoPath }
  );

  // Track visited stops
  useEffect(() => {
    if (currentStopId && !visitedStops.includes(currentStopId)) {
      setVisitedStops(prev => [...prev, currentStopId]);
    }
  }, [currentStopId]);

  const progress = guide ? Math.round((visitedStops.length / guide.stops.length) * 100) : 0;

  const navigateToStop = (stopId: string) => {
    setLocation(`/guide/${commit}/${stopId}?repo=${encodeURIComponent(repoPath)}`);
  };

  // Get lines around the current stop's line
  const codeLines = useMemo(() => {
    if (!fileContentQuery.data || !currentStop) return [];
    
    const lines = fileContentQuery.data.lines;
    const targetLine = currentStop.line;
    const lineEnd = (currentStop as any).lineEnd || targetLine;
    const contextBefore = 5;
    const contextAfter = 10;
    
    const start = Math.max(0, targetLine - contextBefore - 1);
    const end = Math.min(lines.length, lineEnd + contextAfter);
    
    return lines.slice(start, end).map((line, i) => ({
      number: start + i + 1,
      content: line,
      isHighlighted: start + i + 1 >= targetLine && start + i + 1 <= lineEnd,
    }));
  }, [fileContentQuery.data, currentStop]);

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
          <Link href={`/guides?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="ghost" size="icon">
              <ArrowLeft className="h-5 w-5" />
            </Button>
          </Link>
          <div className="flex items-center gap-2">
            <BookOpen className="h-6 w-6 text-purple-500" />
            <span className="text-xl font-bold">{guide?.title || "Loading..."}</span>
          </div>
          {guide && (
            <div className="ml-auto flex items-center gap-4">
              <span className="text-sm text-muted-foreground">
                Stop {currentStopIndex + 1} of {guide.stops.length}
              </span>
              <Progress value={progress} className="w-32 h-2" />
            </div>
          )}
        </div>
      </header>

      <main className="container py-6">
        {guideQuery.isLoading && (
          <div className="text-muted-foreground">Loading guide...</div>
        )}
        
        {guideQuery.error && (
          <div className="text-destructive">Error: {guideQuery.error.message}</div>
        )}
        
        {guide && currentStop && (
          <div className="grid lg:grid-cols-3 gap-6">
            {/* Stop content */}
            <div className="lg:col-span-2 space-y-4">
              {/* Stop header */}
              <Card>
                <CardHeader>
                  <div className="flex items-center gap-2 text-sm text-muted-foreground mb-2">
                    <MapPin className="h-4 w-4" />
                    <span>Stop {currentStopIndex + 1}</span>
                    <span>•</span>
                    <FileCode className="h-4 w-4" />
                    <Link href={`/file/${currentStop.file}?repo=${encodeURIComponent(repoPath)}`}>
                      <span className="hover:underline">{currentStop.file}</span>
                    </Link>
                    <span>Line {currentStop.line}</span>
                  </div>
                  <CardTitle>{currentStop.title}</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="prose prose-sm dark:prose-invert max-w-none">
                    <Streamdown>{currentStop.content}</Streamdown>
                  </div>
                  
                  {currentStop.questions && currentStop.questions.length > 0 && (
                    <div className="mt-4 p-4 bg-muted/50 rounded-lg">
                      <h4 className="font-medium mb-2">Questions to consider:</h4>
                      <ul className="list-disc list-inside space-y-1 text-sm text-muted-foreground">
                        {currentStop.questions.map((q: string, i: number) => (
                          <li key={i}>{q}</li>
                        ))}
                      </ul>
                    </div>
                  )}
                </CardContent>
              </Card>

              {/* Code view */}
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm flex items-center gap-2">
                    <Code2 className="h-4 w-4" />
                    {currentStop.file}
                  </CardTitle>
                </CardHeader>
                <CardContent className="p-0">
                  <div className="overflow-x-auto">
                    <table className="w-full text-sm font-mono">
                      <tbody>
                        {codeLines.map((line) => (
                          <tr
                            key={line.number}
                            className={line.isHighlighted ? "bg-yellow-500/10" : ""}
                          >
                            <td className="w-12 px-3 py-0.5 text-right text-muted-foreground select-none border-r border-border">
                              {line.number}
                            </td>
                            <td className="px-4 py-0.5 whitespace-pre">
                              <span dangerouslySetInnerHTML={{ __html: line.content || "&nbsp;" }} />
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </CardContent>
              </Card>

              {/* Navigation */}
              <div className="flex justify-between">
                <Button
                  variant="outline"
                  onClick={() => {
                    if (currentStopIndex > 0) {
                      navigateToStop(guide.stops[currentStopIndex - 1].id);
                    }
                  }}
                  disabled={currentStopIndex === 0}
                >
                  <ChevronLeft className="h-4 w-4 mr-2" />
                  Previous
                </Button>
                
                <Button
                  onClick={() => {
                    if (currentStopIndex < guide.stops.length - 1) {
                      navigateToStop(guide.stops[currentStopIndex + 1].id);
                    }
                  }}
                  disabled={currentStopIndex === guide.stops.length - 1}
                >
                  Next
                  <ChevronRight className="h-4 w-4 ml-2" />
                </Button>
              </div>
            </div>

            {/* Sidebar - Stop list */}
            <div className="space-y-4">
              <Card>
                <CardHeader>
                  <CardTitle className="text-sm">Tour Stops</CardTitle>
                </CardHeader>
                <CardContent className="p-0">
                  <div className="divide-y divide-border">
                    {guide.stops.map((stop, index) => {
                      const isVisited = visitedStops.includes(stop.id);
                      const isCurrent = stop.id === currentStopId;
                      
                      return (
                        <button
                          key={stop.id}
                          onClick={() => navigateToStop(stop.id)}
                          className={`w-full text-left px-4 py-3 hover:bg-muted/50 transition-colors ${
                            isCurrent ? "bg-primary/10" : ""
                          }`}
                        >
                          <div className="flex items-center gap-3">
                            <div className={`w-6 h-6 rounded-full flex items-center justify-center text-xs ${
                              isCurrent 
                                ? "bg-primary text-primary-foreground" 
                                : isVisited 
                                ? "bg-green-500/20 text-green-500" 
                                : "bg-muted text-muted-foreground"
                            }`}>
                              {index + 1}
                            </div>
                            <div className="flex-1 min-w-0">
                              <p className={`text-sm truncate ${isCurrent ? "font-medium" : ""}`}>
                                {stop.title}
                              </p>
                              <p className="text-xs text-muted-foreground truncate">
                                {stop.file}:{stop.line}
                              </p>
                            </div>
                          </div>
                        </button>
                      );
                    })}
                  </div>
                </CardContent>
              </Card>

              {/* Guide info */}
              <Card>
                <CardHeader>
                  <CardTitle className="text-sm">About This Guide</CardTitle>
                </CardHeader>
                <CardContent className="text-sm space-y-2">
                  {guide.description && (
                    <p className="text-muted-foreground">{guide.description}</p>
                  )}
                  {guide.difficulty && (
                    <div>
                      <span className="text-muted-foreground">Difficulty:</span>{" "}
                      <Badge variant="outline">{guide.difficulty}</Badge>
                    </div>
                  )}
                  {guide.estimatedTime && (
                    <div>
                      <span className="text-muted-foreground">Time:</span>{" "}
                      {guide.estimatedTime}
                    </div>
                  )}
                  {guide.prerequisites && guide.prerequisites.length > 0 && (
                    <div>
                      <span className="text-muted-foreground">Prerequisites:</span>
                      <ul className="list-disc list-inside mt-1">
                        {guide.prerequisites.map((p: string, i: number) => (
                          <li key={i} className="text-xs">{p}</li>
                        ))}
                      </ul>
                    </div>
                  )}
                </CardContent>
              </Card>

              {/* Completion */}
              {progress === 100 && (
                <Card className="border-green-500">
                  <CardContent className="pt-6 text-center">
                    <div className="text-green-500 mb-2">🎉</div>
                    <p className="font-medium">Guide Complete!</p>
                    <p className="text-sm text-muted-foreground mb-4">
                      You've visited all stops in this guide.
                    </p>
                    <Link href={`/guides?repo=${encodeURIComponent(repoPath)}`}>
                      <Button variant="outline" size="sm">
                        <Home className="h-4 w-4 mr-2" />
                        Back to Guides
                      </Button>
                    </Link>
                  </CardContent>
                </Card>
              )}
            </div>
          </div>
        )}
      </main>
    </div>
  );
}
