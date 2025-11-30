import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Loader2, Download, FileText, ChevronLeft, AlertCircle, CheckCircle, Clock } from "lucide-react";
import { trpc } from "@/lib/trpc";
import { useAuth } from "@/_core/hooks/useAuth";
import { getLoginUrl } from "@/const";
import { useLocation } from "wouter";

export default function Jobs() {
  const { user, loading: authLoading } = useAuth();
  const [, setLocation] = useLocation();
  const [selectedJobId, setSelectedJobId] = useState<number | null>(null);

  // Fetch PDF jobs
  const { data: jobs = [], isLoading, refetch } = trpc.pdf.listJobs.useQuery(undefined, {
    enabled: !!user,
    refetchInterval: 5000, // Refresh every 5 seconds to show progress
  });

  // Fetch selected job details
  const { data: selectedJob } = trpc.pdf.getJob.useQuery(
    { jobId: selectedJobId! },
    { enabled: !!selectedJobId, refetchInterval: 2000 }
  );

  const getStatusIcon = (status: string) => {
    switch (status) {
      case "completed":
        return <CheckCircle className="w-5 h-5 text-green-500" />;
      case "failed":
        return <AlertCircle className="w-5 h-5 text-red-500" />;
      case "processing":
        return <Loader2 className="w-5 h-5 text-blue-500 animate-spin" />;
      default:
        return <Clock className="w-5 h-5 text-yellow-500" />;
    }
  };

  const getStatusBadge = (status: string) => {
    const variants: Record<string, "default" | "secondary" | "destructive" | "outline"> = {
      completed: "default",
      failed: "destructive",
      processing: "secondary",
      pending: "outline",
    };

    return (
      <Badge variant={variants[status] || "outline"} className="capitalize">
        {status}
      </Badge>
    );
  };

  const parseLogs = (logsJson: string | null) => {
    if (!logsJson) return [];
    try {
      return JSON.parse(logsJson);
    } catch {
      return [];
    }
  };

  // Show login prompt if not authenticated
  if (!authLoading && !user) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background">
        <div className="text-center max-w-md">
          <h2 className="text-2xl font-semibold mb-4 text-foreground">Please Log In</h2>
          <p className="text-muted-foreground mb-6">
            You need to be logged in to view your PDF jobs.
          </p>
          <Button onClick={() => window.location.href = getLoginUrl()} size="lg">
            Log In
          </Button>
        </div>
      </div>
    );
  }

  // Show loading state
  if (authLoading || isLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background">
        <Loader2 className="w-8 h-8 animate-spin text-primary" />
      </div>
    );
  }

  return (
    <div className="min-h-screen flex flex-col bg-background text-foreground">
      {/* Header */}
      <header className="border-b border-border bg-card">
        <div className="container py-3 md:py-4 flex items-center justify-between">
          <div className="flex items-center gap-3">
            <Button onClick={() => setLocation("/")} variant="ghost" size="sm">
              <ChevronLeft className="w-4 h-4 mr-1" />
              Back
            </Button>
            <h1 className="text-lg md:text-2xl font-bold text-foreground">PDF Jobs</h1>
          </div>
          <Button onClick={() => refetch()} variant="outline" size="sm">
            Refresh
          </Button>
        </div>
      </header>

      {/* Main Content */}
      <main className="flex-1 container py-6">
        {jobs.length === 0 ? (
          <div className="flex items-center justify-center h-full">
            <div className="text-center max-w-md">
              <FileText className="w-16 h-16 mx-auto mb-4 text-muted-foreground" />
              <h2 className="text-2xl font-semibold mb-2 text-foreground">No PDF Jobs Yet</h2>
              <p className="text-muted-foreground mb-6">
                Create a photobook and export it as PDF to see your jobs here.
              </p>
              <Button onClick={() => setLocation("/")} size="lg">
                Go to Photobook
              </Button>
            </div>
          </div>
        ) : (
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Jobs List */}
            <div className="space-y-4">
              <h2 className="text-xl font-semibold">All Jobs ({jobs.length})</h2>
              {jobs.map((job) => (
                <Card
                  key={job.id}
                  className={`cursor-pointer transition-all ${
                    selectedJobId === job.id ? "ring-2 ring-primary" : ""
                  }`}
                  onClick={() => setSelectedJobId(job.id)}
                >
                  <CardHeader>
                    <div className="flex items-start justify-between">
                      <div className="flex items-center gap-2">
                        {getStatusIcon(job.status)}
                        <CardTitle className="text-lg">Job #{job.id}</CardTitle>
                      </div>
                      {getStatusBadge(job.status)}
                    </div>
                    <CardDescription>
                      Created {new Date(job.createdAt).toLocaleString()}
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="space-y-2 text-sm">
                      <div className="flex justify-between">
                        <span className="text-muted-foreground">Photos:</span>
                        <span className="font-medium">
                          {JSON.parse(job.photoIds).length} images
                        </span>
                      </div>
                      {job.completedAt && (
                        <div className="flex justify-between">
                          <span className="text-muted-foreground">Completed:</span>
                          <span className="font-medium">
                            {new Date(job.completedAt).toLocaleString()}
                          </span>
                        </div>
                      )}
                      {job.status === "completed" && job.resultUrl && (
                        <Button
                          onClick={(e) => {
                            e.stopPropagation();
                            window.open(job.resultUrl!, "_blank");
                          }}
                          variant="default"
                          size="sm"
                          className="w-full mt-2"
                        >
                          <Download className="w-4 h-4 mr-2" />
                          Download PDF
                        </Button>
                      )}
                      {job.status === "failed" && job.errorMessage && (
                        <div className="mt-2 p-2 bg-destructive/10 rounded text-destructive text-xs">
                          Error: {job.errorMessage}
                        </div>
                      )}
                    </div>
                  </CardContent>
                </Card>
              ))}
            </div>

            {/* Job Details */}
            <div className="lg:sticky lg:top-6 lg:self-start">
              {selectedJob ? (
                <Card>
                  <CardHeader>
                    <div className="flex items-center justify-between">
                      <CardTitle>Job Details #{selectedJob.id}</CardTitle>
                      {getStatusBadge(selectedJob.status)}
                    </div>
                    <CardDescription>
                      View detailed logs and information
                    </CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <div className="space-y-2">
                      <h3 className="font-semibold">Information</h3>
                      <div className="space-y-1 text-sm">
                        <div className="flex justify-between">
                          <span className="text-muted-foreground">Status:</span>
                          <span className="capitalize font-medium">{selectedJob.status}</span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-muted-foreground">Photos:</span>
                          <span className="font-medium">
                            {JSON.parse(selectedJob.photoIds).length} images
                          </span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-muted-foreground">Created:</span>
                          <span className="font-medium">
                            {new Date(selectedJob.createdAt).toLocaleString()}
                          </span>
                        </div>
                        {selectedJob.completedAt && (
                          <div className="flex justify-between">
                            <span className="text-muted-foreground">Completed:</span>
                            <span className="font-medium">
                              {new Date(selectedJob.completedAt).toLocaleString()}
                            </span>
                          </div>
                        )}
                      </div>
                    </div>

                    {selectedJob.logs && (
                      <div className="space-y-2">
                        <h3 className="font-semibold">Processing Logs</h3>
                        <div className="bg-muted rounded-lg p-3 max-h-96 overflow-y-auto">
                          <div className="space-y-1 font-mono text-xs">
                            {parseLogs(selectedJob.logs).map((log: any, idx: number) => (
                              <div key={idx} className="flex gap-2">
                                <span className="text-muted-foreground shrink-0">
                                  {new Date(log.timestamp).toLocaleTimeString()}
                                </span>
                                <span
                                  className={`${
                                    log.level === "error"
                                      ? "text-red-500"
                                      : log.level === "warn"
                                      ? "text-yellow-500"
                                      : "text-foreground"
                                  }`}
                                >
                                  [{log.level.toUpperCase()}] {log.message}
                                </span>
                              </div>
                            ))}
                          </div>
                        </div>
                      </div>
                    )}

                    {selectedJob.status === "completed" && selectedJob.resultUrl && (
                      <Button
                        onClick={() => window.open(selectedJob.resultUrl!, "_blank")}
                        variant="default"
                        size="lg"
                        className="w-full"
                      >
                        <Download className="w-5 h-5 mr-2" />
                        Download PDF
                      </Button>
                    )}
                  </CardContent>
                </Card>
              ) : (
                <Card>
                  <CardContent className="flex items-center justify-center h-64">
                    <p className="text-muted-foreground">
                      Select a job to view details
                    </p>
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
