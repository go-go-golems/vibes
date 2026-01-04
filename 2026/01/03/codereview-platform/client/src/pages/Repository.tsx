import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { trpc } from "@/lib/trpc";
import { Code2, FolderGit2, ArrowLeft } from "lucide-react";
import { useState } from "react";
import { Link } from "wouter";
import { toast } from "sonner";

// Default test repository path
const DEFAULT_REPO_PATH = "/home/ubuntu/test-repo";

export default function Repository() {
  const [repoPath, setRepoPath] = useState(DEFAULT_REPO_PATH);
  
  const branchesQuery = trpc.repository.branches.useQuery(
    { repoPath },
    { enabled: !!repoPath }
  );

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
            <span className="text-xl font-bold">Repository Settings</span>
          </div>
        </div>
      </header>

      <main className="container py-8">
        <div className="max-w-2xl mx-auto space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <FolderGit2 className="h-5 w-5" />
                Repository Path
              </CardTitle>
              <CardDescription>
                Configure the local git repository to browse
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="repoPath">Repository Path</Label>
                <Input
                  id="repoPath"
                  value={repoPath}
                  onChange={(e) => setRepoPath(e.target.value)}
                  placeholder="/path/to/repository"
                />
                <p className="text-sm text-muted-foreground">
                  Enter the absolute path to a local git repository
                </p>
              </div>

              {branchesQuery.isLoading && (
                <p className="text-sm text-muted-foreground">Checking repository...</p>
              )}

              {branchesQuery.error && (
                <p className="text-sm text-destructive">
                  Error: {branchesQuery.error.message}
                </p>
              )}

              {branchesQuery.data && (
                <div className="space-y-2">
                  <Label>Available Branches</Label>
                  <div className="flex flex-wrap gap-2">
                    {branchesQuery.data.map((branch, index) => (
                      <span
                        key={branch}
                        className={`px-2 py-1 rounded text-sm ${
                          index === 0
                            ? "bg-primary text-primary-foreground"
                            : "bg-muted text-muted-foreground"
                        }`}
                      >
                        {branch}
                      </span>
                    ))}
                  </div>
                </div>
              )}

              <div className="flex gap-2">
                <Button asChild>
                  <Link href={`/browse?repo=${encodeURIComponent(repoPath)}`}>
                    Browse Repository
                  </Link>
                </Button>
                <Button
                  variant="outline"
                  onClick={() => {
                    localStorage.setItem("repoPath", repoPath);
                    toast.success("Repository path saved");
                  }}
                >
                  Save as Default
                </Button>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Quick Links</CardTitle>
              <CardDescription>
                Navigate to different sections of the platform
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-2">
              <Link href={`/browse?repo=${encodeURIComponent(repoPath)}`}>
                <Button variant="outline" className="w-full justify-start">
                  Browse Files
                </Button>
              </Link>
              <Link href={`/reviews?repo=${encodeURIComponent(repoPath)}`}>
                <Button variant="outline" className="w-full justify-start">
                  View Code Reviews
                </Button>
              </Link>
              <Link href={`/quizzes?repo=${encodeURIComponent(repoPath)}`}>
                <Button variant="outline" className="w-full justify-start">
                  Take Quizzes
                </Button>
              </Link>
              <Link href={`/guides?repo=${encodeURIComponent(repoPath)}`}>
                <Button variant="outline" className="w-full justify-start">
                  Follow Guides
                </Button>
              </Link>
            </CardContent>
          </Card>
        </div>
      </main>
    </div>
  );
}
