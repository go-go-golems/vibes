import { Button } from "@/components/ui/button";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { trpc } from "@/lib/trpc";
import { Code2, Folder, FileCode, ArrowLeft, GitBranch, ChevronRight, Home, Loader2 } from "lucide-react";
import { useState, useMemo, useEffect } from "react";
import { Link, useRoute, useSearch } from "wouter";

export default function FileBrowser() {
  const [, routeParams] = useRoute("/browse/*");
  const search = useSearch();
  const searchParams = new URLSearchParams(search);
  const currentPath = (routeParams as Record<string, string> | null)?.["*"] || "";
  
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>();
  
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

  // Use repo from URL or demo repo
  const repoPath = searchParams.get("repo") || demoStatus.data?.path || "";
  
  const branchesQuery = trpc.repository.branches.useQuery(
    { repoPath },
    { enabled: !!repoPath && demoStatus.data?.initialized }
  );
  
  // Set default branch when loaded
  const currentBranch = useMemo(() => {
    if (selectedBranch) return selectedBranch;
    // Default to 'main' or first branch
    return branchesQuery.data?.[0] || 'main';
  }, [selectedBranch, branchesQuery.data]);
  
  const fileTreeQuery = trpc.repository.fileTree.useQuery(
    { repoPath, branch: currentBranch, path: currentPath },
    { enabled: !!currentBranch && !!repoPath }
  );

  // Build breadcrumb parts
  const pathParts = currentPath ? currentPath.split("/").filter(Boolean) : [];

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
            <span className="text-xl font-bold">File Browser</span>
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
                {branchesQuery.data?.map((branch, index) => (
                  <SelectItem key={branch} value={branch}>
                    {branch}
                    {index === 0 && " (current)"}
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
          {pathParts.map((part: string, index: number) => {
            const pathTo = pathParts.slice(0, index + 1).join("/");
            return (
              <div key={pathTo} className="flex items-center">
                <ChevronRight className="h-4 w-4 text-muted-foreground" />
                <Link href={`/browse/${pathTo}?repo=${encodeURIComponent(repoPath)}`}>
                  <Button variant="ghost" size="sm" className="h-7 px-2">
                    {part}
                  </Button>
                </Link>
              </div>
            );
          })}
        </nav>

        {/* File tree */}
        <div className="border border-border rounded-lg bg-card">
          {fileTreeQuery.isLoading && (
            <div className="p-4 text-muted-foreground">Loading...</div>
          )}
          
          {fileTreeQuery.error && (
            <div className="p-4 text-destructive">
              Error: {fileTreeQuery.error.message}
            </div>
          )}
          
          {fileTreeQuery.data?.length === 0 && (
            <div className="p-4 text-muted-foreground">Empty directory</div>
          )}
          
          {fileTreeQuery.data?.map((item) => (
            <Link
              key={item.path}
              href={
                item.type === "directory"
                  ? `/browse/${item.path}?repo=${encodeURIComponent(repoPath)}`
                  : `/file/${item.path}?repo=${encodeURIComponent(repoPath)}&branch=${currentBranch || ""}`
              }
            >
              <div className="flex items-center gap-3 px-4 py-3 hover:bg-muted/50 border-b border-border last:border-b-0 cursor-pointer transition-colors">
                {item.type === "directory" ? (
                  <Folder className="h-5 w-5 text-blue-400" />
                ) : (
                  <FileCode className="h-5 w-5 text-muted-foreground" />
                )}
                <span className={item.type === "directory" ? "font-medium" : ""}>
                  {item.name}
                </span>
              </div>
            </Link>
          ))}
        </div>

        {/* Quick navigation */}
        <div className="mt-6 flex gap-4">
          <Link href={`/reviews?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="outline">View Reviews</Button>
          </Link>
          <Link href={`/quizzes?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="outline">Take Quizzes</Button>
          </Link>
          <Link href={`/guides?repo=${encodeURIComponent(repoPath)}`}>
            <Button variant="outline">Follow Guides</Button>
          </Link>
        </div>
      </main>
    </div>
  );
}
