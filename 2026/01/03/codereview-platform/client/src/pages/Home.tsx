import { useAuth } from "@/_core/hooks/useAuth";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { getLoginUrl } from "@/const";
import { trpc } from "@/lib/trpc";
import { Code2, FileCode, GitBranch, GraduationCap, BookOpen, LogIn, LogOut, Loader2 } from "lucide-react";
import { Link } from "wouter";
import { useEffect } from "react";

export default function Home() {
  const { user, loading, isAuthenticated, logout } = useAuth();
  
  // Auto-initialize demo repository on first load
  const demoStatus = trpc.repository.demoStatus.useQuery();
  const initDemo = trpc.repository.initDemo.useMutation({
    onSuccess: () => {
      demoStatus.refetch();
    }
  });

  useEffect(() => {
    if (demoStatus.data && !demoStatus.data.initialized && !initDemo.isPending) {
      initDemo.mutate();
    }
  }, [demoStatus.data]);

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
      <header className="border-b border-border bg-card">
        <div className="container flex h-16 items-center justify-between">
          <div className="flex items-center gap-2">
            <Code2 className="h-6 w-6 text-primary" />
            <span className="text-xl font-bold">Code Review Platform</span>
          </div>
          <nav className="flex items-center gap-4">
            <Link href="/browse" className="text-muted-foreground hover:text-foreground transition-colors">
              Browse
            </Link>
            <Link href="/reviews" className="text-muted-foreground hover:text-foreground transition-colors">
              Reviews
            </Link>
            <Link href="/quizzes" className="text-muted-foreground hover:text-foreground transition-colors">
              Quizzes
            </Link>
            <Link href="/guides" className="text-muted-foreground hover:text-foreground transition-colors">
              Guides
            </Link>
            {loading ? null : isAuthenticated ? (
              <div className="flex items-center gap-2">
                <span className="text-sm text-muted-foreground">{user?.name}</span>
                <Button variant="ghost" size="sm" onClick={() => logout()}>
                  <LogOut className="h-4 w-4" />
                </Button>
              </div>
            ) : (
              <Button asChild size="sm">
                <a href={getLoginUrl()}>
                  <LogIn className="h-4 w-4 mr-2" />
                  Sign In
                </a>
              </Button>
            )}
          </nav>
        </div>
      </header>

      {/* Demo initialization status */}
      {initDemo.isPending && (
        <div className="bg-primary/10 border-b border-primary/20 py-2 px-4">
          <div className="container flex items-center gap-2 text-sm text-primary">
            <Loader2 className="h-4 w-4 animate-spin" />
            Initializing demo repository with sample code reviews, quizzes, and guides...
          </div>
        </div>
      )}

      {/* Hero */}
      <section className="py-20 px-4">
        <div className="container text-center">
          <h1 className="text-4xl md:text-6xl font-bold mb-6 bg-gradient-to-r from-primary to-blue-400 bg-clip-text text-transparent">
            Learn Through Code Reviews
          </h1>
          <p className="text-xl text-muted-foreground max-w-2xl mx-auto mb-8">
            An educational platform that combines code review with interactive quizzes and guided tours.
            Explore real codebases, understand best practices, and test your knowledge.
          </p>
          <div className="flex gap-4 justify-center">
            <Button asChild size="lg">
              <Link href="/browse">
                <FileCode className="h-5 w-5 mr-2" />
                Browse Code
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg">
              <Link href="/guides">
                <BookOpen className="h-5 w-5 mr-2" />
                Start Learning
              </Link>
            </Button>
          </div>
        </div>
      </section>

      {/* Features */}
      <section className="py-16 px-4 bg-muted/30">
        <div className="container">
          <h2 className="text-3xl font-bold text-center mb-12">Features</h2>
          <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-6">
            <Card className="bg-card hover:shadow-lg transition-shadow">
              <CardHeader>
                <GitBranch className="h-10 w-10 text-primary mb-2" />
                <CardTitle>Code Browser</CardTitle>
                <CardDescription>
                  Navigate through repositories with syntax highlighting and branch support
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Link href="/browse">
                  <Button variant="link" className="p-0">Browse Code →</Button>
                </Link>
              </CardContent>
            </Card>

            <Card className="bg-card hover:shadow-lg transition-shadow">
              <CardHeader>
                <Code2 className="h-10 w-10 text-green-500 mb-2" />
                <CardTitle>Code Reviews</CardTitle>
                <CardDescription>
                  Rich annotations with educational content, gotchas, and best practices
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Link href="/reviews">
                  <Button variant="link" className="p-0">View Reviews →</Button>
                </Link>
              </CardContent>
            </Card>

            <Card className="bg-card hover:shadow-lg transition-shadow">
              <CardHeader>
                <GraduationCap className="h-10 w-10 text-yellow-500 mb-2" />
                <CardTitle>Quizzes</CardTitle>
                <CardDescription>
                  Test your understanding with multiple choice, code completion, and scenario questions
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Link href="/quizzes">
                  <Button variant="link" className="p-0">Take Quizzes →</Button>
                </Link>
              </CardContent>
            </Card>

            <Card className="bg-card hover:shadow-lg transition-shadow">
              <CardHeader>
                <BookOpen className="h-10 w-10 text-purple-500 mb-2" />
                <CardTitle>Guided Tours</CardTitle>
                <CardDescription>
                  Step-by-step walkthroughs that explain code flow and architecture
                </CardDescription>
              </CardHeader>
              <CardContent>
                <Link href="/guides">
                  <Button variant="link" className="p-0">Start Tours →</Button>
                </Link>
              </CardContent>
            </Card>
          </div>
        </div>
      </section>

      {/* How it works */}
      <section className="py-16 px-4">
        <div className="container">
          <h2 className="text-3xl font-bold text-center mb-12">How It Works</h2>
          <div className="max-w-3xl mx-auto space-y-8">
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 rounded-full bg-primary text-primary-foreground flex items-center justify-center font-bold">1</div>
              <div>
                <h3 className="text-xl font-semibold mb-2">Browse the Codebase</h3>
                <p className="text-muted-foreground">
                  Navigate through files and directories with syntax highlighting. Switch between branches to see different versions of the code.
                </p>
              </div>
            </div>
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 rounded-full bg-primary text-primary-foreground flex items-center justify-center font-bold">2</div>
              <div>
                <h3 className="text-xl font-semibold mb-2">Read Code Reviews</h3>
                <p className="text-muted-foreground">
                  Explore annotated code reviews that explain why certain decisions were made, common pitfalls, and best practices.
                </p>
              </div>
            </div>
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 rounded-full bg-primary text-primary-foreground flex items-center justify-center font-bold">3</div>
              <div>
                <h3 className="text-xl font-semibold mb-2">Test Your Knowledge</h3>
                <p className="text-muted-foreground">
                  Take quizzes embedded in reviews or standalone. Track your progress and revisit topics you need to improve.
                </p>
              </div>
            </div>
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 rounded-full bg-primary text-primary-foreground flex items-center justify-center font-bold">4</div>
              <div>
                <h3 className="text-xl font-semibold mb-2">Follow Guided Tours</h3>
                <p className="text-muted-foreground">
                  Walk through the codebase step-by-step with guided tours that explain how different parts connect and work together.
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="border-t border-border py-8 px-4">
        <div className="container text-center text-muted-foreground">
          <p>Code Review Knowledge Platform • Built with Git Notes and YAML DSL</p>
        </div>
      </footer>
    </div>
  );
}
