import React, { useState } from 'react';
import { useLocation } from 'wouter';
import { trpc } from '@/lib/trpc';
import { useAuth } from '@/_core/hooks/useAuth';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { 
  FileText, 
  Search, 
  LogIn, 
  Settings, 
  Loader2,
  BookOpen,
  ArrowRight,
  Calendar
} from 'lucide-react';
import { getLoginUrl } from '@/const';
import { formatDistanceToNow } from 'date-fns';

export default function Home() {
  const [, navigate] = useLocation();
  const { user, loading: authLoading, isAuthenticated, logout } = useAuth();
  const [searchQuery, setSearchQuery] = useState('');

  const { data: documents, isLoading } = trpc.documents.list.useQuery();

  const filteredDocuments = documents?.filter(doc =>
    doc.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
    doc.description?.toLowerCase().includes(searchQuery.toLowerCase()) ||
    doc.category?.toLowerCase().includes(searchQuery.toLowerCase())
  );

  // Group documents by category
  const groupedDocuments = filteredDocuments?.reduce((acc, doc) => {
    const category = doc.category || 'Uncategorized';
    if (!acc[category]) acc[category] = [];
    acc[category].push(doc);
    return acc;
  }, {} as Record<string, typeof documents>);

  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-50 to-white dark:from-slate-950 dark:to-slate-900">
      {/* Header */}
      <header className="sticky top-0 z-50 bg-white/80 dark:bg-slate-900/80 backdrop-blur-sm border-b border-slate-200 dark:border-slate-800">
        <div className="container flex items-center justify-between h-16">
          <div className="flex items-center gap-3">
            <BookOpen className="h-6 w-6 text-blue-600" />
            <span className="font-semibold text-lg">Quiz Docs</span>
          </div>
          <div className="flex items-center gap-3">
            {authLoading ? (
              <Loader2 className="h-5 w-5 animate-spin" />
            ) : isAuthenticated ? (
              <>
                <Button variant="outline" onClick={() => navigate('/admin')}>
                  <Settings className="mr-2 h-4 w-4" />
                  Manage
                </Button>
                <Button variant="ghost" onClick={() => logout()}>
                  Sign Out
                </Button>
              </>
            ) : (
              <Button asChild>
                <a href={getLoginUrl()}>
                  <LogIn className="mr-2 h-4 w-4" />
                  Sign In
                </a>
              </Button>
            )}
          </div>
        </div>
      </header>

      {/* Hero Section */}
      <section className="container py-16 md:py-24">
        <div className="max-w-3xl mx-auto text-center">
          <h1 className="text-4xl md:text-5xl font-bold tracking-tight mb-6">
            Interactive Documents with{' '}
            <span className="text-blue-600">Embedded Quizzes</span>
          </h1>
          <p className="text-xl text-muted-foreground mb-8">
            Browse educational content with built-in assessments. Test your knowledge as you learn.
          </p>
          
          {/* Search */}
          <div className="relative max-w-xl mx-auto">
            <Search className="absolute left-4 top-1/2 -translate-y-1/2 h-5 w-5 text-muted-foreground" />
            <Input
              placeholder="Search documents..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              className="pl-12 h-12 text-lg"
            />
          </div>
        </div>
      </section>

      {/* Documents Section */}
      <section className="container pb-16">
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-8 w-8 animate-spin text-blue-600" />
          </div>
        ) : filteredDocuments && filteredDocuments.length > 0 ? (
          <div className="space-y-12">
            {Object.entries(groupedDocuments || {}).map(([category, docs]) => (
              <div key={category}>
                <h2 className="text-2xl font-semibold mb-6 flex items-center gap-2">
                  <Badge variant="secondary" className="text-base px-3 py-1">
                    {category}
                  </Badge>
                  <span className="text-muted-foreground text-sm font-normal">
                    {docs?.length} document{docs?.length !== 1 ? 's' : ''}
                  </span>
                </h2>
                <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6">
                  {docs?.map((doc) => (
                    <Card 
                      key={doc.id} 
                      className="group hover:shadow-lg transition-all duration-200 cursor-pointer border-slate-200 dark:border-slate-800"
                      onClick={() => navigate(`/documents/${doc.slug}`)}
                    >
                      <CardHeader>
                        <div className="flex items-start justify-between">
                          <FileText className="h-10 w-10 text-blue-500 mb-2" />
                          <ArrowRight className="h-5 w-5 text-muted-foreground opacity-0 group-hover:opacity-100 transition-opacity" />
                        </div>
                        <CardTitle className="group-hover:text-blue-600 transition-colors">
                          {doc.title}
                        </CardTitle>
                        {doc.description && (
                          <CardDescription className="line-clamp-2">
                            {doc.description}
                          </CardDescription>
                        )}
                      </CardHeader>
                      <CardContent>
                        <div className="flex items-center gap-2 text-sm text-muted-foreground">
                          <Calendar className="h-4 w-4" />
                          {formatDistanceToNow(new Date(doc.updatedAt), { addSuffix: true })}
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </div>
            ))}
          </div>
        ) : (
          <div className="text-center py-16">
            <FileText className="h-16 w-16 mx-auto text-muted-foreground mb-4" />
            <h3 className="text-xl font-medium mb-2">
              {searchQuery ? 'No documents found' : 'No documents yet'}
            </h3>
            <p className="text-muted-foreground mb-6">
              {searchQuery 
                ? 'Try adjusting your search terms'
                : 'Be the first to create an interactive document with quizzes.'
              }
            </p>
            {isAuthenticated && !searchQuery && (
              <Button onClick={() => navigate('/admin/new')}>
                Create Document
              </Button>
            )}
          </div>
        )}
      </section>

      {/* Footer */}
      <footer className="border-t border-slate-200 dark:border-slate-800 py-8">
        <div className="container text-center text-sm text-muted-foreground">
          <p>Interactive markdown documents with embedded quizzes</p>
        </div>
      </footer>
    </div>
  );
}
