import React, { useState, useEffect, useRef } from 'react';
import { useParams, useLocation } from 'wouter';
import { trpc } from '@/lib/trpc';
import { useAuth } from '@/_core/hooks/useAuth';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Textarea } from '@/components/ui/textarea';
import { Switch } from '@/components/ui/switch';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Tooltip, TooltipContent, TooltipTrigger } from '@/components/ui/tooltip';
import { Separator } from '@/components/ui/separator';
import { ScrollArea } from '@/components/ui/scroll-area';
import { MarkdownRenderer } from '@/components/MarkdownRenderer';
import { presets, presetCategories, getPresetById } from '@/lib/presets';
import { widgets, formWrapper } from '@/lib/widgets';
import { toast } from 'sonner';
import { 
  ArrowLeft, 
  Save, 
  Eye, 
  Code, 
  Loader2, 
  FileText, 
  Sparkles,
  Plus,
  Wand2
} from 'lucide-react';
import { getLoginUrl } from '@/const';

export default function DocumentEditor() {
  const params = useParams<{ id?: string }>();
  const [, navigate] = useLocation();
  const { user, loading: authLoading, isAuthenticated } = useAuth();
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  
  const isEditing = !!params.id;
  const documentId = params.id ? parseInt(params.id) : undefined;

  const [title, setTitle] = useState('');
  const [content, setContent] = useState('');
  const [description, setDescription] = useState('');
  const [category, setCategory] = useState('');
  const [isPublished, setIsPublished] = useState(false);
  const [activeTab, setActiveTab] = useState('edit');
  const [selectedPresetCategory, setSelectedPresetCategory] = useState<string>('all');

  // Fetch existing document if editing
  const { data: existingDoc, isLoading: docLoading } = trpc.documents.getById.useQuery(
    { id: documentId! },
    { enabled: isEditing && !!documentId }
  );

  // Populate form when document loads
  useEffect(() => {
    if (existingDoc) {
      setTitle(existingDoc.title);
      setContent(existingDoc.content);
      setDescription(existingDoc.description || '');
      setCategory(existingDoc.category || '');
      setIsPublished(existingDoc.isPublished);
    }
  }, [existingDoc]);

  const utils = trpc.useUtils();

  const createMutation = trpc.documents.create.useMutation({
    onSuccess: (data) => {
      toast.success('Document created successfully!');
      utils.documents.list.invalidate();
      utils.documents.myDocuments.invalidate();
      navigate(`/documents/${data.slug}`);
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to create document');
    },
  });

  const updateMutation = trpc.documents.update.useMutation({
    onSuccess: () => {
      toast.success('Document updated successfully!');
      utils.documents.list.invalidate();
      utils.documents.myDocuments.invalidate();
      if (documentId) {
        utils.documents.getById.invalidate({ id: documentId });
      }
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to update document');
    },
  });

  const handleSave = () => {
    if (!title.trim()) {
      toast.error('Title is required');
      return;
    }
    if (!content.trim()) {
      toast.error('Content is required');
      return;
    }

    if (isEditing && documentId) {
      updateMutation.mutate({
        id: documentId,
        title,
        content,
        description: description || undefined,
        category: category || undefined,
        isPublished,
      });
    } else {
      createMutation.mutate({
        title,
        content,
        description: description || undefined,
        category: category || undefined,
        isPublished,
      });
    }
  };

  const insertAtCursor = (text: string) => {
    const textarea = textareaRef.current;
    if (!textarea) {
      setContent(prev => prev + '\n' + text);
      return;
    }

    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const before = content.substring(0, start);
    const after = content.substring(end);
    
    // Add newlines if needed
    const prefix = before.length > 0 && !before.endsWith('\n') ? '\n' : '';
    const suffix = after.length > 0 && !after.startsWith('\n') ? '\n' : '';
    
    const newContent = before + prefix + text + suffix + after;
    setContent(newContent);
    
    // Focus and set cursor position after the inserted text
    setTimeout(() => {
      textarea.focus();
      const newPosition = start + prefix.length + text.length;
      textarea.setSelectionRange(newPosition, newPosition);
    }, 0);
    
    toast.success('Inserted!');
  };

  const loadPreset = (presetId: string) => {
    const preset = getPresetById(presetId);
    if (preset) {
      setTitle(preset.title);
      setContent(preset.content);
      setCategory(preset.category);
      toast.success(`Loaded "${preset.name}" preset`);
    }
  };

  const insertWidget = (snippet: string) => {
    insertAtCursor(snippet);
  };

  const insertFormWrapper = () => {
    const formId = `quiz-${Date.now().toString(36)}`;
    const snippet = `<form id="${formId}">
name: Quiz Title
description: Quiz description here
fields:
</form>`;
    insertAtCursor(snippet);
  };

  const isSaving = createMutation.isPending || updateMutation.isPending;

  const filteredPresets = selectedPresetCategory === 'all' 
    ? presets 
    : presets.filter(p => p.category === selectedPresetCategory);

  if (authLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-blue-600" />
      </div>
    );
  }

  if (!isAuthenticated) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-slate-50 dark:bg-slate-950">
        <Card className="w-full max-w-md mx-4">
          <CardHeader>
            <CardTitle>Sign In Required</CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-muted-foreground mb-4">
              You need to sign in to create or edit documents.
            </p>
            <Button asChild className="w-full">
              <a href={getLoginUrl()}>Sign In</a>
            </Button>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (docLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-blue-600" />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-slate-50 dark:bg-slate-950">
      {/* Header */}
      <header className="sticky top-0 z-50 bg-white dark:bg-slate-900 border-b border-slate-200 dark:border-slate-800">
        <div className="container flex items-center justify-between h-16">
          <div className="flex items-center gap-4">
            <Button variant="ghost" size="icon" onClick={() => navigate('/admin')}>
              <ArrowLeft className="h-5 w-5" />
            </Button>
            <h1 className="text-lg font-semibold">
              {isEditing ? 'Edit Document' : 'New Document'}
            </h1>
          </div>
          <Button onClick={handleSave} disabled={isSaving}>
            {isSaving ? (
              <>
                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                Saving...
              </>
            ) : (
              <>
                <Save className="mr-2 h-4 w-4" />
                Save
              </>
            )}
          </Button>
        </div>
      </header>

      <main className="container py-6">
        <div className="grid lg:grid-cols-3 gap-6">
          {/* Sidebar */}
          <div className="lg:col-span-1 space-y-6">
            {/* Document Settings */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Document Settings</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <Label htmlFor="title">Title</Label>
                  <Input
                    id="title"
                    value={title}
                    onChange={(e) => setTitle(e.target.value)}
                    placeholder="Enter document title"
                  />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="description">Description</Label>
                  <Textarea
                    id="description"
                    value={description}
                    onChange={(e) => setDescription(e.target.value)}
                    placeholder="Brief description of the document"
                    rows={2}
                  />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="category">Category</Label>
                  <Input
                    id="category"
                    value={category}
                    onChange={(e) => setCategory(e.target.value)}
                    placeholder="e.g., Tutorial, Quiz, Guide"
                  />
                </div>
                <div className="flex items-center justify-between">
                  <Label htmlFor="published">Published</Label>
                  <Switch
                    id="published"
                    checked={isPublished}
                    onCheckedChange={setIsPublished}
                  />
                </div>
              </CardContent>
            </Card>

            {/* Preset Templates */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base flex items-center gap-2">
                  <Sparkles className="h-4 w-4 text-yellow-500" />
                  Preset Templates
                </CardTitle>
                <CardDescription>
                  Load a pre-built template to get started quickly
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-3">
                <Select value={selectedPresetCategory} onValueChange={setSelectedPresetCategory}>
                  <SelectTrigger>
                    <SelectValue placeholder="Filter by category" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All Categories</SelectItem>
                    {presetCategories.map(cat => (
                      <SelectItem key={cat} value={cat}>{cat}</SelectItem>
                    ))}
                  </SelectContent>
                </Select>
                <ScrollArea className="h-48">
                  <div className="space-y-2 pr-4">
                    {filteredPresets.map(preset => (
                      <button
                        key={preset.id}
                        onClick={() => loadPreset(preset.id)}
                        className="w-full text-left p-3 rounded-lg border border-slate-200 dark:border-slate-700 hover:bg-slate-50 dark:hover:bg-slate-800 transition-colors"
                      >
                        <div className="font-medium text-sm">{preset.name}</div>
                        <div className="text-xs text-muted-foreground mt-1">
                          {preset.description}
                        </div>
                      </button>
                    ))}
                  </div>
                </ScrollArea>
              </CardContent>
            </Card>

            {/* Quick Insert Widgets */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base flex items-center gap-2">
                  <Wand2 className="h-4 w-4 text-purple-500" />
                  Quick Insert
                </CardTitle>
                <CardDescription>
                  Click to insert form fields at cursor
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-3">
                  {/* Form Wrapper Button */}
                  <Button 
                    variant="outline" 
                    className="w-full justify-start gap-2"
                    onClick={insertFormWrapper}
                  >
                    <FileText className="h-4 w-4 text-blue-500" />
                    <span>New Form Block</span>
                  </Button>
                  
                  <Separator />
                  
                  {/* Field Widgets */}
                  <div className="grid grid-cols-2 gap-2">
                    {widgets.map(widget => {
                      const Icon = widget.icon;
                      return (
                        <Tooltip key={widget.id}>
                          <TooltipTrigger asChild>
                            <Button
                              variant="outline"
                              size="sm"
                              className="justify-start gap-2 h-auto py-2"
                              onClick={() => insertWidget(widget.snippet)}
                            >
                              <Icon className="h-4 w-4 text-muted-foreground" />
                              <span className="text-xs">{widget.name}</span>
                            </Button>
                          </TooltipTrigger>
                          <TooltipContent side="top">
                            <p>{widget.description}</p>
                          </TooltipContent>
                        </Tooltip>
                      );
                    })}
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Main Content - Editor/Preview */}
          <div className="lg:col-span-2">
            <Card className="h-full">
              <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full">
                <CardHeader className="pb-0">
                  <TabsList>
                    <TabsTrigger value="edit" className="gap-2">
                      <Code className="h-4 w-4" />
                      Edit
                    </TabsTrigger>
                    <TabsTrigger value="preview" className="gap-2">
                      <Eye className="h-4 w-4" />
                      Preview
                    </TabsTrigger>
                  </TabsList>
                </CardHeader>
                <CardContent className="pt-4">
                  <TabsContent value="edit" className="mt-0">
                    <Textarea
                      ref={textareaRef}
                      value={content}
                      onChange={(e) => setContent(e.target.value)}
                      placeholder="Write your markdown content here...

You can embed quizzes like this:

<form id=my-quiz>
name: My Quiz
fields:
  - name: answer
    label: What is your answer?
    type: text
</form>

Or use the Quick Insert buttons on the left to add form fields!"
                      className="min-h-[600px] font-mono text-sm"
                    />
                  </TabsContent>
                  <TabsContent value="preview" className="mt-0">
                    <div className="min-h-[600px] border rounded-lg p-4 bg-white dark:bg-slate-900 overflow-auto">
                      {content ? (
                        <MarkdownRenderer
                          content={content}
                          documentId={documentId || 0}
                          readOnly={true}
                        />
                      ) : (
                        <p className="text-muted-foreground text-center py-12">
                          Start writing to see the preview
                        </p>
                      )}
                    </div>
                  </TabsContent>
                </CardContent>
              </Tabs>
            </Card>
          </div>
        </div>
      </main>
    </div>
  );
}
