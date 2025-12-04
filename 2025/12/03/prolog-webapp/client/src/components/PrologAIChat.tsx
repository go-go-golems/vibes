import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Textarea } from "@/components/ui/textarea";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Bug, Copy, Loader2, Plus, Sparkles } from "lucide-react";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { Label } from "@/components/ui/label";
import { trpc } from "@/lib/trpc";
import { toast } from "sonner";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "@/components/ui/dialog";

export type FactWithDocstring = { fact: string; docstring?: string };

interface PrologAIChatProps {
  mode: "facts" | "query";
  existingFacts: FactWithDocstring[];
  onFactsGenerated?: (facts: FactWithDocstring[]) => void;
  onQueryGenerated?: (query: string) => void;
}

export function PrologAIChat({ mode, existingFacts, onFactsGenerated, onQueryGenerated }: PrologAIChatProps) {
  const [input, setInput] = useState("");
  const [generatedItems, setGeneratedItems] = useState<string[]>([]);
  const [reasoning, setReasoning] = useState<string>("");
  const [docstrings, setDocstrings] = useState<Record<string, string>>({});
  const [debugInfo, setDebugInfo] = useState<any>(null);
  const [showDebug, setShowDebug] = useState(false);
  const [selectedModel, setSelectedModel] = useState<string>("gemini-2.0-flash-exp");

  const generateFactsMutation = trpc.prolog.generateFactsFromNL.useMutation({
    onSuccess: (data) => {
      console.log('[PrologAIChat] Received data:', data);
      console.log('[PrologAIChat] Docstrings:', data.docstrings);
      console.log('[PrologAIChat] Facts:', data.facts);
      if (data.facts.length > 0) {
        setGeneratedItems(data.facts);
        setReasoning(data.reasoning || "");
        setDocstrings(data.docstrings || {});
        setDebugInfo(data.debug);
        toast.success(`Generated ${data.facts.length} fact(s)/rule(s)`);
      } else {
        toast.error("No valid facts generated. Try rephrasing your description.");
      }
    },
    onError: (error) => {
      toast.error(`Failed to generate facts: ${error.message}`);
    },
  });

  const generateQueryMutation = trpc.prolog.generateQueryFromNL.useMutation({
    onSuccess: (data) => {
      if (data.query) {
        setGeneratedItems([data.query]);
        setReasoning(data.reasoning || "");
        setDebugInfo(data.debug);
        toast.success("Query generated successfully!");
      } else {
        toast.error("No valid query generated. Try rephrasing your question.");
      }
    },
    onError: (error) => {
      toast.error(`Failed to generate query: ${error.message}`);
    },
  });

  const handleGenerate = () => {
    if (!input.trim()) {
      toast.error("Please enter a description or question");
      return;
    }

    setDebugInfo(null); // Clear previous debug info
    setReasoning("");
    setDocstrings({});

    if (mode === "facts") {
      generateFactsMutation.mutate({
        description: input,
        existingFacts: existingFacts.length > 0 ? existingFacts.map(f => f.fact) : undefined,
        model: selectedModel,
      });
    } else {
      if (existingFacts.length === 0) {
        toast.error("Please add some facts first before generating queries");
        return;
      }
      // Extract docstrings from existing facts
      const docstringsMap: Record<string, string> = {};
      existingFacts.forEach(f => {
        if (f.docstring) {
          docstringsMap[f.fact] = f.docstring;
        }
      });
      
      generateQueryMutation.mutate({
        question: input,
        facts: existingFacts.map(f => f.fact),
        docstrings: Object.keys(docstringsMap).length > 0 ? docstringsMap : undefined,
        model: selectedModel,
      });
    }
  };

  const handleAddAll = () => {
    if (mode === "facts" && onFactsGenerated) {
      // Convert generated items to fact objects with docstrings
      const factsWithDocstrings: FactWithDocstring[] = generatedItems.map(fact => ({
        fact,
        docstring: docstrings[fact]
      }));
      onFactsGenerated(factsWithDocstrings);
      setGeneratedItems([]);
      setInput("");
      setDebugInfo(null);
      setReasoning("");
      setDocstrings({});
      toast.success("Facts added to knowledge base!");
    } else if (mode === "query" && onQueryGenerated && generatedItems[0]) {
      onQueryGenerated(generatedItems[0]);
      setGeneratedItems([]);
      setInput("");
      setDebugInfo(null);
      setReasoning("");
      toast.success("Query added!");
    }
  };

  const isLoading = generateFactsMutation.isPending || generateQueryMutation.isPending;

  return (
    <div className="space-y-4">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Sparkles className="h-5 w-5" />
            AI Assistant
          </CardTitle>
          <CardDescription>
            {mode === "facts"
              ? "Describe facts or rules in natural language, and AI will generate Prolog syntax"
              : "Ask a question in natural language, and AI will generate a Prolog query"}
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="model-select">Model</Label>
            <Select value={selectedModel} onValueChange={setSelectedModel}>
              <SelectTrigger id="model-select">
                <SelectValue placeholder="Select a model" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="gemini-2.0-flash-exp">Gemini 2.0 Flash (Experimental)</SelectItem>
                <SelectItem value="gemini-2.5-flash">Gemini 2.5 Flash</SelectItem>
                <SelectItem value="gpt-4o">GPT-4o</SelectItem>
                <SelectItem value="gpt-4o-mini">GPT-4o Mini</SelectItem>
                <SelectItem value="claude-3-5-sonnet-20241022">Claude 3.5 Sonnet</SelectItem>
                <SelectItem value="claude-3-5-haiku-20241022">Claude 3.5 Haiku</SelectItem>
              </SelectContent>
            </Select>
          </div>
          
          <Textarea
            placeholder={
              mode === "facts"
                ? "e.g., Tom is the parent of Bob. Alice is the parent of Tom. A grandparent is someone who is a parent of a parent."
                : "e.g., Who are the grandparents of Bob? What colors do we know about?"
            }
            value={input}
            onChange={(e) => setInput(e.target.value)}
            rows={4}
            disabled={isLoading}
          />
          
          <div className="flex gap-2">
            <Button
              onClick={handleGenerate}
              disabled={isLoading || !input.trim()}
              className="flex-1"
            >
              {isLoading ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Generating...
                </>
              ) : (
                <>
                  <Sparkles className="mr-2 h-4 w-4" />
                  {mode === "facts" ? "Generate Facts/Rules" : "Generate Query"}
                </>
              )}
            </Button>
            
            {debugInfo && (
              <Dialog open={showDebug} onOpenChange={setShowDebug}>
                <DialogTrigger asChild>
                  <Button variant="outline" size="icon">
                    <Bug className="h-4 w-4" />
                  </Button>
                </DialogTrigger>
                <DialogContent className="max-w-3xl max-h-[80vh] overflow-y-auto">
                  <DialogHeader>
                    <DialogTitle>Debug Information</DialogTitle>
                    <DialogDescription>
                      View the exact prompts sent to the LLM and the full response
                    </DialogDescription>
                  </DialogHeader>
                  <div className="space-y-4">
                    <div>
                      <div className="flex items-center justify-between mb-2">
                        <h3 className="font-semibold">System Prompt:</h3>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            navigator.clipboard.writeText(debugInfo.systemPrompt);
                            toast.success("System prompt copied to clipboard");
                          }}
                        >
                          <Copy className="h-4 w-4" />
                        </Button>
                      </div>
                      <pre className="bg-muted p-3 rounded-md text-xs overflow-x-auto whitespace-pre-wrap">
                        {debugInfo.systemPrompt}
                      </pre>
                    </div>
                    <div>
                      <div className="flex items-center justify-between mb-2">
                        <h3 className="font-semibold">User Prompt:</h3>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            navigator.clipboard.writeText(debugInfo.userPrompt);
                            toast.success("User prompt copied to clipboard");
                          }}
                        >
                          <Copy className="h-4 w-4" />
                        </Button>
                      </div>
                      <pre className="bg-muted p-3 rounded-md text-xs overflow-x-auto whitespace-pre-wrap">
                        {debugInfo.userPrompt}
                      </pre>
                    </div>
                    {debugInfo.availablePredicates && debugInfo.availablePredicates.length > 0 && (
                      <div>
                        <h3 className="font-semibold mb-2">Available Predicates:</h3>
                        <pre className="bg-muted p-3 rounded-md text-xs overflow-x-auto">
                          {debugInfo.availablePredicates.join('\\n')}
                        </pre>
                      </div>
                    )}
                    {debugInfo.availableAtoms && debugInfo.availableAtoms.length > 0 && (
                      <div>
                        <h3 className="font-semibold mb-2">Available Atoms (Constants):</h3>
                        <pre className="bg-muted p-3 rounded-md text-xs overflow-x-auto">
                          {debugInfo.availableAtoms.join('\\n')}
                        </pre>
                      </div>
                    )}
                    <div>
                      <div className="flex items-center justify-between mb-2">
                        <h3 className="font-semibold">LLM Response:</h3>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            navigator.clipboard.writeText(JSON.stringify(debugInfo.fullResponse, null, 2));
                            toast.success("LLM response copied to clipboard");
                          }}
                        >
                          <Copy className="h-4 w-4" />
                        </Button>
                      </div>
                      <pre className="bg-muted p-3 rounded-md text-xs overflow-x-auto">
                        {JSON.stringify(debugInfo.fullResponse, null, 2)}
                      </pre>
                    </div>
                  </div>
                </DialogContent>
              </Dialog>
            )}
          </div>

          {generatedItems.length > 0 && (
            <div className="space-y-3">
              {reasoning && (
                <div>
                  <h4 className="font-semibold mb-2 text-sm">💭 Chain of Thought:</h4>
                  <div className="bg-blue-50 dark:bg-blue-950 p-3 rounded-md text-sm border border-blue-200 dark:border-blue-800">
                    {reasoning}
                  </div>
                </div>
              )}
              
              <div className="space-y-2">
                <div className="flex items-center justify-between">
                  <h4 className="font-semibold">
                    {mode === "facts" ? "Generated Facts/Rules:" : "Generated Query:"}
                  </h4>
                  <Button onClick={handleAddAll} size="sm">
                    <Plus className="mr-2 h-4 w-4" />
                    {mode === "facts" ? "Add All" : "Add Query"}
                  </Button>
                </div>
                <div className="bg-muted p-3 rounded-md space-y-2">
                  {generatedItems.map((item, idx) => (
                    <div key={idx} className="space-y-1">
                      <div className="font-mono text-sm">{item}</div>
                      {docstrings[item] && (
                        <div className="text-xs text-muted-foreground italic pl-4 border-l-2 border-muted-foreground/30">
                          {docstrings[item]}
                        </div>
                      )}
                    </div>
                  ))}
                </div>
              </div>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
