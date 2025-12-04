import { useState, useEffect } from "react";
import { trpc } from "@/lib/trpc";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Textarea } from "@/components/ui/textarea";
import { Input } from "@/components/ui/input";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Loader2, Play, Plus, Trash2, BookOpen, Home, Sparkles } from "lucide-react";
import { toast } from "sonner";
import { APP_TITLE } from "@/const";
import { Link } from "wouter";
import { PrologAIChat } from "@/components/PrologAIChat";

export default function PrologPlayground() {
  type FactWithDocstring = { fact: string; docstring?: string };
  const [facts, setFacts] = useState<FactWithDocstring[]>([]);
  const [newFact, setNewFact] = useState("");
  const [query, setQuery] = useState("");
  const [results, setResults] = useState<any>(null);
  const [isQuerying, setIsQuerying] = useState(false);
  const [selectedPreset, setSelectedPreset] = useState<string>("");

  const { data: presets, isLoading: presetsLoading } = trpc.prolog.presets.useQuery();
  const queryMutation = trpc.prolog.query.useMutation();

  // Load preset when selected
  useEffect(() => {
    if (selectedPreset && presets) {
      const preset = presets.find((p) => p.id.toString() === selectedPreset);
      if (preset) {
        setFacts(preset.facts.map((f: string) => ({ fact: f })));
        setResults(null);
        toast.success(`Loaded preset: ${preset.name}`);
      }
    }
  }, [selectedPreset, presets]);

  const addFact = () => {
    if (!newFact.trim()) {
      toast.error("Please enter a fact");
      return;
    }
    setFacts([...facts, { fact: newFact.trim() }]);
    setNewFact("");
    toast.success("Fact added");
  };

  const removeFact = (index: number) => {
    setFacts(facts.filter((_, i) => i !== index));
    toast.success("Fact removed");
  };

  const clearFacts = () => {
    setFacts([]);
    setResults(null);
    setSelectedPreset("");
    toast.success("All facts cleared");
  };

  const executeQuery = async () => {
    if (!query.trim()) {
      toast.error("Please enter a query");
      return;
    }

    setIsQuerying(true);
    try {
      const result = await queryMutation.mutateAsync({
        facts: facts.map(f => f.fact),
        query: query.trim(),
      });
      setResults(result);
      
      if (result.success && result.solutions.length === 0) {
        toast.info("No solutions found");
      } else if (result.success) {
        toast.success(`Found ${result.solutions.length} solution(s)`);
      } else {
        toast.error(result.error || "Query failed");
      }
    } catch (error) {
      toast.error("Failed to execute query");
    } finally {
      setIsQuerying(false);
    }
  };

  const currentPreset = presets?.find((p) => p.id.toString() === selectedPreset);

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 via-indigo-50 to-purple-50">
      <header className="bg-white shadow-sm border-b">
        <div className="container mx-auto px-4 py-4">
          <div className="flex items-center gap-4">
            <Link href="/">
              <Button variant="ghost" size="icon">
                <Home className="h-5 w-5" />
              </Button>
            </Link>
            <div>
              <h1 className="text-2xl font-bold text-gray-900">{APP_TITLE}</h1>
              <p className="text-sm text-gray-600">PAIP Chapter 11 Implementation</p>
            </div>
          </div>
        </div>
      </header>

      <main className="container mx-auto px-4 py-8">
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Left Column: Facts & Presets */}
          <div className="lg:col-span-1 space-y-6">
            {/* Presets */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <BookOpen className="h-5 w-5" />
                  Presets
                </CardTitle>
                <CardDescription>Load example Prolog programs</CardDescription>
              </CardHeader>
              <CardContent>
                <Select value={selectedPreset} onValueChange={setSelectedPreset} disabled={presetsLoading}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select a preset..." />
                  </SelectTrigger>
                  <SelectContent>
                    {presets?.map((preset) => (
                      <SelectItem key={preset.id} value={preset.id.toString()}>
                        {preset.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
                
                {currentPreset && (
                  <div className="mt-4 space-y-2">
                    <p className="text-sm text-gray-600">{currentPreset.description}</p>
                    <div className="text-xs text-gray-500">
                      <strong>Category:</strong> {currentPreset.category}
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>

            {/* Facts Management */}
            <Card>
              <CardHeader>
                <CardTitle>Facts & Rules</CardTitle>
                <CardDescription>Define your Prolog knowledge base</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <Tabs defaultValue="manual" className="w-full">
                  <TabsList className="grid w-full grid-cols-2">
                    <TabsTrigger value="manual">Manual</TabsTrigger>
                    <TabsTrigger value="ai">
                      <Sparkles className="mr-2 h-4 w-4" />
                      AI
                    </TabsTrigger>
                  </TabsList>
                  
                  <TabsContent value="manual" className="space-y-4">
                    <div className="flex gap-2">
                      <Input
                        placeholder="e.g., (parent tom bob)"
                        value={newFact}
                        onChange={(e) => setNewFact(e.target.value)}
                        onKeyDown={(e) => e.key === "Enter" && addFact()}
                      />
                      <Button onClick={addFact} size="icon">
                        <Plus className="h-4 w-4" />
                      </Button>
                    </div>
                  </TabsContent>
                  
                  <TabsContent value="ai" className="space-y-4">
                    <PrologAIChat
                      mode="facts"
                      existingFacts={facts}
                      onFactsGenerated={(newFacts) => {
                        setFacts([...facts, ...newFacts]);
                      }}
                    />
                  </TabsContent>
                </Tabs>

                <div className="space-y-2 max-h-96 overflow-y-auto">
                  {facts.length === 0 ? (
                    <p className="text-sm text-gray-500 text-center py-4">
                      No facts yet. Add some or load a preset!
                    </p>
                  ) : (
                    facts.map((factObj, index) => (
                      <div
                        key={index}
                        className="flex items-start gap-2 p-2 bg-gray-50 rounded"
                      >
                        <div className="flex-1">
                          <div className="text-sm font-mono break-all">{factObj.fact}</div>
                          {factObj.docstring && (
                            <div className="text-xs text-gray-600 mt-1 italic">{factObj.docstring}</div>
                          )}
                        </div>
                        <Button
                          variant="ghost"
                          size="icon"
                          className="h-6 w-6 shrink-0"
                          onClick={() => removeFact(index)}
                        >
                          <Trash2 className="h-3 w-3" />
                        </Button>
                      </div>
                    ))
                  )}
                </div>

                {facts.length > 0 && (
                  <Button variant="outline" onClick={clearFacts} className="w-full">
                    Clear All Facts
                  </Button>
                )}
              </CardContent>
            </Card>
          </div>

          {/* Right Column: Query & Results */}
          <div className="lg:col-span-2 space-y-6">
            {/* Query Input */}
            <Card>
              <CardHeader>
                <CardTitle>Query</CardTitle>
                <CardDescription>Ask questions about your knowledge base</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <Tabs defaultValue="manual" className="w-full">
                  <TabsList className="grid w-full grid-cols-2">
                    <TabsTrigger value="manual">Manual</TabsTrigger>
                    <TabsTrigger value="ai">
                      <Sparkles className="mr-2 h-4 w-4" />
                      AI
                    </TabsTrigger>
                  </TabsList>
                  
                  <TabsContent value="manual" className="space-y-4">
                    <Textarea
                      placeholder="e.g., (parent tom ?child)"
                      value={query}
                      onChange={(e) => setQuery(e.target.value)}
                      rows={3}
                    />
                    <Button onClick={executeQuery} disabled={isQuerying || facts.length === 0} className="w-full">
                      {isQuerying ? (
                        <>
                          <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                          Executing...
                        </>
                      ) : (
                        <>
                          <Play className="mr-2 h-4 w-4" />
                          Execute Query
                        </>
                      )}
                    </Button>

                    {/* Example Queries */}
                    {currentPreset && currentPreset.exampleQueries.length > 0 && (
                      <div className="space-y-2">
                        <p className="text-sm font-medium">Example Queries:</p>
                        <div className="flex flex-wrap gap-2">
                          {currentPreset.exampleQueries.map((exampleQuery: string, idx: number) => (
                            <Button
                              key={idx}
                              variant="outline"
                              size="sm"
                              onClick={() => setQuery(exampleQuery)}
                            >
                              {exampleQuery}
                            </Button>
                          ))}
                        </div>
                      </div>
                    )}
                  </TabsContent>
                  
                  <TabsContent value="ai" className="space-y-4">
                    <PrologAIChat
                      mode="query"
                      existingFacts={facts}
                      onQueryGenerated={(generatedQuery) => {
                        setQuery(generatedQuery);
                      }}
                    />
                    
                    {query && (
                      <div className="space-y-2">
                        <p className="text-sm font-medium">Current Query:</p>
                        <div className="p-3 bg-muted rounded font-mono text-sm">{query}</div>
                        <Button onClick={executeQuery} disabled={isQuerying || facts.length === 0} className="w-full">
                          {isQuerying ? (
                            <>
                              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                              Executing...
                            </>
                          ) : (
                            <>
                              <Play className="mr-2 h-4 w-4" />
                              Execute Query
                            </>
                          )}
                        </Button>
                      </div>
                    )}
                  </TabsContent>
                </Tabs>
              </CardContent>
            </Card>

            {/* Results */}
            {results && (
              <Card>
                <CardHeader>
                  <CardTitle>Results</CardTitle>
                  <CardDescription>
                    {results.success
                      ? `Found ${results.solutions.length} solution(s)`
                      : "Query failed"}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  {results.success ? (
                    results.solutions.length > 0 ? (
                      <div className="space-y-4">
                        {results.solutions.map((solution: any, idx: number) => (
                          <div key={idx} className="p-4 bg-green-50 border border-green-200 rounded-lg">
                            <h3 className="font-semibold text-green-900 mb-2">Solution {idx + 1}:</h3>
                            <div className="space-y-1">
                              {Object.entries(solution).map(([key, value]) => (
                                <div key={key} className="font-mono text-sm">
                                  <span className="text-green-700">{key}</span> ={" "}
                                  <span className="text-green-900 font-semibold">{String(value)}</span>
                                </div>
                              ))}
                            </div>
                          </div>
                        ))}
                      </div>
                    ) : (
                      <p className="text-gray-500 text-center py-4">No solutions found</p>
                    )
                  ) : (
                    <div className="p-4 bg-red-50 border border-red-200 rounded-lg">
                      <p className="text-red-900">{results.error || "Unknown error"}</p>
                    </div>
                  )}
                </CardContent>
              </Card>
            )}
          </div>
        </div>
      </main>
    </div>
  );
}
