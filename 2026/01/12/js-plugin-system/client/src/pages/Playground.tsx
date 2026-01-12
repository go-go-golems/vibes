// Design Philosophy: Technical Brutalism - Asymmetric split-pane IDE-like layout
// Plugin list (left) + Editor (center) + Widget output (right/bottom)

import React from "react";
import { useSelector, useDispatch } from "react-redux";
import type { RootState } from "@/store/store";
import { pluginLoadStarted, pluginLoadSucceeded, pluginLoadFailed } from "@/store/store";
import { PluginSandboxClient } from "@/lib/pluginSandboxClient";
import { store } from "@/store/store";
import { PluginList } from "@/components/PluginList";
import { PluginEditor } from "@/components/PluginEditor";
import { PluginWidget } from "@/components/PluginWidget";
import { Button } from "@/components/ui/button";
import { PRESET_PLUGINS } from "@/lib/presets";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Plus, Zap } from "lucide-react";
import { useLocation } from "wouter";

const UI_STORAGE_KEY = "pluginPlaygroundUiState";

export default function Playground() {
  const dispatch = useDispatch();
  const [, setLocation] = useLocation();
  const plugins = useSelector((state: RootState) => state.plugins.plugins);
  const [sandbox, setSandbox] = React.useState<PluginSandboxClient | null>(null);
  const [selectedPluginId, setSelectedPluginId] = React.useState<string | null>(null);
  const [editorCode, setEditorCode] = React.useState("");
  const [selectedPreset, setSelectedPreset] = React.useState<string>("");
  const prevPluginIdsRef = React.useRef<Set<string>>(new Set());
  const hasRestoredPluginsRef = React.useRef(false);
  const uiPersistRef = React.useRef<number | null>(null);

  // Initialize sandbox
  React.useEffect(() => {
    const workerUrl = new URL("../workers/pluginSandbox.worker.ts", import.meta.url);
    const client = new PluginSandboxClient({
      store,
      workerUrl,
      allowDispatch: (pluginId, action) => {
        return typeof action?.type === "string" && action.type.startsWith(`plugin.${pluginId}/`);
      },
    });
    setSandbox(client);

    return () => {
      client.terminate();
    };
  }, []);

  React.useEffect(() => {
    if (typeof window === "undefined") return;
    try {
      const raw = window.localStorage.getItem(UI_STORAGE_KEY);
      if (!raw) return;
      const parsed = JSON.parse(raw) as {
        selectedPluginId?: string | null;
        editorCode?: string;
        selectedPreset?: string;
      };
      if (typeof parsed.selectedPluginId === "string") {
        setSelectedPluginId(parsed.selectedPluginId);
      }
      if (typeof parsed.editorCode === "string") {
        setEditorCode(parsed.editorCode);
      }
      if (typeof parsed.selectedPreset === "string") {
        setSelectedPreset(parsed.selectedPreset);
      }
    } catch {
      // Ignore invalid localStorage payloads.
    }
  }, []);

  React.useEffect(() => {
    if (typeof window === "undefined") return;
    if (uiPersistRef.current) {
      window.clearTimeout(uiPersistRef.current);
    }
    uiPersistRef.current = window.setTimeout(() => {
      const payload = {
        selectedPluginId,
        editorCode,
        selectedPreset,
      };
      try {
        window.localStorage.setItem(UI_STORAGE_KEY, JSON.stringify(payload));
      } catch {
        // Ignore localStorage errors.
      }
    }, 200);

    return () => {
      if (uiPersistRef.current) {
        window.clearTimeout(uiPersistRef.current);
      }
    };
  }, [selectedPluginId, editorCode, selectedPreset]);

  React.useEffect(() => {
    if (!sandbox) return;
    const currentIds = new Set(Object.keys(plugins));
    const removed: string[] = [];
    for (const id of prevPluginIdsRef.current) {
      if (!currentIds.has(id)) {
        removed.push(id);
      }
    }
    removed.forEach((id) => {
      sandbox.unloadPlugin(id).catch((err) => {
        console.error(`[PluginSandbox] Failed to unload ${id}:`, err);
      });
    });
    prevPluginIdsRef.current = currentIds;
  }, [sandbox, plugins]);

  const handleLoadPlugin = React.useCallback(
    async (pluginId: string, code: string) => {
      if (!sandbox) return;

      dispatch(pluginLoadStarted({ id: pluginId, code }));

      try {
        const meta = await sandbox.loadPlugin(pluginId, code);
        dispatch(pluginLoadSucceeded({ id: pluginId, meta }));
      } catch (error) {
        dispatch(pluginLoadFailed({ id: pluginId, error: String(error) }));
      }
    },
    [sandbox, dispatch]
  );

  React.useEffect(() => {
    if (!sandbox || hasRestoredPluginsRef.current) return;
    hasRestoredPluginsRef.current = true;
    const savedPlugins = Object.values(store.getState().plugins.plugins);
    savedPlugins.forEach((plugin) => {
      if (plugin.code) {
        handleLoadPlugin(plugin.id, plugin.code);
      }
    });
  }, [sandbox, handleLoadPlugin]);

  const handleLoadPreset = React.useCallback(
    (presetId: string) => {
      const preset = PRESET_PLUGINS.find((p) => p.id === presetId);
      if (!preset) return;

      setEditorCode(preset.code);
      setSelectedPreset(presetId);
      handleLoadPlugin(preset.id, preset.code);
    },
    [handleLoadPlugin]
  );

  const handleRunEditor = React.useCallback(() => {
    if (!editorCode.trim()) return;
    
    const pluginId = selectedPluginId || `plugin_${Date.now()}`;
    handleLoadPlugin(pluginId, editorCode);
  }, [editorCode, selectedPluginId, handleLoadPlugin]);

  const selectedPlugin = selectedPluginId ? plugins[selectedPluginId] : null;

  // Get all enabled plugins and their widgets
  const enabledPlugins = Object.values(plugins).filter(
    (p) => p.enabled && p.status === "loaded"
  );

  return (
    <div className="h-screen flex flex-col bg-background">
      {/* Header */}
      <header className="border-b border-accent/30 bg-card/50 px-6 py-4 shadow-[0_2px_20px_rgba(0,255,255,0.1)]">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="font-mono text-2xl font-bold uppercase tracking-wider text-accent">
              Plugin Playground
            </h1>
            <p className="font-mono text-xs text-muted-foreground mt-1">
              QuickJS VM Sandbox • React + Redux + WASM
            </p>
          </div>
          
          <div className="flex items-center gap-3">
            <Button
              variant="outline"
              size="sm"
              onClick={() => setLocation("/docs")}
              className="font-mono text-xs uppercase tracking-wide border-accent/50"
            >
              Docs
            </Button>
            <Select value={selectedPreset} onValueChange={handleLoadPreset}>
              <SelectTrigger className="w-[200px] font-mono text-xs border-accent/30">
                <SelectValue placeholder="Load preset..." />
              </SelectTrigger>
              <SelectContent>
                {PRESET_PLUGINS.map((preset) => (
                  <SelectItem key={preset.id} value={preset.id} className="font-mono text-xs">
                    {preset.name}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <div className="flex-1 flex min-h-0">
        {/* Left Sidebar - Plugin List */}
        <div className="w-64 flex-shrink-0">
          <PluginList
            onSelectPlugin={setSelectedPluginId}
            selectedPluginId={selectedPluginId}
          />
        </div>

        {/* Center - Editor */}
        <div className="flex-1 flex flex-col min-w-0 border-r border-accent/30">
          <PluginEditor
            code={editorCode}
            onChange={setEditorCode}
            onRun={handleRunEditor}
          />
        </div>

        {/* Right - Widget Output */}
        <div className="w-96 flex-shrink-0 flex flex-col bg-card/30">
          <div className="px-4 py-3 border-b border-accent/30 bg-accent/5">
            <h2 className="font-mono text-sm uppercase tracking-wider font-bold text-accent">
              Live Widgets
            </h2>
            <p className="font-mono text-xs text-muted-foreground mt-1">
              {enabledPlugins.length} plugin(s) active
            </p>
          </div>

          <div className="flex-1 overflow-y-auto p-4 space-y-4">
            {enabledPlugins.length === 0 ? (
              <div className="flex flex-col items-center justify-center h-full text-center p-8">
                <Zap className="w-12 h-12 text-accent/30 mb-4" />
                <p className="font-mono text-sm text-muted-foreground">
                  No active widgets
                </p>
                <p className="font-mono text-xs text-muted-foreground mt-2">
                  Load a preset or write your own plugin
                </p>
              </div>
            ) : (
              enabledPlugins.map((plugin) =>
                plugin.meta.widgets.map((widgetId) => (
                  <div key={`${plugin.id}-${widgetId}`} className="mb-4">
                    <div className="font-mono text-xs uppercase tracking-wider text-accent mb-2 px-1">
                      {plugin.meta.title || plugin.id} / {widgetId}
                    </div>
                    {sandbox && (
                      <PluginWidget
                        sandbox={sandbox}
                        pluginId={plugin.id}
                        widgetId={widgetId}
                      />
                    )}
                  </div>
                ))
              )
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
