import React from "react";
import { useSelector, useDispatch } from "react-redux";
import { RootState } from "@/store/store";
import {
  pluginLoadFailed,
  pluginLoadStarted,
  pluginLoadSucceeded,
  pluginRemoved,
  pluginToggled,
} from "@/store/store";
import { WidgetRenderer } from "@/components/WidgetRenderer";
import { pluginManager } from "@/lib/pluginManager";
import { presetPlugins } from "@/lib/presetPlugins";
import type { UINode } from "@/lib/uiTypes";
import { Button } from "@/components/ui/button";

/**
 * Plugin Playground - Load and run multiple plugins simultaneously
 * Features:
 * - Preset plugin selector
 * - Plugin editor with syntax highlighting
 * - Multi-plugin view showing all loaded plugins
 * - Real-time state updates via Redux
 */
export default function Playground() {
  const state = useSelector((s: RootState) => s);
  const dispatch = useDispatch();
  const [selectedPreset, setSelectedPreset] = React.useState<string>("");
  const [customCode, setCustomCode] = React.useState<string>("");
  const [error, setError] = React.useState<string>("");
  const plugins = useSelector((s: RootState) => s.plugins.plugins);
  const pluginList = Object.values(plugins);

  // Create UI builder for plugins
  const uiBuilder = {
    text: (content: string) => ({
      kind: "text" as const,
      text: content,
    }),
    button: (label: string, props?: any) => ({
      kind: "button" as const,
      props: { label, ...props },
    }),
    input: (value: string, props?: any) => ({
      kind: "input" as const,
      props: { value, ...props },
    }),
    row: (children: UINode[]) => ({
      kind: "row" as const,
      children,
    }),
    panel: (children: UINode[]) => ({
      kind: "panel" as const,
      children,
    }),
    badge: (text: string) => ({
      kind: "badge" as const,
      text,
    }),
    table: (rows: any[][], props?: any) => ({
      kind: "table" as const,
      props: { headers: props?.headers || [], rows },
    }),
  };

  const createActions = (namespace: string, actionNames: string[]) => {
    const actions: Record<string, any> = {};
    for (const name of actionNames) {
      actions[name] = (payload?: any) => ({
        type: `${namespace}/${name}`,
        payload,
      });
    }
    return actions;
  };

  const loadPreset = async (presetId: string) => {
    try {
      setError("");
      const preset = presetPlugins.find((p) => p.id === presetId);
      if (!preset) {
        throw new Error(`Preset not found: ${presetId}`);
      }

      dispatch(pluginLoadStarted({ id: preset.id, code: preset.code }));
      const plugin = await pluginManager.loadPlugin(preset.code, {
        ui: uiBuilder,
        createActions,
      });
      if (plugin.id !== preset.id) {
        dispatch(pluginRemoved(preset.id));
        dispatch(pluginLoadStarted({ id: plugin.id, code: preset.code }));
      }
      dispatch(
        pluginLoadSucceeded({
          id: plugin.id,
          meta: {
            id: plugin.id,
            title: plugin.title,
            widgets: Object.keys(plugin.widgets),
          },
        })
      );
      setSelectedPreset(presetId);
    } catch (err) {
      setError(`Failed to load preset: ${String(err)}`);
      dispatch(pluginLoadFailed({ id: presetId, error: String(err) }));
    }
  };

  const loadCustom = async () => {
    let tempId = "custom";
    try {
      setError("");
      if (!customCode.trim()) {
        throw new Error("Plugin code cannot be empty");
      }

      tempId = `custom_${Date.now()}`;
      dispatch(pluginLoadStarted({ id: tempId, code: customCode }));
      const plugin = await pluginManager.loadPlugin(customCode, {
        ui: uiBuilder,
        createActions,
      });

      if (plugin.id !== tempId) {
        dispatch(pluginRemoved(tempId));
        dispatch(pluginLoadStarted({ id: plugin.id, code: customCode }));
      }
      dispatch(
        pluginLoadSucceeded({
          id: plugin.id,
          meta: {
            id: plugin.id,
            title: plugin.title,
            widgets: Object.keys(plugin.widgets),
          },
        })
      );
    } catch (err) {
      setError(`Failed to load custom plugin: ${String(err)}`);
      dispatch(pluginLoadFailed({ id: tempId, error: String(err) }));
    }
  };

  const unloadPlugin = (id: string) => {
    pluginManager.removePlugin(id);
    dispatch(pluginRemoved(id));
  };

  const handleEvent = (pluginId: string, widgetId: string, eventRef: any, eventPayload?: any) => {
    try {
      pluginManager.callHandler(pluginId, widgetId, eventRef.handler, dispatch, eventPayload, state);
    } catch (err) {
      console.error("Event error:", err);
    }
  };

  return (
    <div className="min-h-screen bg-background text-foreground p-4">
      <div className="max-w-7xl mx-auto">
        <h1 className="text-3xl font-bold text-cyan-400 mb-2 font-mono">PLUGIN PLAYGROUND</h1>
        <p className="text-muted-foreground mb-6 font-mono text-sm">
          QuickJS VM • React + Redux • Multi-Plugin Support
        </p>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
          {/* Left Panel: Plugin Selector */}
          <div className="border border-cyan-400/30 rounded-sm p-4 bg-card/50">
            <h2 className="text-lg font-bold text-cyan-400 mb-4 font-mono">PRESETS</h2>
            <div className="space-y-2">
              {presetPlugins.map((preset) => {
                const isLoaded = plugins[preset.id]?.status === "loaded";
                return (
                <Button
                  key={preset.id}
                  onClick={() => loadPreset(preset.id)}
                  variant={isLoaded ? "default" : "outline"}
                  className="w-full justify-start font-mono text-xs"
                >
                  {preset.title}
                  {isLoaded && " ✓"}
                </Button>
              );
              })}
            </div>

            <div className="mt-6 border-t border-cyan-400/20 pt-4">
              <h3 className="text-sm font-bold text-cyan-400 mb-2 font-mono">LOADED</h3>
              <div className="space-y-1">
                {pluginList.length === 0 ? (
                  <div className="text-muted-foreground text-xs font-mono">No plugins loaded</div>
                ) : (
                  pluginList.map((plugin) => (
                    <div key={plugin.id} className="flex items-center justify-between text-xs font-mono">
                      <div className="flex flex-col">
                        <span>{plugin.id}</span>
                        <span className="text-muted-foreground">{plugin.status}</span>
                      </div>
                      <div className="flex items-center gap-2">
                        <button
                          onClick={() => dispatch(pluginToggled(plugin.id))}
                          className={plugin.enabled ? "text-cyan-400" : "text-muted-foreground"}
                        >
                          {plugin.enabled ? "ON" : "OFF"}
                        </button>
                        <button
                          onClick={() => unloadPlugin(plugin.id)}
                          className="text-red-400 hover:text-red-300"
                        >
                          ✕
                        </button>
                      </div>
                    </div>
                  ))
                )}
              </div>
            </div>
          </div>

          {/* Middle Panel: Code Editor */}
          <div className="border border-cyan-400/30 rounded-sm p-4 bg-card/50">
            <h2 className="text-lg font-bold text-cyan-400 mb-4 font-mono">CUSTOM PLUGIN</h2>
            <textarea
              value={customCode}
              onChange={(e) => setCustomCode(e.target.value)}
              placeholder="definePlugin(({ ui, createActions }) => { ... })"
              className="w-full h-48 bg-background/50 border border-cyan-400/20 rounded p-2 font-mono text-xs text-foreground resize-none focus:outline-none focus:border-cyan-400"
            />
            <Button onClick={loadCustom} className="w-full mt-2 font-mono text-xs">
              LOAD PLUGIN
            </Button>
            {error && <div className="mt-2 text-red-400 text-xs font-mono">{error}</div>}
          </div>

          {/* Right Panel: Live Widgets */}
          <div className="border border-cyan-400/30 rounded-sm p-4 bg-card/50">
            <h2 className="text-lg font-bold text-cyan-400 mb-4 font-mono">LIVE WIDGETS</h2>
            {pluginList.length === 0 ? (
              <div className="text-muted-foreground text-xs font-mono">No plugins loaded</div>
            ) : (
              <div className="space-y-4 max-h-96 overflow-y-auto">
                {pluginList
                  .filter((p) => p.status === "loaded" && p.enabled)
                  .map((pluginMeta) => {
                  const plugin = pluginManager.getPlugin(pluginMeta.id);
                  if (!plugin) return null;

                  return (
                    <div key={pluginMeta.id} className="border border-cyan-400/20 rounded p-2 bg-background/30">
                      <div className="text-xs font-bold text-cyan-400 mb-2 font-mono">{plugin.title}</div>
                      <div className="space-y-2">
                        {Object.entries(plugin.widgets).map(([widgetId, widget]) => {
                          try {
                            const tree = widget.render({ state });
                            return (
                              <div key={widgetId}>
                                <WidgetRenderer
                                  tree={tree}
                                  onEvent={(eventRef, eventPayload) =>
                                    handleEvent(pluginMeta.id, widgetId, eventRef, eventPayload)
                                  }
                                />
                              </div>
                            );
                          } catch (err) {
                            return (
                              <div key={widgetId} className="text-red-400 text-xs font-mono">
                                Render error: {String(err)}
                              </div>
                            );
                          }
                        })}
                      </div>
                    </div>
                  );
                })}
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
