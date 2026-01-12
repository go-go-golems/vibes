// Design Philosophy: Technical Brutalism - Show widget state and execution status
// Real-time rendering with visible loading states

import React from "react";
import { useSelector } from "react-redux";
import type { RootState } from "@/store/store";
import type { UINode, UIEventRef } from "@/lib/uiTypes";
import { WidgetRenderer } from "./WidgetRenderer";
import { PluginSandboxClient } from "@/lib/pluginSandboxClient";
import { Loader2 } from "lucide-react";

interface PluginWidgetProps {
  sandbox: PluginSandboxClient;
  pluginId: string;
  widgetId: string;
}

export function PluginWidget({ sandbox, pluginId, widgetId }: PluginWidgetProps) {
  const state = useSelector((s: RootState) => s);
  const [tree, setTree] = React.useState<UINode | null>(null);
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState<{
    name?: string;
    message: string;
    stack?: string;
    raw?: unknown;
  } | null>(null);
  const [renderTrigger, setRenderTrigger] = React.useState(0);

  const normalizeError = React.useCallback((err: unknown) => {
    if (err && typeof err === "object") {
      const maybeName = (err as { name?: unknown }).name;
      const maybeMessage = (err as { message?: unknown }).message;
      const maybeStack = (err as { stack?: unknown }).stack;
      const name = typeof maybeName === "string" ? maybeName : undefined;
      const message = typeof maybeMessage === "string" ? maybeMessage : String(err);
      const stack = typeof maybeStack === "string" ? maybeStack : undefined;
      return { name, message, stack, raw: err };
    }
    return { message: String(err) };
  }, []);

  React.useEffect(() => {
    let alive = true;
    setError(null);
    const shouldShowLoading = tree === null;
    if (shouldShowLoading) {
      setLoading(true);
    }

    // Use a small delay to ensure Redux state updates are processed
    const timeoutId = setTimeout(() => {
      if (!alive) return;
      
      console.log("[PluginWidget] Starting render for", pluginId, widgetId, "trigger:", renderTrigger);
      sandbox
        .render(pluginId, widgetId, state)
        .then((tree) => {
          console.log("[PluginWidget] Render succeeded, tree:", tree);
          if (alive) {
            // tree is already an object (parsed by evalJson in the worker)
            console.log("[PluginWidget] Rendered tree object:", tree);
            (window as any).__lastTree = tree;
            setTree(tree);
            setLoading(false);
          }
        })
        .catch((err) => {
          if (alive) {
            console.error("[PluginWidget] Render failed:", err);
            (window as any).__lastRenderError = String(err);
            setError(normalizeError(err));
            setLoading(false);
          }
        });
    }, 0);

    return () => {
      alive = false;
      clearTimeout(timeoutId);
    };
  }, [sandbox, pluginId, widgetId, state, renderTrigger]);

  const onEvent = React.useCallback(
    (ref: UIEventRef, eventPayload?: any) => {
      console.log("[PluginWidget] Event triggered:", ref, eventPayload);
      sandbox.event(pluginId, widgetId, ref.handler, eventPayload, state).catch((err) => {
        console.error(`[PluginWidget] Event handler error:`, err);
        setError(normalizeError(err));
      });
      // Force a re-render by incrementing the trigger
      // This ensures the effect re-runs and the widget is re-rendered with updated state
      Promise.resolve().then(() => {
        setRenderTrigger(t => t + 1);
      });
    },
    [sandbox, pluginId, widgetId, state]
  );

  if (loading && !tree) {
    return (
      <div className="flex items-center gap-2 text-yellow-500 font-mono text-sm p-4 border border-yellow-500/30 bg-yellow-500/5">
        <Loader2 className="animate-spin w-4 h-4" />
        <span>RENDERING...</span>
      </div>
    );
  }

  if (error) {
    return (
      <div className="border border-destructive/50 bg-destructive/10 rounded-sm p-4 font-mono text-sm text-destructive">
        <div className="font-bold uppercase tracking-wide mb-2">RENDER ERROR</div>
        <div className="text-xs text-destructive/80">
          {error.name ? `${error.name}: ` : ""}
          {error.message}
        </div>
        <details className="mt-3 text-xs text-destructive/90">
          <summary className="cursor-pointer select-none">Show error details</summary>
          <div className="mt-2 space-y-2">
            {error.stack && (
              <pre className="text-[11px] overflow-auto max-h-48 whitespace-pre-wrap">
                {error.stack}
              </pre>
            )}
            {error.raw && (
              <pre className="text-[11px] overflow-auto max-h-48 whitespace-pre-wrap">
                {JSON.stringify(error.raw, null, 2)}
              </pre>
            )}
          </div>
        </details>
      </div>
    );
  }

  if (!tree) {
    return (
      <div className="border border-accent/30 bg-accent/5 rounded-sm p-4 font-mono text-sm text-accent">
        <div className="font-bold uppercase tracking-wide mb-2">NO TREE</div>
        <p className="text-xs">Widget tree is null</p>
      </div>
    );
  }

  return <WidgetRenderer tree={tree} onEvent={onEvent} />;
}
