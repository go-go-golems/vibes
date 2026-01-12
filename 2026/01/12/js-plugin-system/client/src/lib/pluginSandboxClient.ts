// Design Philosophy: Technical Brutalism - Expose system operations transparently
// Main thread RPC client that bridges Redux store with sandbox worker

import type { Store } from "@reduxjs/toolkit";

type RpcResponse = { id: number; ok: true; result: any } | { id: number; ok: false; error: any };
type DispatchMessage = { type: "dispatch"; pluginId: string; actionJson: string };

export type PluginMeta = {
  id: string;
  title?: string;
  description?: string;
  widgets: string[];
};

export class PluginSandboxClient {
  private worker: Worker;
  private nextId = 1;
  private pending = new Map<number, { resolve: (v: any) => void; reject: (e: any) => void }>();
  private allow: (pluginId: string, action: any) => boolean;

  constructor(args: {
    store: Store;
    workerUrl: URL;
    allowDispatch?: (pluginId: string, action: any) => boolean;
  }) {
    const { store, workerUrl } = args;

    this.allow =
      args.allowDispatch ??
      ((pluginId, action) => typeof action?.type === "string" && action.type.startsWith(`plugin.${pluginId}/`));

    this.worker = new Worker(workerUrl, { type: "module" });

    this.worker.onmessage = (e: MessageEvent<any>) => {
      const data = e.data as RpcResponse | DispatchMessage;

      if ((data as DispatchMessage).type === "dispatch") {
        const { pluginId, actionJson } = data as DispatchMessage;
        const action = JSON.parse(actionJson);

        if (this.allow(pluginId, action)) {
          store.dispatch(action);
        } else {
          console.warn(`[PluginSandbox] Blocked action from ${pluginId}:`, action);
        }
        return;
      }

      const resp = data as RpcResponse;
      const pending = this.pending.get(resp.id);
      if (!pending) return;

      this.pending.delete(resp.id);

      if (resp.ok) pending.resolve(resp.result);
      else pending.reject(resp.error);
    };
  }

  private call(type: string, payload: any) {
    const id = this.nextId++;
    const msg = { id, type, ...payload };
    const promise = new Promise<any>((resolve, reject) => this.pending.set(id, { resolve, reject }));
    this.worker.postMessage(msg);
    return promise;
  }

  loadPlugin(pluginId: string, code: string): Promise<PluginMeta> {
    return this.call("loadPlugin", { pluginId, code });
  }

  render(pluginId: string, widgetId: string, state: any): Promise<any> {
    return this.call("render", { pluginId, widgetId, state });
  }

  event(pluginId: string, widgetId: string, handler: string, event: any, state: any): Promise<void> {
    return this.call("event", { pluginId, widgetId, handler, event, state });
  }

  terminate() {
    this.worker.terminate();
  }
}
