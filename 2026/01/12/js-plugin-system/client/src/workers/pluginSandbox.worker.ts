// Design Philosophy: Technical Brutalism - Raw exposure of VM operations
// This worker runs QuickJS WASM to execute untrusted plugin code in isolation

// @ts-ignore - QuickJS WASM types
import { getQuickJS } from "quickjs-emscripten";

type RpcRequest =
  | { id: number; type: "loadPlugin"; pluginId: string; code: string }
  | { id: number; type: "render"; pluginId: string; widgetId: string; state: any }
  | { id: number; type: "event"; pluginId: string; widgetId: string; handler: string; event: any; state: any };

type RpcResponse =
  | { id: number; ok: true; result: any }
  | { id: number; ok: false; error: any };

type DispatchMessage = { type: "dispatch"; pluginId: string; actionJson: string };

// Bootstrap code injected into every plugin VM
// Provides ui.* constructors and definePlugin global
const BOOTSTRAP = `
  (function () {
    function node(kind, props, children) {
      return { kind, props: props || {}, children: children || [] };
    }

    globalThis.ui = {
      panel(children, props) { return node("panel", props, children); },
      row(children, props) { return node("row", props, children); },
      column(children, props) { return node("column", props, children); },
      text(text, props) { return { kind: "text", props: props || {}, text: String(text) }; },
      badge(text, props) { return { kind: "badge", props: props || {}, text: String(text) }; },
      
      button(label, opts) {
        opts = opts || {};
        return node("button", { label: String(label), onClick: opts.onClick, variant: opts.variant }, []);
      },
      
      input(value, opts) {
        opts = opts || {};
        return node("input", {
          value: value == null ? "" : String(value),
          placeholder: opts.placeholder || "",
          onChange: opts.onChange
        }, []);
      },

      counter(value, opts) {
        opts = opts || {};
        return node("counter", {
          value: Number(value) || 0,
          onIncrement: opts.onIncrement,
          onDecrement: opts.onDecrement
        }, []);
      },

      table(rows, opts) {
        opts = opts || {};
        return node("table", {
          headers: opts.headers || [],
          rows: rows || []
        }, []);
      }
    };

    globalThis.createActions = function(namespace, names) {
      const out = {};
      for (const n of names) {
        out[n] = function(payload) {
          return { type: namespace + "/" + n, payload };
        };
      }
      return out;
    };

    globalThis.__plugin = null;
    globalThis.definePlugin = function(defOrFactory) {
      globalThis.__plugin =
        (typeof defOrFactory === "function")
          ? defOrFactory({ ui: globalThis.ui, createActions: globalThis.createActions })
          : defOrFactory;
    };
  })();
`;

type PluginVm = {
  vm: any;
  deadlineMs: number;
};

const plugins = new Map<string, PluginVm>();
let QuickJS: any | null = null;

async function ensureQuickJS() {
  if (!QuickJS) QuickJS = await getQuickJS();
  return QuickJS;
}

function unwrap(vm: any, result: any) {
  if (result.error) {
    const err = vm.dump(result.error);
    result.error.dispose();
    throw err;
  }
  return result.value;
}

function setGlobalString(vm: any, name: string, value: string) {
  const h = vm.newString(value);
  vm.setProp(vm.global, name, h);
  h.dispose();
}

function evalJson(vm: any, code: string, filename?: string) {
  const h = unwrap(vm, vm.evalCode(code, filename));
  const s = vm.getString(h);
  h.dispose();
  return JSON.parse(s);
}

function createPluginVm(pluginId: string) {
  const vm = QuickJS!.newContext();

  let deadlineMs = Number.POSITIVE_INFINITY;
  vm.runtime.setInterruptHandler(() => Date.now() > deadlineMs);

  // Only expose dispatch to host
  const hostDispatch = vm.newFunction("__hostDispatch", (actionJsonHandle: any) => {
    const actionJson = vm.getString(actionJsonHandle);
    const msg: DispatchMessage = { type: "dispatch", pluginId, actionJson };
    postMessage(msg);
    return vm.undefined;
  });
  vm.setProp(vm.global, "__hostDispatch", hostDispatch);
  hostDispatch.dispose();

  // Register console object with native functions
  const consoleObj = vm.newObject();
  const logFn = vm.newFunction("log", () => vm.undefined);
  const errorFn = vm.newFunction("error", () => vm.undefined);
  const warnFn = vm.newFunction("warn", () => vm.undefined);
  const infoFn = vm.newFunction("info", () => vm.undefined);
  const debugFn = vm.newFunction("debug", () => vm.undefined);
  
  vm.setProp(consoleObj, "log", logFn);
  vm.setProp(consoleObj, "error", errorFn);
  vm.setProp(consoleObj, "warn", warnFn);
  vm.setProp(consoleObj, "info", infoFn);
  vm.setProp(consoleObj, "debug", debugFn);
  
  logFn.dispose();
  errorFn.dispose();
  warnFn.dispose();
  infoFn.dispose();
  debugFn.dispose();
  
  vm.setProp(vm.global, "console", consoleObj);
  consoleObj.dispose();

  // Install widget DSL
  unwrap(vm, vm.evalCode(BOOTSTRAP, "bootstrap.js")).dispose();

  const entry: PluginVm = {
    vm,
    get deadlineMs() {
      return deadlineMs;
    },
    set deadlineMs(v: number) {
      deadlineMs = v;
    },
  } as any;

  plugins.set(pluginId, entry);
  return entry;
}

function getOrCreate(pluginId: string) {
  if (plugins.has(pluginId)) return plugins.get(pluginId)!;
  return createPluginVm(pluginId);
}

function withDeadline(p: PluginVm, ms: number, fn: () => any) {
  p.deadlineMs = Date.now() + ms;
  try {
    return fn();
  } finally {
    p.deadlineMs = Number.POSITIVE_INFINITY;
  }
}

async function handleLoadPlugin(msg: Extract<RpcRequest, { type: "loadPlugin" }>) {
  await ensureQuickJS();
  const p = getOrCreate(msg.pluginId);
  const { vm } = p;

  return withDeadline(p, 100, () => {
    unwrap(vm, vm.evalCode(msg.code, `${msg.pluginId}.plugin.js`)).dispose();

    const meta = evalJson(
      vm,
      `
      JSON.stringify({
        id: __plugin && __plugin.id,
        title: __plugin && __plugin.title,
        description: __plugin && __plugin.description,
        widgets: __plugin && __plugin.widgets ? Object.keys(__plugin.widgets) : []
      })
      `
    );
    return meta;
  });
}

async function handleRender(msg: Extract<RpcRequest, { type: "render" }>) {
  const p = getOrCreate(msg.pluginId);
  const { vm } = p;

  return withDeadline(p, 100, () => {
    setGlobalString(vm, "__WIDGET_ID", msg.widgetId);
    setGlobalString(vm, "__STATE_JSON", JSON.stringify(msg.state ?? null));

    const tree = evalJson(
      vm,
      `
      (function(){
        const state = JSON.parse(__STATE_JSON);
        const w = __plugin.widgets[__WIDGET_ID];
        const tree = w.render({ state: state });
        return JSON.stringify(tree);
      })()
      `
    );

    return tree;
  });
}

async function handleEvent(msg: Extract<RpcRequest, { type: "event" }>) {
  const p = getOrCreate(msg.pluginId);
  const { vm } = p;

  return withDeadline(p, 100, () => {
    setGlobalString(vm, "__WIDGET_ID", msg.widgetId);
    setGlobalString(vm, "__HANDLER", msg.handler);
    setGlobalString(vm, "__STATE_JSON", JSON.stringify(msg.state ?? null));
    setGlobalString(vm, "__EVENT_JSON", JSON.stringify(msg.event ?? null));

    unwrap(
      vm,
      vm.evalCode(
        `
        (function(){
          const state = JSON.parse(__STATE_JSON);
          const event = JSON.parse(__EVENT_JSON);

          const widget = __plugin.widgets[__WIDGET_ID];
          const fn = widget.handlers[__HANDLER];

          const dispatch = (action) => __hostDispatch(JSON.stringify(action));

          fn({ dispatch, state, event });
          return 0;
        })()
        `,
        "handler.js"
      )
    ).dispose();

    return null;
  });
}

self.onmessage = async (e: MessageEvent<RpcRequest>) => {
  const msg = e.data;
  let resp: RpcResponse;

  try {
    let result: any;
    if (msg.type === "loadPlugin") result = await handleLoadPlugin(msg);
    else if (msg.type === "render") result = await handleRender(msg);
    else result = await handleEvent(msg);

    resp = { id: msg.id, ok: true, result };
  } catch (error) {
    resp = { id: msg.id, ok: false, error };
  }

  postMessage(resp);
};
