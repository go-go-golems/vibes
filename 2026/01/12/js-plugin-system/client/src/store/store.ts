// Design Philosophy: Technical Brutalism - Redux as the contract boundary
// All plugin state lives here, plugins can only influence via actions

import { configureStore, createSlice, PayloadAction } from "@reduxjs/toolkit";
import type { PluginMeta } from "@/lib/pluginSandboxClient";

export type PluginStatus = "idle" | "loading" | "loaded" | "error";

export interface LoadedPlugin {
  id: string;
  code: string;
  meta: PluginMeta;
  status: PluginStatus;
  error?: string;
  enabled: boolean;
}

interface PluginsState {
  plugins: Record<string, LoadedPlugin>;
  counter: number;
  calculator: {
    display: string;
    accumulator: number;
    operation: string | null;
  };
  greeter: {
    name: string;
  };
}

const STORAGE_KEY = "pluginPlaygroundState";
const STORAGE_VERSION = 1;

const initialState: PluginsState = {
  plugins: {},
  counter: 0,
  calculator: {
    display: "0",
    accumulator: 0,
    operation: null,
  },
  greeter: {
    name: "",
  },
};

type PersistedPluginEntry = {
  id: string;
  code: string;
  meta?: PluginMeta;
  enabled?: boolean;
};

type PersistedState = {
  version: number;
  plugins: {
    plugins: Record<string, PersistedPluginEntry>;
    counter: number;
    calculator: {
      display: string;
      accumulator: number;
      operation: string | null;
    };
    greeter: {
      name: string;
    };
  };
};

function sanitizePersistedState(raw: unknown): PluginsState | null {
  if (!raw || typeof raw !== "object") return null;
  const parsed = raw as Partial<PersistedState>;
  if (parsed.version !== STORAGE_VERSION || !parsed.plugins) return null;

  const pluginsSlice = parsed.plugins as PersistedState["plugins"];
  const plugins: Record<string, LoadedPlugin> = {};
  const storedPlugins = pluginsSlice.plugins;

  if (storedPlugins && typeof storedPlugins === "object") {
    for (const [id, entry] of Object.entries(storedPlugins)) {
      if (!entry || typeof entry !== "object") continue;
      const typedEntry = entry as PersistedPluginEntry;
      if (typeof typedEntry.code !== "string" || !typedEntry.code.trim()) continue;

      const meta = typedEntry.meta && typeof typedEntry.meta === "object"
        ? {
            id: typeof typedEntry.meta.id === "string" ? typedEntry.meta.id : id,
            title: typeof typedEntry.meta.title === "string" ? typedEntry.meta.title : undefined,
            description:
              typeof typedEntry.meta.description === "string"
                ? typedEntry.meta.description
                : undefined,
            widgets: Array.isArray(typedEntry.meta.widgets)
              ? typedEntry.meta.widgets.filter((widget) => typeof widget === "string")
              : [],
          }
        : { id, widgets: [] };

      plugins[id] = {
        id,
        code: typedEntry.code,
        meta,
        status: "idle",
        enabled: typeof typedEntry.enabled === "boolean" ? typedEntry.enabled : true,
      };
    }
  }

  const counter =
    typeof pluginsSlice.counter === "number" ? pluginsSlice.counter : initialState.counter;
  const calculator = pluginsSlice.calculator ?? initialState.calculator;
  const greeter = pluginsSlice.greeter ?? initialState.greeter;

  return {
    plugins,
    counter,
    calculator: {
      display: typeof calculator.display === "string" ? calculator.display : "0",
      accumulator: typeof calculator.accumulator === "number" ? calculator.accumulator : 0,
      operation:
        calculator.operation === null || typeof calculator.operation === "string"
          ? calculator.operation
          : null,
    },
    greeter: {
      name: typeof greeter.name === "string" ? greeter.name : "",
    },
  };
}

function loadPersistedState(): { plugins: PluginsState } | undefined {
  if (typeof window === "undefined") return undefined;
  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    if (!raw) return undefined;
    const parsed = JSON.parse(raw);
    const restored = sanitizePersistedState(parsed);
    if (!restored) return undefined;
    return { plugins: restored };
  } catch {
    return undefined;
  }
}

function savePersistedState(state: { plugins: PluginsState }) {
  if (typeof window === "undefined") return;
  const plugins = state.plugins;

  const storedPlugins: Record<string, PersistedPluginEntry> = {};
  for (const [id, plugin] of Object.entries(plugins.plugins)) {
    storedPlugins[id] = {
      id,
      code: plugin.code,
      meta: plugin.meta,
      enabled: plugin.enabled,
    };
  }

  const persisted: PersistedState = {
    version: STORAGE_VERSION,
    plugins: {
      plugins: storedPlugins,
      counter: plugins.counter,
      calculator: plugins.calculator,
      greeter: plugins.greeter,
    },
  };

  try {
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(persisted));
  } catch {
    // Ignore storage quota or serialization errors.
  }
}

const pluginsSlice = createSlice({
  name: "plugins",
  initialState,
  reducers: {
    pluginLoadStarted(state, action: PayloadAction<{ id: string; code: string }>) {
      const previous = state.plugins[action.payload.id];
      const enabled = previous ? previous.enabled : true;
      state.plugins[action.payload.id] = {
        id: action.payload.id,
        code: action.payload.code,
        meta: { id: action.payload.id, widgets: [] },
        status: "loading",
        enabled,
      };
    },
    pluginLoadSucceeded(state, action: PayloadAction<{ id: string; meta: PluginMeta }>) {
      const plugin = state.plugins[action.payload.id];
      if (plugin) {
        plugin.status = "loaded";
        plugin.meta = action.payload.meta;
        plugin.error = undefined;
      }
    },
    pluginLoadFailed(state, action: PayloadAction<{ id: string; error: string }>) {
      const plugin = state.plugins[action.payload.id];
      if (plugin) {
        plugin.status = "error";
        plugin.error = action.payload.error;
      }
    },
    pluginToggled(state, action: PayloadAction<string>) {
      const plugin = state.plugins[action.payload];
      if (plugin) {
        plugin.enabled = !plugin.enabled;
      }
    },
    pluginRemoved(state, action: PayloadAction<string>) {
      delete state.plugins[action.payload];
    },
    pluginCodeUpdated(state, action: PayloadAction<{ id: string; code: string }>) {
      const plugin = state.plugins[action.payload.id];
      if (plugin) {
        plugin.code = action.payload.code;
        plugin.status = "idle";
      }
    },
  },
  extraReducers: (builder) => {
    builder.addMatcher(
      (action) => action.type.startsWith("plugin."),
      (state, action: any) => {
        if (action.type === "plugin.counter/incremented") {
          state.counter += 1;
        } else if (action.type === "plugin.counter/decremented") {
          state.counter -= 1;
        } else if (action.type === "plugin.counter/reset") {
          state.counter = 0;
        } else if (action.type === "plugin.calculator/digit") {
          const digit = action.payload;
          if (state.calculator.display === "0") {
            state.calculator.display = String(digit);
          } else {
            state.calculator.display += String(digit);
          }
        } else if (action.type === "plugin.calculator/clear") {
          state.calculator.display = "0";
          state.calculator.accumulator = 0;
          state.calculator.operation = null;
        } else if (action.type === "plugin.calculator/operation") {
          const op = action.payload;
          state.calculator.accumulator = parseFloat(state.calculator.display);
          state.calculator.operation = op;
          state.calculator.display = "0";
        } else if (action.type === "plugin.calculator/equals") {
          const current = parseFloat(state.calculator.display);
          let result = current;
          if (state.calculator.operation === "+") {
            result = state.calculator.accumulator + current;
          } else if (state.calculator.operation === "-") {
            result = state.calculator.accumulator - current;
          } else if (state.calculator.operation === "*") {
            result = state.calculator.accumulator * current;
          } else if (state.calculator.operation === "/") {
            result = state.calculator.accumulator / current;
          }
          state.calculator.display = String(result);
          state.calculator.accumulator = 0;
          state.calculator.operation = null;
        } else if (action.type === "plugin.greeter/nameChanged") {
          state.greeter.name = action.payload || "";
        }
      }
    );
  },
});

export const {
  pluginLoadStarted,
  pluginLoadSucceeded,
  pluginLoadFailed,
  pluginToggled,
  pluginRemoved,
  pluginCodeUpdated,
} = pluginsSlice.actions;

export const store = configureStore({
  reducer: {
    plugins: pluginsSlice.reducer,
  },
  preloadedState: loadPersistedState(),
});

let persistTimeout: ReturnType<typeof setTimeout> | null = null;
store.subscribe(() => {
  if (persistTimeout) return;
  persistTimeout = setTimeout(() => {
    persistTimeout = null;
    savePersistedState(store.getState());
  }, 200);
});

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;
