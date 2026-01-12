---
Title: 'Options: normalize store vs registry wiring'
Ticket: js-plugin-redux-fix
Status: active
Topics:
    - frontend
    - redux
    - plugins
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2026/01/12/js-plugin-system/client/src/components/PluginList.tsx
      Note: Registry UI for plugin lifecycle
    - Path: 2026/01/12/js-plugin-system/client/src/lib/pluginSandboxClient.ts
      Note: Sandbox runtime client (worker path)
    - Path: 2026/01/12/js-plugin-system/client/src/lib/presets.ts
      Note: Preset plugin scripts used by the sandbox runtime
    - Path: 2026/01/12/js-plugin-system/client/src/pages/Playground.tsx
      Note: Primary runtime wiring for registry vs in-process path
    - Path: 2026/01/12/js-plugin-system/client/src/store/store.ts
      Note: Redux state shape and plugin registry
ExternalSources: []
Summary: Decision analysis for normalizing Redux state vs wiring the plugin registry UI/runtime.
LastUpdated: 2026-01-12T16:25:00-05:00
WhatFor: Clarify the tradeoffs between normalizing the state shape and reintroducing the registry/sandbox flow.
WhenToUse: Use when deciding how to stabilize the plugin runtime and UI.
---


# Options: Normalize Store vs Registry Wiring

## Goal
Explain the two stabilization options:
1) normalize Redux state so plugins read from a single, predictable shape; and
2) reintroduce the plugin registry wiring (either by restoring the sandbox path or wiring the current in-process path to the registry UI).

## Current symptoms
- Calculator and greeter were not showing state because `presetPlugins.ts` read `state.calculator`/`state.greeter` while the reducer writes under `state.plugins.*`.
- The simplified `Playground.tsx` path bypassed the Redux plugin registry and sandbox/worker pipeline entirely.
- There are now two “truths” for counters: `state.plugins.counter` and a top-level `state.counter` reducer.

## Option 1: Normalize Redux state shape

### What it means
Pick a single, consistent shape for plugin state and remove the duplicate reducer. Two realistic sub-variants:
- **Option 1A (plugins slice as source of truth):** Keep plugin state under `state.plugins.*` and remove the top-level `counter` reducer.
- **Option 1B (flatten to root):** Move `counter`, `calculator`, and `greeter` to the root, update reducers, and update plugins to read from root.

### Pros
- Eliminates ambiguity and reduces “wrong path” bugs.
- Simplifies plugin render code; no conditional fallback logic.
- Makes debugging and DevTools inspection predictable.

### Cons / risks
- Requires consistent updates across reducers and preset plugins.
- May affect any test or UI that implicitly expects the old dual shape.

### Implementation sketch
- In `client/src/store/store.ts`, remove the duplicate top-level `counter` reducer.
- Standardize preset and custom plugins on a single state shape.
- Document the contract in a single place (analysis + README or comments).

## Option 2: Wire the plugin registry UI/runtime

### What it means
The plugin registry (`pluginsSlice`) tracks plugin lifecycle and status; the UI should reflect that registry. Two realistic sub-variants:
- **Option 2A (restore sandbox path):** Reintroduce the worker-based `PluginSandboxClient` in `Playground.tsx` and use `PluginList`, `PluginEditor`, and `PluginWidget` as the primary UI.
- **Option 2B (wire in-process runtime):** Keep an in-process loader for loading/rendering, but dispatch lifecycle actions (`pluginLoadStarted`, `pluginLoadSucceeded`, `pluginLoadFailed`, `pluginRemoved`, `pluginToggled`) so the registry UI stays in sync.
  - Note: the in-process loader was removed during cleanup; reintroduce it only if you need this path.

### Pros
- Restores the “complete” behavior: plugin list, statuses, enable/disable toggles, and predictable registry state.
- Provides a single source for plugin metadata consumed by dashboard-style plugins.

### Cons / risks
- Option 2A requires more work to reconnect worker/sandbox code and handle messaging.
- Option 2B keeps the less-safe `new Function` path and does not restore isolation.

### Implementation sketch (Option 2B)
- In `client/src/pages/Playground.tsx`, dispatch plugin lifecycle actions on load/unload.
- Drive the left panel and the live widget list from `state.plugins.plugins` instead of local component state.
- Filter rendering by `status === "loaded"` and `enabled === true`.

## Current implementation status
- **Option 1A done:** removed the duplicate top-level counter reducer and aligned preset/mini plugin selectors to `state.plugins.*`.
- **Option 2A done:** restored `PluginSandboxClient` + `pluginSandbox.worker.ts` in `Playground.tsx`, with registry-driven UI and widget rendering.

## Open questions
- Do we want QuickJS isolation to be a core requirement? If yes, Option 2A becomes necessary.
- Should plugin state be namespaced by plugin ID (e.g., `state.plugins.data[pluginId]`) rather than shared fields?
