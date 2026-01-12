---
Title: Plugin system deep analysis
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
    - Path: 2026/01/12/js-plugin-system/client/src/components/PluginWidget.tsx
      Note: Sandbox widget renderer with loading behavior
    - Path: 2026/01/12/js-plugin-system/client/src/components/WidgetRenderer.tsx
      Note: UI DSL rendering and event dispatch
    - Path: 2026/01/12/js-plugin-system/client/src/lib/pluginManager.ts
      Note: In-process plugin loader and handler invoker
    - Path: 2026/01/12/js-plugin-system/client/src/lib/pluginSandboxClient.ts
      Note: Alternate in-process sandbox and event forwarding
    - Path: 2026/01/12/js-plugin-system/client/src/lib/presetPlugins.ts
      Note: Preset plugin definitions used by Playground
    - Path: 2026/01/12/js-plugin-system/client/src/lib/presets.ts
      Note: Unused preset set showing a different state and event contract
    - Path: 2026/01/12/js-plugin-system/client/src/pages/Playground.tsx
      Note: Active runtime path for loading and rendering plugins
    - Path: 2026/01/12/js-plugin-system/client/src/store/store.ts
      Note: Redux state shape and plugin reducers
    - Path: 2026/01/12/js-plugin-system/client/src/workers/pluginSandbox.worker.ts
      Note: Worker sandbox implementation that is currently unwired
    - Path: ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/add_bundle_diff.py
      Note: Script to record bundle commit diffs into sqlite
    - Path: ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/build_snapshot_sqlite.py
      Note: Script to build snapshot sqlite DB for current vs bundle
    - Path: ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/various/js-plugin-diff.sqlite
      Note: SQLite snapshot/diff data backing the bundle history analysis
ExternalSources: []
Summary: Deep dive into the current plugin experiment, runtime wiring, and likely breakpoints.
LastUpdated: 2026-01-12T15:55:06.000508708-05:00
WhatFor: Map how plugins execute, how Redux is used, and where the current experiment diverges from the intended architecture.
WhenToUse: Use when fixing the plugin experiment or reconciling plugin contracts and state shape.
---





# Plugin System Deep Analysis

## Goal
Provide a deep, file-based map of how the current plugin experiment is wired, where state and event contracts diverge, and how the history artifacts (pasted content, test logs, bundle) line up with the implementation.

## System map (current workspace)

### Entry point and active runtime
- `client/src/pages/Playground.tsx` is the active entry. It initializes `PluginSandboxClient` and drives plugin lifecycle through the Redux registry.
- Plugins are loaded through the sandbox worker, and widget rendering uses `PluginWidget` (which calls `sandbox.render`).
- UI rendering is still handled by `client/src/components/WidgetRenderer.tsx`, which converts data-only UI trees into React components.

### Alternative runtimes and dead code paths
- `client/src/lib/pluginManager.ts` and `client/src/lib/presetPlugins.ts` remain as an in-process alternative, but `Playground` now uses the sandbox path.

## Plugin API and UI DSL

### UI DSL contract
- `client/src/lib/uiTypes.ts` defines `UINode` and `UIEventRef` with a `handler` and optional `args`.
- UI constructors exist in both `Playground.tsx` and `pluginSandboxClient.ts` (duplicated logic).
- `WidgetRenderer.tsx` is the only place that maps `UINode` to React elements.

### Plugin definition contract
- Plugins are expected to call `definePlugin(...)` and return an object with `id`, `title`, `widgets`, and handler functions.
- `client/src/lib/pluginManager.ts` runs plugin code via `new Function("definePlugin", code)` and stores plugin instances in-memory.
- `client/src/lib/pluginSandboxClient.ts` runs plugin code via `new Function(code)` with a global `window.definePlugin` (different call pattern).

## Redux contract and state shape

### Store structure
- `client/src/store/store.ts` defines a `plugins` slice with sub-state for `counter`, `calculator`, and `greeter`.
- The store also defines a top-level `counter` reducer that updates a second counter value.

### Resulting state shape
- Root state shape is effectively:
  - `state.plugins` (slice) with `counter`, `calculator`, `greeter`, and `plugins` registry
  - `state.counter` (top-level reducer)
- This means `state.calculator` and `state.greeter` do not exist at the root level, only under `state.plugins`.

## Event and render flow (current path)

### Rendering and event flow in use (Playground)
1. `Playground.tsx` loads plugin code from `client/src/lib/presetPlugins.ts` using `pluginManager`.
2. Each widget renders via `widget.render({ state })` using the root Redux state.
3. `WidgetRenderer.tsx` emits events using `onEvent(ref, eventPayload)`.
4. `Playground` wires `onEvent` to `handleEvent(pluginId, widgetId, eventRef)` but discards the `eventPayload`.
5. `pluginManager.callHandler(...)` passes only `eventRef.args` and does not forward input change payloads.

### Event payload mismatch
- `WidgetRenderer` passes payloads for inputs (object with `value`) and buttons (raw args).
- `Playground` ignores the payload entirely, so input values never reach plugin handlers.
- `pluginSandboxClient.event` passes `eventPayload?.args` as the second arg and `eventPayload` as `context.event`, which does not match handlers that expect args as the second parameter.

## Preset plugin definitions

### Used presets
- `client/src/lib/presetPlugins.ts` is used by `Playground` and expects:
  - `state.calculator` and `state.greeter` at the root level
  - handler arguments passed as the second parameter
- Because the store nests these under `state.plugins`, calculator and greeter plugins are reading from the wrong path.

### Unused presets
- `client/src/lib/presets.ts` exists but is unused. It references `state.plugins.*` and handler `event` objects, which suggests an earlier contract that no longer matches the active path.

## Sandbox and QuickJS history

### Intended design (from pasted content)
- `pasted_content(1).txt` outlines the intended architecture: Redux is the contract boundary, plugins dispatch namespaced actions, and a widget DSL is rendered by React.
- `pasted_content_2.txt` sketches a QuickJS-in-worker design with a strict dispatch bridge.

### Actual implementation
- `pluginSandbox.worker.ts` now uses QuickJS again and is wired into `Playground` via `PluginSandboxClient`.
- `pluginSandboxClient.ts` runs as a worker RPC client and enforces a dispatch allowlist.
- `package.json` includes `quickjs-emscripten`, but no implementation imports or uses it.

## History artifacts

### Debug findings and tests
- `DEBUG_FINDINGS.md` notes stale state suspicion and an event pipeline that references a worker path that is not currently wired.
- `test-log.md` claims QuickJS VM isolation and a Monaco editor; the current UI uses a simple textarea and does not wire `PluginEditor.tsx`.

### Git artifacts
- `plugin-playground/.git` and `/tmp/plugin-playground/.git` are incomplete (missing `objects/`). Commands like:
  - `git --git-dir /path/to/.git --work-tree /path/to status -sb`
  fail with `fatal: not a git repository`.
- `plugin-playground.bundle` is a valid Git bundle (verified), with a complete history and a `main` branch.
- Cloning the bundle into `/tmp/plugin-playground-bundle` works and shows four checkpoint commits referencing QuickJS and multi-plugin support, even though the implementation still uses `new Function`.

## Bundle history diff (what changed between iterations)

### Snapshot relationship to current workspace
- The top-level workspace matches the bundle HEAD (`7736a42`) for all shared paths; the only extra content is the duplicated `plugin-playground/` subtree.

### cca2452 -> dc83162
- Only `client/src/workers/pluginSandbox.worker.ts` changed.
- The diff added a stubbed `console` object inside the QuickJS VM to avoid plugin errors when scripts call `console.*`.

### dc83162 -> 7736a42 (simplify pass)
- `client/src/pages/Playground.tsx` rewired from the sandbox-based layout (PluginList + PluginEditor + PluginWidget) to a simplified local-state layout using `pluginManager` and a textarea.
- `client/src/lib/pluginSandboxClient.ts` changed from a worker RPC client to an in-process evaluator using `new Function`, and ignores the worker URL/allowlist.
- `client/src/workers/pluginSandbox.worker.ts` dropped QuickJS entirely and now evaluates plugins with `new Function` inside the worker.
- `client/src/lib/pluginManager.ts`, `client/src/lib/presetPlugins.ts`, and `client/src/lib/minimalPlugin.ts` were added; these bypass the sandbox path.
- `client/src/store/store.ts` added calculator/greeter state under the `plugins` slice plus a second top-level `counter` reducer.
- `client/src/components/PluginWidget.tsx` added render retry/debugging and an explicit re-render trigger, but it is no longer wired by `Playground`.

These diffs are recorded in `various/js-plugin-diff.sqlite` under the ticket, sourced from `/tmp/plugin-playground-bundle`.

## Likely breakpoints for "does not really work"

1. State path mismatch (fixed)
   - Preset and minimal plugins now read from `state.plugins.*` after removing the duplicate root counter reducer.

2. Input event payload dropped (fixed for sandbox path)
   - `WidgetRenderer` now sends button args as `{ args }`, and `PluginWidget` passes the payload into the sandbox handler.

3. Input focus dropped on re-render (fixed)
   - `PluginWidget` used to swap to a loading placeholder on every state change, unmounting inputs. It now keeps the tree mounted unless there is no tree yet.

4. Sandbox handler argument mismatch (fixed)
   - Button events now deliver `{ args }` and input events deliver `{ value }`, matching the worker handler contract.

5. Duplicate counter state (fixed)
   - The top-level `counter` reducer was removed; the plugins slice is the single source of truth.

6. Unwired plugin registry (fixed)
   - `Playground` dispatches lifecycle actions and renders from the registry.

7. QuickJS / worker plan not implemented (fixed)
   - The worker is restored and used as the active path.

8. Simplify pass removed the sandbox wiring (fixed)
   - The sandboxed runtime is restored as the active path in `Playground`, so registry status and worker isolation are live again.

## Concrete fix direction (next steps)

- Choose a single runtime path (in-process `pluginManager` or worker-based sandbox) and delete or wire the unused one.
- Align the plugin contract:
  - Decide whether handlers receive `args` as the second parameter or an `event` object in context.
  - Make `WidgetRenderer`, `Playground`, and sandbox handler invocation consistent.
- Normalize Redux state shape:
  - Either move plugin state to root or update plugin code to read from `state.plugins.*`.
  - Remove the duplicate `counter` reducer to avoid drift.
- Wire plugin registry actions if Redux should track plugin lifecycle, or remove the slice if local state is the intended source of truth.
- If QuickJS is required, implement the worker design from `pasted_content_2.txt` and remove `new Function` evaluators.
