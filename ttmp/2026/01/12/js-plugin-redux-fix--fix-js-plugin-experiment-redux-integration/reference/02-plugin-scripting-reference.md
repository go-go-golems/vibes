---
Title: Plugin scripting reference
Ticket: js-plugin-redux-fix
Status: active
Topics:
    - frontend
    - redux
    - plugins
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2026/01/12/js-plugin-system/client/src/components/WidgetRenderer.tsx
      Note: UI DSL renderer
    - Path: 2026/01/12/js-plugin-system/client/src/lib/presets.ts
      Note: Preset examples used by the sandbox
    - Path: 2026/01/12/js-plugin-system/client/src/lib/uiTypes.ts
      Note: UI DSL type definitions
    - Path: 2026/01/12/js-plugin-system/client/src/store/store.ts
      Note: Redux state shape for plugins
    - Path: 2026/01/12/js-plugin-system/client/src/workers/pluginSandbox.worker.ts
      Note: Sandbox lifecycle and handler contract
ExternalSources: []
Summary: Reference guide for the plugin DSL, handler contract, and sandbox lifecycle.
LastUpdated: 2026-01-12T16:43:00-05:00
WhatFor: Copy/paste-ready guide to authoring plugins and understanding the sandbox contract.
WhenToUse: Use when writing or debugging plugin scripts and their Redux interactions.
---


# Plugin Scripting Reference

## Goal
Provide a concise, copy/paste-ready reference for writing plugin scripts that run in the QuickJS sandbox and interact with Redux state.

## Context
- Plugins run inside a QuickJS worker and can only call `definePlugin` plus the injected `ui` and `createActions` helpers.
- Plugins do not access the DOM or network; all side effects must be requested via Redux actions.
- The Redux state contract for plugins lives under `state.plugins.*`.

## Quick Reference

### Plugin entrypoint
```js
definePlugin(({ ui, createActions }) => {
  const actions = createActions("plugin.example", ["requested"]);

  return {
    id: "example",
    title: "Example Plugin",
    description: "Short summary",

    widgets: {
      ExampleWidget: {
        title: "Widget Title",
        render({ state }) {
          return ui.panel([
            ui.text("Hello"),
            ui.button("Run", { onClick: { handler: "run", args: "ping" } })
          ]);
        },
        handlers: {
          run({ dispatch, event }) {
            dispatch(actions.requested({ payload: event.args }));
          }
        }
      }
    }
  };
});
```

### Handler contract
- `render({ state })` gets the full Redux state object.
- `handlers.<name>({ dispatch, state, event })` receives:
  - `dispatch(action)` to send Redux actions.
  - `state` for read-only access (same object passed to render).
  - `event` payload:
    - `event.args` for button clicks.
    - `event.value` for input changes.

### UI DSL
| UI helper | Shape | Notes |
| --- | --- | --- |
| `ui.panel(children)` | Panel container | Most widgets return a panel as the root |
| `ui.row(children)` | Horizontal stack | Pairs with buttons and badges |
| `ui.column(children)` | Vertical stack | Use for column layout |
| `ui.text(text)` | Text node | Uses monospace styling |
| `ui.badge(text)` | Badge | Status badges and tags |
| `ui.button(label, { onClick, variant })` | Button | `onClick.handler` required |
| `ui.input(value, { placeholder, onChange })` | Input | `onChange.handler` required |
| `ui.counter(value, { onIncrement, onDecrement })` | Counter | Uses +/- controls |
| `ui.table(rows, { headers })` | Table | Read-only grid |

### Action namespace rules
- Actions should be namespaced as `plugin.<pluginId>/<action>`.
- The sandbox allowlist enforces this namespace by default.

### Redux state contract (plugin-visible)
```
state.plugins.counter
state.plugins.greeter
state.plugins.calculator
state.plugins.plugins   // registry metadata
```

### Sandbox lifecycle
- Load: `loadPlugin(id, code)` returns metadata.
- Render: `render(pluginId, widgetId, state)` returns a UI tree.
- Event: `event(pluginId, widgetId, handler, event, state)` executes handler.
- Unload: `unloadPlugin(pluginId)` disposes the VM context.

## Usage Examples

### Counter widget
```js
definePlugin(({ ui, createActions }) => {
  const actions = createActions("plugin.counter", ["incremented", "decremented"]);

  return {
    id: "counter",
    title: "Counter Control",
    widgets: {
      CounterWidget: {
        title: "Counter",
        render({ state }) {
          const count = state.plugins.counter || 0;
          return ui.panel([
            ui.text("Count: " + count),
            ui.row([
              ui.button("-", { onClick: { handler: "dec" } }),
              ui.button("+", { onClick: { handler: "inc" } })
            ])
          ]);
        },
        handlers: {
          inc({ dispatch }) { dispatch(actions.incremented()); },
          dec({ dispatch }) { dispatch(actions.decremented()); }
        }
      }
    }
  };
});
```

### Greeter input
```js
definePlugin(({ ui, createActions }) => {
  const actions = createActions("plugin.greeter", ["nameChanged"]);

  return {
    id: "greeter",
    title: "Greeter",
    widgets: {
      GreeterWidget: {
        title: "Greeter",
        render({ state }) {
          const name = state.plugins.greeter?.name || "";
          return ui.panel([
            ui.text(name ? ("Hello, " + name) : "Enter your name"),
            ui.input(name, { onChange: { handler: "nameChanged" } })
          ]);
        },
        handlers: {
          nameChanged({ dispatch, event }) {
            dispatch(actions.nameChanged(event.value));
          }
        }
      }
    }
  };
});
```

## Related
- `analysis/01-plugin-system-deep-analysis.md`
- `analysis/02-options-normalize-store-vs-registry-wiring.md`
