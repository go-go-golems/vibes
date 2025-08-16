# Refactoring for async TUI integration in the JavaScript + Uhoh REPL

This document explains the changes attempted to make uhoh TUIs work inside the existing REPL UI without launching a separate Bubble Tea program, how printing/logging was redirected to avoid overlapping the active UI, where we ended up, and concrete next steps to finish the async flow so `createUI` can resolve with final values.

## Goals

- Avoid launching a new `tea.Program` for uhoh forms from JS and instead render the form inside the existing REPL UI.
- Avoid alt-screen buffer switching that made it appear like the program "blanked" or never returned.
- Redirect `console.log` output into the REPL console history so it never overlaps or corrupts the form view.
- Move toward an async model where `createUI` can resolve with final values after the user submits the form, without blocking the entire application.

## What was tried (chronological highlights)

1) Initial symptom: Running `./repl examples/test_simple.js` showed the form, but submitting seemed to never return. The logs showed it blocked in `Program.Run()` with alt-screen.

2) Headless/auto-submit experiment: Added an `auto_submit` flag to `createUI` to skip UI and immediately return defaults. This verified value extraction and avoided blocking, but it wasn’t the intended UX. Removed afterward.

3) Removed alt-screen usage: Both the REPL (`cmd/main.go`) and evaluator form runner were adjusted to avoid `tea.WithAltScreen()`. This prevented the “blank screen”/buffer confusion. Also switched to running `*huh.Form` directly where possible instead of creating a new `tea.Program`.

4) Attempted delegation via channel: Added a channel (`UIRequest`/`UIReply`) so the evaluator could request the REPL to host the form. This worked conceptually but introduced unnecessary coupling. We reverted this to a simpler signal approach.

5) Final direction: Return a small “UI signal” from `createUI` to JS; the REPL detects this, builds the form as a child model, and replaces the input panel while keeping the history visible. No extra program, no alt-screen, no channels.

6) Redirect printing: Intercepted `console.log` in the evaluator and forwarded lines into the REPL’s history viewport via a hook, avoiding overlap with the active form.

## Current architecture

### REPL model layout (single Program)

- Top-level REPL model renders three areas:
  - Console history viewport (top)
  - Input widget (bottom) OR uhoh form model (bottom) when active
  - A small help/footer line

### Evaluator (`pkg/evaluator/js_uhoh_evaluator.go`)

- `NewJSUhohEvaluator` now:
  - Sets up `console.log` to call an `outputHook` (if set) instead of writing to stdout.
  - Exposes `SetOutputHook(func(string))` so the REPL can wire logging to its history viewport.
  - Exposes a simple event registration API `on(event, fn)` (to be used with async flows/promise resolution later) and an internal `Emit(event, payload)` helper for the host to notify JS.

- `createUI(formDef)` now:
  - Builds the uhoh form and initial values from YAML using `uhoh.BuildBubbleTeaModelFromYAML`.
  - Returns a UI signal object to JS rather than running the form: `{ "__uhoh_ui__": true, form_yaml: string, request_id: number }`.
  - The form is not executed in the evaluator anymore; it is hosted in the REPL.

### REPL (`cmd/repl_model.go`)

- On `NewREPLModel`, the REPL registers `eval.SetOutputHook(...)` so any `console.log` lines append to the history viewport and never conflict with the form rendering.

- During evaluation of JS input:
  - If the result is a JSON object with `__uhoh_ui__ == true`, the REPL:
    - Reads `form_yaml`, builds the form via `uhoh.BuildBubbleTeaModelFromYAML`, and stores `m.child = form` and `m.childVals = vals`.
    - Replaces the input area with the child model and forwards Bubble Tea messages (key/window size) to it.
  - If not a UI signal, the text result is appended to the history viewport.

- Child completion: Not fully implemented yet. The REPL currently forwards messages to the child; when the form finishes, the REPL needs to detect completion, extract final values from `m.childVals`, and then either print results and/or notify JS.

## Why it looked like “auto-submitting”

Because `createUI` now returns immediately with the UI signal object, any `console.log("Result:", result)` in JS prints that immediate signal rather than waiting for user submission. No form submission occurs at that point—the child form is only mounted by the REPL afterward.

## What’s missing (and how to finish it)

We’re close. Two key pieces remain:

1) Detecting child completion in the REPL and extracting final values.
   - The REPL has `m.childVals` (initial values map) from `uhoh.BuildBubbleTeaModelFromYAML`.
   - On completion, call `uhoh.ExtractFinalValues(m.childVals)` to get the final values.
   - Then clear `m.child = nil` and restore the input area.

2) Async return of final values to JS.
   - Provide `createUIAsync(formDef)` in JS that returns a Promise.
   - Implementation pattern (based on go-go-goja async docs):
     - `createUIAsync` calls internal `createUI` to get `{ request_id }` and immediately returns a Promise.
     - Register `on('ui:completed', fn)` listener in JS; when the REPL signals completion with the matching `request_id`, resolve the Promise with final values.
   - Host-side signaling:
     - When the child form completes, the REPL should call `eval.Emit('ui:completed', { request_id, values })`.
     - The evaluator will invoke any JS listeners registered via `on('ui:completed', fn)`.

### Concrete next steps

1) REPL child completion wrapper
- Implement a thin wrapper around the child form to detect completion without quitting the entire REPL. Options:
  - Wrap the `tea.Model` form with a controller model that:
    - Forwards `Update`/`View` to the form
    - Intercepts `tea.QuitMsg` or form’s completion condition to trigger completion
    - On completion: extract final values with `uhoh.ExtractFinalValues(m.childVals)`, then:
      - Append a summary of values to the history viewport
      - Call `eval.Emit('ui:completed', { request_id, values })`
      - Set `m.child = nil` and restore input

2) Promise-based `createUIAsync` in evaluator/JS
- Add a JS function (registered in the evaluator) similar to:
  ```javascript
  function createUIAsync(def) {
    const signal = createUI(def); // returns { __uhoh_ui__: true, request_id }
    return new Promise((resolve, reject) => {
      on('ui:completed', (payload) => {
        if (payload && payload.request_id === signal.request_id) {
          resolve(payload.values);
        }
      });
      on('ui:error', (payload) => {
        if (payload && payload.request_id === signal.request_id) {
          reject(payload.error || 'unknown');
        }
      });
    });
  }
  ```
- Register the function inside the evaluator alongside `createUI`.

3) Finalize printing
- Ensure all `console.log` output goes through the `outputHook` so logs are always appended to the history and not rendered directly to stdout.
- Optionally format multi-line objects via JSON pretty-print in REPL before appending to history.

4) README updates
- Document the sync signal-based `createUI` and the async `createUIAsync` (Promise) patterns with examples.

5) Optional: unify direct file execution
- Either keep direct `./repl file.js` as a blocking, form-running mode using `huh.Form.Run()` (works for one-off scripts), or reuse the same UI signal pattern by embedding a minimal REPL host even for single-run mode. The former is simpler for now.

## Files touched and important changes

- `pkg/evaluator/js_uhoh_evaluator.go`
  - Removed running a new Bubble Tea Program for forms; instead, return a UI signal `{ __uhoh_ui__, form_yaml, request_id }`.
  - Added `SetOutputHook(func(string))` for redirecting `console.log` to REPL history.
  - Implemented a tiny event system `on(event, fn)` and `Emit(event, payload)` to support async completion notifications.
  - Left `loadFile`/`Evaluate` behavior intact except for formatting results and logging.

- `cmd/main.go`
  - Removed `tea.WithAltScreen()` to avoid buffer switching; REPL runs on the normal screen.

- `cmd/repl_model.go`
  - Top-level REPL model now renders three parts: history viewport, input widget, and optional child form (replacing the input when active).
  - Detects the UI signal from `createUI` result; builds the uhoh model via YAML; stores the initial values map; forwards messages to the child.
  - Wires evaluator `SetOutputHook` so `console.log` appears in the history viewport.
  - Completion detection is pending (see next steps above).

## Known limitations

- `createUI` returns immediately (with a signal), so any JS logging of the returned value will show the signal, not final values.
- Child completion detection and JS resolution are not yet implemented (requires the wrapper model + `Emit('ui:completed', ...)`, plus the Promise function in JS).
- No explicit error propagation from the child UI path back to JS yet (add `ui:error`).

## Troubleshooting notes

- If the form "disappears" unexpectedly, verify alt-screen is not enabled anywhere. Both REPL and child hosting must avoid `tea.WithAltScreen()` to keep output predictable.
- Always run inside `tmux` if terminal behavior is inconsistent; it makes it easier to capture and inspect pane output.
- Check `repl.log` for detailed evaluator logging (JS execution flow, createUI calls, YAML, etc.).

## Appendix: code sketches

### Wrapper child model for completion
```go
type childHost struct {
    inner     tea.Model
    done      bool
}

func (c childHost) Init() tea.Cmd { return c.inner.Init() }
func (c childHost) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    // forward to inner and detect Quit/completion
    if _, ok := msg.(tea.QuitMsg); ok {
        c.done = true
        // return self to signal done; REPL can check c.done
        return c, nil
    }
    m, cmd := c.inner.Update(msg)
    c.inner = m
    return c, cmd
}
func (c childHost) View() string { return c.inner.View() }
```

### On child completion in REPL
```go
// pseudo inside REPL Update
if host, ok := m.child.(childHost); ok && host.done {
    finalVals, _ := uhoh.ExtractFinalValues(m.childVals)
    // print to history
    // emit event for JS: eval.Emit("ui:completed", map[string]any{"request_id": reqID, "values": finalVals})
    m.child = nil
}
```

### JS async wrapper
```javascript
function createUIAsync(def) {
  const signal = createUI(def);
  return new Promise((resolve, reject) => {
    on('ui:completed', ({request_id, values}) => {
      if (request_id === signal.request_id) resolve(values);
    });
    on('ui:error', ({request_id, error}) => {
      if (request_id === signal.request_id) reject(error);
    });
  });
}
```

---

This refactor positions the project to: (1) render forms inside the REPL without extra Programs or alt-screen; (2) cleanly redirect logs to history; (3) complete the async flow so JS can `await createUIAsync(def)` and receive final values upon user submission.


