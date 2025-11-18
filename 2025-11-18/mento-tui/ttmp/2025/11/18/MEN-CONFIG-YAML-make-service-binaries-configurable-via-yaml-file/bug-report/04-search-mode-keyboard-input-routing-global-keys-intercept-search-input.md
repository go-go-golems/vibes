---
Title: 'Search mode keyboard input routing: global keys intercept search input'
Ticket: MEN-CONFIG-YAML
Status: active
Topics:
    - configuration
    - yaml
    - services
DocType: bug-report
Intent: long-term
Owners: []
RelatedFiles:
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/app.go
      Note: Global key handlers intercept keys before they reach child screens, preventing search input from receiving ESC, Enter, and other keys
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/logviewer.go
      Note: Search mode handlers exist but are never reached due to parent-level interception
ExternalSources: []
Summary: When in search mode, global key handlers in app.go intercept ESC, Enter, and other keys before they reach the search input component, preventing proper search functionality
LastUpdated: 2025-11-18T09:32:53.468071616-05:00
---

# Search mode keyboard input routing: global keys intercept search input

## Description

When the log viewer is in search mode (activated by pressing `/`), certain keyboard inputs are intercepted by global key handlers in `app.go` before they reach the search input component. This prevents the search functionality from working correctly.

Specifically:
- **ESC key**: Intercepted by app.go, navigates back to dashboard instead of exiting search mode
- **Enter key**: Intercepted by app.go, tries to start a service instead of applying search filter
- **Other keys**: May be intercepted depending on global handlers

## Steps to Reproduce

1. Start the application: `go run cmd/main.go`
2. Navigate to log viewer screen (press `t` or `l`)
3. Press `/` to enter search mode
4. Type some characters (these work correctly)
5. Press **ESC**
   - **Expected**: Exit search mode, return to normal log viewer
   - **Actual**: Navigates back to dashboard screen
6. Press `/` again to enter search mode
7. Type a search query
8. Press **Enter**
   - **Expected**: Apply search filter and exit search mode
   - **Actual**: Nothing happens (or may try to start service if on dashboard)

## Root Cause Analysis

### Problem: Message Routing Order

The issue stems from the message routing order in `app.go`. Global key handlers are checked **before** passing messages to child screens:

**`internal/ui/app.go:90-133`** - Global key handlers intercept first:
```go
case tea.KeyMsg:
    switch msg.String() {
    case "ctrl+c", "q":
        // ... quit handler ...
    case "h", "?":
        // ... help handler ...
    case "esc":  // ← Intercepts ESC before child screen sees it
        if m.currentScreen != DashboardScreen {
            m.currentScreen = DashboardScreen
        }
        return m, nil
    case "enter":  // ← Intercepts Enter before child screen sees it
        if m.currentScreen == DashboardScreen {
            // ... start service ...
        }
        return m, nil
    }
}

// Only reaches here if no global key matched
// Update current screen
switch m.currentScreen {
case LogViewerScreen:
    tmpModel, cmd = m.logViewer.Update(msg)  // ← Search mode handler never reached
    // ...
}
```

**`internal/ui/logviewer.go:51-69`** - Search mode handlers exist but are unreachable:
```go
if m.searchMode {
    switch msg := msg.(type) {
    case tea.KeyMsg:
        switch msg.String() {
        case "esc":  // ← Never reached due to parent interception
            m.searchMode = false
            m.searchInput.SetValue("")
            m.updateViewport()
            return m, nil
        case "enter":  // ← Never reached due to parent interception
            m.searchMode = false
            m.updateViewport()
            return m, nil
        }
    }
    // Update search input
    m.searchInput, cmd = m.searchInput.Update(msg)
    // ...
}
```

### Why This Happens

The current architecture processes global keys at the parent level (`app.go`) before delegating to child screens. This works well for normal navigation but breaks when child screens need to handle the same keys in a different context (like search mode).

### Similar Patterns in Codebase

This pattern appears in other places:

1. **Dashboard screen** (`dashboard.go:42-64`): Handles keys directly, but doesn't have conflicting global handlers
2. **Viewport components**: Keys are passed through, but viewport handles scrolling keys that don't conflict
3. **No other screens** currently have modal/input states that would conflict with global keys

## Impact

- **Severity**: High - Search functionality is broken
- **Frequency**: Every time user tries to use search
- **User Impact**: Cannot properly exit search mode or apply search filters

## Suggested Fixes

### Option 1: Check child screen state before global handlers (Recommended)

Modify `app.go` to check if the current screen is in a special mode before handling global keys:

```go
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    switch msg := msg.(type) {
    case tea.KeyMsg:
        // Check if current screen needs to handle this key first
        if m.currentScreen == LogViewerScreen {
            // Let log viewer handle it first (may be in search mode)
            tmpModel, cmd := m.logViewer.Update(msg)
            if l, ok := tmpModel.(LogViewerModel); ok {
                m.logViewer = l
                // If log viewer consumed the key (e.g., in search mode), return early
                if m.logViewer.searchMode {
                    return m, cmd
                }
            }
        }
        
        // Now handle global keys if not consumed by child
        switch msg.String() {
        case "ctrl+c", "q":
            // ... existing handlers ...
        case "esc":
            // Only handle if not in a modal state
            if m.currentScreen != DashboardScreen {
                m.currentScreen = DashboardScreen
            }
            return m, nil
        // ... rest of global handlers ...
        }
    }
    
    // Normal message routing for non-key messages
    // ...
}
```

### Option 2: Add "consumed" flag pattern

Have child screens indicate if they consumed a key:

```go
// In logviewer.go
type LogViewerModel struct {
    // ... existing fields ...
    consumedKey bool  // NEW: flag to indicate key was consumed
}

func (m LogViewerModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    m.consumedKey = false
    
    if m.searchMode {
        // ... handle search mode ...
        m.consumedKey = true  // Mark as consumed
        return m, cmd
    }
    // ...
}

// In app.go
case LogViewerScreen:
    tmpModel, cmd := m.logViewer.Update(msg)
    if l, ok := tmpModel.(LogViewerModel); ok {
        m.logViewer = l
        if m.logViewer.consumedKey {
            return m, cmd  // Don't process global handlers
        }
    }
```

### Option 3: Reverse routing order (delegate first, then global)

Always delegate to child screen first, then handle globally if needed:

```go
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    var cmd tea.Cmd
    var tmpModel tea.Model
    
    // Always update current screen first
    switch m.currentScreen {
    case LogViewerScreen:
        tmpModel, cmd = m.logViewer.Update(msg)
        if l, ok := tmpModel.(LogViewerModel); ok {
            m.logViewer = l
            // If in search mode, don't process global keys
            if m.logViewer.searchMode {
                return m, cmd
            }
        }
    // ... other screens ...
    }
    
    // Now handle global keys only if not in special mode
    switch msg := msg.(type) {
    case tea.KeyMsg:
        switch msg.String() {
        case "esc":
            // Only handle if not in search mode
            if m.currentScreen == LogViewerScreen && m.logViewer.searchMode {
                return m, cmd  // Already handled by log viewer
            }
            if m.currentScreen != DashboardScreen {
                m.currentScreen = DashboardScreen
            }
            return m, nil
        // ... other global handlers ...
        }
    }
    
    return m, cmd
}
```

### Recommended Approach

**Option 3 (Reverse routing order)** is recommended because:
1. It's the most flexible - child screens can consume any key
2. Follows the principle of "most specific handler wins"
3. Scales better if other screens add modal/input states
4. Minimal changes to existing code

However, it requires careful handling to ensure global keys still work when not in special modes.

## Key Precedence Matrix (proposed)

While in Log Viewer search mode:

- Global always: `ctrl+c`, `q` (quit should always work)
- Handled by Log Viewer: `esc` (exit search), `enter` (apply filter), alphanumerics and editing keys
- Ignored by global layer while searchMode=true: navigation keys that the input may consume

While NOT in search mode:

- Global: `q`, `ctrl+c`, `h`, `?`, dashboard `enter`, navigation
- Log Viewer: `tab`, `1-4`, `/` (enter search), scrolling keys

This should be documented in code comments and reflected in the Help screen.

## Concrete Implementation Plan

1. In `internal/ui/app.go`, delegate to the active screen first, then handle global keys:

```go
// Pseudocode inside Model.Update
// 1) Update current screen first (so it can consume keys)
switch m.currentScreen {
case LogViewerScreen:
    tmp, cmd := m.logViewer.Update(msg)
    if l, ok := tmp.(LogViewerModel); ok {
        m.logViewer = l
        // If still in search mode after update, skip global handling
        if m.logViewer.searchMode {
            return m, cmd
        }
    }
// ... other screens ...
}

// 2) Now handle global keys (q, ctrl+c, etc.)
switch msg := msg.(type) {
case tea.KeyMsg:
    switch msg.String() { /* existing global handlers */ }
}
```

2. Keep `ctrl+c` and `q` as global regardless of search state for predictable quit behavior.

3. Update `internal/ui/help.go` bindings:
   - Change Log Viewer bindings to reflect that `/` enters search (no longer "coming soon").
   - Add `l` as alternative for Log Viewer from dashboard if we keep both `t` and `l`.

4. Add tests (manual acceptable):
   - tmux script that enters search and validates ESC and Enter are handled by Log Viewer.
   - Verify `q`/`ctrl+c` still quit while in search mode.

## Other Locations To Apply The Pattern

- `internal/ui/config.go`: If/when editing is added (e.g., textinput for config), adopt the same delegate-first routing.
- `internal/ui/help.go`: No input today, but if a search/filter is added, use the same pattern.
- Any future modals or dialogs: Prefer a screen-local "modal state" with delegate-first routing and a clear escape path.

## Documentation/UX Updates

- Update the Help screen to remove "(coming soon)" for search and to document `/` (enter search), `esc` (cancel), `enter` (apply), and the persistent global `q`/`ctrl+c`.
- Consider showing a small inline hint in Log Viewer when search mode is active: "Enter to apply, ESC to cancel" (already implemented in header).

## Additional Considerations

1. **Other special modes**: Consider if other screens might need similar handling (e.g., config editor, filter dialogs)
2. **Key priority**: Document which keys are "global" vs "screen-specific"
3. **Testing**: Ensure global keys still work correctly when NOT in search mode
4. **Future screens**: Consider a pattern for modal/input states that can be reused

## Testing

After fix:
1. Enter search mode → type characters → should work
2. Press ESC in search mode → should exit search mode (not navigate to dashboard)
3. Press Enter in search mode → should apply filter and exit search mode
4. Press ESC when NOT in search mode → should navigate to dashboard (existing behavior)
5. Press Enter on dashboard → should start service (existing behavior)
6. Test all other global keys still work correctly

## Related Issues

- Similar routing issues may exist for other input components (future config editor, etc.)
- Consider establishing a pattern for modal/input state handling across all screens
- Help screen documentation may need updates if key behavior changes
