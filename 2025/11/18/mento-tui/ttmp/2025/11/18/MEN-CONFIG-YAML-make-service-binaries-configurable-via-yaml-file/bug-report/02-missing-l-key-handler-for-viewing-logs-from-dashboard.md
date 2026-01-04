---
Title: 'Missing ''l'' key handler for viewing logs from dashboard'
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
      Note: Missing case "l" handler in Update() method
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/dashboard.go
      Note: Shows "[L] View Logs" in UI but key is not handled
ExternalSources: []
Summary: Dashboard displays "[L] View Logs" hint but pressing 'l' key does nothing; only 't' key works to view logs
LastUpdated: 2025-11-18T09:32:53.468071616-05:00
---

# Missing 'l' key handler for viewing logs from dashboard

## Description

The dashboard screen displays "[L] View Logs" as a hint in the service card actions, but pressing the `l` key does not navigate to the log viewer. Only the `t` key works to view logs, which is inconsistent with the UI hint.

## Steps to Reproduce

1. Start the application: `go run cmd/main.go`
2. On the dashboard screen, select a service (using arrow keys)
3. Notice the service card shows: "[Enter] Start  [L] View Logs" or "[R] Restart  [S] Stop  [L] View Logs"
4. Press `l` key
5. **Expected**: Navigate to log viewer screen
6. **Actual**: Nothing happens (no navigation)

## Root Cause Analysis

### Code Location

**`internal/ui/app.go`** - The `Update()` method handles key presses but lacks a case for `"l"`:

```go
case tea.KeyMsg:
    switch msg.String() {
    // ... other cases ...
    case "t":
        if m.currentScreen == DashboardScreen {
            m.currentScreen = LogViewerScreen
        }
        return m, nil
    // Missing: case "l": handler
    }
```

### UI Hint Location

**`internal/ui/dashboard.go:165-168`** - The dashboard displays the hint:

```go
if selected {
    actions := "[R] Restart  [S] Stop  [L] View Logs"
    if svc.Status == models.StatusStopped {
        actions = "[Enter] Start  [L] View Logs"
    }
    content.WriteString(actions)
}
```

### Why This Happens

The UI was designed to show `[L]` as a shortcut hint, but the key handler was never implemented. The `t` key handler exists (line 107-111 in app.go), but `l` key handler is missing.

## Impact

- **Severity**: Medium - Feature inconsistency, poor UX
- **Frequency**: Every time user tries to use the advertised shortcut
- **User Impact**: Confusion, users may think the application is broken or unresponsive

## Suggested Fix

Add a case handler for `"l"` key in `internal/ui/app.go`:

```go
case "l":
    if m.currentScreen == DashboardScreen {
        m.currentScreen = LogViewerScreen
        return m, nil
    }
```

Or, to be consistent with the existing `"t"` handler pattern:

```go
case "l", "t":  // Support both 'l' and 't' for logs
    if m.currentScreen == DashboardScreen {
        m.currentScreen = LogViewerScreen
    }
    return m, nil
```

## Additional Considerations

1. **Consistency**: Decide whether to support both `l` and `t`, or remove one hint from UI
2. **Documentation**: Update help screen if both keys are supported
3. **Other screens**: Check if `l` key should work from other screens (e.g., config screen)

## Testing

After fix:
1. Press `l` from dashboard → should navigate to log viewer
2. Press `t` from dashboard → should still work (if keeping both)
3. Verify help screen documents the correct key(s)
4. Test from other screens if applicable

## Validation (tmux + capture-pane)

Run the TUI in a tmux session so we can script keystrokes and capture output to validate behavior before/after the fix.

```bash
# Start the app in tmux (detached)
tmux new-session -d -s mento 'go run cmd/main.go --config ./mento-tui.yaml'
sleep 2

# Capture current screen to a file
tmux capture-pane -p -t mento:0.0 -S -200 > /tmp/before_l.txt

# Send the 'l' key to attempt to open Log Viewer
tmux send-keys -t mento:0.0 l
sleep 1

# Capture after pressing 'l'
tmux capture-pane -p -t mento:0.0 -S -200 > /tmp/after_l.txt

# Check for Log Viewer header
if grep -q 'LOG VIEWER' /tmp/after_l.txt; then
  echo 'PASS: l opens Log Viewer'
else
  echo 'FAIL: l did not open Log Viewer'
fi
```

Notes:
- Before the fix, this should print FAIL (no handler).
- After the fix, this should print PASS. Keep `t` as an alternative shortcut if desired.

## Related Issues

- Help screen may need update to reflect correct key bindings
- Consider audit of all UI hints vs actual key handlers for consistency
