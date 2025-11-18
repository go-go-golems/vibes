---
Title: 'Resize crash: negative Repeat count in UI components'
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
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/logviewer.go
      Note: Line 113 and 134 have strings.Repeat calls that can go negative
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/dashboard.go
      Note: Lines 86, 99, 136, 150 have strings.Repeat calls that can go negative
ExternalSources: []
Summary: Program crashes with "strings: negative Repeat count" panic when terminal is resized to a small width, causing calculations in strings.Repeat() to become negative
LastUpdated: 2025-11-18T09:32:53.468071616-05:00
---

# Resize crash: negative Repeat count in UI components

## Description

When the terminal window is resized to a small width (typically < 55 characters), the program crashes with a panic:

```
panic: strings: negative Repeat count
```

The crash occurs in multiple UI components that use `strings.Repeat()` for spacing calculations without checking if the result would be negative.

## Steps to Reproduce

1. Start the application: `go run cmd/main.go`
2. Resize the terminal window to a narrow width (< 55 characters)
3. The program immediately crashes with the panic

Alternatively:
1. Start the application
2. Navigate to the Log Viewer screen (press `t`)
3. Resize terminal to narrow width
4. Crash occurs

## Root Cause Analysis

The issue stems from multiple locations where `strings.Repeat()` is called with calculated values that can become negative when the terminal width is small:

### Affected Locations

1. **`internal/ui/logviewer.go:113`**
   ```go
   strings.Repeat(" ", m.width-55)
   ```
   - Crashes when `m.width < 55`

2. **`internal/ui/logviewer.go:134`**
   ```go
   strings.Repeat(" ", m.width-len(tabs)*12-30)
   ```
   - Crashes when `m.width < (len(tabs)*12 + 30)` ≈ 78 characters

3. **`internal/ui/dashboard.go:86`**
   ```go
   strings.Repeat(" ", m.width-50)
   ```
   - Crashes when `m.width < 50`

4. **`internal/ui/dashboard.go:99`**
   ```go
   strings.Repeat(" ", m.width-35)
   ```
   - Crashes when `m.width < 35`

5. **`internal/ui/dashboard.go:136`**
   ```go
   strings.Repeat(" ", 50-len(svc.Name))
   ```
   - Crashes when service name length > 50 characters

6. **`internal/ui/dashboard.go:150`**
   ```go
   strings.Repeat(" ", 50-len(svc.Status.String()))
   ```
   - Less likely but could crash if status string > 50 chars

### Why This Happens

The `strings.Repeat()` function in Go panics when given a negative count. The UI components calculate spacing dynamically based on terminal width but don't guard against cases where the terminal is too narrow for the fixed-width elements (headers, footers, etc.).

## Impact

- **Severity**: High - Application crashes completely
- **Frequency**: Occurs whenever terminal is resized to narrow width
- **User Impact**: Data loss (unsaved state), poor user experience

## Suggested Fixes

### Option 1: Guard with max() helper (Recommended)

Create a helper function to ensure non-negative values:

```go
func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

// Usage:
strings.Repeat(" ", max(0, m.width-55))
```

### Option 2: Use conditional spacing

Check before calling Repeat:

```go
spacing := m.width - 55
if spacing < 0 {
    spacing = 0
}
strings.Repeat(" ", spacing)
```

### Option 3: Prefer Lipgloss-based layout (no manual spacing)

Leverage Lipgloss' width measurement and layout helpers instead of computing spaces. This avoids negative counts entirely and handles narrow terminals gracefully.

Key building blocks:
- `lipgloss.Width(s string)` — measure printable width
- `style.Width(n)` and `.Align()` — constrain and align content to a fixed cell width
- `lipgloss.JoinHorizontal/JoinVertical` — compose blocks without manual padding

Examples:

1) Log Viewer header (left title + right controls):

```go
left := " LOG VIEWER"
right := "[TAB] Switch  [/] Search  [ESC] Back"

rightW := lipgloss.Width(right)
leftW := max(0, m.width-rightW)

header := lipgloss.JoinHorizontal(lipgloss.Top,
    lipgloss.NewStyle().Width(leftW).Render(left),
    lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
)
```

2) Log Viewer tab bar (tabs on left, auto-scroll status on right):

```go
tabs := []string{"Identity", "Frontend", "Worker", "All"}
var tabsSeg strings.Builder
tabsSeg.WriteString(" ")
for i, t := range tabs {
    if i == m.selectedTab {
        tabsSeg.WriteString(ButtonActiveStyle.Render(t))
    } else {
        tabsSeg.WriteString(ButtonStyle.Render(t))
    }
}
right := fmt.Sprintf("Auto-scroll: %s", map[bool]string{true: "ON", false: "OFF"}[m.autoScroll])
rightW := lipgloss.Width(right)
leftW := max(0, m.width-rightW)

tabBar := lipgloss.JoinHorizontal(lipgloss.Top,
    lipgloss.NewStyle().Width(leftW).Render(tabsSeg.String()),
    lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
)
```

3) Dashboard header (left title + right help):

```go
left := " MENTO SERVICES MANAGER"
right := "[Q] Quit  [H] Help"
rightW := lipgloss.Width(right)
leftW := max(0, m.width-rightW)

header := lipgloss.JoinHorizontal(lipgloss.Top,
    lipgloss.NewStyle().Width(leftW).Render(left),
    lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
)
```

4) Service card lines (fixed left column without len/Repeat):

```go
nameCol := lipgloss.NewStyle().Width(50).Render(ServiceNameStyle.Render(svc.Name))
nameLine := nameCol + fmt.Sprintf("Port: %s", portStr)

statusCol := lipgloss.NewStyle().Width(50).Render(StatusStyle(svc.Status.String()).Render(svc.Status.String()))
statusLine := fmt.Sprintf("%s %s%s", svc.Status.Icon(), statusCol, pidInfo)
```

This removes all negative-space calculations, uses display width (including wide runes), and degrades gracefully on narrow terminals (content wraps or truncates instead of panicking).

Also consider an early minimum-width guard for better UX:

```go
if m.width < 40 { // pick a sensible minimum
    return "Terminal too narrow. Please widen the window."
}
```

### Recommended Approach

1. Refactor headers, tab bars, and fixed-width rows to Lipgloss-based layouts as above
2. Use `lipgloss.Width()` instead of `len()` for measuring text widths
3. Keep a small `max()` helper where arithmetic is still needed (Go 1.21+ has builtin max)
4. Add a minimum-width fallback message to each screen's `View()`

## Testing

After fix:
1. Test with various terminal widths (narrow to wide)
2. Test resize operations dynamically
3. Test with long service names
4. Verify no panics occur

## Related Issues

- Similar pattern may exist in other UI components (config.go, help.go)
- Consider a comprehensive audit of all `strings.Repeat()` calls
