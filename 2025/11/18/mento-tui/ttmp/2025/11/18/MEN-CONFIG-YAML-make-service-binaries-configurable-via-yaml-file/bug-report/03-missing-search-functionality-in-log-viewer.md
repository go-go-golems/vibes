---
Title: 'Missing ''/'' search functionality in log viewer'
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
      Note: Header shows "[/] Search" hint but no handler exists in Update() method
ExternalSources: []
Summary: Log viewer displays "[/] Search" hint in header but pressing '/' key does nothing; search functionality is not implemented
LastUpdated: 2025-11-18T09:32:53.468071616-05:00
---

# Missing '/' search functionality in log viewer

## Description

The log viewer screen displays "[/] Search" as a hint in the header, but pressing the `/` key does nothing. The search functionality is advertised but not implemented.

## Steps to Reproduce

1. Start the application: `go run cmd/main.go`
2. Navigate to log viewer screen (press `t` from dashboard)
3. Notice the header shows: "LOG VIEWER ... [TAB] Switch  [/] Search  [ESC] Back"
4. Press `/` key
5. **Expected**: Enter search mode or open search input
6. **Actual**: Nothing happens (no search functionality)

## Root Cause Analysis

### Code Location

**`internal/ui/logviewer.go:112-113`** - Header displays the hint:

```go
header := lipgloss.NewStyle().
    Width(m.width).
    BorderStyle(lipgloss.NormalBorder()).
    BorderBottom(true).
    BorderForeground(ColorBorder).
    Render(fmt.Sprintf(" LOG VIEWER%s[TAB] Switch  [/] Search  [ESC] Back",
        strings.Repeat(" ", m.width-55)))
```

**`internal/ui/logviewer.go:36-66`** - The `Update()` method handles key presses but lacks a case for `"/"`:

```go
func (m LogViewerModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    switch msg := msg.(type) {
    case tea.WindowSizeMsg:
        // ... handle resize ...
    case tea.KeyMsg:
        switch msg.String() {
        case "tab":
            // ... tab switching ...
        case "1", "2", "3", "4":
            // ... tab selection ...
        // Missing: case "/": search handler
        }
    }
    // ...
}
```

### Why This Happens

The UI was designed to show search functionality (common pattern in log viewers), but the implementation was never completed. The hint suggests the feature exists, but no handler or search state exists in the model.

## Impact

- **Severity**: Medium - Missing advertised feature
- **Frequency**: Every time user tries to search logs
- **User Impact**: Frustration, users expect search functionality based on UI hint

## Suggested Implementation

### Option 1: Basic Search (Recommended for MVP)

Add search state and handler:

```go
type LogViewerModel struct {
    manager     *services.Manager
    viewport    viewport.Model
    selectedTab int
    width       int
    height      int
    autoScroll  bool
    searchMode  bool        // NEW: track search mode
    searchQuery string      // NEW: store search query
}

// In Update():
case "/":
    m.searchMode = true
    m.searchQuery = ""
    return m, nil
case "esc":
    if m.searchMode {
        m.searchMode = false
        m.searchQuery = ""
        return m, nil
    }
case tea.KeyMsg:
    if m.searchMode {
        switch msg.String() {
        case "enter":
            // Apply search filter
            m.searchMode = false
            m.updateViewport()
            return m, nil
        case "backspace":
            if len(m.searchQuery) > 0 {
                m.searchQuery = m.searchQuery[:len(m.searchQuery)-1]
            }
            return m, nil
        default:
            // Append to search query
            if len(msg.String()) == 1 {
                m.searchQuery += msg.String()
            }
            return m, nil
        }
    }
```

### Option 2: Use Bubbles TextInput Component

Leverage the existing Bubbles library for better UX:

```go
import "github.com/charmbracelet/bubbles/textinput"

type LogViewerModel struct {
    // ... existing fields ...
    searchInput textinput.Model
    searchMode  bool
}

// Initialize search input
func NewLogViewerModel(manager *services.Manager) LogViewerModel {
    ti := textinput.New()
    ti.Placeholder = "Search logs..."
    ti.CharLimit = 100
    ti.Width = 50
    
    return LogViewerModel{
        // ... existing initialization ...
        searchInput: ti,
        searchMode: false,
    }
}
```

### Filtering Implementation

Update `updateViewport()` to filter by search query:

```go
func (m *LogViewerModel) updateViewport() {
    var lines []models.LogLine
    // ... existing tab filtering ...
    
    // Apply search filter if active
    if m.searchMode && m.searchQuery != "" {
        filtered := make([]models.LogLine, 0)
        query := strings.ToLower(m.searchQuery)
        for _, line := range lines {
            if strings.Contains(strings.ToLower(line.Message), query) ||
               strings.Contains(strings.ToLower(line.Service), query) {
                filtered = append(filtered, line)
            }
        }
        lines = filtered
    }
    
    // ... rest of viewport update ...
}
```

### UI Updates

Update header/footer to show search state:

```go
// In View():
if m.searchMode {
    header = fmt.Sprintf(" LOG VIEWER%sSearch: %s_ [ESC] Cancel",
        strings.Repeat(" ", m.width-30-len(m.searchQuery)),
        m.searchQuery)
} else {
    header = fmt.Sprintf(" LOG VIEWER%s[TAB] Switch  [/] Search  [ESC] Back",
        strings.Repeat(" ", m.width-55))
}

// Update footer to show filter status
filterStatus := "<none>"
if m.searchQuery != "" {
    filterStatus = fmt.Sprintf("'%s' (%d matches)", m.searchQuery, len(lines))
}
```

## Additional Considerations

1. **Search scope**: Should search across all tabs or only current tab?
2. **Case sensitivity**: Case-insensitive search is more user-friendly
3. **Regex support**: Consider regex patterns for advanced users
4. **Highlight matches**: Highlight matching text in log lines
5. **Navigation**: Allow jumping between matches (n/N keys)
6. **Clear search**: Easy way to clear and return to unfiltered view

## Testing

After implementation:
1. Press `/` → should enter search mode
2. Type search query → should filter logs in real-time
3. Press Enter → should apply filter
4. Press ESC → should exit search mode
5. Test with various search queries
6. Test case-insensitive matching
7. Test with empty query
8. Verify search works across all tabs

## Validation (tmux + capture-pane)

Use tmux to drive the UI and capture the pane to validate search mode and filtered output.

```bash
# Start the app in tmux (detached)
tmux new-session -d -s mento 'go run cmd/main.go --config ./mento-tui.yaml'
sleep 2

# Navigate to Log Viewer using 't'
tmux send-keys -t mento:0.0 t
sleep 1

# Capture current screen
tmux capture-pane -p -t mento:0.0 -S -200 > /tmp/before_search.txt

# Try to enter search and query "worker"
tmux send-keys -t mento:0.0 /
tmux send-keys -t mento:0.0 w o r k e r Enter
sleep 1

# Capture after attempting search
tmux capture-pane -p -t mento:0.0 -S -200 > /tmp/after_search.txt

# Heuristics:
# - Before fix: no visible 'Search:' prompt and no filtering → expect FAIL
# - After fix: header shows 'Search:' or filter indicator and content changes → expect PASS

if grep -qi 'Search:' /tmp/after_search.txt; then
  echo 'PASS: search mode visible'
else
  echo 'FAIL: search mode not visible'
fi
```

Tip: You can also `diff -u /tmp/before_search.txt /tmp/after_search.txt` to confirm content changes.

## Related Issues

- Consider adding search to other screens (dashboard, config viewer)
- May want to persist search query when switching tabs
- Consider search history/autocomplete for common queries
