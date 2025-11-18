---
Title: Analysis: Ordered Map and Search for Config Viewer
Ticket: MEN-CONFIG-ORDERED-SEARCH
Status: active
Topics:
    - ui
    - config
    - search
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles:
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/config.go
      Note: Current implementation uses map[string]string which doesn't preserve order
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/models/models.go
      Note: Config struct defines Database, OAuth, and ServiceConfig as map[string]string
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/logviewer.go
      Note: Reference implementation for search functionality using textinput and search mode
ExternalSources: []
Summary: Analysis of implementing ordered map for environment variable display order and search functionality to filter environment variables in the config viewer
LastUpdated: 2025-11-18T10:02:30.831690643-05:00
---

# Analysis: Ordered Map and Search for Config Viewer

## Problem Statement

The config viewer currently displays environment variables from three categories:
- Database configuration
- OAuth credentials  
- Service configuration

**Current Issues:**
1. **Order instability**: Go's `map[string]string` doesn't preserve insertion order, causing environment variables to appear in different orders on each render
2. **No search capability**: Users cannot filter environment variables to find specific keys or values

## Current Implementation

### Data Structure

**`internal/models/models.go:128-133`**:
```go
type Config struct {
	EnvSources    []EnvSource
	Database      map[string]string
	OAuth         map[string]string
	ServiceConfig map[string]string
}
```

**`internal/ui/config.go:144-155`** - Rendering:
```go
func (m ConfigModel) renderConfigBox(items map[string]string) string {
	var content strings.Builder
	for key, value := range items {  // ← Order not guaranteed!
		line := fmt.Sprintf("%s  %s",
			ConfigKeyStyle.Render(key),
			ConfigValueStyle.Render(value))
		content.WriteString(line)
		content.WriteString("\n")
	}
	return ConfigBoxStyle.Width(m.width - 8).Render(content.String())
}
```

### Why Order Matters

1. **User experience**: Consistent ordering helps users locate variables quickly
2. **Documentation**: If order is meaningful (e.g., grouped by purpose), preserving it aids understanding
3. **Debugging**: Stable order makes it easier to compare configurations across sessions

## Solution 1: Ordered Map Implementation

### Option A: Use External Library

**Recommended: `elliotchance/orderedmap`**

**Pros:**
- Well-maintained, actively used
- Amortized O(1) operations for Set, Get, Delete, Len
- Simple API similar to standard map
- No generics required (works with Go 1.18)

**Cons:**
- Adds external dependency
- Slight performance overhead vs native map

**Installation:**
```bash
go get github.com/elliotchance/orderedmap
```

**Usage Example:**
```go
import "github.com/elliotchance/orderedmap"

// Replace map[string]string with:
om := orderedmap.NewOrderedMap()
om.Set("KEY1", "value1")
om.Set("KEY2", "value2")

// Iterate in order:
for el := om.Front(); el != nil; el = el.Next() {
    key := el.Key.(string)
    value := el.Value.(string)
}
```

**Alternative: `jimschubert/ordered-map`**
- Generic implementation (requires Go 1.18+)
- More type-safe
- Similar performance characteristics

### Option B: Custom Ordered Map Implementation

**Pros:**
- No external dependencies
- Full control over implementation
- Can optimize for specific use case

**Cons:**
- More code to maintain
- Need to handle edge cases
- Testing overhead

**Simple Implementation:**
```go
type OrderedMap struct {
	keys   []string
	values map[string]string
}

func NewOrderedMap() *OrderedMap {
	return &OrderedMap{
		keys:   make([]string, 0),
		values: make(map[string]string),
	}
}

func (om *OrderedMap) Set(key, value string) {
	if _, exists := om.values[key]; !exists {
		om.keys = append(om.keys, key)
	}
	om.values[key] = value
}

func (om *OrderedMap) Get(key string) (string, bool) {
	val, ok := om.values[key]
	return val, ok
}

func (om *OrderedMap) Keys() []string {
	return om.keys
}

func (om *OrderedMap) Iterate(fn func(key, value string)) {
	for _, key := range om.keys {
		fn(key, om.values[key])
	}
}
```

### Recommendation: Use `elliotchance/orderedmap`

**Rationale:**
- Battle-tested library
- Minimal code changes required
- Good performance
- Well-documented

### Implementation Steps

1. **Update `models.Config` struct**:
   ```go
   type Config struct {
       EnvSources    []EnvSource
       Database      *orderedmap.OrderedMap
       OAuth         *orderedmap.OrderedMap
       ServiceConfig *orderedmap.OrderedMap
   }
   ```

2. **Update `loadConfig()` function**:
   ```go
   func loadConfig() *models.Config {
       db := orderedmap.NewOrderedMap()
       db.Set("ONE_ON_ONE_V3_DATABASE_URL", maskSecret(os.Getenv("ONE_ON_ONE_V3_DATABASE_URL")))
       db.Set("WORKFLOWS_DATABASE_URL", maskSecret(os.Getenv("WORKFLOWS_DATABASE_URL")))
       // ... etc
       
       return &models.Config{
           Database: db,
           // ...
       }
   }
   ```

3. **Update `renderConfigBox()` function**:
   ```go
   func (m ConfigModel) renderConfigBox(items *orderedmap.OrderedMap) string {
       var content strings.Builder
       for el := items.Front(); el != nil; el = el.Next() {
           key := el.Key.(string)
           value := el.Value.(string)
           line := fmt.Sprintf("%s  %s",
               ConfigKeyStyle.Render(key),
               ConfigValueStyle.Render(value))
           content.WriteString(line)
           content.WriteString("\n")
       }
       return ConfigBoxStyle.Width(m.width - 8).Render(content.String())
   }
   ```

**Estimated Effort:** 2-3 hours
- Update data structures: 30 min
- Update loadConfig: 30 min
- Update rendering: 30 min
- Testing: 1 hour

## Solution 2: Search Functionality

### Reference Implementation

The log viewer (`internal/ui/logviewer.go`) already implements search functionality that we can adapt:

**Key Components:**
1. **Search mode state**: `searchMode bool`
2. **Text input**: `searchInput textinput.Model`
3. **Search activation**: Press `/` to enter search mode
4. **Filtering logic**: Case-insensitive substring matching

### Implementation Plan

#### 1. Add Search State to ConfigModel

```go
type ConfigModel struct {
	manager     *services.Manager
	viewport    viewport.Model
	config      *models.Config
	width       int
	height      int
	searchMode  bool              // NEW
	searchInput textinput.Model   // NEW
	searchQuery string            // NEW (cached for filtering)
}
```

#### 2. Initialize Search Input

```go
func NewConfigModel(manager *services.Manager) ConfigModel {
	ti := textinput.New()
	ti.Placeholder = "Search env vars..."
	ti.CharLimit = 100
	ti.Width = 50
	
	return ConfigModel{
		manager:     manager,
		viewport:    viewport.New(80, 20),
		config:      loadConfig(),
		searchMode:  false,
		searchInput: ti,
		searchQuery: "",
	}
}
```

#### 3. Handle Search Mode in Update()

```go
func (m ConfigModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	
	// Handle search mode first (similar to logviewer.go)
	if m.searchMode {
		switch msg := msg.(type) {
		case tea.KeyMsg:
			switch msg.String() {
			case "esc":
				m.searchMode = false
				m.searchInput.SetValue("")
				m.searchQuery = ""
				m.updateViewport()
				return m, nil
			case "enter":
				m.searchMode = false
				m.searchQuery = m.searchInput.Value()
				m.updateViewport()
				return m, nil
			}
		}
		// Update search input
		m.searchInput, cmd = m.searchInput.Update(msg)
		m.searchQuery = m.searchInput.Value() // Update filter in real-time
		m.updateViewport()
		return m, cmd
	}
	
	// Normal mode handling
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "/":
			m.searchMode = true
			m.searchInput.SetValue("")
			m.searchInput.Focus()
			return m, nil
		}
	// ... rest of handlers
	}
	
	m.viewport, cmd = m.viewport.Update(msg)
	return m, cmd
}
```

#### 4. Add Filtering Logic

```go
func (m *ConfigModel) updateViewport() {
	var content strings.Builder
	query := strings.ToLower(m.searchQuery)
	
	// Environment Sources (always shown, but could be filtered)
	content.WriteString(ConfigSectionStyle.Render("ENVIRONMENT SOURCES"))
	content.WriteString("\n")
	for _, src := range m.config.EnvSources {
		// Filter env sources if search active
		if query != "" && !strings.Contains(strings.ToLower(src.Path), query) {
			continue
		}
		icon := "✅"
		if !src.Loaded {
			icon = "❌"
		}
		content.WriteString(fmt.Sprintf("%s %s\n", icon, src.Path))
	}
	content.WriteString("\n")
	
	// Database (with filtering)
	content.WriteString(ConfigSectionStyle.Render("DATABASE"))
	content.WriteString("\n")
	content.WriteString(m.renderConfigBox(m.config.Database, query))
	content.WriteString("\n")
	
	// OAuth (with filtering)
	content.WriteString(ConfigSectionStyle.Render("OAUTH CREDENTIALS"))
	content.WriteString("\n")
	content.WriteString(m.renderConfigBox(m.config.OAuth, query))
	content.WriteString("\n")
	
	// Service Config (with filtering)
	content.WriteString(ConfigSectionStyle.Render("SERVICE CONFIGURATION"))
	content.WriteString("\n")
	content.WriteString(m.renderConfigBox(m.config.ServiceConfig, query))
	
	m.viewport.SetContent(content.String())
}

func (m ConfigModel) renderConfigBox(items *orderedmap.OrderedMap, query string) string {
	var content strings.Builder
	queryLower := strings.ToLower(query)
	
	for el := items.Front(); el != nil; el = el.Next() {
		key := el.Key.(string)
		value := el.Value.(string)
		
		// Filter: match key or value
		if queryLower != "" {
			keyMatch := strings.Contains(strings.ToLower(key), queryLower)
			valueMatch := strings.Contains(strings.ToLower(value), queryLower)
			if !keyMatch && !valueMatch {
				continue // Skip if no match
			}
		}
		
		line := fmt.Sprintf("%s  %s",
			ConfigKeyStyle.Render(key),
			ConfigValueStyle.Render(value))
		content.WriteString(line)
		content.WriteString("\n")
	}
	
	return ConfigBoxStyle.Width(m.width - 8).Render(content.String())
}
```

#### 5. Update View() to Show Search UI

```go
func (m ConfigModel) View() string {
	if m.width == 0 {
		return "Loading..."
	}
	
	m.updateViewport()
	
	var b strings.Builder
	
	// Header with search mode support
	var header string
	if m.searchMode {
		left := " CONFIGURATION"
		searchPrompt := fmt.Sprintf("Search: %s", m.searchInput.View())
		right := "[Enter] Apply  [ESC] Cancel"
		rightW := lipgloss.Width(right)
		searchW := max(0, m.width-rightW-lipgloss.Width(left))
		
		header = lipgloss.NewStyle().
			Width(m.width).
			BorderStyle(lipgloss.NormalBorder()).
			BorderBottom(true).
			BorderForeground(ColorBorder).
			Render(lipgloss.JoinHorizontal(lipgloss.Top,
				lipgloss.NewStyle().Width(lipgloss.Width(left)).Render(left),
				lipgloss.NewStyle().Width(searchW).Render(searchPrompt),
				lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
			))
	} else {
		left := " CONFIGURATION"
		right := "[E] Edit  [/] Search  [ESC] Back"
		rightW := lipgloss.Width(right)
		leftW := max(0, m.width-rightW)
		
		header = lipgloss.NewStyle().
			Width(m.width).
			BorderStyle(lipgloss.NormalBorder()).
			BorderBottom(true).
			BorderForeground(ColorBorder).
			Render(lipgloss.JoinHorizontal(lipgloss.Top,
				lipgloss.NewStyle().Width(leftW).Render(left),
				lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
			))
	}
	
	b.WriteString(header)
	b.WriteString("\n\n")
	
	// Viewport with config
	b.WriteString(m.viewport.View())
	
	// Footer with search status (optional)
	if m.searchQuery != "" {
		footer := lipgloss.NewStyle().
			Width(m.width).
			BorderStyle(lipgloss.NormalBorder()).
			BorderTop(true).
			BorderForeground(ColorBorder).
			Padding(0, 1).
			Render(fmt.Sprintf(" Filter: '%s'", m.searchQuery))
		b.WriteString("\n")
		b.WriteString(footer)
	}
	
	return b.String()
}
```

### Search Behavior Options

**Option 1: Real-time filtering (Recommended)**
- Filter updates as user types
- Immediate feedback
- Matches log viewer behavior

**Option 2: Apply on Enter**
- User types query, presses Enter to apply
- Less frequent viewport updates
- More predictable behavior

**Recommendation:** Real-time filtering for consistency with log viewer

### Search Scope

**Option A: Search all sections**
- Single search query filters Database, OAuth, ServiceConfig, and EnvSources
- Simpler UX

**Option B: Section-specific search**
- Search within current section only
- More complex but more precise

**Recommendation:** Option A (search all sections) for simplicity

### Estimated Effort: 3-4 hours
- Add search state: 30 min
- Implement search mode handling: 1 hour
- Add filtering logic: 1 hour
- Update UI: 1 hour
- Testing: 30 min

## Combined Implementation

### Dependencies

**New:**
- `github.com/elliotchance/orderedmap` (for ordered map)

**Existing:**
- `github.com/charmbracelet/bubbles/textinput` (already used in logviewer.go)

### Files to Modify

1. **`internal/models/models.go`**
   - Change `map[string]string` to `*orderedmap.OrderedMap` for Database, OAuth, ServiceConfig

2. **`internal/ui/config.go`**
   - Add search state fields to ConfigModel
   - Update loadConfig() to use OrderedMap
   - Update renderConfigBox() to accept OrderedMap and query
   - Add search mode handling in Update()
   - Update View() to show search UI
   - Add filtering logic in updateViewport()

3. **`go.mod`**
   - Add `github.com/elliotchance/orderedmap` dependency

### Testing Considerations

1. **Order preservation:**
   - Verify environment variables appear in insertion order
   - Test that order doesn't change between renders
   - Test that order persists after search filter

2. **Search functionality:**
   - Test entering search mode (`/`)
   - Test exiting search mode (ESC)
   - Test applying filter (Enter)
   - Test real-time filtering as user types
   - Test case-insensitive matching
   - Test matching both keys and values
   - Test empty search query (shows all)
   - Test search across all sections

3. **Edge cases:**
   - Empty config sections
   - Very long environment variable names/values
   - Special characters in search query
   - Window resize during search mode

### Potential Issues

1. **Type assertions**: OrderedMap uses `interface{}` for keys/values, requiring type assertions
   - **Mitigation**: Add helper methods or wrapper functions

2. **Performance**: Filtering on every keystroke could be slow with many env vars
   - **Mitigation**: Current implementation is simple and should be fast enough; optimize if needed

3. **Search mode routing**: Need to ensure global keys don't intercept search input (similar to log viewer fix)
   - **Mitigation**: Follow the same pattern as log viewer (delegate to ConfigModel first)

### Migration Path

1. **Phase 1**: Add OrderedMap support (backward compatible if we add conversion helpers)
2. **Phase 2**: Add search functionality
3. **Phase 3**: Update help screen to document search feature

## Summary

### Total Estimated Effort: 5-7 hours

**Breakdown:**
- Ordered Map implementation: 2-3 hours
- Search functionality: 3-4 hours
- Integration and testing: 1 hour

### Benefits

1. **Consistent ordering**: Environment variables always appear in the same order
2. **Better UX**: Users can quickly find specific environment variables
3. **Consistency**: Search functionality matches log viewer pattern
4. **Maintainability**: Clear separation of concerns

### Risks

1. **Low**: Well-understood patterns (ordered map libraries, search from log viewer)
2. **Breaking changes**: Minimal - only internal data structure changes
3. **Performance**: Should be fine for typical config sizes (< 100 vars)

### Next Steps

1. Review and approve approach
2. Create implementation tasks
3. Implement ordered map first
4. Add search functionality
5. Test and document
