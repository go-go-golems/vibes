# Tmux Integration Research

## Go Libraries for Tmux Integration

### 1. jubnzv/go-tmux
- **GitHub**: https://github.com/jubnzv/go-tmux
- **Stars**: 45
- **Features**:
  - Session management (create, kill, attach)
  - Window management
  - Pane operations
  - Configuration application
- **Last Updated**: 10 months ago
- **Example Usage**:
  ```go
  // Create instance of the running tmux server
  server := new(gotmux.Server)
  
  // Create a new session with windows
  session := gotmux.Session{Name: "example-session"}
  w1 := gotmux.Window{Name: "first", Id: 0}
  w2 := gotmux.Window{Name: "second", Id: 1}
  session.AddWindow(w1)
  session.AddWindow(w2)
  
  // Apply configuration and attach
  conf := gotmux.Configuration{
    Server: server,
    Sessions: []*gotmux.Session{&session},
    ActiveSession: nil
  }
  conf.Apply()
  session.AttachSession()
  ```

### 2. GianlucaP106/gotmux
- **GitHub**: https://github.com/GianlucaP106/gotmux
- **Features**:
  - Comprehensive interface for tmux operations
  - Session, window, and pane management
  - Clean API design

### 3. wricardo/gomux
- **GitHub**: https://github.com/wricardo/gomux
- **Features**:
  - Go wrapper for tmux sessions, windows, and panes
  - Simpler API but less comprehensive

## Integration Requirements for REPL

1. **Session Management**:
   - Create new tmux sessions from within the REPL
   - Attach to existing sessions
   - List available sessions

2. **Editor Integration**:
   - Spawn editors in new panes/windows
   - Edit JavaScript definitions in external editors
   - Return to REPL after editing

3. **Console Log Features**:
   - Fork console output to separate panes
   - Redirect JavaScript console.log to dedicated panes
   - Toggle visibility of log panes

4. **Command Interface**:
   - Add slash commands for tmux operations
   - Provide intuitive syntax for pane/window management

## Recommended Approach

Based on the research, **jubnzv/go-tmux** appears to be the most suitable library for our needs:
- It has a clean, well-documented API
- Provides all the necessary functionality for session/window/pane management
- Has examples that demonstrate the required operations
- Is actively maintained with recent updates

The implementation should:
1. Create a tmux wrapper module in the REPL
2. Add slash commands for tmux operations
3. Implement editor spawning with content synchronization
4. Add console log redirection to separate panes
