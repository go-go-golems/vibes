# i3 IPC Protocol Research

## Overview
i3 window manager provides an IPC (Inter-Process Communication) interface using Unix domain sockets. This allows external programs to query window manager state and send commands.

## Socket Connection
- Default socket path: `$XDG_RUNTIME_DIR/i3/ipc-socket.%p` where %p is the PID of i3
- Can be obtained via: `i3 --get-socketpath`
- Environment variable: `I3SOCK`

## Message Format
All messages use binary format:
- Magic string: "i3-ipc" (6 bytes)
- Message length: 32-bit integer (4 bytes)
- Message type: 32-bit integer (4 bytes)
- Payload: variable length

## Key Message Types for Window Management

### GET_TREE (type 4)
- **Purpose**: Get the complete i3 layout tree
- **Message**: No payload required
- **Reply**: JSON tree structure with all containers and windows

### RUN_COMMAND (type 0)
- **Purpose**: Execute i3 commands (focus, move, etc.)
- **Message**: Command string as payload
- **Reply**: Success/error status

### GET_WORKSPACES (type 1)
- **Purpose**: Get list of current workspaces
- **Message**: No payload
- **Reply**: Array of workspace objects

## Tree Node Properties (GET_TREE Reply)

### Essential Properties for Window Management:
- **id** (integer): Internal container ID
- **name** (string): Container name (window title for windows)
- **type** (string): "root", "output", "con", "floating_con", "workspace", "dockarea"
- **focused** (bool): Whether this container is currently focused
- **focus** (array of integer): Child node IDs in focus order
- **window** (integer or null): X11 window ID of actual client window
- **window_properties** (map): Contains title, instance, class, window_role, machine, transient_for
- **window_type** (string): Window type (normal, dialog, utility, toolbar, etc.)

### Geometry Properties:
- **rect** (map): Absolute display coordinates {x, y, width, height}
- **window_rect** (map): Client window coordinates relative to container
- **deco_rect** (map): Window decoration coordinates
- **geometry** (map): Original window geometry when mapped

### Layout Properties:
- **layout** (string): "splith", "splitv", "stacked", "tabbed", "dockarea", "output"
- **orientation** (string): "none", "horizontal", "vertical" (OBSOLETE)
- **percent** (float or null): Percentage of parent container
- **border** (string): "normal", "none", "pixel"
- **current_border_width** (integer): Border width in pixels

### State Properties:
- **urgent** (bool): Whether container has urgency hint
- **marks** (array of string): List of marks assigned to container
- **sticky** (bool): Whether window is sticky (present on all workspaces)
- **fullscreen_mode** (integer): 0 (no fullscreen), 1 (fullscreen on output), 2 (fullscreen globally)
- **floating** (string): Floating state information

## Commands for Focus and Cursor Control

### Focus Commands:
- `focus left|right|up|down` - Focus adjacent window
- `focus parent|child` - Focus parent/child container
- `focus output <output>` - Focus specific output
- `[criteria] focus` - Focus window matching criteria

### Window Selection Criteria:
- `[class="<class>"]` - Match window class
- `[instance="<instance>"]` - Match window instance
- `[title="<title>"]` - Match window title
- `[id="<id>"]` - Match container ID
- `[window_id="<window_id>"]` - Match X11 window ID

### Workspace Commands:
- `workspace <name>` - Switch to workspace
- `workspace number <num>` - Switch to numbered workspace
- `move container to workspace <name>` - Move window to workspace

## Cursor Movement
i3 itself doesn't control cursor position, but external tools can:
- Use `xdotool mousemove <x> <y>` to move cursor
- Calculate cursor position from window_rect coordinates
- Focus window first, then move cursor to its center

## Implementation Strategy for Go Program

1. **Connect to i3 IPC socket**
2. **Query tree structure** using GET_TREE
3. **Parse JSON response** to build window/workspace list
4. **Display in bubbletea TUI** with navigation
5. **Send focus commands** when user selects window/workspace
6. **Move cursor** to focused window center using external tool

## Next Steps
- Search for existing Go i3 IPC libraries
- Research bubbletea implementation patterns
- Test i3-msg commands with current setup

