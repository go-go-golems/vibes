Output advanced-tui-proper.gif

Set Shell bash
Set FontSize 14
Set Width 1400
Set Height 900
Set Theme "Dracula"

# Navigate to the advanced TUI directory
Type "cd /home/ubuntu/pty-tui-advanced"
Enter
Sleep 1s

# Show what we're about to run
Type "echo '=== Advanced PTY TUI with Borders and Dynamic Resizing ==='"
Enter
Sleep 2s

# Start the advanced TUI application (NOT in background)
Type "./enhanced-pty-tui"
Enter
Sleep 4s

# Take screenshot of the initial TUI state
Screenshot tui-initial-state.txt

# Start auto demo to show dynamic resizing
Type "a"
Sleep 3s

# Take screenshot during auto demo
Screenshot tui-auto-demo.txt

# Let auto demo run for a bit
Sleep 5s

# Stop auto demo
Type "a"
Sleep 1s

# Take screenshot after stopping auto demo
Screenshot tui-auto-stopped.txt

# Show manual size presets
Type "1"
Sleep 2s

# Take screenshot of tiny size
Screenshot tui-size-tiny.txt

Type "3"
Sleep 2s

# Take screenshot of medium size
Screenshot tui-size-medium.txt

Type "5"
Sleep 2s

# Take screenshot of large size
Screenshot tui-size-large.txt

# Send some commands to show PTY interaction
Type "c"
Sleep 2s

# Take screenshot of command execution
Screenshot tui-command-sent.txt

Type "c"
Sleep 2s

# Change border style
Type "b"
Sleep 1s

# Take screenshot of border change
Screenshot tui-border-changed.txt

Type "b"
Sleep 1s

# Change color theme
Type "t"
Sleep 1s

# Take screenshot of theme change
Screenshot tui-theme-changed.txt

# Manual resize cycle
Type "r"
Sleep 2s

# Take screenshot of manual resize
Screenshot tui-manual-resize.txt

Type "r"
Sleep 2s

# Final command
Type "c"
Sleep 2s

# Take final screenshot
Screenshot tui-final-state.txt

# Exit gracefully
Type "q"
Sleep 1s

