Output advanced-pty-showcase.gif

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

Type "echo 'Starting the enhanced Bubble Tea PTY application...'"
Enter
Sleep 1s

# Start the advanced TUI application
Type "./enhanced-pty-tui"
Enter
Sleep 4s

# Let it initialize and show the bordered interface
Sleep 2s

# Start auto demo to show dynamic resizing
Type "a"
Sleep 2s

# Let the auto demo run for a bit to show resizing
Sleep 8s

# Stop auto demo
Type "a"
Sleep 1s

# Show manual size presets
Type "1"
Sleep 2s

Type "3"
Sleep 2s

Type "5"
Sleep 2s

# Send some commands to show PTY interaction
Type "c"
Sleep 2s

Type "c"
Sleep 2s

# Change border style
Type "b"
Sleep 1s

Type "b"
Sleep 1s

# Change color theme
Type "t"
Sleep 1s

# Manual resize cycle
Type "r"
Sleep 2s

Type "r"
Sleep 2s

# Final command
Type "c"
Sleep 2s

# Exit gracefully
Type "q"
Sleep 1s

