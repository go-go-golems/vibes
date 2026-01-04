Output pty-screenshots.gif

Set Shell bash
Set FontSize 14
Set Width 1400
Set Height 900
Set Theme "Dracula"

# Navigate to directory
Type "cd /home/ubuntu/pty-tui-advanced"
Enter
Sleep 1s

# Start the enhanced TUI
Type "./enhanced-pty-tui"
Enter
Sleep 3s

# Take initial state screenshot
Screenshot initial-state.png

# Size preset 1 (tiny)
Type "1"
Sleep 2s
Screenshot size-tiny.png

# Size preset 3 (medium)
Type "3"
Sleep 2s
Screenshot size-medium.png

# Size preset 5 (large)
Type "5"
Sleep 2s
Screenshot size-large.png

# Send command
Type "c"
Sleep 2s
Screenshot command-sent.png

# Change border style
Type "b"
Sleep 1s
Screenshot border-changed.png

# Change theme
Type "t"
Sleep 1s
Screenshot theme-changed.png

# Toggle metrics
Type "m"
Sleep 1s
Screenshot metrics-toggled.png

# Manual resize
Type "r"
Sleep 2s
Screenshot manual-resize.png

# Start auto demo
Type "a"
Sleep 3s
Screenshot auto-demo.png

# Stop auto demo
Type "a"
Sleep 1s
Screenshot auto-stopped.png

# Final state
Screenshot final-state.png

# Exit
Type "q"
Sleep 1s

