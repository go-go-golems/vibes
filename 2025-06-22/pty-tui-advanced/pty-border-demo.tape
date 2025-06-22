Output pty-border-resize-demo.gif

Set Shell bash
Set FontSize 16
Set Width 1400
Set Height 900
Set Theme "Dracula"

# Introduction
Type "echo '=== PTY with Borders and Dynamic Resizing Demo ==='"
Enter
Sleep 2s

Type "echo 'Starting advanced Bubble Tea PTY application...'"
Enter
Sleep 1s

# Start the TUI
Type "./enhanced-pty-tui"
Enter
Sleep 3s

# Take screenshot to validate bordered PTY
Screenshot bordered-pty-initial.txt

# Show the PTY border clearly
Sleep 2s

# Start with size preset 1 (tiny)
Type "1"
Sleep 3s

# Take screenshot of tiny PTY
Screenshot pty-tiny-border.txt

# Resize to medium
Type "3"
Sleep 3s

# Take screenshot of medium PTY
Screenshot pty-medium-border.txt

# Resize to large
Type "5"
Sleep 3s

# Take screenshot of large PTY
Screenshot pty-large-border.txt

# Send a command to show PTY interaction
Type "c"
Sleep 3s

# Take screenshot of command in PTY
Screenshot pty-command-interaction.txt

# Change border style to show visual difference
Type "b"
Sleep 2s

# Take screenshot of different border
Screenshot pty-different-border.txt

Type "b"
Sleep 2s
Type "b"
Sleep 2s

# Take screenshot of another border style
Screenshot pty-thick-border.txt

# Manual resize cycle to show dynamic behavior
Type "r"
Sleep 3s
Type "r"
Sleep 3s
Type "r"
Sleep 3s

# Take screenshot of manual resize
Screenshot pty-manual-resize.txt

# Start auto demo for dynamic resizing
Type "a"
Sleep 8s

# Take screenshot during auto resize
Screenshot pty-auto-resize.txt

# Stop auto demo
Type "a"
Sleep 1s

# Final command
Type "c"
Sleep 2s

# Take final screenshot
Screenshot pty-final-demo.txt

# Exit
Type "q"
Sleep 2s

