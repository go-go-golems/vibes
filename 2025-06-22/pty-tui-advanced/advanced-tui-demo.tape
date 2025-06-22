Output advanced-pty-tui-demo.gif

Set Shell bash
Set FontSize 14
Set Width 1600
Set Height 1000
Set Theme "Dracula"

# Start the advanced TUI application
Type "./enhanced-pty-tui"
Enter
Sleep 3s

# Take initial screenshot to validate TUI startup
Screenshot tui-startup-validation.txt

# Start auto demo to show dynamic resizing
Type "a"
Sleep 1s

# Take screenshot to validate auto demo started
Screenshot auto-demo-started.txt

# Let the auto demo run through several resize cycles
Sleep 15s

# Take screenshot during auto demo
Screenshot auto-demo-running.txt

# Stop auto demo
Type "a"
Sleep 1s

# Manual resize demonstrations
Type "1"
Sleep 2s

# Take screenshot of tiny size
Screenshot resize-tiny.txt

Type "3"
Sleep 2s

# Take screenshot of medium size
Screenshot resize-medium.txt

Type "5"
Sleep 2s

# Take screenshot of extra large size
Screenshot resize-extra-large.txt

# Send some manual commands
Type "c"
Sleep 2s
Type "c"
Sleep 2s

# Take screenshot of command execution
Screenshot command-execution.txt

# Change border style
Type "b"
Sleep 1s
Type "b"
Sleep 1s

# Take screenshot of different border style
Screenshot border-style-change.txt

# Change color theme
Type "t"
Sleep 1s
Type "t"
Sleep 1s

# Take screenshot of different color theme
Screenshot color-theme-change.txt

# Toggle metrics
Type "m"
Sleep 1s

# Take screenshot with metrics disabled
Screenshot metrics-disabled.txt

Type "m"
Sleep 1s

# Manual resize cycle
Type "r"
Sleep 2s
Type "r"
Sleep 2s
Type "r"
Sleep 2s

# Take final screenshot
Screenshot final-state.txt

# Exit gracefully
Type "q"
Sleep 2s

