# VHS tape file for TUI + Goja Demo
# This demonstrates the enhanced TUI application running in goja

Output demo.gif
Output demo.txt

Set FontSize 14
Set Width 1200
Set Height 800
Set Theme "Dracula"

# Start the TUI application
Type "cd ~/ink-goja-project/go-app && ./tui-app"
Enter
Sleep 2s

# Show initial state
Sleep 1s

# Increment counter a few times
Type "+"
Sleep 500ms
Type "+"
Sleep 500ms
Type "+"
Sleep 500ms

# Increase progress
Type "p"
Sleep 500ms
Type "p"
Sleep 500ms
Type "p"
Sleep 500ms

# Decrement counter
Type "-"
Sleep 500ms

# Show more progress
Type "p"
Sleep 500ms
Type "p"
Sleep 500ms

# Reset everything
Type "r"
Sleep 1s

# Final demonstration
Type "+"
Sleep 300ms
Type "+"
Sleep 300ms
Type "p"
Sleep 300ms
Type "p"
Sleep 300ms

# Wait a moment to show final state
Sleep 2s

# Quit the application
Type "q"
Sleep 1s

