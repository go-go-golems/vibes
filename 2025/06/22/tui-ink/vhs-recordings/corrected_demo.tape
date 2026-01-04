# VHS tape file for corrected TUI + Goja Demo
# This demonstrates the fixed TUI application with proper input handling

Output corrected_demo.gif
Output corrected_demo.txt

Set FontSize 14
Set Width 1200
Set Height 800
Set Theme "Dracula"

# Start the TUI application
Type "cd ~/ink-goja-project/go-app && ./tui-app"
Enter
Sleep 3s

# Show initial state
Sleep 1s

# Increment counter multiple times (single character input)
Type "+"
Sleep 800ms
Type "+"
Sleep 800ms
Type "+"
Sleep 800ms

# Increase progress
Type "p"
Sleep 800ms
Type "p"
Sleep 800ms
Type "p"
Sleep 800ms

# Decrement counter
Type "-"
Sleep 800ms

# Show more progress
Type "p"
Sleep 800ms
Type "p"
Sleep 800ms

# Reset everything
Type "r"
Sleep 1500ms

# Final demonstration
Type "+"
Sleep 600ms
Type "+"
Sleep 600ms
Type "p"
Sleep 600ms
Type "p"
Sleep 600ms

# Wait a moment to show final state
Sleep 2s

# Quit the application
Type "q"
Sleep 1s

