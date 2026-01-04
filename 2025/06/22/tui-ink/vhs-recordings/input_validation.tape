# VHS tape file for input validation - creates text screenshots
# This validates that the corrected TUI responds to single character input

Output input_validation.txt

Set FontSize 12
Set Width 120
Set Height 120

# Start the TUI application
Type "cd ~/ink-goja-project/go-app && ./tui-app"
Enter
Sleep 2s

# Test increment commands
Type "+"
Sleep 500ms
Type "+"
Sleep 500ms

# Test progress commands  
Type "p"
Sleep 500ms
Type "p"
Sleep 500ms

# Test decrement
Type "-"
Sleep 500ms

# Test reset
Type "r"
Sleep 1s

# Test final increment
Type "+"
Sleep 500ms

# Quit
Type "q"
Sleep 1s

