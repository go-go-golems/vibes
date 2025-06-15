# VHS Demo Script for Goja JavaScript REPL with Tmux and Vim Integration

# Output file
Output demos/full_demo.gif

# Terminal settings
Set FontSize 18
Set Width 1200
Set Height 700
Set TypingSpeed 50ms
Set Theme "Catppuccin Mocha"

# Build the REPL first to avoid compilation time during recording
Hide
Type "cd ~/goja-repl && go build -o repl cmd/repl/main.go"
Enter
Sleep 3s
Show

# Start tmux session
Type "tmux new-session -s goja-demo"
Enter
Sleep 2s

# Start the REPL inside tmux
Type "cd ~/goja-repl && ./repl"
Enter
Sleep 3s

# Basic JavaScript evaluation
Type "2 + 2"
Sleep 500ms
Enter
Sleep 1s

# Demonstrate code wrapping with a long string
Type "const longString = 'This is a very long string that should wrap automatically to fit within the terminal width. The REPL now supports proper code wrapping for better readability.'"
Sleep 500ms
Enter
Sleep 2s

# Demonstrate multiline input with Ctrl+J
Type "function calculateArea(radius) {"
Sleep 500ms
Ctrl+j
Sleep 1s
Type "  // Calculate the area of a circle"
Sleep 500ms
Ctrl+j
Sleep 1s
Type "  return Math.PI * radius * radius;"
Sleep 500ms
Ctrl+j
Sleep 1s
Type "}"
Sleep 500ms
Enter
Sleep 1s
Type ""
Sleep 500ms
Enter
Sleep 2s

# Use the defined function
Type "calculateArea(5)"
Sleep 500ms
Enter
Sleep 1s

# Demonstrate vim integration
Type "/vim"
Sleep 500ms
Enter
Sleep 2s

# In vim, type some JavaScript code
Type "i"
Sleep 500ms
Type "// This code was written in vim"
Sleep 500ms
Enter
Type "const numbers = [1, 2, 3, 4, 5];"
Sleep 500ms
Enter
Type "const doubled = numbers.map(n => n * 2);"
Sleep 500ms
Enter
Type "console.log('Doubled numbers:', doubled);"
Sleep 500ms
Enter
Type "doubled.reduce((sum, n) => sum + n, 0);"
Sleep 500ms
Escape
Sleep 500ms
Type ":wq"
Sleep 500ms
Enter
Sleep 2s

# Show the result of the vim-edited code
Sleep 2s

# Demonstrate tmux log window
Type "/log view"
Sleep 500ms
Enter
Sleep 2s

# Return to REPL
Type "/log return"
Sleep 500ms
Enter
Sleep 1s

# Exit the REPL
Type "/quit"
Sleep 500ms
Enter
Sleep 1s

# Exit tmux
Type "exit"
Sleep 500ms
Enter
Sleep 2s
