# VHS Demo Script for Goja JavaScript REPL - Part 1: Basic Features

# Output file
Output demos/demo_part1.gif

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

# Start the REPL
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
Sleep 2s

# Exit the REPL
Type "/quit"
Sleep 500ms
Enter
Sleep 2s
