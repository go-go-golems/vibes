# VHS Demo Script for Goja JavaScript REPL with Enhanced Features (Part 1)

# Output file
Output demos/enhanced_part1.gif

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

# Exit the REPL
Type "/quit"
Sleep 500ms
Enter
Sleep 2s
