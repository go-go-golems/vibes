# VHS Demo Script for Goja JavaScript REPL

# Output file
Output demos/basic_usage.gif

# Terminal settings
Set FontSize 18
Set Width 1000
Set Height 600
Set TypingSpeed 50ms
Set Theme "Catppuccin Mocha"

# Build the REPL first to avoid compilation time during recording
Hide
Type "cd ~/goja-repl && go build -o repl_demo cmd/repl/main.go"
Enter
Sleep 3s
Show

# Start the demo
Type "cd ~/goja-repl && ./repl_demo"
Enter
Sleep 2s

# Basic JavaScript evaluation
Type "2 + 2"
Sleep 500ms
Enter
Sleep 1s

# String manipulation
Type "'Hello, ' + 'Goja REPL!'"
Sleep 500ms
Enter
Sleep 1s

# Variable declaration and usage
Type "const name = 'JavaScript'; `Running ${name} in Go!`"
Sleep 500ms
Enter
Sleep 1s

# Array operations
Type "[1, 2, 3, 4].map(n => n * n).filter(n => n > 5)"
Sleep 500ms
Enter
Sleep 1s

# Function definition and execution
Type "function factorial(n) { return n <= 1 ? 1 : n * factorial(n-1) }; factorial(5)"
Sleep 500ms
Enter
Sleep 1s

# Object manipulation
Type "const obj = { name: 'Goja', type: 'REPL' }; obj.version = '1.0'; obj"
Sleep 500ms
Enter
Sleep 2s

# Show slash commands
Type "/help"
Sleep 500ms
Enter
Sleep 2s

# Exit the REPL
Type "/quit"
Sleep 500ms
Enter
Sleep 2s
