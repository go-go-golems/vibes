# VHS Demo Script for Goja JavaScript REPL with Tmux Features

# Output file
Output demos/tmux_features.gif

# Terminal settings
Set FontSize 18
Set Width 1200
Set Height 700
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

# Define a function with console.log
Type "function calculateArea(radius) {"
Enter
Type "  console.log(`Calculating area for radius: ${radius}`);"
Enter
Type "  return Math.PI * radius * radius;"
Enter
Type "}"
Sleep 500ms
Enter
Sleep 1s

# Use the function
Type "calculateArea(5)"
Sleep 500ms
Enter
Sleep 1s

# Start tmux session
Type "/tmux start"
Sleep 500ms
Enter
Sleep 3s

# Use the function again to see logs in tmux
Type "calculateArea(10)"
Sleep 500ms
Enter
Sleep 1s

# View logs
Type "/log view"
Sleep 500ms
Enter
Sleep 2s

# Return to REPL
Type "/log return"
Sleep 500ms
Enter
Sleep 1s

# Edit the function
Type "/edit variable calculateArea"
Sleep 500ms
Enter
Sleep 3s

# Exit the REPL
Type "/quit"
Sleep 500ms
Enter
Sleep 2s
