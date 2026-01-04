Output tui-validation-demo.gif

Set Shell bash
Set FontSize 14
Set Width 1200
Set Height 800
Set Theme "Dracula"

# Show validation capabilities
Type "echo '=== TUI Validation with Text Screenshots ==='"
Enter
Sleep 2s

Type "echo 'This demo shows how text screenshots validate TUI functionality'"
Enter
Sleep 2s

# Build and prepare
Type "echo 'Building enhanced TUI application...'"
Enter
Type "go build -o enhanced-pty-tui enhanced-main.go"
Enter
Sleep 3s

# Take screenshot to validate build
Screenshot build-validation.txt

Type "echo 'Starting TUI with validation points...'"
Enter
Sleep 1s

# Start TUI
Type "./enhanced-pty-tui"
Enter
Sleep 3s

# Take startup validation screenshot
Screenshot startup-validation.txt

# Test size presets with validation
Type "1"
Sleep 2s

# Validate tiny size
Screenshot size-tiny-validation.txt

Type "3"
Sleep 2s

# Validate medium size
Screenshot size-medium-validation.txt

Type "5"
Sleep 2s

# Validate large size
Screenshot size-large-validation.txt

# Test commands with validation
Type "c"
Sleep 2s

# Validate command execution
Screenshot command-validation.txt

# Test border changes with validation
Type "b"
Sleep 1s

# Validate border change
Screenshot border-validation.txt

# Test color theme with validation
Type "t"
Sleep 1s

# Validate theme change
Screenshot theme-validation.txt

# Test metrics toggle
Type "m"
Sleep 1s

# Validate metrics disabled
Screenshot metrics-off-validation.txt

Type "m"
Sleep 1s

# Validate metrics enabled
Screenshot metrics-on-validation.txt

# Test auto demo
Type "a"
Sleep 5s

# Validate auto demo running
Screenshot auto-demo-validation.txt

Type "a"
Sleep 1s

# Final validation
Screenshot final-validation.txt

# Exit
Type "q"
Sleep 2s

# Show validation results
Type "echo '=== Validation Results ==='"
Enter
Sleep 1s

Type "ls -la *validation*.txt | wc -l"
Enter
Sleep 1s

Type "echo 'text screenshot validation files created'"
Enter
Sleep 2s

Type "echo 'Sample validation content:'"
Enter
Type "head -5 startup-validation.txt 2>/dev/null || echo 'Files created during recording'"
Enter
Sleep 2s

