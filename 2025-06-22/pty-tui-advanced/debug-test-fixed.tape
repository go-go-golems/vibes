Output debug-test-fixed.gif

Set Shell bash
Set FontSize 14
Set Width 1000
Set Height 600

# Clear log and start fresh
Type "rm -f pty-debug.log"
Enter
Sleep 1s

# Start the fixed debug version
Type "./enhanced-pty-debug"
Enter
Sleep 3s

# Take initial screenshot
Screenshot fixed-initial.txt
Screenshot fixed-initial.png

# Test auto demo
Type "a"
Sleep 2s
Screenshot fixed-auto-demo.txt
Screenshot fixed-auto-demo.png

# Let it run a few steps
Sleep 5s
Screenshot fixed-running.txt
Screenshot fixed-running.png

# Stop auto demo
Type "a"
Sleep 1s

# Test manual resize
Type "r"
Sleep 2s
Screenshot fixed-resize.txt
Screenshot fixed-resize.png

# Test command
Type "c"
Sleep 2s
Screenshot fixed-command.txt
Screenshot fixed-command.png

# Exit
Type "q"
Sleep 1s

