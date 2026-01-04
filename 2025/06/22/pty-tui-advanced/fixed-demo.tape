Output advanced-pty-fixed-demo.gif

Set Shell bash
Set FontSize 14
Set Width 1200
Set Height 800

# Clear previous logs
Type "rm -f pty-debug.log"
Enter
Sleep 1s

# Start the fixed version
Type "./enhanced-pty-fixed"
Enter
Sleep 3s

# Take initial screenshot
Screenshot fixed-demo-initial.txt
Screenshot fixed-demo-initial.png
Sleep 1s

# Start auto demo mode
Type "a"
Sleep 2s
Screenshot fixed-demo-auto-start.txt
Screenshot fixed-demo-auto-start.png

# Let auto demo run through several cycles
Sleep 4s
Screenshot fixed-demo-step-1.txt
Screenshot fixed-demo-step-1.png

Sleep 4s
Screenshot fixed-demo-step-2.txt
Screenshot fixed-demo-step-2.png

Sleep 4s
Screenshot fixed-demo-step-3.txt
Screenshot fixed-demo-step-3.png

Sleep 4s
Screenshot fixed-demo-step-4.txt
Screenshot fixed-demo-step-4.png

# Stop auto demo
Type "a"
Sleep 1s
Screenshot fixed-demo-auto-stop.txt
Screenshot fixed-demo-auto-stop.png

# Test manual resize
Type "r"
Sleep 2s
Screenshot fixed-demo-resize-1.txt
Screenshot fixed-demo-resize-1.png

Type "r"
Sleep 2s
Screenshot fixed-demo-resize-2.txt
Screenshot fixed-demo-resize-2.png

# Test command execution
Type "c"
Sleep 2s
Screenshot fixed-demo-command.txt
Screenshot fixed-demo-command.png

# Final state
Sleep 2s
Screenshot fixed-demo-final.txt
Screenshot fixed-demo-final.png

# Exit
Type "q"
Sleep 1s

