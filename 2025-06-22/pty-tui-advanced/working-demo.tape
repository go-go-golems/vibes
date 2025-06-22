Output advanced-pty-working-demo.gif

Set Shell bash
Set FontSize 14
Set Width 1200
Set Height 800

# Clear logs
Type "rm -f pty-debug.log"
Enter
Sleep 1s

# Start the final working version
Type "./enhanced-pty-final"
Enter
Sleep 3s

# Take initial screenshot
Screenshot working-initial.txt
Screenshot working-initial.png
Sleep 1s

# Start auto demo
Type "a"
Sleep 2s
Screenshot working-auto-start.txt
Screenshot working-auto-start.png

# Let auto demo run through cycles
Sleep 5s
Screenshot working-step-1.txt
Screenshot working-step-1.png

Sleep 5s
Screenshot working-step-2.txt
Screenshot working-step-2.png

Sleep 5s
Screenshot working-step-3.txt
Screenshot working-step-3.png

# Stop auto demo
Type "a"
Sleep 1s
Screenshot working-auto-stop.txt
Screenshot working-auto-stop.png

# Test manual resize
Type "r"
Sleep 2s
Screenshot working-resize-1.txt
Screenshot working-resize-1.png

Type "r"
Sleep 2s
Screenshot working-resize-2.txt
Screenshot working-resize-2.png

# Test command
Type "c"
Sleep 2s
Screenshot working-command.txt
Screenshot working-command.png

# Final screenshot
Sleep 2s
Screenshot working-final.txt
Screenshot working-final.png

# Exit
Type "q"
Sleep 1s

