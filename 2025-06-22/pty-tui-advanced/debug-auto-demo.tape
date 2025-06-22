Output debug-auto-demo.gif

Set Shell bash
Set FontSize 14
Set Width 1200
Set Height 800

# Clear any existing log
Type "rm -f pty-debug.log"
Enter
Sleep 1s

# Start the debug version
Type "./enhanced-pty-debug"
Enter
Sleep 2s

# Take initial screenshot
Screenshot debug-initial.txt
Screenshot debug-initial.png
Sleep 1s

# Start auto demo
Type "a"
Sleep 1s
Screenshot debug-auto-start.txt
Screenshot debug-auto-start.png

# Let auto demo run for several steps
Sleep 3s
Screenshot debug-step-1.txt
Screenshot debug-step-1.png

Sleep 3s
Screenshot debug-step-2.txt
Screenshot debug-step-2.png

Sleep 3s
Screenshot debug-step-3.txt
Screenshot debug-step-3.png

Sleep 3s
Screenshot debug-step-4.txt
Screenshot debug-step-4.png

# Stop auto demo
Type "a"
Sleep 1s
Screenshot debug-auto-stop.txt
Screenshot debug-auto-stop.png

# Exit
Type "q"
Sleep 1s

# Show the debug log
Type "echo '=== DEBUG LOG ANALYSIS ==='"
Enter
Type "tail -50 pty-debug.log"
Enter
Sleep 2s
Screenshot debug-log-analysis.txt
Screenshot debug-log-analysis.png

