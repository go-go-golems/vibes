Output search-debug.txt

Set FontSize 14
Set Width 120
Set Height 120

Type "cd /home/ubuntu/conversation-manager"
Enter
Sleep 1s

Type "./conversation-manager"
Enter
Sleep 2s

# Start search - this should show the issue
Type "/"
Sleep 1s

# Type search query - check if / appears
Type "python"
Sleep 2s

# Try to navigate results
Type "j"
Sleep 1s

# Try to select result
Enter
Sleep 1s

# Exit
Type "q"
Sleep 1s

