Output search-fixed.txt

Set FontSize 14
Set Width 120
Set Height 120

Type "cd /home/ubuntu/conversation-manager"
Enter
Sleep 1s

Type "./conversation-manager"
Enter
Sleep 2s

# Start search - should NOT show "/" in input
Type "/"
Sleep 1s

# Type search query - should show clean input
Type "python"
Sleep 2s

# Navigate search results with j/k
Type "j"
Sleep 1s
Type "k"
Sleep 1s

# Try another search
Ctrl+U
Sleep 1s
Type "react"
Sleep 2s

# Navigate and select
Type "j"
Sleep 1s
Enter
Sleep 1s

# Exit
Type "q"
Sleep 1s

