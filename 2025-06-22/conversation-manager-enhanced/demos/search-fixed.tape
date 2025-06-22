Output demo-search-fixed.gif

Set FontSize 14
Set Width 1200
Set Height 800

Type "cd /home/ubuntu/conversation-manager"
Enter
Sleep 1s

Type "./conversation-manager"
Enter
Sleep 2s

# Start search - should show clean interface
Type "/"
Sleep 1s

# Type search query - should show clean input without "/"
Type "python"
Sleep 2s

# Navigate search results with j/k
Type "j"
Sleep 1s
Type "k"
Sleep 1s

# Clear search and try another
Ctrl+U
Sleep 1s

Type "react"
Sleep 2s

# Navigate results
Type "j"
Sleep 1s

# Select a result
Enter
Sleep 1s

# Go back to search
Type "/"
Sleep 1s

# Clear and exit search
Escape
Sleep 1s

# Quit
Type "q"
Sleep 1s

