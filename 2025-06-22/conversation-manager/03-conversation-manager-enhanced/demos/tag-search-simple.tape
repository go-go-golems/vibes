Output demo-tag-search-simple.gif

Set FontSize 14
Set Width 1000
Set Height 600

Type "cd /home/ubuntu/conversation-manager && ./conversation-manager"
Enter
Sleep 3s

# Start search mode
Type "/"
Sleep 1s

# Search for python tag
Type "tag:python"
Sleep 2s

# Clear and search for react
Ctrl+U
Type "tag:react"
Sleep 2s

# Navigate results
Type "j"
Sleep 1s

# Clear and try general search
Ctrl+U
Type "machine learning"
Sleep 2s

# Exit search
Escape
Sleep 1s

# Try filter mode
Type "f"
Sleep 2s

# Navigate to tags
Type "right"
Sleep 1s

# Select a tag filter
Type " "
Sleep 1s

# Exit
Escape
Type "q"
Sleep 1s

