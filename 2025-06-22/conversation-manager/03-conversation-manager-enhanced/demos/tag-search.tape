Output demo-tag-search.gif

Set FontSize 14
Set Width 1200
Set Height 800

Type "cd /home/ubuntu/conversation-manager"
Enter
Sleep 1s

Type "./conversation-manager"
Enter
Sleep 2s

# Show initial conversation list with tag indicators
Sleep 2s

# Start search mode
Type "/"
Sleep 1s

# Search for a specific tag using tag: syntax
Type "tag:python"
Sleep 2s

# Clear and try another tag search
Ctrl+U
Sleep 1s

Type "tag:react"
Sleep 2s

# Navigate through search results
Type "j"
Sleep 1s
Type "k"
Sleep 1s

# Clear and try general search that includes tags
Ctrl+U
Sleep 1s

Type "machine learning"
Sleep 2s

# Show tag suggestions with Tab
Type "j"
Sleep 1s

# Select a result
Enter
Sleep 1s

# Go back to search and try tag mode
Type "/"
Sleep 1s

# Try the enhanced tag search mode (Ctrl+T)
Ctrl+T
Sleep 1s

# Type in tag search mode
Type "code"
Sleep 2s

# Select a tag
Enter
Sleep 1s

# Exit search
Escape
Sleep 1s

# Try filter mode to show tag filtering
Type "f"
Sleep 2s

# Navigate to tags category
Type "right"
Sleep 1s

# Navigate through tag options
Type "down"
Sleep 1s
Type "down"
Sleep 1s

# Toggle a tag filter
Type " "
Sleep 1s

# Try the detailed tag view
Type "t"
Sleep 2s

# Navigate in tag details
Type "right"
Sleep 1s
Type "down"
Sleep 1s

# Toggle a tag in details view
Type " "
Sleep 1s

# Exit tag details
Escape
Sleep 1s

# Exit filter mode
Escape
Sleep 1s

# Quit application
Type "q"
Sleep 1s

