Output demo-date-search.gif

Set FontSize 14
Set Width 1000
Set Height 600

Type "cd /home/ubuntu/conversation-manager && ./conversation-manager"
Enter
Sleep 3s

# Start search mode
Type "/"
Sleep 1s

# Search for conversations from today
Type "range:today"
Sleep 2s

# Clear and search for last week
Ctrl+U
Type "range:last-week"
Sleep 2s

# Clear and try after: syntax
Ctrl+U
Type "after:yesterday"
Sleep 2s

# Clear and try before: syntax
Ctrl+U
Type "before:2024-06-22"
Sleep 2s

# Clear and combine with tag search
Ctrl+U
Type "tag:python range:today"
Sleep 2s

# Clear and try date range mode (Ctrl+D)
Ctrl+U
Sleep 1s
Ctrl+D
Sleep 1s

# Type in date mode
Type "last-30-days"
Sleep 2s

# Select the date range
Enter
Sleep 2s

# Exit search
Escape
Sleep 1s

# Try another date search
Type "/"
Sleep 1s

# Search for specific date
Type "on:2024-06-21"
Sleep 2s

# Navigate results
Type "j"
Sleep 1s

# Exit and quit
Escape
Type "q"
Sleep 1s

