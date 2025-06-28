Set Shell "bash"
Set FontSize 14
Set Width 120
Set Height 120

Type "cd /home/ubuntu/graph-generator"
Enter
Sleep 1s

Type "./graph-generator"
Enter
Sleep 3s

# Main menu - both PNG and text
Screenshot screenshots/main_menu.png
Screenshot screenshots/main_menu.txt
Sleep 2s

# Select organizational chart
Enter
Sleep 2s

# Input screen - both formats
Screenshot screenshots/org_chart_input.png
Screenshot screenshots/org_chart_input.txt
Sleep 2s

# Generate diagram
Ctrl+G
Sleep 4s

# Generated diagram - both formats
Screenshot screenshots/org_chart_diagram.png
Screenshot screenshots/org_chart_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Select network topology
Type "j"
Sleep 500ms
Enter
Sleep 2s

# Network input screen
Screenshot screenshots/network_input.png
Screenshot screenshots/network_input.txt
Sleep 2s

# Generate network diagram
Ctrl+G
Sleep 4s

# Network diagram output
Screenshot screenshots/network_diagram.png
Screenshot screenshots/network_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Select process flow
Type "j"
Sleep 500ms
Enter
Sleep 2s

# Process flow input
Screenshot screenshots/process_input.png
Screenshot screenshots/process_input.txt
Sleep 2s

# Generate process diagram
Ctrl+G
Sleep 4s

# Process diagram output
Screenshot screenshots/process_diagram.png
Screenshot screenshots/process_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Test filtering
Type "/"
Sleep 500ms
Type "database"
Sleep 1s

# Filtered menu
Screenshot screenshots/filtered_menu.png
Screenshot screenshots/filtered_menu.txt
Sleep 2s

# Select database schema
Enter
Sleep 2s

# Database input
Screenshot screenshots/database_input.png
Screenshot screenshots/database_input.txt
Sleep 2s

# Generate database diagram
Ctrl+G
Sleep 4s

# Database diagram
Screenshot screenshots/database_diagram.png
Screenshot screenshots/database_diagram.txt
Sleep 2s

# Exit application
Type "q"
Sleep 1s
Type "q"
Sleep 1s

