Set Shell "bash"
Set FontSize 12
Set Width 120
Set Height 120

Type "cd /home/ubuntu/graph-generator"
Enter
Sleep 1s

Type "./graph-generator"
Enter
Sleep 3s

# Take screenshot of improved main menu
Screenshot improved_main_menu.txt
Sleep 2s

# Select first item (Organizational Chart)
Enter
Sleep 2s

# Take screenshot of improved input screen
Screenshot improved_org_input.txt
Sleep 2s

# Generate diagram
Ctrl+G
Sleep 4s

# Take screenshot of improved diagram view
Screenshot improved_org_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Select different graph type (Simple Process Flow)
Type "j"
Sleep 500ms
Type "j"
Sleep 500ms
Enter
Sleep 2s

# Take screenshot of process flow input
Screenshot improved_process_input.txt
Sleep 2s

# Generate process flow diagram
Ctrl+G
Sleep 4s

# Take screenshot of process flow diagram
Screenshot improved_process_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Test filtering
Type "/"
Sleep 500ms
Type "data"
Sleep 1s

# Take screenshot of filtered results
Screenshot improved_filtered_menu.txt
Sleep 2s

# Clear filter and quit
Escape
Sleep 1s
Type "q"
Sleep 1s

