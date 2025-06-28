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

# Main menu screenshot
Screenshot main_menu.png
Screenshot main_menu.txt
Sleep 2s

# Select first item
Enter
Sleep 2s

# Input screen screenshot
Screenshot org_input.png
Screenshot org_input.txt
Sleep 2s

# Generate diagram
Ctrl+G
Sleep 4s

# Diagram screenshot
Screenshot org_diagram.png
Screenshot org_diagram.txt
Sleep 2s

# Go back and select network
Type "q"
Sleep 1s
Type "j"
Sleep 500ms
Enter
Sleep 2s

# Network input
Screenshot network_input.png
Screenshot network_input.txt
Sleep 1s

# Generate network diagram
Ctrl+G
Sleep 4s

# Network diagram
Screenshot network_diagram.png
Screenshot network_diagram.txt
Sleep 2s

# Exit
Type "q"
Sleep 1s
Type "q"
Sleep 1s

