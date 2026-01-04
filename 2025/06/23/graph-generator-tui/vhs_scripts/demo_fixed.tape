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

# Take screenshot of main menu
Screenshot fixed_main_menu.txt
Sleep 2s

# Press Enter to select first item
Enter
Sleep 2s

# Take screenshot of input screen with textarea
Screenshot fixed_org_chart_input.txt
Sleep 2s

# Generate diagram with default template
Ctrl+G
Sleep 4s

# Take screenshot of generated diagram
Screenshot fixed_org_chart_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Select second item (Network Topology)
Type "j"
Sleep 500ms
Enter
Sleep 2s

# Take screenshot of network input
Screenshot fixed_network_input.txt
Sleep 2s

# Generate network diagram
Ctrl+G
Sleep 4s

# Take screenshot of network diagram
Screenshot fixed_network_diagram.txt
Sleep 2s

# Quit application
Type "q"
Sleep 1s
Type "q"
Sleep 1s

