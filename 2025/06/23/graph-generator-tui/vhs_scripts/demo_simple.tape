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
Screenshot main_menu.txt
Sleep 2s

# Press Enter to select first item
Enter
Sleep 2s

# Take screenshot of input screen
Screenshot org_chart_input.txt
Sleep 2s

# Generate diagram with default template
Ctrl+G
Sleep 4s

# Take screenshot of generated diagram
Screenshot org_chart_diagram.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Take screenshot of menu again
Screenshot menu_return.txt
Sleep 1s

# Quit application
Type "q"
Sleep 1s

