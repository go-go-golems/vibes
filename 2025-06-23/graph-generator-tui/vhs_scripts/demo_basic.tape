Set Shell "bash"
Set FontSize 14
Set Width 120
Set Height 120
Set TypingSpeed 100ms

Type "cd /home/ubuntu/graph-generator && ./graph-generator"
Enter
Sleep 2s

# Show the main menu
Screenshot menu.txt
Sleep 1s

# Navigate through the list
Type "j"
Sleep 500ms
Type "j"
Sleep 500ms
Type "j"
Sleep 500ms
Screenshot menu_navigation.txt

# Select an item
Enter
Sleep 2s
Screenshot input_screen.txt

# Generate a diagram
Ctrl+G
Sleep 3s
Screenshot diagram_view.txt

# Go back to menu
Type "q"
Sleep 1s
Screenshot back_to_menu.txt

# Quit
Type "q"
Sleep 1s

