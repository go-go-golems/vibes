Set Shell "bash"
Set FontSize 14
Set Width 120
Set Height 120
Set TypingSpeed 50ms

Type "cd /home/ubuntu/graph-generator && ./graph-generator"
Enter
Sleep 2s

# Show the main menu
Screenshot menu.txt
Sleep 1s

# Select the first item (Organizational Chart)
Enter
Sleep 2s
Screenshot input_screen.txt

# Clear the input and type a simple diagram
Ctrl+A
Type "graph TD"
Enter
Type "    A[Start] --> B[Process]"
Enter
Type "    B --> C[End]"
Sleep 1s
Screenshot simple_input.txt

# Generate the diagram
Ctrl+G
Sleep 3s
Screenshot simple_diagram.txt

# Go back to edit
Escape
Sleep 1s

# Go back to menu
Escape
Sleep 1s
Screenshot back_to_menu.txt

# Select a different graph type
Type "j"
Sleep 500ms
Enter
Sleep 2s

# Generate the default template
Ctrl+G
Sleep 3s
Screenshot network_diagram.txt

# Quit
Type "q"
Sleep 1s
Type "q"
Sleep 1s

