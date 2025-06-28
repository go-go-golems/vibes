Set Shell "bash"
Set FontSize 12
Set Width 120
Set Height 120

# Start the application
Type "cd /home/ubuntu/graph-generator && ./graph-generator"
Enter
Sleep 3s

# Show main menu with all graph types
Screenshot final_demo_menu.txt
Sleep 2s

# Demo 1: Organizational Chart
Enter
Sleep 2s
Screenshot final_demo_org_input.txt
Sleep 1s
Ctrl+G
Sleep 3s
Screenshot final_demo_org_output.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Demo 2: Network Topology
Type "j"
Sleep 500ms
Enter
Sleep 2s
Screenshot final_demo_network_input.txt
Sleep 1s
Ctrl+G
Sleep 3s
Screenshot final_demo_network_output.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Demo 3: Process Flow
Type "j"
Sleep 500ms
Enter
Sleep 2s
Screenshot final_demo_process_input.txt
Sleep 1s
Ctrl+G
Sleep 3s
Screenshot final_demo_process_output.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Demo 4: System Architecture
Type "j"
Sleep 500ms
Enter
Sleep 2s
Screenshot final_demo_system_input.txt
Sleep 1s
Ctrl+G
Sleep 3s
Screenshot final_demo_system_output.txt
Sleep 2s

# Go back to menu
Type "q"
Sleep 1s

# Demo filtering feature
Type "/"
Sleep 500ms
Type "database"
Sleep 1s
Screenshot final_demo_filter.txt
Sleep 2s

# Select filtered item
Enter
Sleep 2s
Screenshot final_demo_database_input.txt
Sleep 1s
Ctrl+G
Sleep 3s
Screenshot final_demo_database_output.txt
Sleep 2s

# Exit application
Type "q"
Sleep 1s
Type "q"
Sleep 1s

