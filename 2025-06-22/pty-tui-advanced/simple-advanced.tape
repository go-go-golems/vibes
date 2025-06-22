Output simple-advanced-demo.gif

Set Shell bash
Set FontSize 16
Set Width 1200
Set Height 800
Set Theme "Dracula"

# Show the advanced TUI briefly
Type "cd /home/ubuntu/pty-tui-advanced && echo 'Advanced PTY TUI Demo'"
Enter
Sleep 2s

Type "./enhanced-pty-tui &"
Enter
Sleep 3s

# Send some key presses to interact with the TUI
Type "1"
Sleep 2s

Type "3"
Sleep 2s

Type "5"
Sleep 2s

Type "c"
Sleep 2s

Type "q"
Sleep 1s

Type "echo 'Advanced PTY TUI demo completed'"
Enter
Sleep 2s

