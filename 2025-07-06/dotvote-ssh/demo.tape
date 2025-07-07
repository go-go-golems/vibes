Output demo.gif

Set FontSize 14
Set Width 1200
Set Height 800

Type "# Dot Voting SSH Application Demo"
Enter
Sleep 1s

Type "# Starting the SSH server..."
Enter
Type "cd /home/ubuntu/dotvote-ssh"
Enter
Type "export PATH=/usr/local/go/bin:$PATH"
Enter
Type "./dotvote &"
Enter
Sleep 2s

Type "# The server is now running on localhost:2323"
Enter
Type "# Users can connect with their SSH keys:"
Enter
Type "# - Facilitators can create and manage sessions"
Enter
Type "# - Participants can join sessions and vote"
Enter
Sleep 2s

Type "# Example connection (facilitator):"
Enter
Type "ssh -i facilitator_key ubuntu@localhost -p 2323"
Enter
Sleep 3s

Type "# Example connection (participant):"
Enter
Type "ssh -i participant_key ubuntu@localhost -p 2323"
Enter
Sleep 3s

Type "# Features:"
Enter
Type "# ✅ Role-based authentication with SSH keys"
Enter
Type "# ✅ Session creation and management"
Enter
Type "# ✅ Real-time dot voting"
Enter
Type "# ✅ Live results and final rankings"
Enter
Type "# ✅ Beautiful TUI with charmbracelet components"
Enter
Sleep 2s

Screenshot demo_screenshot.png

