# Dot Vote SSH Application

A real-time dot voting application built with [charmbracelet/wish](https://github.com/charmbracelet/wish) that allows teams to conduct voting sessions over SSH.

## Features

- 🔐 **SSH Key Authentication**: Role-based access control using SSH public keys
- 🗳️ **Real-time Voting**: Live dot voting with immediate feedback
- 👥 **Multi-user Sessions**: Support for multiple participants per session
- 🎨 **Beautiful TUI**: Built with charmbracelet's Bubble Tea and Lip Gloss
- 📊 **Live Results**: Real-time vote tracking and final rankings
- 🔧 **Session Management**: Create, join, and manage voting sessions

## Architecture

The application follows charmbracelet best practices with a clean separation of concerns:

```
dotvote-ssh/
├── cmd/dotvote/           # Main application entry point
├── pkg/
│   ├── auth/              # SSH authentication and role management
│   ├── session/           # Session and voting logic
│   └── ui/
│       ├── model/         # Bubble Tea models for each screen
│       ├── view/          # Lip Gloss styles and rendering
│       ├── keys/          # Key bindings and help
│       └── bubbles/       # Custom bubble components
└── roles.json             # SSH key fingerprint to role mapping
```

## User Roles

### Facilitator
- Create new voting sessions
- Add and manage ideas
- Start and stop voting
- View live results
- Export results

### Participant
- Join existing sessions
- Cast votes on ideas
- View voting progress
- See final results

## Quick Start

### 1. Build the Application

```bash
go build -o dotvote ./cmd/dotvote
```

### 2. Set Up SSH Keys

Generate SSH keys for testing:

```bash
# Facilitator key
ssh-keygen -t ed25519 -f facilitator_key -N ""

# Participant key
ssh-keygen -t ed25519 -f participant_key -N ""
```

### 3. Configure Roles

Create or update `roles.json` with SSH key fingerprints:

```bash
go run create_roles.go
```

This creates a `roles.json` file mapping SSH key fingerprints to roles:

```json
{
  "key_fingerprint_1": "facilitator",
  "key_fingerprint_2": "participant"
}
```

### 4. Start the Server

```bash
./dotvote
```

The server starts on `localhost:2323` by default.

### 5. Connect via SSH

**Facilitator:**
```bash
ssh -i facilitator_key ubuntu@localhost -p 2323
```

**Participant:**
```bash
ssh -i participant_key ubuntu@localhost -p 2323
```

## Usage Flow

### For Facilitators

1. **Connect** via SSH with facilitator key
2. **Create Session** - Choose "Create Mode" and enter session title
3. **Add Ideas** - Add voting topics using 'a' key
4. **Start Voting** - Press 's' to begin the voting phase
5. **Monitor Progress** - Watch live results (optional)
6. **Close Voting** - Press 'c' to end voting and show results
7. **Export Results** - Save results to CSV (if implemented)

### For Participants

1. **Connect** via SSH with participant key
2. **Join Session** - Enter session code and your name
3. **Wait for Voting** - Wait for facilitator to start voting
4. **Cast Votes** - Use space/v to vote on ideas
5. **View Results** - See final rankings when voting closes

## Screen Layouts

### Landing Screen
```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            🗳️  Dot Vote                                     │
│                         Join a voting session                              │
│                                                                             │
│                    Session Code: [____]                                    │
│                    Your Name: [________________]                           │
│                           [ Join Session ]                                 │
│                         TAB Next field  ENTER Join                         │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Facilitator Dashboard
```
┌─ Facilitator Dashboard ────────────────────────────────────────────────────┐
│ Session: ABCD  👥 3 participants                        Status: ⏸️ Setup   │
└─────────────────────────────────────────────────────────────────────────────┘

Ideas (TAB to edit, ENTER to add new):
► Add mobile app support                                              [Edit] [×]
  Improve dashboard performance                                       [Edit] [×]
  [+ Add new idea...]

Settings:
  Dots per person: [5] ←→     ☑️ Allow multiple dots per idea

Controls:
  [🟢 Start Voting]  [🔴 Close Voting]  [👁️ Show Live Results: OFF]
```

### Voting Screen
```
┌─ Dot Vote Session ─────────────────────────────────────────────────────────┐
│ Session: Product Planning                      Dots: 💙💙💙💙💙 (3/5 used)    │
└─────────────────────────────────────────────────────────────────────────────┘

🗳️ Cast your votes!

Ideas:
► Add mobile app support                                              💙💙
  Improve dashboard performance                                       💙
  Dark mode theme
  Better notification system

Your votes:
  💙💙 Add mobile app support
  💙 Improve dashboard performance

↑/↓ Navigate  SPACE/v Vote  x Remove vote  'q' Quit
```

### Results Screen
```
┌─ Voting Results ───────────────────────────────────────────────────────────┐
│ Session: Product Planning                              📊 Final Results     │
└─────────────────────────────────────────────────────────────────────────────┘

Final Rankings:

1. Add mobile app support                    ████████████████████ 6 votes
2. Dark mode theme                          ████████████████▌ 5 votes
3. Improve dashboard performance            ████████████ 3 votes
4. Better notification system              ████████ 2 votes

Participants: Alice, Bob, Carol

┌─ Facilitator Actions ──────────────────────────────────────────────────────┐
│ [📥 Export CSV]  [🔄 New Session]  [↩️ Restart Voting]                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Key Bindings

### Global
- `q` / `Ctrl+C` - Quit application
- `?` - Toggle help
- `↑/↓` - Navigate up/down
- `←/→` - Navigate left/right
- `Enter` - Select/Confirm
- `Esc` - Back/Cancel

### Facilitator Specific
- `s` - Start voting
- `c` - Close voting
- `r` - Reset voting
- `R` - Show results
- `a` - Add new idea
- `e` - Edit selected item
- `d` - Delete selected item
- `n` - New session

### Participant Specific
- `v` / `Space` - Cast vote
- `x` - Remove vote

## Configuration

### Command Line Options

```bash
./dotvote -host localhost -port 2323 -roles roles.json
```

- `-host` - Host to bind to (default: localhost)
- `-port` - Port to listen on (default: 2323)
- `-roles` - Path to roles configuration file (default: roles.json)

### Roles Configuration

The `roles.json` file maps SSH key fingerprints to user roles:

```json
{
  "ssh_key_fingerprint_hex": "facilitator",
  "another_key_fingerprint": "participant"
}
```

## Development

### Prerequisites

- Go 1.23+
- SSH client
- tmux (for testing)
- vhs (for demos, optional)

### Building

```bash
go mod tidy
go build -o dotvote ./cmd/dotvote
```

### Testing

1. Start the server in tmux:
```bash
tmux new-session -d -s dotvote './dotvote'
```

2. Connect with different SSH keys:
```bash
ssh -i facilitator_key ubuntu@localhost -p 2323
ssh -i participant_key ubuntu@localhost -p 2323
```

3. Test the full voting workflow

### Project Structure

Following charmbracelet best practices:

- **cmd/dotvote/main.go** - Application entry point (~40 lines)
- **pkg/ui/model/** - Bubble Tea models (one per screen)
- **pkg/ui/view/** - Centralized Lip Gloss styles
- **pkg/ui/keys/** - Key bindings with help integration
- **pkg/session/** - Business logic for sessions and voting
- **pkg/auth/** - SSH authentication and role management

## Dependencies

- [charmbracelet/wish](https://github.com/charmbracelet/wish) - SSH server framework
- [charmbracelet/bubbletea](https://github.com/charmbracelet/bubbletea) - TUI framework
- [charmbracelet/lipgloss](https://github.com/charmbracelet/lipgloss) - Styling
- [charmbracelet/bubbles](https://github.com/charmbracelet/bubbles) - UI components
- [charmbracelet/ssh](https://github.com/charmbracelet/ssh) - SSH utilities

## License

MIT License - see LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Follow charmbracelet coding guidelines
4. Add tests for new functionality
5. Submit a pull request

## Troubleshooting

### Connection Issues

- Ensure SSH keys are properly configured in `roles.json`
- Check that the server is running on the correct port
- Verify SSH client can connect to the host

### Authentication Problems

- Regenerate `roles.json` using the `create_roles.go` script
- Ensure SSH key fingerprints match exactly
- Check file permissions on SSH keys

### UI Issues

- Ensure terminal supports ANSI colors
- Try resizing terminal window
- Check that terminal width is at least 80 characters

## Future Enhancements

- [ ] Persistent session storage
- [ ] Web interface for results viewing
- [ ] Integration with external authentication systems
- [ ] Advanced voting algorithms (ranked choice, etc.)
- [ ] Session templates and presets
- [ ] Real-time notifications
- [ ] Audit logging
- [ ] Multi-language support

