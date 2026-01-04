# Tmux Integration Testing Guide

## Prerequisites
- Ensure tmux is installed: `which tmux && tmux -V`
- If not installed, install with: `sudo apt-get install -y tmux`

## Manual Testing Steps

### 1. Basic REPL Functionality
1. Build the REPL: `cd ~/goja-repl && go build -o repl cmd/repl/main.go`
2. Start the REPL: `./repl`
3. Test basic JavaScript: `2 + 2`
4. Test code wrapping with a long string
5. Test multiline input with Ctrl+J

### 2. Tmux Session Management
1. Start a tmux session from REPL: `/tmux start`
2. Verify the session is created with multiple windows
3. Test switching between windows

### 3. Vim Integration
1. Use the vim command: `/vim`
2. Edit code in vim
3. Save and exit vim (`:wq`)
4. Verify the code is inserted into the REPL

### 4. Log Window
1. Switch to log window: `/log view`
2. Execute code with console.log statements
3. Verify logs appear in the log window
4. Return to main window: `/log return`

### 5. Editor Integration
1. Define a variable or function
2. Edit it with: `/edit variable [name]`
3. Verify changes are applied

## Troubleshooting
- If tmux commands fail, ensure tmux is installed and in PATH
- Check for error messages in the REPL output
- Verify the tmux session exists with: `tmux ls`
- If a session is stuck, kill it with: `tmux kill-session -t goja-repl`
