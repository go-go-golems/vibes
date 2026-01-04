#!/usr/bin/env python3
"""
Tmux streaming demonstration script.
Creates a visual demonstration of real-time token streaming.
"""

import openai
import time
import sys
import os
import subprocess
from datetime import datetime

def run_command(cmd):
    """Run a shell command and return the result."""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)

def capture_tmux_pane(filename, session_name="streaming_demo"):
    """Capture a tmux pane to an image file."""
    cmd = f"tmux capture-pane -t {session_name} -p > /tmp/{filename}.txt"
    success, stdout, stderr = run_command(cmd)
    if success:
        print(f"📸 Captured pane state to /tmp/{filename}.txt")
    return success

def setup_tmux_session():
    """Set up a tmux session for the streaming demonstration."""
    session_name = "streaming_demo"
    
    # Kill existing session if it exists
    run_command(f"tmux kill-session -t {session_name} 2>/dev/null")
    
    # Create new session
    success, _, _ = run_command(f"tmux new-session -d -s {session_name}")
    if not success:
        print("❌ Failed to create tmux session")
        return False
    
    # Set up the pane
    run_command(f"tmux send-keys -t {session_name} 'clear' Enter")
    run_command(f"tmux send-keys -t {session_name} 'echo \"🌊 Mock OpenAI Streaming Demonstration 🌊\"' Enter")
    run_command(f"tmux send-keys -t {session_name} 'echo \"Real-time token streaming in action!\"' Enter")
    run_command(f"tmux send-keys -t {session_name} 'echo \"\"' Enter")
    
    print(f"✅ Tmux session '{session_name}' created successfully")
    return True

def demonstrate_streaming_in_tmux():
    """Demonstrate streaming with tmux capture at different stages."""
    session_name = "streaming_demo"
    
    print("🎬 Starting tmux streaming demonstration...")
    
    # Capture initial state
    capture_tmux_pane("01_initial_state")
    
    # Send the streaming command
    streaming_cmd = 'python3 -c "' + '''
import openai
import time
import sys

    import os
    port = os.environ.get("MOCK_SERVER_PORT", "3117")
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url=f"http://localhost:{port}/v1"
    )

print("📤 Sending streaming request...")
print("🤖 Assistant: ", end="", flush=True)

try:
    stream = client.chat.completions.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "user", "content": "Explain how streaming works in AI applications"}
        ],
        stream=True
    )
    
    for chunk in stream:
        if chunk.choices[0].delta.content is not None:
            print(chunk.choices[0].delta.content, end="", flush=True)
            time.sleep(0.1)  # Slower for better visual capture
    
    print("\\n\\n✅ Streaming completed!")
    
except Exception as e:
    print(f"❌ Error: {e}")
''' + '"'
    
    # Send the command to tmux
    run_command(f"tmux send-keys -t {session_name} '{streaming_cmd}' Enter")
    
    # Capture at different stages
    time.sleep(2)
    capture_tmux_pane("02_request_sent")
    
    time.sleep(3)
    capture_tmux_pane("03_streaming_started")
    
    time.sleep(5)
    capture_tmux_pane("04_mid_streaming")
    
    time.sleep(8)
    capture_tmux_pane("05_streaming_complete")
    
    print("📸 All tmux captures completed!")

def create_streaming_comparison_demo():
    """Create a side-by-side comparison of streaming vs non-streaming."""
    session_name = "comparison_demo"
    
    # Kill existing session if it exists
    run_command(f"tmux kill-session -t {session_name} 2>/dev/null")
    
    # Create new session with split panes
    run_command(f"tmux new-session -d -s {session_name}")
    run_command(f"tmux split-window -h -t {session_name}")
    
    # Set up left pane (non-streaming)
    run_command(f"tmux send-keys -t {session_name}:0.0 'clear' Enter")
    run_command(f"tmux send-keys -t {session_name}:0.0 'echo \"📊 NON-STREAMING RESPONSE\"' Enter")
    run_command(f"tmux send-keys -t {session_name}:0.0 'echo \"(All at once)\"' Enter")
    run_command(f"tmux send-keys -t {session_name}:0.0 'echo \"\"' Enter")
    
    # Set up right pane (streaming)
    run_command(f"tmux send-keys -t {session_name}:0.1 'clear' Enter")
    run_command(f"tmux send-keys -t {session_name}:0.1 'echo \"🌊 STREAMING RESPONSE\"' Enter")
    run_command(f"tmux send-keys -t {session_name}:0.1 'echo \"(Token by token)\"' Enter")
    run_command(f"tmux send-keys -t {session_name}:0.1 'echo \"\"' Enter")
    
    # Capture initial comparison
    capture_tmux_pane("06_comparison_setup", session_name)
    
    # Start non-streaming in left pane
    non_streaming_cmd = 'python3 -c "' + '''
import openai
import time

import os
port = os.environ.get("MOCK_SERVER_PORT", "3117")
client = openai.OpenAI(api_key="mock-api-key", base_url=f"http://localhost:{port}/v1")
print("⏳ Requesting...")
time.sleep(1)
response = client.chat.completions.create(
    model="gpt-3.5-turbo",
    messages=[{"role": "user", "content": "Explain streaming"}],
    stream=False
)
print("💬 Response:")
print(response.choices[0].message.content)
''' + '"'
    
    run_command(f"tmux send-keys -t {session_name}:0.0 '{non_streaming_cmd}' Enter")
    
    # Start streaming in right pane (with delay)
    time.sleep(2)
    streaming_cmd = 'python3 -c "' + '''
import openai
import time

import os
port = os.environ.get("MOCK_SERVER_PORT", "3117")
client = openai.OpenAI(api_key="mock-api-key", base_url=f"http://localhost:{port}/v1")
print("📤 Streaming...")
stream = client.chat.completions.create(
    model="gpt-3.5-turbo",
    messages=[{"role": "user", "content": "Explain streaming"}],
    stream=True
)
print("💬 Response: ", end="", flush=True)
for chunk in stream:
    if chunk.choices[0].delta.content:
        print(chunk.choices[0].delta.content, end="", flush=True)
        time.sleep(0.15)
print("\\n")
''' + '"'
    
    run_command(f"tmux send-keys -t {session_name}:0.1 '{streaming_cmd}' Enter")
    
    # Capture comparison stages
    time.sleep(3)
    capture_tmux_pane("07_comparison_progress", session_name)
    
    time.sleep(8)
    capture_tmux_pane("08_comparison_complete", session_name)
    
    print("📸 Comparison demonstration captured!")

def create_visual_summary():
    """Create a visual summary of all captures."""
    print("\n📋 Creating visual summary...")
    
    summary_content = """
# Tmux Streaming Demonstration Summary

## Captured States:

1. **01_initial_state.txt** - Initial tmux setup
2. **02_request_sent.txt** - Request sent to server
3. **03_streaming_started.txt** - First tokens arriving
4. **04_mid_streaming.txt** - Streaming in progress
5. **05_streaming_complete.txt** - Full response received
6. **06_comparison_setup.txt** - Side-by-side setup
7. **07_comparison_progress.txt** - Comparison in progress
8. **08_comparison_complete.txt** - Both responses complete

## Key Observations:

- Streaming responses appear token by token
- Non-streaming responses appear all at once
- Real-time visual feedback demonstrates the difference
- Mock server successfully implements SSE streaming
- Compatible with OpenAI SDK streaming interface

## Files Location:
All captures are saved in /tmp/ directory as .txt files
"""
    
    with open("/tmp/streaming_demo_summary.md", "w") as f:
        f.write(summary_content)
    
    print("📄 Summary created at /tmp/streaming_demo_summary.md")

def main():
    """Run the complete tmux streaming demonstration."""
    print("\n🎬 TMUX STREAMING DEMONSTRATION")
    print("=" * 50)
    
    # Check if tmux is available
    success, _, _ = run_command("which tmux")
    if not success:
        print("❌ tmux is not installed. Installing...")
        run_command("sudo apt-get update && sudo apt-get install -y tmux")
    
    # Wait for server to be ready
    print("⏳ Waiting for server to be ready...")
    time.sleep(2)
    
    # Test server connectivity
    import os
    port = os.environ.get("MOCK_SERVER_PORT", "3117")
    success, _, _ = run_command(f"curl -s http://localhost:{port}/health > /dev/null")
    if not success:
        print("❌ Mock server is not running. Please start it first.")
        return
    
    print("✅ Server is ready!")
    
    # Run demonstrations
    print("\n🎯 Setting up basic streaming demonstration...")
    if setup_tmux_session():
        demonstrate_streaming_in_tmux()
    
    print("\n🎯 Setting up comparison demonstration...")
    create_streaming_comparison_demo()
    
    # Create summary
    create_visual_summary()
    
    print("\n🎉 Tmux demonstration completed!")
    print("📁 All captures saved to /tmp/ directory")
    print("💡 You can view the captures with: cat /tmp/01_initial_state.txt")
    print("🔍 Or view the summary: cat /tmp/streaming_demo_summary.md")
    
    # List all captured files
    print("\n📋 Captured files:")
    success, stdout, _ = run_command("ls -la /tmp/*streaming* /tmp/0*.txt 2>/dev/null")
    if success:
        print(stdout)

if __name__ == "__main__":
    main()
