#!/bin/bash

# Demo script for agent_manager.sh
# Shows how the agent manager works with mock LLM outputs

echo "=== Agent Manager Demo ==="
echo

# Test 1: Switch to CODER mode
echo "1. LLM output with SET_AGENT(CODER):"
echo "---"
./agent_manager.sh "$(cat <<'EOF'
I'll help you implement that feature. Let me switch to coding mode first.

SET_AGENT(CODER)

Now I'll write the function you requested...
EOF
)"
echo

# Test 2: Switch to DEBUGGER mode
echo "2. LLM output with SET_AGENT(DEBUGGER):"
echo "---"
./agent_manager.sh "$(cat <<'EOF'
There seems to be an issue with your code. Let me analyze this systematically.

SET_AGENT(DEBUGGER)

I'll start by examining the error logs and trace the execution path...
EOF
)"
echo

# Test 3: No SET_AGENT command (should show current agent)
echo "3. LLM output with no SET_AGENT (shows current mode):"
echo "---"
./agent_manager.sh "$(cat <<'EOF'
The bug appears to be in the memory allocation routine. The pointer arithmetic
is incorrect on line 42, causing a buffer overflow.
EOF
)"
echo

# Test 4: Unknown agent type
echo "4. LLM output with unknown agent type:"
echo "---"
./agent_manager.sh "$(cat <<'EOF'
Let me switch to a specialized mode for this task.

SET_AGENT(WIZARD)

Time to work some magic on this codebase!
EOF
)"
echo

# Test 5: Switch to ARCHITECT mode
echo "5. LLM output with SET_AGENT(ARCHITECT):"
echo "---"
./agent_manager.sh "$(cat <<'EOF'
This system needs a complete redesign. Let me think about the architecture.

SET_AGENT(ARCHITECT)

I'll start by analyzing the current system structure and identifying bottlenecks...
EOF
)"
echo

echo "=== Demo Complete ==="
