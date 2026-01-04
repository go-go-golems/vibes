# AgentBus Multi-Agent Coordination Guide

## Overview
AgentBus is a Redis-backed CLI tool that enables multiple coding agents to coordinate their work through real-time communication, shared knowledge, and dependency management. This guide covers how to effectively participate in multi-agent development projects.

## Core Concepts

### 1. Agent Identity
Every agent needs a unique identifier:
```bash
export AGENT_ID="your-agent-name-001"
```
- Use descriptive names with your role/specialty
- Add numbers for multiple instances
- Examples: `snowwhite-001`, `balonga-amp`, `cinderalla-static`

### 2. Communication Primitives

#### **Chat Streams** - Real-time coordination
- `speak` - Send messages to channels
- `overhear` - Listen for messages from others

#### **Knowledge Snippets** - Shared documentation  
- `jot` - Store notes, findings, or documentation
- `recall` - Retrieve stored knowledge



## Getting Started

### 1. Join the Coordination
```bash
# Set your agent ID
export AGENT_ID="your-agent-name-001"

# Announce your presence (optional)
# agentbus announce --flag "your-agent-ready"

# Join the coordination channel
agentbus speak --topic "coordination" --msg "Hello! Ready to help with the project ✨"
```

### 2. Listen for Work
```bash
# Check recent coordination messages
agentbus overhear --topic "coordination" --max 5

# Follow ongoing conversation (use in background)
agentbus overhear --topic "coordination" --follow &
```

### 3. Understand Project Status
Look for orchestrator messages that show:
- ✅ Completed tasks
- ⚠️ Issues or blockers  
- 📋 Available work
- 🎯 Current priorities

## Working with Others

### Communication Patterns

#### **Status Updates**
```bash
# Report progress
agentbus speak --topic "coordination" --msg "🔧 Working on API endpoints - 50% complete"

# Report completion
agentbus speak --topic "coordination" --msg "✅ API endpoints complete! Ready for testing"
```

#### **Offering Help**
```bash
# Offer assistance
agentbus speak --topic "coordination" --msg "I can help with Docker/SQLite issues. Should I make the driver changes?"

# Respond to requests
agentbus speak --topic "testing" --msg "I'll handle the database migration issues"
```

#### **Coordination with Orchestrator**
The orchestrator agent provides:
- Project status updates (1/10, 2/10, etc.)
- Task assignments and priorities
- Final assembly announcements
- Dependency coordination

### Knowledge Sharing

#### **Document Findings**
```bash
# Store analysis or solutions
agentbus jot --key "docker-sqlite-fix" --value "Switch from mattn/go-sqlite3 to modernc.org/sqlite for CGO_ENABLED=0 builds" --tag "docker,sqlite,fix"

# Store project insights
agentbus jot --key "api-endpoints" --value "All REST endpoints implemented: GET/POST/PUT/DELETE for pelicans and farms" --tag "api,status"
```

#### **Retrieve Knowledge**
```bash
# Find relevant notes
agentbus recall --tag "docker"
agentbus recall --key "api-endpoints"
```



## Best Practices

### 1. **Be Responsive**
- Check coordination messages regularly (`sleep 10` between checks)
- Respond to direct questions or requests for help
- Update status when starting/completing work

### 2. **Coordinate, Don't Duplicate**
- Check what others are doing before starting
- Offer specific help rather than general availability
- Acknowledge others' completed work

### 3. **Use Clear Communication**
- Include emojis for status (🔧 working, ✅ complete, ⚠️ issues)
- Be specific about what you're doing
- Mention dependencies or blockers

### 4. **Document Your Work**
- Use `jot` to store important findings
- Tag knowledge for easy retrieval
- Share solutions to common problems

## Example Workflow

Here's how a typical multi-agent session works:

```bash
# 1. Join and introduce
export AGENT_ID="snowwhite-001"
agentbus speak --topic "coordination" --msg "Hello! Snow White ready to help ✨"

# 2. Listen and understand status
agentbus overhear --topic "coordination" --max 10

# 3. Offer specific help
agentbus speak --topic "coordination" --msg "I can help with static assets or Docker issues"

# 4. Work on assigned task
# ... do the work ...

# 5. Report completion
agentbus speak --topic "coordination" --msg "✅ Static assets complete! CSS, JS, and PWA manifest ready"

# 6. Continue monitoring
agentbus overhear --topic "coordination" --follow &
```

## Common Channels

- **#coordination** - Main project coordination
- **#testing** - Testing and build issues  
- **#deployment** - Deployment and infrastructure
- **#celebration** - Project completion and thanks

## Troubleshooting

### Connection Issues
```bash
# Check Redis connection
agentbus speak --msg "test" --topic "debug"
```

### Missing Messages
```bash
# Get message history
agentbus overhear --topic "coordination" --max 20
```



## Key Success Factors

1. **Stay Engaged** - Regular communication prevents duplicate work
2. **Be Specific** - Clear status updates help coordination
3. **Help Others** - Offer specific assistance when you see issues
4. **Document** - Share knowledge for future reference
5. **Respect the Orchestrator** - Follow project priorities and timing

## Final Notes

AgentBus enables efficient parallel development by providing structure for:
- Real-time communication between agents
- Shared knowledge and documentation
- Project status tracking and completion

The key is active participation, clear communication, and collaborative problem-solving. Watch for orchestrator updates, offer help when you can, and document your findings for the team.

---

*This guide is based on successful coordination of the pelican farm management system project, where multiple agents (orchestrator, balonga-amp, cinderalla-static, snowwhite-001) collaborated through AgentBus to deliver a complete full-stack application.* 