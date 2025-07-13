# TaskPlanner - Dynamic Hierarchical Task Planning

A powerful CLI-driven hierarchical task planning system with real-time coordination capabilities, built with Go, SQLite, and Redis.

## 🚀 Features

### Core Functionality
- **Hierarchical Task Organization**: Create complex task hierarchies with parent-child relationships
- **Dynamic Task Planning**: Flexible task creation, modification, and dependency management
- **Priority-Based Management**: 1-10 priority scale for effective task prioritization
- **Effort Estimation**: Track estimated and actual hours for better project planning
- **Status Tracking**: Comprehensive task status management (planned, active, completed, blocked, cancelled)

### Multi-Agent Coordination
- **Agent-Based Assignment**: Assign tasks to specific agents
- **Real-Time Coordination**: Redis-powered task claiming and coordination
- **Task Claiming**: Prevent conflicts with TTL-based task claiming
- **Agent Registration**: Track agent capabilities and status

### Data Management
- **Persistent Storage**: SQLite database for reliable data persistence
- **Real-Time Updates**: Redis for live coordination and notifications
- **Audit Trail**: Complete task history and change tracking
- **Transaction Support**: ACID compliance for data integrity

### CLI Interface
- **Glazed Framework**: Modern CLI with rich formatting options
- **Multiple Output Formats**: JSON, table, and text output
- **Comprehensive Help**: Built-in help system for all commands
- **Environment Configuration**: Flexible configuration via flags and environment variables

## 🏗️ Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   CLI Commands  │    │  Redis Client   │    │ SQLite Database │
│                 │    │                 │    │                 │
│ • plan-*        │    │ • Task Claims   │    │ • Plans         │
│ • task-*        │    │ • Coordination  │    │ • Tasks         │
│ • coordination  │    │ • Notifications │    │ • Dependencies  │
│ • monitoring    │    │ • Agent Status  │    │ • History       │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                    ┌─────────────────┐
                    │  Core Models    │
                    │                 │
                    │ • Plan          │
                    │ • Task          │
                    │ • Agent         │
                    │ • Dependencies  │
                    │ • History       │
                    └─────────────────┘
```

## 📦 Installation

### Prerequisites
- Go 1.21+ (latest version recommended)
- GCC (for SQLite CGO compilation)
- Redis (optional, for real-time coordination)

### Build from Source
```bash
# Clone or extract the project
cd taskplanner

# Install dependencies
go mod tidy

# Build with CGO enabled (required for SQLite)
CGO_ENABLED=1 go build -o taskplanner .

# Make executable
chmod +x taskplanner
```

### System Requirements
- Linux/macOS/Windows
- 50MB disk space
- 32MB RAM minimum

## 🚀 Quick Start

### 1. Set Agent ID
```bash
export AGENT_ID="your-agent-name"
# or use --agent flag with each command
```

### 2. Create a Plan
```bash
./taskplanner plan-create \
    --name "Website Redesign" \
    --description "Complete website overhaul" \
    --status active
```

### 3. Create Tasks
```bash
# Create main task
./taskplanner task-create \
    --title "Frontend Development" \
    --description "Build user interface" \
    --plan-id <PLAN_ID> \
    --priority 9 \
    --estimated-hours 80

# Create subtask
./taskplanner task-create \
    --title "Component Library" \
    --description "Build reusable components" \
    --plan-id <PLAN_ID> \
    --parent-id <PARENT_TASK_ID> \
    --priority 8 \
    --estimated-hours 40
```

### 4. List and Manage
```bash
# List all plans
./taskplanner plan-list

# List tasks for a plan
./taskplanner task-list --plan-id <PLAN_ID>

# Show detailed plan view
./taskplanner plan-show --plan-id <PLAN_ID>
```

## 📚 Command Reference

### Plan Management
```bash
# Create a new plan
./taskplanner plan-create --name "Project Name" --description "Description" --status active

# List all plans
./taskplanner plan-list [--created-by AGENT] [--status STATUS] [--limit N]

# Show plan details with task hierarchy
./taskplanner plan-show --plan-id <ID> [--show-tasks] [--max-depth N]

# Delete a plan
./taskplanner plan-delete --plan-id <ID> [--force]
```

### Task Management
```bash
# Create a task
./taskplanner task-create \
    --title "Task Title" \
    --description "Description" \
    --plan-id <PLAN_ID> \
    [--parent-id <PARENT_ID>] \
    [--priority 1-10] \
    [--agent-id <AGENT>] \
    [--due-date "2024-01-15T10:00:00Z"] \
    [--estimated-hours 8.5]

# List tasks
./taskplanner task-list \
    [--plan-id <PLAN_ID>] \
    [--parent-id <PARENT_ID>] \
    [--agent-id <AGENT>] \
    [--status <STATUS>] \
    [--priority <PRIORITY>]

# Show task details
./taskplanner task-show --task-id <ID>

# Update task (stub - not fully implemented)
./taskplanner task-update --task-id <ID> [options]

# Delete task (stub - not fully implemented)
./taskplanner task-delete --task-id <ID>
```

### Coordination Commands
```bash
# Claim a task for execution
./taskplanner task-claim --task-id <ID> [--ttl 1800]

# Release a task claim
./taskplanner task-release --task-id <ID>

# Assign task to agent
./taskplanner task-assign --task-id <ID> --agent-id <AGENT>
```

### Monitoring
```bash
# Monitor real-time updates (requires Redis)
./taskplanner monitor

# Show system status
./taskplanner status
```

## ⚙️ Configuration

### Environment Variables
```bash
# Agent identification
export AGENT_ID="agent-name"

# Database configuration
export DATABASE_PATH="./taskplanner.db"

# Redis configuration
export REDIS_URL="redis://localhost:6379"

# Logging
export LOG_LEVEL="info"  # debug, info, warn, error
```

### Command Line Flags
```bash
# Global flags available for all commands
--agent string       Agent ID
--database string    SQLite database path (default "./taskplanner.db")
--redis-url string   Redis connection URL (default "redis://localhost:6379")
--format string      Output format: json, table, text (default "json")
--log-level string   Log level (default "info")
```

## 🔧 Development

### Project Structure
```
taskplanner/
├── main.go                 # Application entry point
├── pkg/
│   ├── commands/           # CLI command implementations
│   │   ├── common.go       # Shared utilities
│   │   ├── plan_*.go       # Plan management commands
│   │   ├── task_*.go       # Task management commands
│   │   └── stubs.go        # Placeholder implementations
│   ├── database/           # Database layer
│   │   ├── interface.go    # Database interface
│   │   └── sqlite.go       # SQLite implementation
│   ├── models/             # Data models
│   │   └── models.go       # Core data structures
│   └── redis/              # Redis client
│       └── client.go       # Redis coordination
├── demo.sh                 # Demo script
├── simple_demo.sh          # Simple demo
└── README.md               # This file
```

### Adding New Commands
1. Create command file in `pkg/commands/`
2. Implement the `GlazeCommand` interface
3. Add to command creation functions in `main.go`
4. Update help documentation

### Database Schema
The application uses SQLite with the following main tables:
- `plans` - Project plans
- `tasks` - Individual tasks with hierarchy
- `task_dependencies` - Task dependencies
- `task_history` - Audit trail
- `agents` - Agent registration

## 🧪 Testing

### Run Demo
```bash
# Simple demo
./simple_demo.sh

# Full demo (requires manual plan ID input)
./demo.sh
```

### Manual Testing
```bash
# Test basic functionality
export AGENT_ID="test-agent"
./taskplanner plan-create --name "Test Plan" --status active
./taskplanner plan-list
./taskplanner task-create --title "Test Task" --plan-id <ID> --priority 5
./taskplanner task-list --plan-id <ID>
```

## 🚨 Known Limitations

### Current Implementation Status
- ✅ Core plan and task CRUD operations
- ✅ Hierarchical task relationships
- ✅ SQLite persistence with migrations
- ✅ Redis coordination framework
- ✅ CLI interface with glazed
- ⚠️ Some advanced features are stubs (marked in code)
- ⚠️ Task dependencies not fully implemented
- ⚠️ Agent management partially implemented
- ⚠️ Statistics and reporting stubs

### Redis Dependency
- Application works without Redis (coordination features disabled)
- Redis connection failures are handled gracefully
- Task claiming requires Redis to be running

## 🤝 Contributing

### Development Setup
1. Install Go 1.21+
2. Install build tools: `sudo apt install build-essential`
3. Clone repository
4. Run `go mod tidy`
5. Build with `CGO_ENABLED=1 go build`

### Code Style
- Follow standard Go conventions
- Use structured logging with zerolog
- Implement proper error handling
- Add comprehensive help text for commands

## 📄 License

This project is provided as-is for demonstration purposes.

## 🙋 Support

For questions or issues:
1. Check the help system: `./taskplanner --help`
2. Review command-specific help: `./taskplanner <command> --help`
3. Check the demo scripts for usage examples
4. Review the source code for implementation details

---

**Built with ❤️ using Go, SQLite, Redis, and the Glazed CLI framework**

