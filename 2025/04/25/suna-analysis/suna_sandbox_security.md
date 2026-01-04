# Suna Sandbox and Security Architecture

## Sandbox Overview

The Suna agent operates within a sandboxed environment to provide isolation and security when executing potentially risky operations like web browsing, file operations, and shell commands. This sandbox is a critical security component that prevents the agent from affecting the host system or accessing sensitive user data directly.

## Sandbox Implementation

### Container-Based Isolation

The sandbox is implemented using Docker containers with the following characteristics:

1. **Isolation**: Each agent run gets its own isolated container environment
2. **Resource Limitations**: CPU, memory, and disk quotas are enforced
3. **Network Access**: Controlled outbound internet access with restrictions
4. **Persistence**: Container state persists for the duration of the project
5. **Custom Image**: Purpose-built Docker image with necessary tools

### Key Components

The sandbox environment includes:

1. **Headless Browser**: Chromium browser for web automation
2. **File System**: Isolated filesystem for agent operations
3. **Shell Environment**: Restricted shell for command execution
4. **Automation API**: REST API for browser and UI control
5. **Port Exposure**: Controlled exposure of internal services

## Security Architecture

### Multi-Layer Security Model

Suna employs a multi-layered security approach:

1. **Authentication Layer**
   - Supabase authentication for user identity
   - JWT tokens for API access
   - Session management with timeouts

2. **Authorization Layer**
   - Row-Level Security in the database
   - Account-based access control
   - Project and thread ownership verification

3. **Isolation Layer**
   - Container-based sandbox for execution
   - Resource quotas and limits
   - Network filtering and restrictions

4. **Monitoring Layer**
   - Execution logging
   - Error tracking
   - Activity monitoring

### Sandbox Security Features

The sandbox implements several security measures:

1. **Process Isolation**
   - Container-level process isolation
   - Non-root user execution
   - Limited capabilities

2. **Resource Controls**
   - CPU usage limits
   - Memory allocation caps
   - Disk quota enforcement
   - Execution time limits

3. **Network Security**
   - Outbound access only (no inbound)
   - Filtered access to external services
   - No access to internal network resources
   - HTTP(S) protocol restrictions

4. **File System Security**
   - Read-only system directories
   - Writable workspace directory only
   - No access to host filesystem
   - Size limits on created files

5. **Browser Security**
   - Headless operation
   - Disabled plugins and extensions
   - Content security policies
   - Sandboxed rendering

## Sandbox Lifecycle

### Creation

1. The sandbox is created when:
   - A new project is started
   - An existing project is accessed after inactivity

2. Creation process:
   - Generate a unique sandbox ID
   - Create Docker container with workspace
   - Initialize browser and automation API
   - Generate access credentials
   - Store reference in project metadata

### Usage

The agent interacts with the sandbox through several tools:

1. **SandboxShellTool**: Executes shell commands
   - Commands run inside the container
   - Output is captured and returned
   - Sessions can be maintained for stateful operations

2. **SandboxBrowserTool**: Controls the browser
   - Navigation to websites
   - Element interaction (click, input)
   - Content extraction
   - Screenshot capture

3. **SandboxFilesTool**: Manages files
   - File creation and reading
   - Directory operations
   - Content manipulation

4. **ComputerUseTool**: Controls UI
   - Mouse movement and clicks
   - Keyboard input
   - Screen capture
   - Process management

5. **SandboxDeployTool**: Deploys websites
   - Packages web content
   - Deploys to public hosting
   - Provides access URLs

6. **SandboxExposeTool**: Exposes services
   - Maps internal ports to public URLs
   - Enables external access to services
   - Manages access controls

### Termination

Sandboxes are terminated when:
- The project is deleted
- Manual cleanup is triggered
- Extended inactivity occurs
- System maintenance requires it

## Security Considerations

### Data Protection

1. **User Data**: Never stored directly in sandbox
2. **Credentials**: Not exposed to the sandbox environment
3. **API Keys**: Managed by the backend, not the sandbox
4. **Content**: Processed data is temporary and isolated

### Restricted Operations

The sandbox prevents:
1. Access to host system resources
2. Unauthorized network connections
3. Execution of privileged operations
4. Installation of unauthorized software
5. Access to other users' sandboxes

### Monitoring and Auditing

Security is maintained through:
1. Comprehensive logging of all operations
2. Activity tracking in the database
3. Error monitoring and alerting
4. Regular security audits
5. Automated scanning for vulnerabilities

## Technical Implementation Details

### Sandbox Container

The sandbox container is defined with:

```dockerfile
FROM debian:bookworm-slim

# Install dependencies
RUN apt-get update && apt-get install -y \
    python3 python3-pip nodejs npm \
    chromium xvfb \
    supervisor \
    curl wget git zip unzip \
    [additional tools...]

# Set up non-root user
RUN useradd -m -s /bin/bash sandbox
USER sandbox
WORKDIR /home/sandbox/workspace

# Initialize automation API
COPY --chown=sandbox:sandbox ./browser_api.py /home/sandbox/
COPY --chown=sandbox:sandbox ./supervisord.conf /etc/supervisor/conf.d/

# Start services
CMD ["supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
```

### Browser Automation API

The browser automation API provides:

1. **Navigation**: URL loading and history management
2. **Interaction**: Element selection and manipulation
3. **Extraction**: Content capture and processing
4. **Screenshots**: Visual state capturing
5. **JavaScript**: Script execution in page context

### File System Structure

The sandbox file system is organized as:

```
/home/sandbox/
├── workspace/        # Main working directory
│   ├── src/          # Source code
│   ├── data/         # Data files
│   ├── output/       # Generated content
│   └── temp/         # Temporary files
├── browser_api.py    # Browser automation API
└── supervisord.conf  # Service configuration
```

### Networking

The sandbox networking is configured with:

1. **Internal Network**: Container-specific network
2. **Exposed Ports**:
   - 8000: Automation API
   - 8080: Web services
   - 6080: VNC (for debugging)
3. **Outbound Access**: Full HTTP(S) access
4. **Inbound Access**: Controlled through the API gateway

## Conclusion

The sandbox architecture is a critical component of Suna's security model, providing isolation, resource control, and monitoring while allowing the agent to perform a wide range of operations safely. By containing potentially risky operations within an isolated environment, Suna can provide powerful capabilities while maintaining a strong security posture.