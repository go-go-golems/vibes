# Suna Architecture Analysis

## Core Components

### 1. Agent Core (`backend/agent`)

The agent's core functionality is implemented in the `agent` directory, with these primary files:

- **api.py**: Defines the FastAPI endpoints for agent management:
  - `/thread/{thread_id}/agent/start`: Start an agent run for a specific thread
  - `/agent-run/{agent_run_id}/stop`: Stop a running agent
  - `/thread/{thread_id}/agent-runs`: Get all agent runs for a thread
  - `/agent-run/{agent_run_id}`: Get status and details of a specific agent run
  - `/agent-run/{agent_run_id}/stream`: Stream agent responses in real-time

- **run.py**: Implements the main agent execution loop:
  - Initializes tools and LLM connections
  - Manages execution flow and iteration control
  - Handles streaming responses back to the caller
  - Processes tool usage and responses
  - Tracks billing and subscription status

- **prompt.py**: Contains the system prompt that defines the agent's capabilities:
  - Core identity and capabilities
  - Execution environment details
  - Tool usage guidelines
  - Content creation rules
  - Communication protocols
  - Workflow management via todo.md
  - Data processing guidelines

### 2. Tool System (`backend/agent/tools`)

The tools provide various capabilities to the agent:

- **sb_browser_tool.py**: Enables web browsing and interaction:
  - Navigation: Go to URLs, back/forward, scrolling
  - Element interaction: Click, input text, send keyboard shortcuts
  - Content extraction: Get page text, screenshot, element details
  - Automated browser actions: Wait for elements, execute JavaScript

- **web_search_tool.py**: Performs web searches and content extraction:
  - `web_search`: Search the web using Tavily API
  - `crawl_webpage`: Extract full text content from specific webpages

- **sb_shell_tool.py**: Executes shell commands in the sandbox environment:
  - Run terminal commands and capture output
  - Manage sessions for long-running commands
  - Execute file and system operations

- **sb_files_tool.py**: Manages file operations:
  - Create, read, update, and delete files
  - List directories and get file information
  - Upload and download files

- **computer_use_tool.py**: Controls the sandbox computer directly:
  - Mouse movement and clicks
  - Keyboard input
  - Screen capture
  - Automated UI interaction

- **data_providers_tool.py**: Accesses structured data from various sources:
  - LinkedIn: Profile and company data
  - Twitter: User and tweet data
  - Zillow: Real estate listings
  - Amazon: Product information
  - Yahoo Finance: Stock and company data

- **message_tool.py**: Facilitates user communication:
  - Send messages to users
  - Receive user input
  - Display attachments

- **sb_deploy_tool.py**: Deploys static websites:
  - Package and deploy websites to Cloudflare Pages
  - Generate public URLs for created content

- **sb_expose_tool.py**: Exposes internal services:
  - Make sandbox services accessible publicly
  - Generate access URLs for running applications

### 3. AgentPress Framework (`backend/agentpress`)

AgentPress is a framework for managing agent execution:

- **thread_manager.py**: Manages conversation threads:
  - Store and retrieve messages
  - Track tool usage
  - Manage conversation context

- **tool_registry.py**: Registers and manages available tools:
  - Tool discovery and registration
  - Tool invocation and execution
  - Tool result handling

- **response_processor.py**: Processes LLM responses:
  - Extract tool calls from LLM responses
  - Execute tools and capture results
  - Stream responses back to the client

- **context_manager.py**: Manages conversation context:
  - Optimize context window usage
  - Select relevant messages for context
  - Maintain conversation history

- **tool.py**: Base classes for tool implementation:
  - Defines tool interface and contract
  - Provides methods for success/failure responses
  - Handles schema definition (OpenAPI and XML)

### 4. Sandbox Environment (`backend/sandbox`)

The sandbox provides an isolated environment for browser automation and system operations:

- **sandbox.py**: Manages sandbox creation and lifecycle:
  - Create and initialize sandbox containers
  - Manage connection to sandbox services
  - Execute commands in the sandbox
  - Capture output and screenshots

- **docker/**: Contains Docker configuration for sandboxes:
  - Dockerfile for sandbox environment
  - Browser setup and configuration
  - Automation API implementation

### 5. Infrastructure Services (`backend/services`)

Services provide connections to external systems:

- **supabase.py**: Database connection and operations:
  - Authentication and session management
  - Thread and message storage
  - User and account management

- **redis.py**: Caching and message queuing:
  - Store temporary data
  - Manage pub/sub for agent control
  - Track running agent instances

- **llm.py**: Language model integrations:
  - Connect to Anthropic Claude or OpenAI
  - Format prompts and parse responses
  - Handle streaming responses

### 6. Frontend (`frontend`)

The frontend provides the user interface:

- **app/**: Next.js application components:
  - Pages for conversation threads
  - Account and project management
  - Agent configuration and control

- **components/**: Reusable UI components:
  - Chat interface
  - Message rendering
  - Tool result visualization
  - Authentication forms

## Architecture Overview

The overall architecture follows a service-oriented approach with these key relationships:

1. **User Interaction Flow**:
   - User interacts with the Next.js frontend
   - Frontend sends requests to FastAPI backend
   - Backend initializes a sandbox environment
   - Backend starts an agent run in the thread
   - Agent executes tools and returns responses
   - Responses are streamed back to the frontend

2. **Data Flow**:
   - User messages stored in Supabase
   - Agent responses stored in Supabase
   - File content stored in sandbox and Supabase
   - Tool results streamed through Redis
   - LLM responses processed by AgentPress

3. **Execution Model**:
   - Agent runs in a continuous loop
   - Each iteration involves:
     1. Getting thread history
     2. Sending to LLM
     3. Parsing response for tool calls
     4. Executing tools
     5. Storing results
     6. Repeating until the agent completes

4. **Hierarchy**:
   - Accounts contain Projects
   - Projects contain Threads
   - Threads contain Messages
   - Threads can have Agent Runs
   - Agent Runs contain Responses

## Design Patterns

The codebase uses several design patterns:

1. **Strategy Pattern**: Tool execution strategies in AgentPress
2. **Repository Pattern**: Database access via Supabase
3. **Factory Pattern**: Sandbox creation and management
4. **Observer Pattern**: Streaming responses via Redis pub/sub
5. **Command Pattern**: Tool execution via standardized methods
6. **Adapter Pattern**: Various API integrations via consistent interfaces

## Security Considerations

The architecture includes several security features:

1. **Sandbox Isolation**: Browser and shell commands run in isolated containers
2. **Authentication**: Supabase authentication for user access
3. **Authorization**: Thread access controls based on account membership
4. **Rate Limiting**: Billing-based execution limits
5. **Input Validation**: Parameter validation at API endpoints

## Scaling Considerations

The architecture supports scaling through:

1. **Stateless API**: FastAPI endpoints are stateless and can be horizontally scaled
2. **Instance-based Tracking**: Agent runs are tracked by instance ID in Redis
3. **Database Sharding**: Supabase can be scaled with database sharding
4. **Independent Sandboxes**: Each agent run has its own sandbox environment

## Error Handling

The system has robust error handling:

1. **Tool Errors**: Captured and returned as structured responses
2. **Agent Failures**: Tracked in database and reported to users
3. **Sandbox Issues**: Detected and reported with detailed logs
4. **LLM Errors**: Handled with fallbacks and retries
5. **Database Transactions**: ACID transactions for critical operations

## Monitoring and Observability

The system includes monitoring features:

1. **Logging**: Structured logging throughout the application
2. **Status Tracking**: Agent run status stored and updated in the database
3. **Error Reporting**: Detailed error messages stored with agent runs
4. **Execution Metrics**: Tool usage and execution time tracking