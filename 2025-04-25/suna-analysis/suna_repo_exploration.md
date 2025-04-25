# Suna Repository - Initial Exploration

## Overview
Suna is an open-source generalist AI agent created by Kortix AI that can perform a wide variety of tasks on behalf of users. The agent combines advanced language models (Claude-3, GPT-4o) with specialized tools to create a versatile agent that can browse the web, interact with websites, conduct research, and perform various automated tasks.

## Repository Structure

The repository is organized into several key directories:

- **backend**: Contains the Python FastAPI implementation of the agent
  - **agent**: Core agent implementation
    - **tools**: Various tools for web search, browser automation, data providers, shell commands, etc.
  - **agentpress**: Framework for managing agent threads and tool execution
  - **sandbox**: Sandboxed environment for safe browser automation
  - **services**: Database, LLM, and Redis connections
  - **utils**: Utility functions for authentication, billing, file operations, etc.

- **frontend**: Next.js-based web interface
  - **src**: Source code for the frontend
    - **app**: Next.js app components
    - **components**: Reusable components
    - **hooks**: Custom React hooks
    - **lib**: Utility libraries

- **docs**: Documentation and diagrams

## Key Components

### 1. API Structure
The backend is built with FastAPI and provides endpoints for:
- Agent execution and management
- Thread management
- Sandbox control
- Authentication via Supabase

### 2. Agent Implementation
The agent functionality is defined in the `backend/agent` directory:
- `api.py`: FastAPI endpoints for agent control
- `run.py`: Core execution loop for the agent
- `prompt.py`: System prompts that define the agent's behavior and capabilities
- `tools/`: Directory containing the various tools the agent can use

### 3. Agent Tools
The agent has access to several tools:
- `web_search_tool.py`: Search the web for information using Tavily API
- `sb_browser_tool.py`: Navigate and interact with websites
- `sb_shell_tool.py`: Execute shell commands in the sandbox
- `sb_files_tool.py`: Create, read, and manipulate files
- `computer_use_tool.py`: Control the sandbox browser and GUI
- `data_providers_tool.py`: Access structured data from various providers
- `message_tool.py`: Send messages and interact with the user
- `sb_deploy_tool.py`: Deploy websites to Cloudflare Pages
- `sb_expose_tool.py`: Expose ports to the public internet

### 4. Thread Management
The agent uses a thread-based execution model:
- Each conversation is a "thread"
- Threads belong to "projects"
- Projects belong to "accounts"
- The agent runs in a continuous loop until it decides to stop

### 5. Sandbox Environment
The agent operates in a sandboxed environment:
- Provides a safe execution environment for browser automation and shell commands
- Includes a headless browser for web interactions
- Exposes API endpoints for controlling the browser

### 6. Billing and Authentication
The system uses Supabase for:
- User authentication and account management
- Storing conversations, agent runs, and user data
- Billing and subscription management

## Architecture

The overall architecture follows a modern web application pattern:
1. Next.js frontend for user interface
2. FastAPI backend for agent execution
3. Supabase for authentication and database
4. Redis for caching and pub/sub messaging
5. Sandbox environment for browser automation

Communication between components:
- Frontend to Backend: RESTful API with streaming support
- Backend to LLM: API calls to Anthropic Claude or OpenAI
- Backend to Tools: Direct function calls or API requests
- Tools to External Services: API calls (search, website access, etc.)

## Key Technologies

- **Language Models**: Claude-3-7-Sonnet, GPT-4o
- **Frontend**: Next.js, React
- **Backend**: Python, FastAPI
- **Database**: Supabase (PostgreSQL)
- **Browser Automation**: Custom browser automation API
- **Search**: Tavily API
- **Data Providers**: Various APIs via RapidAPI

## Deployment
The application can be deployed locally or in a cloud environment, with the following components needed:
- API keys for LLMs (Anthropic or OpenAI)
- Supabase account for database and auth
- Optional Tavily API key for search
- Optional RapidAPI key for data providers