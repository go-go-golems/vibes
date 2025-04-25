# SUNA Architecture Diagram

```
+--------------------------------------------------------------------------------------------------+
|                                     SUNA ARCHITECTURE                                             |
+--------------------------------------------------------------------------------------------------+

+--------------------------------------------------------------------------------------------------+
|                                       CLIENT LAYER                                                |
|  +----------------------------+                              +----------------------------+       |
|  |                            |         HTTP/SSE             |                            |       |
|  |     Next.js Frontend       | <------------------------->  |      Browser Client        |       |
|  |                            |                              |                            |       |
|  +----------------------------+                              +----------------------------+       |
+--------------------------------------------------------------------------------------------------+
                                           ^
                                           | Requests/Streaming
                                           v
+--------------------------------------------------------------------------------------------------+
|                                        API LAYER                                                  |
|  +----------------------------+                              +----------------------------+       |
|  |                            |         Messaging            |                            |       |
|  |     FastAPI Backend        | <------------------------->  |    Redis Queue/Pub-Sub     |       |
|  |                            |                              |                            |       |
|  +----------------------------+                              +----------------------------+       |
+--------------------------------------------------------------------------------------------------+
                                           ^
                                           | API Calls
                                           v
+--------------------------------------------------------------------------------------------------+
|                                       AGENT LAYER                                                 |
|  +-------------------+              +-------------------+             +-------------------+       |
|  |                   |              |                   |             |  Tool System      |       |
|  |  Thread Manager   | <----------> |   Agent Runner    | <---------> | +---------------+ |       |
|  |                   |              |                   |             | | Browser Tool  | |       |
|  +-------------------+              +-------------------+             | +---------------+ |       |
|                                                                       | | Search Tool   | |       |
|                                                                       | +---------------+ |       |
|                                                                       | | Shell Tool    | |       |
|                                                                       | +---------------+ |       |
|                                                                       | | Files Tool    | |       |
|                                                                       | +---------------+ |       |
|                                                                       +-------------------+       |
+--------------------------------------------------------------------------------------------------+
                                           ^
                                           | Service Calls
                                           v
+--------------------------------------------------------------------------------------------------+
|                                   INFRASTRUCTURE LAYER                                             |
|  +-------------------+              +-------------------+             +-------------------+       |
|  |                   |              |                   |             |                   |       |
|  | Sandbox Containers| <----------> | PostgreSQL/Supabase| <---------> |     LLM APIs      |       |
|  |                   |              |                   |             | (Claude, GPT-4o,  |       |
|  +-------------------+              +-------------------+             |     Bedrock)      |       |
|                                                                       +-------------------+       |
+--------------------------------------------------------------------------------------------------+
```

## Component Descriptions

### Client Layer
- **Next.js Frontend**: React-based user interface for interaction with the agent
- **Browser Client**: Browser-based access to the agent interface

### API Layer
- **FastAPI Backend**: RESTful API endpoints for agent control and communication
- **Redis Queue/Pub-Sub**: Message broker for agent control and event distribution

### Agent Layer
- **Thread Manager**: Manages conversation context and history
- **Agent Runner**: Orchestrates agent execution and tool coordination
- **Tool System**: Collection of tools that provide various capabilities:
  - Browser Tool: Web browsing and interaction
  - Search Tool: Web search and content extraction
  - Shell Tool: Command execution in sandbox
  - Files Tool: File operations in sandbox

### Infrastructure Layer
- **Sandbox Containers**: Isolated execution environments for web browsing and shell commands
- **PostgreSQL/Supabase**: Database for persistent storage and authentication
- **LLM APIs**: External language model providers (Claude, GPT-4o, Bedrock)

## Data Flow

1. User sends requests via Next.js frontend or Browser Client
2. Requests are processed by FastAPI Backend and distributed via Redis
3. Agent Layer components coordinate to fulfill the request:
   - Thread Manager provides conversation context
   - Agent Runner executes the agent logic
   - Tool System provides specific capabilities
4. Infrastructure Layer provides execution environment and services:
   - Sandbox Containers for isolated execution
   - PostgreSQL/Supabase for data persistence
   - LLM APIs for language model capabilities
5. Results are streamed back to the user through the API and Client layers