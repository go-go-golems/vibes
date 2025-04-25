# Suna Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                            CLIENT LAYER                                 │
│                                                                         │
│  ┌─────────────────────┐    ┌─────────────────────┐                    │
│  │     Next.js         │    │      Browser        │                    │
│  │    Frontend         │    │      Client         │                    │
│  └─────────────────────┘    └─────────────────────┘                    │
│             │                          │                               │
└─────────────┼──────────────────────────┼───────────────────────────────┘
              │                          │
              │ HTTPS/SSE                │ HTTPS
              ▼                          ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                            API LAYER                                    │
│                                                                         │
│  ┌─────────────────────┐    ┌─────────────────────┐                    │
│  │    FastAPI Backend  │    │     Redis Queue     │                    │
│  │     (api.py)        │◄───┤     Pub/Sub         │                    │
│  └─────────────────────┘    └─────────────────────┘                    │
│             │                          ▲                               │
│             │                          │                               │
│             ▼                          │                               │
│  ┌─────────────────────┐    ┌─────────────────────┐                    │
│  │   AgentPress        │    │     Authentication  │                    │
│  │   Framework         │    │     (Supabase)      │                    │
│  └─────────────────────┘    └─────────────────────┘                    │
│             │                          ▲                               │
└─────────────┼──────────────────────────┼───────────────────────────────┘
              │                          │
              ▼                          │
┌─────────────────────────────────────────────────────────────────────────┐
│                            AGENT LAYER                                  │
│                                                                         │
│  ┌─────────────────────┐    ┌─────────────────────┐                    │
│  │    Agent Runner     │    │    Thread Manager   │                    │
│  │    (run.py)         │◄───┤                     │                    │
│  └─────────────────────┘    └─────────────────────┘                    │
│             │                          ▲                               │
│             │                          │                               │
│             ▼                          │                               │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                          Tool System                            │   │
│  │                                                                 │   │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────┐    │   │
│  │  │ Web Search│  │  Browser  │  │   Shell   │  │  Files    │    │   │
│  │  │   Tool    │  │   Tool    │  │   Tool    │  │   Tool    │    │   │
│  │  └───────────┘  └───────────┘  └───────────┘  └───────────┘    │   │
│  │                                                                 │   │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────┐    │   │
│  │  │ Computer  │  │   Data    │  │  Message  │  │  Deploy   │    │   │
│  │  │   Tool    │  │ Providers │  │   Tool    │  │   Tool    │    │   │
│  │  └───────────┘  └───────────┘  └───────────┘  └───────────┘    │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│             │                          │                               │
└─────────────┼──────────────────────────┼───────────────────────────────┘
              │                          │
              ▼                          ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                            INFRASTRUCTURE LAYER                         │
│                                                                         │
│  ┌─────────────────────┐    ┌─────────────────────┐                    │
│  │   Sandbox           │    │     PostgreSQL      │                    │
│  │   Containers        │    │     (Supabase)      │                    │
│  └─────────────────────┘    └─────────────────────┘                    │
│             │                          │                               │
│             ▼                          │                               │
│  ┌─────────────────────┐    ┌─────────────────────┐                    │
│  │   LLM APIs          │    │     External APIs   │                    │
│  │ (Claude/GPT-4o)     │    │   (Tavily, etc.)    │                    │
│  └─────────────────────┘    └─────────────────────┘                    │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Architecture Component Descriptions

### Client Layer
- **Next.js Frontend**: Web UI for user interaction
- **Browser Client**: Browser-based access to the agent

### API Layer
- **FastAPI Backend**: REST API endpoints for agent control
- **Redis Queue/Pub-Sub**: Message broker for agent communication
- **AgentPress Framework**: Core framework for agent execution
- **Authentication**: Supabase-based auth and session management

### Agent Layer
- **Agent Runner**: Executes the agent loop with LLM calls
- **Thread Manager**: Manages conversation threads and context
- **Tool System**: Collection of tools that provide capabilities:
  - **Web Search Tool**: Search and extract web content
  - **Browser Tool**: Navigate and interact with websites
  - **Shell Tool**: Execute shell commands
  - **Files Tool**: Create and manipulate files
  - **Computer Tool**: Control sandbox computer
  - **Data Providers**: Access structured external data
  - **Message Tool**: Communicate with users
  - **Deploy Tool**: Deploy static websites
  - **Expose Tool**: Expose ports and services

### Infrastructure Layer
- **Sandbox Containers**: Isolated execution environments
- **PostgreSQL/Supabase**: Persistent storage and authentication
- **LLM APIs**: Anthropic Claude and OpenAI GPT-4o
- **External APIs**: Third-party services like Tavily

## Data Flow

1. User submits a request via the frontend
2. Request is authenticated and routed to the API
3. API creates or updates a thread
4. Agent is initialized with appropriate tools
5. Agent executes in a loop:
   - Gets thread history
   - Sends to LLM
   - Parses response for tool calls
   - Executes tools
   - Stores results
   - Continues until complete or stopped
6. Results are streamed back to the frontend
7. User can view results and continue the conversation

## Key Interactions

1. **Frontend ↔ API**: REST with streaming SSE responses
2. **Agent ↔ LLM**: API calls to Claude or GPT-4o
3. **Agent ↔ Tools**: Direct method calls
4. **Tools ↔ Sandbox**: API calls to sandbox services
5. **Agent ↔ Database**: Supabase client for data persistence