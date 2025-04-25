# Suna Agent Execution Sequence

## Agent Initialization and Execution Flow

```
┌──────────┐          ┌──────────┐          ┌──────────┐          ┌──────────┐          ┌──────────┐          ┌──────────┐
│  Client  │          │  FastAPI │          │ AgentAPI │          │  Agent   │          │  Tools   │          │   LLM    │
│          │          │          │          │          │          │          │          │          │          │          │
└────┬─────┘          └────┬─────┘          └────┬─────┘          └────┬─────┘          └────┬─────┘          └────┬─────┘
     │                     │                     │                     │                     │                     │
     │ POST /thread/{id}/agent/start             │                     │                     │                     │
     │ ──────────────────>│                     │                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │ initialize()        │                     │                     │                     │
     │                     │ ──────────────────>│                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │ create_sandbox()    │                     │                     │
     │                     │                     │ ──────────────────>│                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │ check_billing()     │                     │                     │
     │                     │                     │ ──────────────────>│                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │ create_agent_run()  │                     │                     │
     │                     │                     │ ──────────────────>│                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │ register_in_redis() │                     │                     │
     │                     │                     │ ──────────────────>│                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │ run_agent_background()                    │                     │
     │                     │                     │ ──────────────────────────────────────>│                     │
     │                     │                     │                     │                     │                     │
     │ agent_run_id        │                     │                     │                     │                     │
     │ <──────────────────│                     │                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │ GET /agent-run/{id}/stream               │                     │                     │                     │
     │ ──────────────────>│                     │                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │ stream_agent_run()  │                     │                     │                     │
     │                     │ ──────────────────>│                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │  SSE Stream Start   │                     │                     │                     │                     │
     │ <──────────────────│                     │                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │                     │ run_agent()         │                     │
     │                     │                     │                     │ ─────────────────────────────────────────>│
     │                     │                     │                     │                     │                     │
     │                     │                     │                     │ LLM Response        │                     │
     │                     │                     │                     │ <─────────────────────────────────────────│
     │                     │                     │                     │                     │                     │
     │                     │                     │                     │ parse_tool_calls()  │                     │
     │                     │                     │                     │ ──────────────────>│                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │                     │ execute_tool()      │                     │
     │                     │                     │                     │ ──────────────────>│                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │                     │ Tool Result         │                     │
     │                     │                     │                     │ <──────────────────│                     │
     │                     │                     │                     │                     │                     │
     │  SSE Event (chunk)  │                     │                     │                     │                     │
     │ <──────────────────────────────────────────────────────────────│                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │                     │      loop (until completion)             │
     │                     │                     │                     │ <─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─│
     │                     │                     │                     │                     │                     │
     │  SSE Completion     │                     │                     │                     │                     │
     │ <──────────────────────────────────────────────────────────────│                     │                     │
     │                     │                     │                     │                     │                     │
     │ POST /agent-run/{id}/stop (optional)      │                     │                     │                     │
     │ ──────────────────>│                     │                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │ stop_agent()        │                     │                     │                     │
     │                     │ ──────────────────>│                     │                     │                     │
     │                     │                     │                     │                     │                     │
     │                     │                     │ publish_stop_signal()                    │                     │
     │                     │                     │ ──────────────────────────────────────>│                     │
     │                     │                     │                     │                     │                     │
     │ stop confirmation   │                     │                     │                     │                     │
     │ <──────────────────│                     │                     │                     │                     │
     │                     │                     │                     │                     │                     │
┌────┴─────┐          ┌────┴─────┐          ┌────┴─────┐          ┌────┴─────┐          ┌────┴─────┐          ┌────┴─────┐
│  Client  │          │  FastAPI │          │ AgentAPI │          │  Agent   │          │  Tools   │          │   LLM    │
│          │          │          │          │          │          │          │          │          │          │          │
└──────────┘          └──────────┘          └──────────┘          └──────────┘          └──────────┘          └──────────┘
```

## Detailed Sequence Steps

1. **Agent Initialization**
   - Client sends a POST request to `/thread/{thread_id}/agent/start`
   - FastAPI initializes the agent API with thread manager and database
   - Agent API retrieves thread information and verifies access
   - Agent API checks billing status
   - Agent API creates or starts a sandbox
   - Agent API creates an entry in the `agent_runs` table
   - Agent API registers the run in Redis with a TTL
   - Agent API starts a background task to run the agent
   - Response with `agent_run_id` is returned to the client

2. **Streaming Setup**
   - Client sends a GET request to `/agent-run/{agent_run_id}/stream`
   - FastAPI verifies access and retrieves agent run data
   - FastAPI sets up a streaming response
   - Initial cached responses are sent to the client
   - Streaming connection is established

3. **Agent Execution Loop**
   - Agent gets recent messages from the thread
   - Agent sends messages to the LLM with the system prompt
   - LLM responds with text that may contain tool calls
   - AgentPress parses the response for tool calls
   - Tools are executed based on the parsed calls
   - Tool results are stored in the database
   - Results are streamed to the client
   - Process repeats until:
     - The agent completes (`<complete>` tool call)
     - The agent requests user input (`<ask>` tool call)
     - The maximum iterations are reached
     - An error occurs
     - The agent is stopped

4. **Optional Stop**
   - Client can send a POST request to `/agent-run/{agent_run_id}/stop`
   - FastAPI verifies access and retrieves agent run data
   - Agent API publishes a stop signal to Redis channels
   - Agent detects the stop signal and gracefully terminates
   - Agent updates the run status to "stopped"
   - Confirmation is sent to the client

5. **Completion**
   - When the agent completes, the status is updated to "completed"
   - A completion message is added to the response stream
   - The agent cleanup process removes Redis keys
   - The streaming connection is closed

## Key Functions in the Sequence

1. **API Layer Functions**
   - `start_agent`: Entry point for starting an agent run
   - `stream_agent_run`: Streams agent responses to the client
   - `stop_agent`: Stops a running agent
   - `get_agent_run`: Gets status and details of an agent run

2. **Agent Layer Functions**
   - `run_agent_background`: Main background task that runs the agent
   - `run_agent`: Core agent execution loop
   - `check_billing_status`: Verifies billing allowance
   - `create_sandbox`: Creates or retrieves a sandbox environment

3. **AgentPress Functions**
   - `run_thread`: Executes a single iteration of the agent
   - `add_message`: Adds messages to the thread
   - `execute_tool`: Executes a tool based on LLM response

4. **Redis Functions**
   - `set`: Stores the agent run with a TTL
   - `publish`: Sends control signals (e.g., STOP)
   - `subscribe`: Listens for control signals

5. **Database Functions**
   - `update_agent_run_status`: Updates the status of an agent run
   - `verify_thread_access`: Verifies user access to a thread

## Error Handling

1. **Billing Errors**
   - Agent checks billing status on start and each iteration
   - If billing limit is reached, agent is stopped with status "failed"
   - Error message includes subscription details

2. **Access Errors**
   - API verifies access to threads and agent runs
   - If access is denied, a 403 error is returned
   - Row-Level Security policies enforce access at the database level

3. **Execution Errors**
   - Tool execution errors are captured and returned as part of the response
   - Agent continues execution despite tool errors
   - Critical errors update the agent run status to "failed"

4. **Timeout Handling**
   - Redis keys have TTL to prevent orphaned runs
   - Sandbox operations have timeout limits
   - Background tasks are monitored and can be cancelled