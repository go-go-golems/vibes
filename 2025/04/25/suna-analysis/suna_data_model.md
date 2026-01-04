# Suna Data Model

## Core Entity Relationships

```
┌───────────────┐      ┌───────────────┐      ┌───────────────┐
│   Account     │      │   Project     │      │   Thread      │
├───────────────┤      ├───────────────┤      ├───────────────┤
│ id            │1────┐│ project_id    │1────┐│ thread_id     │
│ name          │     ○│ name          │     ○│ account_id    │
│ personal      │     ││ description   │     ││ project_id    │
│ billing_status│     ││ account_id    │     ││ is_public     │
│ created_at    │     ││ sandbox       │     ││ created_at    │
│ updated_at    │     ││ is_public     │     ││ updated_at    │
└───────────────┘     ││ created_at    │     │└───────────────┘
                      ││ updated_at    │     │        │1
                      │└───────────────┘     │        │
                      │                      │        │
                      │                      │        │
                      │      ┌───────────────┐        │
                      └─────┐│  AgentRun     │        │
                            ││───────────────│        │
                            ││ id            │        │
                            ││ thread_id     │◇───────┘
                            ││ status        │
                            ││ started_at    │        │1
                            ││ completed_at  │        │
                            ││ error         │        │
                            ││ created_at    │        │
                            ││ updated_at    │        │
                            │└───────────────┘        │
                            │                         │
                            │      ┌───────────────┐  │
                            └─────┐│   Message     │  │
                                  ││───────────────│  │
                                  ││ message_id    │  │
                                  ││ thread_id     │◇─┘
                                  ││ type          │
                                  ││ is_llm_message│
                                  ││ content       │
                                  ││ metadata      │
                                  ││ created_at    │
                                  ││ updated_at    │
                                  │└───────────────┘
                                  │
```

## Entity Descriptions

### Account
- Represents a user account (personal or team)
- Contains billing information and membership
- Primary container for projects and threads
- Has RLS policies based on account membership

### Project
- Container for related threads
- Can have its own sandbox environment
- Belongs to an account
- Can be public or private

### Thread
- Represents a conversation
- Contains messages and agent runs
- Belongs to a project
- Can have direct account association

### Message
- Represents a single interaction in a thread
- Can be a user message, assistant response, or tool result
- Types include: user, assistant, tool, summary, browser_state
- Content is stored as JSON with role and message content
- Metadata can include parsing details, references, etc.

### AgentRun
- Represents a single execution of the agent
- Tied to a specific thread
- Has status: running, completed, stopped, failed
- Tracks start and completion times
- Stores error information if execution fails

## Database Structure

The database is implemented in PostgreSQL via Supabase, with the following key features:

1. **UUID Primary Keys**: All entities use UUID primary keys
2. **Timestamps**: created_at and updated_at timestamps for auditing
3. **Row-Level Security**: RLS policies based on account membership
4. **JSON/JSONB Storage**: Flexible storage for content and metadata
5. **Indexing**: Performance optimized for common query patterns

## Access Control

Access control is implemented through Row-Level Security (RLS) policies:

1. **Account-Based Access**: Users can only access entities associated with accounts they are members of
2. **Public Content**: Some entities can be marked as public, allowing broader access
3. **Hierarchical Permissions**: Access to a thread grants access to its messages and agent runs
4. **Role-Based Restrictions**: Some operations may be restricted to specific roles (owner, admin, etc.)

## Key Relationships

1. **Account to Project**: One-to-many (an account can have multiple projects)
2. **Project to Thread**: One-to-many (a project can have multiple threads)
3. **Thread to Message**: One-to-many (a thread contains multiple messages)
4. **Thread to AgentRun**: One-to-many (a thread can have multiple agent runs)

## Data Storage

1. **Messages**: Stored as structured JSON in the database
2. **Files**: Stored in Supabase storage buckets, referenced in messages
3. **Agent Run States**: Tracked in Redis for active runs, persisted to PostgreSQL
4. **Sandbox State**: Managed in Supabase but connected to external containers