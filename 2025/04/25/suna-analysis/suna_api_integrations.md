# SUNA API Integrations Analysis

This document provides a detailed analysis of the key API integrations in the SUNA project, examining how external services are used, implementation details, and potential considerations.

## 1. LLM API Integration (LiteLLM)

### Implementation Details

SUNA uses LiteLLM as a universal adapter to multiple LLM providers. The implementation in `services/llm.py` showcases several important features:

```python
async def make_llm_api_call(
    messages: List[Dict[str, Any]],
    model_name: str,
    response_format: Optional[Any] = None,
    temperature: float = 0,
    max_tokens: Optional[int] = None,
    tools: Optional[List[Dict[str, Any]]] = None,
    tool_choice: str = "auto",
    api_key: Optional[str] = None,
    api_base: Optional[str] = None,
    stream: bool = False,
    top_p: Optional[float] = None,
    model_id: Optional[str] = None,
    enable_thinking: Optional[bool] = False,
    reasoning_effort: Optional[str] = 'low'
) -> Union[Dict[str, Any], AsyncGenerator]:
```

Key features:
- **Model Prefix Routing**: Uses prefixes in the model name (e.g., `anthropic/`, `openrouter/`, `bedrock/`) to route to different providers
- **Retry Logic**: Implements exponential backoff with jitter for transient errors
- **Error Handling**: Different handling for rate limits vs. other errors
- **Streaming Support**: Works with both streaming and non-streaming responses
- **Provider-Specific Parameters**: Customizes parameters based on the provider:
  - Claude-specific headers
  - OpenRouter site URL and app name
  - Bedrock model IDs and ARNs
  - Custom caching for Anthropic

### Provider Support

The system supports multiple LLM providers:

1. **Anthropic Claude**
   - Default model: claude-3-7-sonnet-latest
   - Custom headers: `anthropic-beta: output-128k-2025-02-19`
   - Special handling for "thinking" capability via `reasoning_effort` parameter

2. **OpenAI**
   - Model support: GPT-4o and others
   - API compatibility for function calling

3. **AWS Bedrock**
   - Model support: Claude models on Bedrock
   - Special handling for inference profiles
   - Example ARN: `arn:aws:bedrock:us-west-2:935064898258:inference-profile/us.anthropic.claude-3-7-sonnet-20250219-v1:0`

4. **OpenRouter**
   - Gateway to multiple models (Groq, Mistral, etc.)
   - Custom headers: HTTP-Referer and X-Title

### Error Handling and Resilience

The implementation includes sophisticated error handling:

```python
# Constants for retry
MAX_RETRIES = 3
RATE_LIMIT_DELAY = 30
RETRY_DELAY = 5

# Retry logic
for attempt in range(MAX_RETRIES):
    try:
        response = await litellm.acompletion(**params)
        return response
    except (litellm.exceptions.RateLimitError, OpenAIError, json.JSONDecodeError) as e:
        last_error = e
        await handle_error(e, attempt, MAX_RETRIES)
```

This provides resilience against:
- Rate limit errors (with longer delays)
- Transient API failures (with shorter delays)
- JSON parsing errors (which can happen with malformed responses)

## 2. Supabase Integration

### Implementation Details

SUNA uses Supabase as its primary database and authentication system. The implementation in `services/supabase.py` shows a singleton pattern:

```python
class DBConnection:
    """Singleton database connection manager using Supabase."""
    
    _instance: Optional['DBConnection'] = None
    _initialized = False
    _client: Optional[AsyncClient] = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance
```

Key features:
- **Lazy Initialization**: Client is only created when needed
- **Service Role Priority**: Prefers service role key over anon key for backend operations
- **Async Support**: Uses the async client for non-blocking operations
- **Centralized Access**: Single point of access for the database

### Database Schema

The database schema includes several key tables:

1. **accounts**: User accounts and team workspaces
2. **projects**: Collections of related threads
3. **threads**: Conversation threads
4. **messages**: Messages in threads (user, assistant, tool outputs)
5. **agent_runs**: Agent execution instances

Security is implemented through Supabase Row-Level Security:

```sql
CREATE POLICY thread_select_policy ON threads
    FOR SELECT
    USING (
        basejump.has_role_on_account(account_id) = true OR 
        EXISTS (
            SELECT 1 FROM projects
            WHERE projects.project_id = threads.project_id
            AND (
                projects.is_public = TRUE OR
                basejump.has_role_on_account(projects.account_id) = true
            )
        )
    );
```

This ensures users can only access their own data or public resources.

## 3. Redis Integration

### Implementation Details

SUNA uses Redis for caching, pub/sub messaging, and agent control. The implementation in `services/redis.py` shows a sophisticated approach:

```python
# Retry configuration
MAX_RETRIES = 5
BASE_RETRY_DELAY = 0.5  # Start with 500ms delay
MAX_RETRY_DELAY = 10.0  # Maximum delay of 10 seconds
RETRY_JITTER = 0.1  # Add 10% random jitter to retry delay

async def with_retry(func, *args, **kwargs):
    """Execute a Redis operation with exponential backoff retry."""
    retries = 0
    last_exception = None
    
    while retries < MAX_RETRIES:
        try:
            return await func(*args, **kwargs)
        except (redis.ConnectionError, redis.TimeoutError, ConnectionResetError) as e:
            # Retry logic
```

Key features:
- **Connection Pooling**: Limits connections to prevent overloading
- **SSL Support**: Secure connection with certificate validation
- **Retry Logic**: Exponential backoff with jitter for transient errors
- **Health Checks**: Regular connection health monitoring
- **Centralized API**: Wrapper functions for common Redis operations

### Pub/Sub for Agent Control

The system uses Redis pub/sub for agent control messages:

```python
async def stop_agent_run(agent_run_id: str, error_message: Optional[str] = None):
    # Update the agent run status
    status = "failed" if error_message else "stopped"
    await update_agent_run_status(client, agent_run_id, status, error=error_message)
    
    # Send stop signal to global channel
    await redis.publish(f"agent_run:{agent_run_id}:control", "STOP")
    
    # Find all instances handling this agent run
    instance_keys = await redis.keys(f"active_run:*:{agent_run_id}")
    
    for key in instance_keys:
        # Extract instance ID from the key pattern
        parts = key.split(":")
        if len(parts) >= 3:
            instance_id = parts[1]
            # Send stop signal to instance-specific channel
            await redis.publish(f"agent_run:{agent_run_id}:control:{instance_id}", "STOP")
```

This enables:
- **Distributed Control**: Any API instance can stop any agent run
- **Instance-Specific Signals**: Control messages can be targeted to specific instances
- **Global Coordination**: All instances receive global messages

## 4. Tavily Search API Integration

### Implementation Details

SUNA uses the Tavily API for web search and content extraction. The implementation in `agent/tools/web_search_tool.py` shows:

```python
class WebSearchTool(Tool):
    """Tool for performing web searches using the Exa API."""

    def __init__(self, api_key: str = None):
        super().__init__()
        # Load environment variables
        load_dotenv()
        # Use the provided API key or get it from environment variables
        self.api_key = api_key or os.getenv("TAVILY_API_KEY")
        if not self.api_key:
            raise ValueError("TAVILY_API_KEY not found in environment variables")

        # Tavily asynchronous search client
        self.tavily_client = AsyncTavilyClient(api_key=self.api_key)
```

Key features:
- **Async Client**: Uses the async client for non-blocking operations
- **Error Handling**: Handles and formats errors for the agent
- **Result Normalization**: Standardizes results from different endpoints
- **Parameter Validation**: Checks and normalizes parameters before API calls

The tool provides two main functions:

1. **web_search**: Searches the web for information
   ```python
   async def web_search(
       self, 
       query: str, 
       summary: bool = True,
       num_results: int = 20
   ) -> ToolResult:
   ```

2. **crawl_webpage**: Extracts full text content from a webpage
   ```python
   async def crawl_webpage(
       self,
       url: str
   ) -> ToolResult:
   ```

These provide the agent with up-to-date information from the web.

## 5. RapidAPI Data Providers

### Implementation Details

SUNA uses RapidAPI for accessing various data sources. The implementation consists of a base class and provider-specific implementations:

Base Class (`RapidDataProviderBase.py`):
```python
class RapidDataProviderBase:
    def __init__(self, base_url: str, endpoints: Dict[str, EndpointSchema]):
        self.base_url = base_url
        self.endpoints = endpoints
    
    def call_endpoint(
            self,
            route: str,
            payload: Optional[Dict[str, Any]] = None
    ):
        endpoint = self.endpoints.get(route)
        if not endpoint:
            raise ValueError(f"Endpoint {route} not found")
        
        url = f"{self.base_url}{endpoint['route']}"
        
        headers = {
            "x-rapidapi-key": os.getenv("RAPID_API_KEY"),
            "x-rapidapi-host": url.split("//")[1].split("/")[0],
            "Content-Type": "application/json"
        }
```

Provider Implementation Example (`LinkedinProvider.py`):
```python
class LinkedinProvider(RapidDataProviderBase):
    def __init__(self):
        endpoints: Dict[str, EndpointSchema] = {
            "person": {
                "route": "/person",
                "method": "POST",
                "name": "Person Data",
                "description": "Fetches any Linkedin profiles data including skills, certificates, experiences, qualifications and much more.",
                "payload": {
                    "link": "LinkedIn Profile URL"
                }
            },
            # ... more endpoints
        }
        base_url = "https://linkedin-data-scraper.p.rapidapi.com"
        super().__init__(base_url, endpoints)
```

The system includes providers for:
- **LinkedIn**: Profile and company data
- **Twitter**: User and tweet data
- **Zillow**: Real estate listings
- **Amazon**: Product information
- **Yahoo Finance**: Stock and company data

These are exposed through the `DataProvidersTool` which provides two functions:

1. **get_data_provider_endpoints**: Discover available endpoints
2. **execute_data_provider_call**: Call specific endpoints with parameters

This modular design allows easy addition of new data providers.

## 6. Daytona Sandbox Integration

### Implementation Details

SUNA uses Daytona for sandbox management. The implementation in `sandbox/sandbox.py` shows:

```python
config = DaytonaConfig(
    api_key=os.getenv("DAYTONA_API_KEY"),
    server_url=os.getenv("DAYTONA_SERVER_URL"),
    target=os.getenv("DAYTONA_TARGET")
)

daytona = Daytona(config)

def create_sandbox(password: str):
    """Create a new sandbox with all required services configured and running."""
    
    sandbox = daytona.create(CreateSandboxParams(
        image="adamcohenhillel/kortix-suna:0.0.20",
        public=True,
        env_vars={
            "CHROME_PERSISTENT_SESSION": "true",
            "RESOLUTION": "1024x768x24",
            "RESOLUTION_WIDTH": "1024",
            "RESOLUTION_HEIGHT": "768",
            "VNC_PASSWORD": password,
        }
    ))
```

Key features:
- **Custom Docker Image**: Uses a specialized image with browser automation tools
- **Persistent Sessions**: Maintains browser state between operations
- **Process Management**: Creates and manages sessions for specific tools
- **Resource Efficiency**: Starts and stops sandboxes as needed

The sandbox provides a secure environment for:
- Browser automation
- File operations
- Shell commands
- Code execution

## 7. Authentication and Billing

### Implementation Details

SUNA uses Supabase for authentication and tracks billing through a custom implementation:

```python
# Define subscription tiers and their monthly limits (in minutes)
SUBSCRIPTION_TIERS = {
    'price_1RGJ9GG6l1KZGqIroxSqgphC': {'name': 'free', 'minutes': 10},
    'price_1RGJ9LG6l1KZGqIrd9pwzeNW': {'name': 'base', 'minutes': 300},
    'price_1RGJ9JG6l1KZGqIrVUU4ZRv6': {'name': 'extra', 'minutes': 2400}
}

async def check_billing_status(client, account_id: str) -> Tuple[bool, str, Optional[Dict]]:
    """Check if an account can run agents based on their subscription and usage."""
    # Get current subscription
    subscription = await get_account_subscription(client, account_id)
    
    # Calculate current month's usage
    current_usage = await calculate_monthly_usage(client, account_id)
    
    # Check if within limits
    if current_usage >= tier_info['minutes']:
        return False, f"Monthly limit of {tier_info['minutes']} minutes reached. Please upgrade your plan or wait until next month.", subscription
    
    return True, "OK", subscription
```

Key features:
- **Subscription Tiers**: Different levels with varying usage limits
- **Usage Tracking**: Calculates total agent run time per month
- **Access Control**: Integrated with Supabase RLS for security
- **Limit Enforcement**: Prevents agent runs when limits are reached

The billing check is performed:
- When starting a new agent run
- Periodically during agent execution
- When resuming an existing agent run

## Integration Summary

SUNA's integrations demonstrate several best practices:

1. **Abstraction Layers**: Wrappers around external APIs for consistent interfaces
2. **Error Resilience**: Robust error handling with retries and fallbacks
3. **Asynchronous Design**: Non-blocking operations for performance
4. **Security**: Proper credential management and access control
5. **Modularity**: Easy to add or replace specific integrations

The system's external dependencies create a rich ecosystem of capabilities while maintaining a clean architecture with clear separation of concerns.