# OpenAI Responses API Research

## Overview
The Responses API is OpenAI's newest and most advanced API, released in March 2025. It combines the strengths of the Chat Completions and Assistants APIs into a single streamlined interface.

## Key Features
- Simplifies development by automatically handling orchestration logic
- Natively integrates OpenAI's built-in tools for web search and file search
- Supports text generation, image analysis, and streaming responses
- More streamlined and user-friendly interface than previous APIs

## Basic API Structure

### Client Initialization
```python
from openai import OpenAI
import os
from dotenv import load_dotenv

load_dotenv()
client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))
```

### Basic Text Generation
```python
response = client.responses.create(
    model="gpt-4o",
    instructions="System prompt/instructions",
    input="User input text",
    temperature=0.7,
    max_output_tokens=200
)
result = response.output_text
```

### Key Parameters
- `model`: The model to use (e.g., "gpt-4o")
- `instructions`: Acts as system prompt, defines AI behavior
- `input`: User input (can be string or array for multimodal)
- `temperature`: Controls randomness (0-2)
- `max_output_tokens`: Limits response length
- Response contains `output_text` property

### Image Analysis
```python
response = client.responses.create(
    model="gpt-4o",
    instructions="You are an image analysis expert",
    input=[
        {"role": "user", "content": "Analyze this image"},
        {
            "role": "user",
            "content": [
                {
                    "type": "input_image",
                    "image_url": image_url
                }
            ],
        },
    ],
    temperature=0.2
)
```

## Sources
- DataCamp Tutorial: https://www.datacamp.com/tutorial/openai-responses-api
- Released: March 2025
- Combines Chat Completions and Assistants APIs



## Streaming Implementation

### Streaming Response Structure
```python
def analyze_customer_feedback(feedback_text):
    print("Analyzing customer feedback in real-time:")
    
    stream = client.responses.create(
        model="gpt-4o",
        instructions="Extract key sentiments, product issues, and actionable insights",
        input=feedback_text,
        stream=True,  # Enable streaming
        temperature=0.3,
        max_output_tokens=500
    )
    
    full_response = ""
    print("\nAnalysis results:")
    for event in stream:
        if event.type == "response.output_text.delta":
            print(event.delta, end="")
            full_response += event.delta
        elif event.type == "response.error":
            print(f"\nError occurred: {event.error}")
    
    return full_response
```

### Streaming Key Points
1. Set `stream=True` in the create method
2. Process response as an iterable of events with specific types
3. Handle different event types separately:
   - `response.output_text.delta` for text chunks
   - `response.error` for errors
4. Events have `.type`, `.delta`, and `.error` properties
5. Real-time UI updates possible by replacing print statements

### Event Types
- `response.output_text.delta`: Contains text chunks as they're generated
- `response.error`: Contains error information if something goes wrong


## Built-in Tools

The Responses API integrates several built-in tools that extend capabilities beyond basic text generation without requiring complex integration code or multiple API calls.

### 1. Web Search Tool
- **Purpose**: Retrieve current information from the internet
- **Usage**: `tools=[{"type": "web_search_preview"}]`
- **Capabilities**:
  - Analyzes queries and synthesizes information from multiple sources
  - Provides proper citations
  - Addresses LLM training data limitations
  - Handles search process while presenting results with citations

**Example**:
```python
response = client.responses.create(
    model="gpt-4o",
    tools=[{"type": "web_search_preview"}],
    input="What are some news related to the stock market?",
)
print(response.output_text)
```

### 2. File Search Tool
- **Purpose**: Extract information from uploaded documents
- **Capabilities**:
  - Search across multiple file types (PDFs, Word documents, presentations, etc.)
  - Find specific information within documents based on natural language queries
  - Extract and synthesize information from multiple documents simultaneously
  - Provide citations to specific sections of source documents
  - Support complex queries that reference information across multiple files

**Use Cases**:
- Document analysis (legal contracts, research papers)
- Building knowledge bases from technical documentation
- Extracting information from multiple documents

**Implementation**:
1. Upload files to OpenAI's files endpoint
2. Pass file IDs to the Responses API when making queries
3. Creates streamlined workflow for document-based applications

### 3. Computer Use Tool
- **Purpose**: Interface interaction capabilities
- **Capabilities**:
  - Navigate websites and web applications autonomously
  - Fill out forms with appropriate information
  - Extract data from web pages and applications
  - Execute multi-step processes across different screens
  - Interact with elements like buttons, dropdowns, and text fields
  - Understand context and purpose of different interface elements

**Technology**: AI can see and interact with screen elements, understand context, and execute actions based on natural language instructions

**Applications**:
- Process automation for repetitive tasks
- Guided assistance for complex workflows
- Accessibility improvements for users with traditional interface difficulties
- Automate form filling, navigate complex websites, perform testing of user interfaces

### Additional Tool Capabilities
- Tools ecosystem continues to grow with OpenAI regularly adding new capabilities
- Comprehensive tools documentation covers implementation details
- New tools for building agents article provides latest updates


## Technical API Structure (from OpenAI Cookbook)

### Basic API Call
```python
from openai import OpenAI
import os
client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

response = client.responses.create(
    model="gpt-4o-mini",
    input="tell me a joke",
)

print(response.output[0].content[0].text)
```

### Key Features

#### 1. Stateful API
- API maintains conversation state automatically
- No need to manually manage conversation history
- Can retrieve responses at any time with full conversation history

```python
# Retrieve previous response
fetched_response = client.responses.retrieve(response_id=response.id)
print(fetched_response.output[0].content[0].text)
```

#### 2. Conversation Continuation
```python
# Continue conversation
response_two = client.responses.create(
    model="gpt-4o-mini",
    input="tell me another",
    previous_response_id=response.id
)
```

#### 3. Conversation Forking
```python
# Fork conversation from any previous point
response_two_forked = client.responses.create(
    model="gpt-4o-mini",
    input="I didn't like that joke, tell me another",
    previous_response_id=response.id  # Fork from first response
)
```

### Response Structure
- `response.output[0].content[0].text` - Main text content
- `response.id` - Unique response identifier
- `response.output` - Array of output objects
- Each output has `content` array with text objects

### Web Search Tool Integration
```python
response = client.responses.create(
    model="gpt-4o",
    input="What's the latest news about AI?",
    tools=[{"type": "web_search"}]
)
```

### Response Output Structure with Tools
```json
[
  {
    "id": "ws_67bd64fe91f081919bec069ad65797f1",
    "status": "completed", 
    "type": "web_search_call"
  },
  {
    "id": "msg_67bd6502568c8191a2cbb154fa3fbf4c",
    "content": [
      {
        "annotations": [
          {
            "index": null,
            "title": "Article Title",
            "type": "url_citation",
            "url": "https://example.com/article"
          }
        ],
        "text": "Response text with citations..."
      }
    ]
  }
]
```

### Key Differences from Other APIs
- **vs Chat Completions**: Built for multi-turn, stateful interactions
- **vs Assistants API**: Less setup required, more streamlined
- **Built for**: Asynchronous and stateful operations
- **Designed for**: Complex, long-running reasoning tasks

