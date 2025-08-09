# Turn Inspector CLI - Examples

This document provides comprehensive examples of using the Turn Inspector CLI tool for various conversation scenarios.

## Table of Contents

1. [Basic Operations](#basic-operations)
2. [Simple Conversations](#simple-conversations)
3. [Tool-Based Conversations](#tool-based-conversations)
4. [Error Handling](#error-handling)
5. [Complex Multi-Turn Scenarios](#complex-multi-turn-scenarios)
6. [Querying and Search](#querying-and-search)
7. [Data Analysis](#data-analysis)
8. [Advanced Use Cases](#advanced-use-cases)

## Basic Operations

### Check Database Status
```bash
# Show current database statistics
./turn-inspector stats

# Show detailed statistics with breakdowns
./turn-inspector stats --detailed
```

### List All Turns
```bash
# List all turns with summary
./turn-inspector list turns

# List with pagination
./turn-inspector list turns --limit 10 --offset 0
```

## Simple Conversations

### Basic User-Assistant Exchange
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Hello, how are you today?"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Hello! I am doing well, thank you for asking. How can I help you today?"}
  }
]' --metadata '{"source":"session","key":"id","value":"simple-001"}' \
   --metadata '{"source":"user","key":"name","value":"Alice"}'
```

### Question and Answer
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "What is the capital of France?"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "The capital of France is Paris."}
  }
]' --metadata '{"source":"topic","key":"category","value":"geography"}'
```

## Tool-Based Conversations

### Weather Query
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "What is the weather like in New York?"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "get_weather",
      "args": {"city": "New York", "units": "fahrenheit"}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Current weather in New York: 72°F, partly cloudy with light winds"
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "The current weather in New York is 72°F and partly cloudy with light winds. It is a pleasant day!"}
  }
]' --metadata '{"source":"session","key":"id","value":"weather-001"}' \
   --metadata '{"source":"user","key":"location","value":"NYC"}'
```

### Calculator Tool
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Calculate 15 * 23"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "calculator",
      "args": {"operation": "multiply", "a": 15, "b": 23}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": 345
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "15 multiplied by 23 equals 345."}
  }
]' --metadata '{"source":"topic","key":"category","value":"math"}'
```

### Database Query Tool
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Show me all users from the database"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "database_query",
      "args": {"query": "SELECT * FROM users LIMIT 10"}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": [
        {"id": 1, "name": "Alice", "email": "alice@example.com"},
        {"id": 2, "name": "Bob", "email": "bob@example.com"}
      ]
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I found 2 users in the database: Alice (alice@example.com) and Bob (bob@example.com)."}
  }
]' --metadata '{"source":"security","key":"permission_level","value":"read_only"}'
```

## Error Handling

### Mathematical Error
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Calculate the square root of -1"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "calculator",
      "args": {"operation": "sqrt", "value": -1}
    }
  },
  {
    "order": 2,
    "kind": "system",
    "role": "system",
    "payload": {
      "error": "Invalid operation: cannot calculate square root of negative number",
      "error_code": "MATH_ERROR_001"
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I cannot calculate the square root of -1 using real numbers. However, in complex numbers, the square root of -1 is represented as i (the imaginary unit)."}
  }
]' --metadata '{"source":"session","key":"id","value":"math-error-001"}' \
   --metadata '{"source":"debug","key":"error_handled","value":"true"}'
```

### API Error
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Get the latest news"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "news_api",
      "args": {"category": "general", "limit": 5}
    }
  },
  {
    "order": 2,
    "kind": "system",
    "role": "system",
    "payload": {
      "error": "API rate limit exceeded",
      "error_code": "RATE_LIMIT_001",
      "retry_after": 300
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I am currently unable to fetch the latest news due to API rate limits. Please try again in a few minutes."}
  }
]' --metadata '{"source":"api","key":"rate_limited","value":"true"}'
```

## Complex Multi-Turn Scenarios

### Travel Planning
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "I need to book a flight from San Francisco to Tokyo and check the weather there"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "search_flights",
      "args": {"from": "SFO", "to": "NRT", "date": "2024-03-15"}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Found 3 flights: UA 838 ($1,200), ANA 106 ($1,150), JAL 002 ($1,300)"
    }
  },
  {
    "order": 3,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "get_weather",
      "args": {"city": "Tokyo", "units": "celsius"}
    }
  },
  {
    "order": 4,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Tokyo weather: 18°C, overcast with occasional rain"
    }
  },
  {
    "order": 5,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I found several flight options for you from San Francisco to Tokyo on March 15th. The best value is ANA 106 at $1,150. The weather in Tokyo will be 18°C and overcast with occasional rain, so pack accordingly!"}
  }
]' --metadata '{"source":"session","key":"id","value":"travel-001"}' \
   --metadata '{"source":"user","key":"preference","value":"economy"}' \
   --metadata '{"source":"booking","key":"agent_id","value":"agent-42"}'
```

### Customer Support
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "system",
    "role": "system",
    "payload": {"text": "User session started. Context: Customer support for premium account."}
  },
  {
    "order": 1,
    "kind": "user",
    "role": "user",
    "payload": {"text": "I am having trouble accessing my premium features"}
  },
  {
    "order": 2,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I understand you are having trouble with your premium features. Let me check your account status."}
  },
  {
    "order": 3,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "check_account_status",
      "args": {"user_id": "premium_user_123"}
    }
  },
  {
    "order": 4,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Account status: Active Premium, expires 2024-12-31, all features enabled"
    }
  },
  {
    "order": 5,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Your premium account is active and all features should be available. Could you tell me which specific feature you are trying to access?"}
  },
  {
    "order": 6,
    "kind": "user",
    "role": "user",
    "payload": {"text": "I cannot access the advanced analytics dashboard"}
  },
  {
    "order": 7,
    "kind": "system",
    "role": "system",
    "payload": {"text": "Escalating to technical support team due to feature access issue."}
  }
]' --metadata '{"source":"session","key":"id","value":"support-001"}' \
   --metadata '{"source":"user","key":"tier","value":"premium"}' \
   --metadata '{"source":"support","key":"category","value":"technical"}' \
   --metadata '{"source":"support","key":"priority","value":"high"}'
```

## Querying and Search

### Find by Metadata
```bash
# Find all turns with session metadata
./turn-inspector query turns --metadata-key session

# Find specific session
./turn-inspector query turns --metadata-key session --metadata-value "weather-001"

# Find premium users
./turn-inspector query turns --metadata-key tier --metadata-value premium
```

### Search by Content
```bash
# Find turns containing "weather"
./turn-inspector query turns --text weather

# Find turns containing "error"
./turn-inspector query turns --text error

# Find turns containing "flight"
./turn-inspector query turns --text flight
```

### Filter by Block Type
```bash
# Find all turns with tool calls
./turn-inspector query turns --block-kind tool_call

# Find all turns with system messages
./turn-inspector query turns --block-kind system

# Find all user interactions
./turn-inspector query turns --block-kind user
```

### Combined Queries
```bash
# Find tool calls containing "weather"
./turn-inspector query turns --block-kind tool_call --text weather

# Find error-handled sessions
./turn-inspector query turns --metadata-key error_handled --metadata-value true
```

## Data Analysis

### View Turn Details
```bash
# Show complete turn information
./turn-inspector show turn --id 1

# Show turn in JSON format
./turn-inspector show turn --id 1 --json

# Show only blocks for a turn
./turn-inspector show blocks --turn-id 1
```

### Database Statistics
```bash
# Basic statistics
./turn-inspector stats

# Detailed statistics with breakdowns
./turn-inspector stats --detailed
```

### Export Data
```bash
# Export turn as JSON
./turn-inspector show turn --id 1 --json > turn-1.json

# Export all turns (requires scripting)
for id in $(./turn-inspector list turns | tail -n +3 | awk '{print $1}'); do
  ./turn-inspector show turn --id $id --json > "turn-${id}.json"
done
```

## Advanced Use Cases

### Code Assistance Conversation
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Help me write a Python function to reverse a string"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {
      "text": "I will help you write a Python function to reverse a string. Here is a simple implementation:",
      "code": "def reverse_string(s):\n    return s[::-1]\n\n# Example usage\nresult = reverse_string(\"hello\")\nprint(result)  # Output: \"olleh\""
    }
  },
  {
    "order": 2,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Can you show me an alternative method?"}
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {
      "text": "Certainly! Here is an alternative method using a loop:",
      "code": "def reverse_string_loop(s):\n    reversed_str = \"\"\n    for char in s:\n        reversed_str = char + reversed_str\n    return reversed_str"
    }
  }
]' --metadata '{"source":"session","key":"id","value":"code-001"}' \
   --metadata '{"source":"user","key":"skill_level","value":"beginner"}' \
   --metadata '{"source":"topic","key":"language","value":"python"}'
```

### Multi-Language Support
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Bonjour, comment allez-vous?", "language": "fr"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Bonjour! Je vais bien, merci. Comment puis-je vous aider?", "language": "fr"}
  },
  {
    "order": 2,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Can you translate that to English?", "language": "en"}
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Of course! I said: \"Hello! I am doing well, thank you. How can I help you?\"", "language": "en"}
  }
]' --metadata '{"source":"language","key":"primary","value":"fr"}' \
   --metadata '{"source":"language","key":"secondary","value":"en"}'
```

### Conversation with Attachments
```bash
./turn-inspector create turn --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {
      "text": "Can you analyze this image?",
      "attachments": [
        {"type": "image", "url": "https://example.com/image.jpg", "description": "Chart showing sales data"}
      ]
    }
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "image_analysis",
      "args": {"image_url": "https://example.com/image.jpg"}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Chart shows quarterly sales data with 15% growth in Q3"
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I can see this is a sales chart showing quarterly data. The chart indicates a 15% growth in Q3, which is a positive trend."}
  }
]' --metadata '{"source":"content","key":"type","value":"image_analysis"}'
```

## Cleanup Operations

### Delete Specific Turn
```bash
# Delete turn with confirmation prompt
./turn-inspector delete turn --id 1

# Delete turn without confirmation
./turn-inspector delete turn --id 1 --confirm
```

### Delete All Data
```bash
# Delete all turns (requires confirmation)
./turn-inspector delete all --confirm
```

### Reset Database
```bash
# Remove database file and start fresh
rm turns.db
./turn-inspector stats  # This will recreate the database
```

## Batch Operations

### Create Multiple Turns from Script
```bash
#!/bin/bash
# Create multiple test turns

for i in {1..5}; do
  ./turn-inspector create turn --blocks "[
    {\"order\": 0, \"kind\": \"user\", \"role\": \"user\", \"payload\": {\"text\": \"Test message $i\"}},
    {\"order\": 1, \"kind\": \"llm_text\", \"role\": \"assistant\", \"payload\": {\"text\": \"Response to test $i\"}}
  ]" --metadata "{\"source\":\"batch\",\"key\":\"number\",\"value\":\"$i\"}"
done
```

### Query and Process Results
```bash
#!/bin/bash
# Find all tool_call turns and show their details

./turn-inspector query turns --block-kind tool_call | tail -n +3 | while read line; do
  id=$(echo $line | awk '{print $1}')
  echo "=== Turn $id ==="
  ./turn-inspector show turn --id $id
  echo
done
```

These examples demonstrate the full range of capabilities available in the Turn Inspector CLI tool. Use them as templates for your own conversation data management needs.

